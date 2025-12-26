#!/usr/bin/env python3
"""
PDF Highlighter (Simplified + Targeted Upgrades)
- Direct variables + supporting quotes
- Robust normalization, numeric-aware variants, windowed chunks for long text
- Token-span fallback and city aliases
- Safe search wrapper across PyMuPDF variants; reference section skip
- Title-only, empty-content annotations
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
from datetime import datetime
from typing import List, Dict, Optional, Tuple
import unicodedata

class PDFHighlighterPlus:
    def __init__(self):
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        self.logger = logging.getLogger(__name__)

        # Direct variables to attempt literal highlighting
        self.DIRECT_VARS = {
            'Country', 'City', 'State_or_Region', 'Data_Collection_Period',
            'Crime_Type', 'Model_Type', 'Data_Sources',
            'Number_of_Data_Sources', 'Crime_Incidents', 'Software_Used'
        }

        # City aliases (lookup in lowercase)
        self.CITY_ALIASES: Dict[str, List[str]] = {
            'the hague': ["The Hague", "'s-Gravenhage", "Den Haag"],
        }

        # Colors
        self.COLORS = {
            'location': [1, 0.8, 0],
            'temporal': [0, 0.8, 1],
            'crime': [1, 0.4, 0.4],
            'method': [0.6, 0.8, 1],
            'rationale': [1, 1, 0.4],
            'analysis': [0.8, 1, 0.6],
            'future': [0.9, 0.7, 1],
            'spatial': [0, 1, 0.8]
        }

    def get_variable_color(self, variable_name: str):
        mapping = {
            'Country': 'location', 'City': 'location', 'State_or_Region': 'location',
            'Data_Collection_Period': 'temporal',
            'Crime_Type': 'crime', 'Crime_Incidents': 'crime', 'Crime_Type_Group': 'crime',
            'Model_Type': 'method', 'Data_Sources': 'method', 'Number_of_Data_Sources': 'method', 'Software_Used': 'method',
            'Unit_Selection_Rationale': 'rationale',
            'MAUP_Discussion': 'analysis', 'Sensitivity_Analysis': 'analysis', 'Data_Limitations': 'analysis',
            'Future_Suggestions': 'future',
            'Spatial_Unit_Name': 'spatial', 'Unit_Size': 'spatial'
        }
        return self.COLORS.get(mapping.get(variable_name, 'method'))

    # --------- normalization helpers ---------
    def normalize_text(self, text: str) -> str:
        if text is None:
            return ""
        t = unicodedata.normalize('NFKC', str(text))
        t = t.replace('\u00A0', ' ').replace('\u00AD', '')  # nbsp, soft hyphen
        t = t.replace('“', '"').replace('”', '"').replace('‘', "'").replace('’', "'")
        t = t.replace('–', '-').replace('—', '-')
        t = re.sub(r"\s+", " ", t).strip()
        return t

    def clean_cell_value(self, s: str) -> str:
        s = self.normalize_text(s or "")
        s = re.sub(r'[“”]', '"', s)
        s = re.sub(r"\s+", " ", s).strip()
        s = re.sub(r"[.。…]+$", "", s)  # drop trailing dots / ellipsis
        return s

    def normalize_for_matching(self, text: str) -> str:
        """
        Aggressive normalization for cross-source matching:
        - NFKC normalize (maps superscripts like ² to 2)
        - Remove NBSP and soft hyphen
        - Keep only [A-Za-z0-9], lowercase
        Example: "2.96 km 2" -> "296km2"; "co-offended" -> "cooffended"
        """
        if text is None:
            return ""
        t = unicodedata.normalize('NFKC', str(text))
        t = t.replace('\u00A0', ' ').replace('\u00AD', '')
        t = re.sub(r"[^0-9A-Za-z]+", "", t)
        return t.lower()

    # Unit/superscript helpers (handle km2/km 2/km² and similar)
    def normalize_units_for_search(self, s: str) -> str:
        if not s:
            return s
        t = s
        # Normalize superscripts to plain digits first
        t = t.replace("km²", "km2").replace("m²", "m2").replace("cm²", "cm2")
        t = re.sub(r"\b(km|m|cm)\s*\^\s*2\b", lambda m: m.group(1) + "2", t, flags=re.IGNORECASE)
        # Collapse optional spaces between unit and 2
        t = re.sub(r"\b(km|m|cm)\s*2\b", lambda m: m.group(1) + "2", t, flags=re.IGNORECASE)
        return t

    def unit_variants(self, s: str) -> List[str]:
        if not s:
            return []
        variants = set()
        base = str(s)
        variants.add(base)
        # Prepare replacements across a few common area units
        unit_patterns = [
            (r"\b(km)\s*2\b", "km2", "km 2", "km²"),
            (r"\b(m)\s*2\b", "m2", "m 2", "m²"),
            (r"\b(cm)\s*2\b", "cm2", "cm 2", "cm²"),
        ]
        # From spaced to no-space/superscript
        for pat, nospace, spaced, super2 in unit_patterns:
            if re.search(pat, base, flags=re.IGNORECASE):
                variants.add(re.sub(pat, nospace, base, flags=re.IGNORECASE))
                variants.add(re.sub(pat, super2, base, flags=re.IGNORECASE))
        # From nospace to spaced/superscript
        for unit, nospace, spaced, super2 in [("km", "km2", "km 2", "km²"), ("m", "m2", "m 2", "m²"), ("cm", "cm2", "cm 2", "cm²")]:
            if re.search(rf"\b{re.escape(nospace)}\b", base, flags=re.IGNORECASE):
                variants.add(re.sub(rf"\b{re.escape(nospace)}\b", spaced, base, flags=re.IGNORECASE))
                variants.add(re.sub(rf"\b{re.escape(nospace)}\b", super2, base, flags=re.IGNORECASE))
        # From superscript to other forms
        for unit, super2, nospace, spaced in [("km", "km²", "km2", "km 2"), ("m", "m²", "m2", "m 2"), ("cm", "cm²", "cm2", "cm 2")]:
            if super2 in base:
                variants.add(base.replace(super2, nospace))
                variants.add(base.replace(super2, spaced))
        # Also provide a fully normalized form where we collapse to nospace
        variants.add(self.normalize_units_for_search(base))
        # Clean up and dedupe
        cleaned = []
        seen = set()
        for v in variants:
            vv = self.clean_cell_value(v)
            if vv and vv.lower() not in seen:
                seen.add(vv.lower()); cleaned.append(vv)
        return cleaned[:12]

    def window_terms(self, s: str, sizes=(12, 10, 8, 6)) -> List[str]:
        words = self.clean_cell_value(s).split()
        out: List[str] = []
        for w in sizes:
            if len(words) >= w:
                out.append(" ".join(words[:w]))
        return out or ([self.clean_cell_value(s)] if s else [])

    # --------- numeric helpers ---------
    def number_variants(self, s: str) -> List[str]:
        out = {s}
        out.add(re.sub(r"[,\u2009\u202F]", "", s))
        out.add(s.replace(",", "\u2009"))
        out.add(s.replace(",", "\u202F"))
        out.add(s.replace(",", " "))
        return [t for t in out if t]

    def number_regex(self, s: str) -> Optional[str]:
        digits_only = re.sub(r"[^\d]", "", s)
        if not digits_only or len(digits_only) < 2:
            return None
        parts = re.findall(r"\d{1,3}", digits_only)
        if len(parts) < 2:
            return None
        core = r"[ ,\u2009\u202F]?"
        return r"(?<!\d)" + core.join(parts) + r"(?!\d)"

    # --------- search helpers ---------
    def safe_search(self, page: fitz.Page, text: str, flags: int = 1) -> List[fitz.Rect]:
        if not isinstance(text, str) or not text.strip():
            return []
        # Try positional signature
        try:
            return page.search_for(text, 64, False, flags) or []
        except TypeError:
            pass
        except Exception:
            pass
        # Try kwargs signature
        try:
            return page.search_for(text, flags=flags) or []
        except Exception:
            return []

    def lite(self, term: str) -> str:
        return re.sub(r"[^0-9A-Za-z\s]+", " ", term).strip()

    def regex_from_phrase(self, s: str) -> str:
        toks = [re.escape(t) for t in s.split()]
        if not toks:
            return ""
        return r"\b" + r"[ \-\u2010\u2011\u2012\u2013\u2014]+".join(toks) + r"\b"

    def tokenize(self, s: str) -> List[str]:
        # Keep key 2-letter units (e.g., km) while dropping common stopwords
        stop = {"of", "in", "to", "and", "the", "for", "on", "an", "or"}
        s = self.normalize_units_for_search(s)
        toks = [w for w in re.findall(r"[A-Za-z0-9]+", s.lower()) if len(w) >= 2]
        return [w for w in toks if w not in stop]

    def token_span_search(self, page: fitz.Page, tokens: List[str]) -> Optional[fitz.Rect]:
        try:
            flags = getattr(fitz, "TEXT_DEHYPHENATE", 0)
            wrds = page.get_text("words", flags=flags) or []
            wrds.sort(key=lambda w: (round(w[1], 1), w[0]))
            words = [(fitz.Rect(w[0], w[1], w[2], w[3]), (w[4] or '').lower()) for w in wrds if len(w) >= 5]
            n, m = len(words), len(tokens)
            if m < 2 or n == 0:
                return None
            i = 0
            while i < n:
                if words[i][1] != tokens[0]:
                    i += 1; continue
                used = [words[i][0]]
                j = i + 1
                k = 1
                steps = 0
                while j < n and k < m and steps <= 6 * m:
                    if words[j][1] == tokens[k]:
                        used.append(words[j][0]); k += 1
                        if k == m:
                            x0 = min(r.x0 for r in used); y0 = min(r.y0 for r in used)
                            x1 = max(r.x1 for r in used); y1 = max(r.y1 for r in used)
                            if x1 > x0 and y1 > y0:
                                return fitz.Rect(x0, y0, x1, y1)
                            return None
                    j += 1; steps += 1
                i += 1
        except Exception:
            return None
        return None

    def find_by_normalized_stream(self, page: fitz.Page, term: str) -> Optional[Tuple[fitz.Rect, List[fitz.Rect]]]:
        """
        Build a page-wide normalized character stream from words and search for a
        normalized query. Maps the substring match back to start/end words and returns
        the bounding rectangle spanning those words. Handles line breaks, hyphenation,
        punctuation differences, and unit spacing (km2/km 2/km²).
        """
        try:
            q = self.normalize_for_matching(term)
            if not q or len(q) < 6:
                return None
            flags = getattr(fitz, "TEXT_DEHYPHENATE", 0)
            wrds = page.get_text("words", flags=flags) or []
            if not wrds:
                return None
            # Prefer logical order if available, else fallback to y/x
            if len(wrds[0]) >= 8:
                wrds.sort(key=lambda w: (w[5], w[6], w[7], w[0], w[1]))
            else:
                wrds.sort(key=lambda w: (round(w[1], 1), w[0]))
            stream = []
            spans = []  # list of (start, end, rect)
            total = 0
            for w in wrds:
                if len(w) < 5:
                    continue
                word_text = w[4] or ""
                norm = self.normalize_for_matching(word_text)
                if not norm:
                    continue
                stream.append(norm)
                start = total
                total += len(norm)
                end = total
                rect = fitz.Rect(w[0], w[1], w[2], w[3])
                spans.append((start, end, rect))
            if not stream:
                return None
            s = "".join(stream)
            pos = s.find(q)
            if pos < 0:
                return None
            pos_end = pos + len(q)
            # Find first span with end > pos
            i_start = None
            for i, (st, en, _) in enumerate(spans):
                if en > pos:
                    i_start = i
                    break
            if i_start is None:
                return None
            # Find last span with start < pos_end
            i_end = i_start
            for j in range(i_start, len(spans)):
                st, en, _ = spans[j]
                if st < pos_end:
                    i_end = j
                else:
                    break
            used = [spans[k][2] for k in range(i_start, i_end + 1)]
            x0 = min(r.x0 for r in used); y0 = min(r.y0 for r in used)
            x1 = max(r.x1 for r in used); y1 = max(r.y1 for r in used)
            if x1 > x0 and y1 > y0:
                return fitz.Rect(x0, y0, x1, y1), used
            return None
        except Exception:
            return None

    def find_reference_heading_y(self, page: fitz.Page) -> Optional[float]:
        pats = (r"^\s*references\s*$", r"^\s*bibliograph(?:y|ies)\s*$", r"^\s*works\s+cited\s*$")
        lines = (page.get_text("text") or "").splitlines()[:40]
        for ln in lines:
            t = ln.strip().lower()
            if any(re.match(p, t) for p in pats):
                return page.rect.height * 0.20
        return None

    def find_first_on_page(self, page: fitz.Page, term: str) -> Optional[fitz.Rect]:
        term = (term or "").strip()
        if len(term) < 3:
            return None
        if term.lower() in {"not", "and", "the", "of", "in"}:
            return None
        SFLAGS = 1
        # Build a small candidate list covering unit variants
        cand_terms: List[str] = []
        seen = set()
        for c in [term] + self.unit_variants(term):
            cc = c.strip()
            if cc and cc.lower() not in seen:
                seen.add(cc.lower()); cand_terms.append(cc)
        # 1) literal against candidates
        for cand in cand_terms:
            hits = self.safe_search(page, cand, flags=SFLAGS)
            if hits:
                return hits[0]
        # 2) numeric
        for cand in cand_terms:
            if re.search(r"\d[, \u2009\u202F]\d", cand):
                for nv in self.number_variants(cand):
                    hits = self.safe_search(page, nv, flags=SFLAGS)
                    if hits:
                        return hits[0]
                pat = self.number_regex(cand)
                if pat:
                    hits = self.safe_search(page, pat, flags=SFLAGS | 4)
                    if hits:
                        return hits[0]
        # 3) regex phrase
        for cand in cand_terms:
            pattern = self.regex_from_phrase(self.clean_cell_value(cand))
            if pattern:
                hits = self.safe_search(page, pattern, flags=SFLAGS | 4)
                if hits:
                    return hits[0]
        # 4) normalized-stream fallback (handles km2/km 2/km², hyphenation, line breaks)
        r_norm = self.find_by_normalized_stream(page, term)
        if r_norm:
            if isinstance(r_norm, tuple):
                return r_norm[0]
            return r_norm
        # 5) windows for long text
        words = term.split()
        if len(words) > 12:
            for w in (12, 10, 8):
                for i in range(len(words) - w + 1):
                    chunk = " ".join(words[i:i + w])
                    unit_chunks = {chunk, self.lite(chunk)}
                    # Add unit variants for chunks
                    for uc in list(unit_chunks):
                        unit_chunks.update(self.unit_variants(uc))
                    for cand in unit_chunks:
                        hits = self.safe_search(page, cand, flags=SFLAGS)
                        if hits:
                            return hits[0]
        # 6) token span
        tok = self.tokenize(term)
        if len(tok) >= 2:
            r = self.token_span_search(page, tok)
            if r:
                return r
        return None

    def expand_to_multiline_rects(self, page: fitz.Page, term: str, base_rect: Optional[fitz.Rect]) -> Optional[List[fitz.Rect]]:
        """If the matched term spans multiple lines, return the list of rects for the full span.
        Uses the normalized-stream matcher and prefers a span that overlaps the base_rect when provided.
        """
        try:
            rs = self.find_by_normalized_stream(page, term)
            if not rs:
                return None
            rect, rects = rs
            if base_rect is None:
                return rects if rects else None
            # Check overlap; require some intersection to accept expansion
            if rect.intersects(base_rect):
                # If single-line, no need to expand
                if len(rects) <= 1:
                    return None
                return rects
            return None
        except Exception:
            return None

    def find_document_wide(self, doc: fitz.Document, term: str, max_pages: Optional[int] = None) -> Optional[Tuple[int, fitz.Rect]]:
        page_range = range(min(max_pages, len(doc))) if max_pages else range(len(doc))
        for p in page_range:
            page = doc[p]
            yref = self.find_reference_heading_y(page)
            if yref is not None and p > 0:
                break
            r = self.find_first_on_page(page, term)
            if r:
                return p, r
        return None

    # --------- quote extraction ---------
    def extract_quotes(self, text: str) -> List[str]:
        if not text or pd.isna(text):
            return []
        t = self.normalize_text(text)
        out: List[str] = []
        # Quotes inside double quotes
        out.extend([q.strip() for q in re.findall(r'"([^"]*)"', t) if q.strip()])
        # Inline dash-delimited quotes: "..." - "..."
        if '" - "' in t:
            parts = [p.strip(' "') for p in t.split('" - "') if p.strip(' "')]
            out.extend([p for p in parts if len(p) > 10])
        # Bullets/newlines
        if not out:
            lines = re.split(r'[\r\n]+|\s*[\-•*]\s+', t)
            out.extend([ln.strip(' "') for ln in lines if ln and len(ln.strip()) > 10])
        # Fallback whole
        if not out and len(t) > 10:
            out.append(t)
        # Dedupe
        seen = set(); ans = []
        for q in out:
            k = q.lower()
            if k not in seen:
                seen.add(k); ans.append(q)
        return ans[:20]

    # --------- variant generation for direct vars ---------
    def direct_variants(self, value: str, var_name: str) -> List[str]:
        if not value:
            return []
        raw = str(value)
        t = self.clean_cell_value(raw)
        ans: List[str] = []
        # City aliases
        if var_name == 'City':
            base = t.lower()
            if base in self.CITY_ALIASES:
                ans.extend(self.CITY_ALIASES[base])
        # Period variants like 2006-2009 vs 2006–2009, and phrases
        if var_name == 'Data_Collection_Period':
            years = re.findall(r"(19|20)\d{2}", t)
            rng = re.findall(r"((?:19|20)\d{2})\s*[\-–—]\s*((?:19|20)\d{2})", t)
            if rng:
                a, b = rng[0]
                ans += [f"{a}-{b}", f"{a}–{b}", f"{a}—{b}", f"{a} to {b}", f"between {a} and {b}"]
            ans.append(t)
        else:
            ans.append(t)
            if '-' in t:
                ans.append(t.replace('-', '–'))
            if '–' in t:
                ans.append(t.replace('–', '-'))
        # Trailing ellipsis -> add windows
        if re.search(r"[.。…]\s*$", raw):
            ans = self.window_terms(raw) + ans
        # Deduplicate
        seen = set(); out: List[str] = []
        for v in ans:
            v = self.clean_cell_value(v)
            if v and v.lower() not in seen:
                seen.add(v.lower()); out.append(v)
        return out[:10]

    # --------- annotation ---------
    def highlight(self, page: fitz.Page, rect_or_rects, color, variable_name: str, value: str) -> bool:
        try:
            annot = None
            # Support multi-line highlights via quads if a list of rects is provided
            if isinstance(rect_or_rects, list):
                try:
                    quads = [fitz.Quad(r) for r in rect_or_rects if isinstance(r, fitz.Rect)]
                    if quads:
                        annot = page.add_highlight_annot(quads)
                except Exception as _:
                    annot = None
            if annot is None:
                # Fallback to single rect
                rect = rect_or_rects if isinstance(rect_or_rects, fitz.Rect) else None
                if rect is None and isinstance(rect_or_rects, list) and rect_or_rects:
                    rect = rect_or_rects[0]
                annot = page.add_highlight_annot(rect)
            annot.set_colors(stroke=color)
            annot.set_opacity(0.7)
            try:
                annot.set_info(title=variable_name)
                try:
                    annot.set_contents("")
                except AttributeError:
                    try:
                        annot.set_content("")
                    except Exception:
                        pass
            except Exception:
                pass
            annot.update()
            return True
        except Exception as e:
            self.logger.error(f"Highlight error: {e}")
            return False

    # --------- driver ---------
    def process_study(self, study_id, pdf_folder="Review_articles", combined_csv=None):
        try:
            self.logger.info(f"Processing Study {study_id}")
            if combined_csv is None:
                combined_csv = "20250820_Analysis & Results/20250820_combined_dataset.csv"
            df = pd.read_csv(combined_csv)
            rowq = df[df['Study_ID'] == int(study_id)]
            if rowq.empty:
                raise ValueError(f"Study {study_id} not found")
            row = rowq.iloc[0]
            # Locate PDF
            patterns = [
                f"*{int(study_id):02d}*.pdf", f"*{study_id}*.pdf", f"*{study_id}_*.pdf",
                (f"0{study_id}_*.pdf" if int(study_id) < 10 else f"{study_id}_*.pdf"),
            ]
            pdf_file: Optional[Path] = None
            for pat in patterns:
                m = list(Path(pdf_folder).glob(pat))
                if m:
                    pdf_file = m[0]; break
            if not pdf_file or not pdf_file.exists():
                raise FileNotFoundError(f"PDF not found for study {study_id}")
            doc = fitz.open(pdf_file)
            self.logger.info(f"Opened PDF: {pdf_file} ({len(doc)} pages)")
            highlights = 0
            # Direct variables (first occurrence only)
            for var_name in self.DIRECT_VARS:
                if var_name not in row or pd.isna(row[var_name]):
                    continue
                val = str(row[var_name]).strip()
                if not val:
                    continue
                variants = self.direct_variants(val, var_name)
                found = False
                for v in variants:
                    hit = self.find_document_wide(doc, v)
                    if hit:
                        p, rect = hit
                        color = self.get_variable_color(var_name)
                        # Expand to multi-line rects when applicable
                        rects = self.expand_to_multiline_rects(doc[p], v, rect)
                        target = rects if rects else rect
                        if self.highlight(doc[p], target, color, var_name, v):
                            highlights += 1
                            found = True
                            break
            # Supporting quotes (first occurrence per quote, search up to first 10 pages)
            quote_cols = [c for c in row.index if c.startswith('Supporting_quotes_for__')]
            for qcol in quote_cols:
                if pd.isna(row[qcol]) or not row[qcol]:
                    continue
                var_name = qcol.replace('Supporting_quotes_for__', '').rstrip('_')
                quotes = self.extract_quotes(row[qcol])
                for q in quotes:
                    # try full, then windows
                    hit = self.find_document_wide(doc, q, max_pages=10)
                    if not hit:
                        for chunk in self.window_terms(q, sizes=(12, 10, 8, 6)):
                            hit = self.find_document_wide(doc, chunk, max_pages=10)
                            if hit:
                                break
                    if hit:
                        p, rect = hit
                        color = self.get_variable_color(var_name)
                        rects = self.expand_to_multiline_rects(doc[p], q, rect)
                        target = rects if rects else rect
                        if self.highlight(doc[p], target, color, var_name, q):
                            highlights += 1
            # Save
            outdir = Path(f"{datetime.now().strftime('%Y%m%d')}_Highlighted_PDFs")
            outdir.mkdir(exist_ok=True)
            out = outdir / f"Study_{study_id}_Highlighted_PLUS.pdf"
            doc.save(out)
            doc.close()
            self.logger.info(f"Saved: {out}")
            self.logger.info(f"Total highlights created: {highlights}")
            return {"success": True, "highlights_created": highlights, "output_file": str(out)}
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {"success": False, "error": str(e)}


def main():
    if len(sys.argv) < 2:
        print("Usage: python pdf_highlighter_plus.py <study_id> [pdf_folder] [combined_csv]")
        sys.exit(1)
    study_id = sys.argv[1]
    pdf_folder = sys.argv[2] if len(sys.argv) > 2 else "Review_articles"
    combined_csv = sys.argv[3] if len(sys.argv) > 3 else "20250820_Analysis & Results/20250820_combined_dataset.csv"
    hl = PDFHighlighterPlus()
    result = hl.process_study(study_id, pdf_folder, combined_csv)
    if result.get('success'):
        print(f"✅ Successfully processed Study {study_id}")
        print(f"   Highlights created: {result.get('highlights_created', 0)}")
        print(f"   Output file: {result.get('output_file', 'unknown')}")
    else:
        print(f"❌ Failed: {result.get('error')}")

if __name__ == "__main__":
    main()
