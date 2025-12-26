#!/usr/bin/env python3
"""
Enhanced Hybrid Precision Quote Highlighter
Combines direct variable values with supporting quotes for targeted PDF highlighting.
- Only highlights first occurrence 
- Excludes reference sections
- Proper supporting quote mapping
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
import math
from datetime import date
from typing import Dict, List, Optional
import time
import unicodedata

class EnhancedHybridHighlighter:
    def __init__(self):
        # Configure console + file logging for easier debugging
        self.logger = logging.getLogger(__name__)
        if not self.logger.handlers:
            self.logger.setLevel(logging.DEBUG)
            fmt = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
            ch = logging.StreamHandler()
            ch.setLevel(logging.INFO)
            ch.setFormatter(fmt)
            fh = logging.FileHandler('highlighter_debug.log', encoding='utf-8')
            fh.setLevel(logging.DEBUG)
            fh.setFormatter(fmt)
            self.logger.addHandler(ch)
            self.logger.addHandler(fh)
        
        # City aliases (helps matching alternate/localized names) - store key in lowercase
        self.CITY_ALIASES = {
            "the hague": ["The Hague", "'s-Gravenhage", "Den Haag"],
        }

        # Variables that use direct values (simple highlighting)
        self.SKIP_SUPPORTING = {
            'Country', 'City', 'State_or_Region', 'Data_Collection_Period',
            'Crime_Type', 'Model_Type', 'Data_Sources',
            'Number_of_Data_Sources', 'Crime_Incidents', 'Software_Used'
        }
        
        # Direct variables that must match the cell verbatim (no splitting into pieces).
        self.EXACT_MATCH_VARS = {
            'Country', 'City', 'State_or_Region',
            'Crime_Type', 'Crime_Type_Group',
            'Crime_Incidents',
            'Model_Type', 'Software_Used',
            'Spatial_Unit_Name', 'Unit_Size',
            'Data_Sources'  # highlight the full list string as printed in the cell
        }
        
        # Clean variable names for annotations (no underscores)
        self.VARIABLE_DISPLAY_NAMES = {
            'Country': 'Country',
            'City': 'City', 
            'State_or_Region': 'State Region',
            'Data_Collection_Period': 'Data Collection Period',
            'Crime_Type': 'Crime Type',
            'Model_Type': 'Model Type',
            'Data_Sources': 'Data Sources',
            'Number_of_Data_Sources': 'Number of Data Sources',
            'Crime_Incidents': 'Crime Incidents',
            'Software_Used': 'Software Used',
            'Unit_Selection_Rationale': 'Unit Selection Rationale',
            'MAUP_Discussion': 'MAUP Discussion',
            'Sensitivity_Analysis': 'Sensitivity Analysis',
            'Future_Suggestions': 'Future Suggestions',
            'Data_Limitations': 'Data Limitations',
            'Crime_Type_Group': 'Crime Type Group',
            'Spatial_Unit_Name': 'Spatial Unit',
            'Unit_Size': 'Unit Size'
        }
        
        # Color scheme for different variable types
        self.COLORS = {
            'location': [1, 0.8, 0],      # Orange - Country, City, State
            'temporal': [0, 0.8, 1],      # Light Blue - Data collection period
            'crime': [1, 0.4, 0.4],       # Light Red - Crime type, incidents
            'method': [0.6, 0.8, 1],      # Light Blue - Model type, data sources, software
            'rationale': [1, 1, 0.4],     # Light Yellow - Unit selection rationale
            'analysis': [0.8, 1, 0.6],    # Light Green - MAUP, sensitivity analysis
            'future': [0.9, 0.7, 1],      # Light Purple - Future suggestions
            'spatial': [0, 1, 0.8]        # Turquoise - Spatial units
        }
        
        # Variables that use STRICT exact matching only (no variants, no splitting)
        self.STRICT_EXACT = {
            'Country', 'City', 'State_or_Region', 'Crime_Type',
            'Crime_Type_Group', 'Spatial_Unit_Name', 'Unit_Size',
            'Number_of_Units', 'Crime_Incidents', 'Number_of_Data_Sources',
            'Data_Collection_Period', 'Model_Type', 'Software_Used'
        }
        
    def get_variable_color(self, variable_name):
        """Get color for variable based on type."""
        color_mapping = {
            'Country': 'location', 'City': 'location', 'State_or_Region': 'location',
            'Data_Collection_Period': 'temporal',
            'Crime_Type': 'crime', 'Crime_Incidents': 'crime', 'Crime_Type_Group': 'crime',
            'Model_Type': 'method', 'Data_Sources': 'method', 'Number_of_Data_Sources': 'method', 'Software_Used': 'method',
            'Unit_Selection_Rationale': 'rationale',
            'MAUP_Discussion': 'analysis', 'Sensitivity_Analysis': 'analysis', 'Data_Limitations': 'analysis',
            'Future_Suggestions': 'future',
            'Spatial_Unit_Name': 'spatial', 'Unit_Size': 'spatial'
        }
        return self.COLORS.get(color_mapping.get(variable_name, 'method'), self.COLORS['method'])
    
    def _find_reference_heading_y(self, page) -> Optional[float]:
        """Find References section to exclude it from highlighting."""
        pats = (r"^\s*references\s*$",
                r"^\s*bibliograph(?:y|ies)\s*$",
                r"^\s*works\s+cited\s*$")

        try:
            d = page.get_text("dict")
            for b in d.get("blocks", []):
                for l in b.get("lines", []):
                    raw = " ".join(s.get("text", "") for s in l.get("spans", []))
                    t = raw.strip().lower()
                    if any(re.match(p, t) for p in pats):
                        return float(l["bbox"][1])  # y0 of the heading line
        except Exception:
            pass

        # Fallback: text mode scan
        lines = (page.get_text("text") or "").splitlines()[:20]
        for ln in lines:
            t = ln.strip().lower()
            if any(re.match(p, t) for p in pats):
                return page.rect.height * 0.20
        return None

    def _plural_variants(self, term: str) -> list:
        t = term.strip()
        outs = {t}
        mapping = {
            "burglary": ["burglary", "burglaries"],
            "robbery": ["robbery", "robberies"],
            "assault": ["assault", "assaults"],
            "homicide": ["homicide", "homicides"],
            "theft": ["theft", "thefts"],
        }
        for k, forms in mapping.items():
            if re.search(rf"\b{k}\b", t, re.I):
                for f in forms:
                    outs.add(re.sub(rf"\b{k}\b", f, t, flags=re.I))

        # hyphen/space swaps (create flat strings, not sets)
        swapped = set()
        for u in list(outs):
            swapped.add(u.replace("-", " "))
            swapped.add(u.replace(" ", "-"))
        outs.update(swapped)

        return list(outs)

    def _year_range_variants(self, s: str) -> list:
        """Generate robust patterns for year ranges and single years."""
        s = self._normalize_for_pdf(s)
        years = re.findall(r'\b((?:19|20)\d{2})\b', s)
        out = []
        if len(years) >= 2:
            a, b = years[0], years[1]
            out += [f"{a}-{b}", f"{a}–{b}", f"{a}—{b}", f"{a} to {b}", f"between {a} and {b}"]
        out += years  # also try each year alone
        # Include phrases around "data" if present
        out += [f"{y} data" for y in years]
        return list(dict.fromkeys(out))[:10]

    def _direct_term_variants(self, text: str, variable_name: str = "") -> List[str]:
        if not text:
            return []
        raw = str(text)
        ends_with_ellipsis = bool(re.search(r"[.。…]\s*$", raw))
        t = self._clean_cell_value(raw)
        if not t:
            return []

        # Reject noisy values like "Not mentioned..." for Software_Used
        if variable_name == "Software_Used" and t.lower().startswith(("not ", "n/a", "unspecified")):
            return []

        out = []

        # City aliases (case-insensitive)
        if variable_name == "City":
            base = t.lower()
            for k, vals in self.CITY_ALIASES.items():
                if base == k.lower():
                    out.extend(vals)

        # STRICT_EXACT: mostly exact, but allow hyphen/en-dash swap
        if variable_name in getattr(self, 'STRICT_EXACT', set()):
            out.append(t)
            if "-" in t: out.append(t.replace("-", "–"))
            if "–" in t: out.append(t.replace("–", "-"))

            # If the CSV had an ellipsis / truncation, use token windows
            if ends_with_ellipsis:
                out = self._window_terms(raw) + out

            # If very long, also try a couple of salient chunks (except Software_Used)
            if len(t) > 40 and variable_name != "Software_Used":
                m = re.findall(r"[A-Z][a-zA-Z]+(?:\s+[A-Z][a-zA-Z]+)*", raw)
                out.extend([s.strip() for s in m[:3] if 3 <= len(s.strip()) <= 40])
                nums = re.findall(r"\d[\d,.\u2009\u00A0]*", raw)
                out.extend([re.sub(r"[,\u2009\u00A0]", "", n) for n in nums[:2]])
        else:
            # Non-strict variables
            out.append(t)
            if variable_name == "Data_Collection_Period":
                out += self._year_range_variants(t)
            if "-" in t or "–" in t:
                out += [t.replace("-", "–"), t.replace("–", "-")]
            if ends_with_ellipsis:
                out = self._window_terms(raw) + out

        # De-dup & tidy
        seen, ans = set(), []
        for v in out:
            v = self._clean_cell_value(v)
            if v and v.lower() not in seen:
                seen.add(v.lower()); ans.append(v)
        return ans[:10]

    def _tokenize(self, s: str) -> List[str]:
        """Extract meaningful tokens from text."""
        return [w for w in re.findall(r"[A-Za-z0-9]+", s.lower()) if len(w) > 2]

    def _token_span_search(self, page, tokens: List[str], clip_rects=None, max_gap=6):
        """Find tokens in proximity on page, handling phrase/linebreak differences."""
        words = self._word_cache(page)  # (Rect, text, block, line)
        # restrict to clip rects if any
        if clip_rects:
            filt = []
            for wr, tx, b, l in words:
                if any(rect.intersects(wr) for rect in clip_rects):
                    filt.append((wr, tx.lower(), b, l))
            words = filt
        else:
            words = [(wr, tx.lower(), b, l) for (wr, tx, b, l) in words]

        n = len(words); m = len(tokens)
        if m < 2 or n == 0:
            return None

        i = 0
        while i < n:
            if words[i][1] != tokens[0]:
                i += 1; continue
            used = [words[i][0]]
            k = 1; j = i + 1; steps = 0
            while j < n and k < m and steps <= max_gap * m:
                if words[j][1] == tokens[k]:
                    used.append(words[j][0]); k += 1
                    if k == m:
                        try:
                            x0 = min(r.x0 for r in used); y0 = min(r.y0 for r in used)
                            x1 = max(r.x1 for r in used); y1 = max(r.y1 for r in used)
                            
                            # Safe rect construction - validate coordinates before creating rect
                            if not all(isinstance(coord, (int, float)) and not math.isnan(coord) and not math.isinf(coord) 
                                     for coord in [x0, y0, x1, y1]):
                                return None
                            if x1 <= x0 or y1 <= y0:
                                return None
                                
                            sr = self._safe_rect((x0, y0, x1, y1), page)
                            if sr:
                                # Return Rect; caller wraps as needed
                                return sr
                        except Exception as e:
                            self.logger.debug(f"Failed to create token span rect: {e}")
                            return None
                j += 1; steps += 1
            i += 1
        return None

    def _normalize_quotes(self, s: str) -> str:
        # unify curly quotes/apostrophes & dashes; remove NBSP/soft hyphen
        return (str(s)
                .replace(""", "\"").replace(""", "\"")
                .replace("'", "'").replace("'", "'")
                .replace("\u00A0", " ").replace("\u00AD", "")
                .replace("–", "-").replace("—", "-"))

    def _normalize_for_pdf(self, s: str) -> str:
        """Normalize CSV text for PDF matching (handle m², ×, curly quotes, stray Â)."""
        if s is None:
            return ""
        s = str(s)

        # unify quotes & dashes
        s = (s
             .replace(""", '"').replace(""", '"')
             .replace("'", "'").replace("'", "'")
             .replace("–", "-").replace("—", "-"))

        # strip non-breaking space & soft hyphen
        s = s.replace("\u00A0", " ").replace("\u00AD", "")

        # fix common glyphs seen in your CSV dump
        s = (s
             .replace("Â", "")        # stray encoding artifact
             .replace("²", "2")       # m² -> m2
             .replace("×", "x"))      # 3×3 -> 3x
        
        # collapse thin/nb spaces in numbers and after "The"
        s = s.replace("\u2009", " ").replace("\u202F", " ")

        # collapse whitespace
        s = re.sub(r"\s+", " ", s).strip()
        return s

    def _clean_cell_value(self, s: str) -> str:
        """Normalize a CSV cell value and strip trailing ellipses/punctuation."""
        s = self._normalize_for_pdf(s or "")
        s = re.sub(r'[“”]', '"', s)
        s = re.sub(r"\s+", " ", s).strip()
        s = re.sub(r"[.。…]+$", "", s)
        return s

    def _window_terms(self, s: str, sizes=(12, 10, 8, 6)) -> list:
        """Generate leading word windows to cope with truncated values (… at end)."""
        words = self._clean_cell_value(s).split()
        out = []
        for w in sizes:
            if len(words) >= w:
                out.append(" ".join(words[:w]))
        return out or ([self._clean_cell_value(s)] if s else [])

    def _word_cache(self, page):
        """Cache words for a page to avoid repeated extraction."""
        if not hasattr(page, "_words_cache"):
            try:
                FLAGS = getattr(fitz, "TEXT_DEHYPHENATE", 0)
                wrds = page.get_text("words", flags=FLAGS) or []
                # words: [x0,y0,x1,y1,"text", block_no, line_no, word_no]
                page._words_cache = [
                    (fitz.Rect(w[0], w[1], w[2], w[3]), w[4], w[5], w[6]) for w in wrds if len(w) >= 8
                ]
            except Exception as e:
                self.logger.debug(f"Error caching words: {e}")
                page._words_cache = []
        return page._words_cache

    def _safe_rect(self, r: fitz.Rect, page: Optional[fitz.Page] = None, min_w: float = 0.5, min_h: float = 0.5) -> Optional[fitz.Rect]:
        """Normalize and validate a rectangle to prevent PyMuPDF 'Point: bad args' errors.
        Accepts fitz.Rect or a 4-seq (x0,y0,x1,y1).
        """
        try:
            # Build rect from various inputs
            if hasattr(r, "x0") and hasattr(r, "y0") and hasattr(r, "x1") and hasattr(r, "y1"):
                rect = fitz.Rect(float(r.x0), float(r.y0), float(r.x1), float(r.y1))
            elif isinstance(r, (tuple, list)) and len(r) >= 4:
                rect = fitz.Rect(float(r[0]), float(r[1]), float(r[2]), float(r[3]))
            else:
                rect = fitz.Rect(r)
            rect.normalize()  # guarantees x0<=x1, y0<=y1
            # Clamp to page if provided
            if page is not None:
                rect = rect & page.rect
            # Reject degenerate / tiny rects
            if rect.width < min_w or rect.height < min_h:
                return None
            # Reject non-finite values
            for v in (rect.x0, rect.y0, rect.x1, rect.y1):
                if not (abs(v) < 1e10):  # crude finiteness check
                    return None
            return rect
        except Exception:
            return None

    def _rects_to_quads_safe(self, rects: List[fitz.Rect], page: Optional[fitz.Page] = None) -> List[fitz.Quad]:
        """Safely convert rectangles to quads, filtering invalid ones."""
        quads = []
        for r in rects:
            sr = self._safe_rect(r, page)
            if sr is None:
                continue
            try:
                quads.append(fitz.Quad(sr))
            except Exception:
                # As a last resort, skip the bad one
                continue
        return quads

    def _safe_search(self, page, text: str, flags: int = 1, clip: Optional[fitz.Rect] = None, hit_max: int = 64):
        """Robust search wrapper across PyMuPDF versions.
        Tries (a) clipped positional, (b) clipped kwargs, (c) unclipped positional, (d) unclipped kwargs.
        Always returns list[Rect].
        """
        if not isinstance(text, str) or not text.strip():
            return []
        sclip = self._safe_rect(clip, page) if clip is not None else None

        # Try: positional + clip
        try:
            if sclip is not None:
                return page.search_for(text, hit_max, False, flags, clip=sclip) or []
        except Exception:
            pass

        # Try: kwargs + clip
        try:
            if sclip is not None:
                return page.search_for(text, flags=flags, clip=sclip) or []
        except Exception:
            pass

        # Fallback: no clip (positional)
        try:
            return page.search_for(text, hit_max, False, flags) or []
        except Exception:
            pass

        # Fallback: no clip (kwargs)
        try:
            return page.search_for(text, flags=flags) or []
        except Exception:
            return []

    def _number_variants(self, s: str) -> List[str]:
        """Create flexible variants of numbers with different separators (comma/thin space/space)."""
        out = {s}
        # strip thousands separators
        out.add(re.sub(r"[,\u2009\u202F]", "", s))
        # replace comma with thin space / narrow NBSP / normal space
        out.add(re.sub(r",", "\u2009", s))
        out.add(re.sub(r",", "\u202F", s))
        out.add(re.sub(r",", " ", s))
        return [t for t in out if t]

    def _number_regex(self, s: str) -> Optional[str]:
        """Create a forgiving numeric regex without relying on word boundaries."""
        digits_only = re.sub(r"[^\d]", "", s)
        if not digits_only or len(digits_only) < 2:
            return None
        parts = re.findall(r"\d{1,3}", digits_only)
        if len(parts) < 2:
            return None
        core = r"[ ,\u2009\u202F]?"
        return r"(?<!\d)" + core.join(parts) + r"(?!\d)"

    def _expand_to_enclosing_quotes(self, page, hit_quad):
        """
        Expand the highlight to cover the full quoted span that contains the hit.
        Falls back to _expand_to_full_span if no quotes are found.
        """
        words = self._word_cache(page)
        if not words:
            return [hit_quad]

        r0 = hit_quad.rect if hasattr(hit_quad, "rect") else fitz.Rect(hit_quad)

        # Collect words on and around the hit lines (small vertical window)
        line_words = [(wr, tx) for (wr, tx, _, _) in words if (wr.y0 <= r0.y1 and wr.y1 >= r0.y0)]
        if not line_words:
            return self._expand_to_full_span(page, hit_quad)

        # Find nearest quote to the left and to the right across subsequent lines (limit lines scanned)
        # Build a linearized sequence of (rect,text) from the first intersecting line downward a few lines.
        lines = {}
        for wr, tx, bno, lno in words:
            lines.setdefault((bno, lno), []).append((wr, tx))
        ordered_keys = sorted(lines.keys())
        # Find start line
        start_idx = None
        for i, key in enumerate(ordered_keys):
            rects = [wr for wr, _ in lines[key]]
            if not rects: 
                continue
            line_rect = fitz.Rect(min(r.x0 for r in rects), min(r.y0 for r in rects),
                                  max(r.x1 for r in rects), max(r.y1 for r in rects))
            if line_rect.y0 <= r0.y1 and line_rect.y1 >= r0.y0:
                start_idx = i
                break
        if start_idx is None:
            return self._expand_to_full_span(page, hit_quad)

        seq = []
        for j in range(start_idx, min(start_idx + 8, len(ordered_keys))):  # scan up to 8 lines
            key = ordered_keys[j]
            row = sorted(lines[key], key=lambda t: t[0].x0)
            seq.extend(row)

        # Locate token indices that fall within r0 in the first line chunk
        # Then expand outward to the nearest quotes (")
        idx_hit = [k for k,(wr,tx) in enumerate(seq) if (wr.y0 <= r0.y1 and wr.y1 >= r0.y0 and wr.x1 >= r0.x0 and wr.x0 <= r0.x1)]
        if not idx_hit:
            return self._expand_to_full_span(page, hit_quad)

        left = min(idx_hit); right = max(idx_hit)

        # Search left for opening quote
        L = left
        while L > 0:
            if '"' in seq[L][1]:
                break
            L -= 1
        # Search right for closing quote
        R = right
        while R < len(seq)-1:
            if '"' in seq[R][1]:
                break
            R += 1

        # If we found quotes on both sides and L<R, use that span; else fall back
        if L < left and R > right and '"' in seq[L][1] and '"' in seq[R][1]:
            rects = [seq[k][0] for k in range(L, R+1)]
            quads = self._rects_to_quads_safe(rects, page)
            if quads:
                return quads
        return self._expand_to_full_span(page, hit_quad)

    def _expand_to_full_span(self, page, hit_quad):
        """
        Grow a found hit to cover the entire sentence/quoted span across lines.
        Returns a list of Quads (one per line) for a proper multi-line highlight.
        """
        try:
            words = self._word_cache(page)
            if not words:
                return [hit_quad]
                
            r0 = hit_quad.rect if hasattr(hit_quad, "rect") else fitz.Rect(hit_quad)

            # collect candidate lines that intersect vertically with the hit and below
            # we'll walk down until we see sentence/quote termination
            # group words by (block,line) to keep natural reading order
            by_line = {}
            for wr, tx, bno, lno in words:
                key = (bno, lno)
                by_line.setdefault(key, []).append((wr, tx))
            
            if not by_line:
                return [hit_quad]
                
            for key in by_line:
                by_line[key].sort(key=lambda t: t[0].x0)

            # find the first line that contains the hit
            keys = sorted(by_line.keys())
            start_idx = None
            for i, key in enumerate(keys):
                try:
                    line_rects = [wr for wr,_ in by_line[key]]
                    if not line_rects:
                        continue
                    line_rect = fitz.Rect(
                        min(wr.x0 for wr in line_rects),
                        min(wr.y0 for wr in line_rects),
                        max(wr.x1 for wr in line_rects),
                        max(wr.y1 for wr in line_rects),
                    )
                    if line_rect.y0 <= r0.y1 and line_rect.y1 >= r0.y0:
                        start_idx = i
                        break
                except Exception as e:
                    self.logger.debug(f"Error processing line {key}: {e}")
                    continue
                    
            if start_idx is None:
                return [hit_quad]  # fallback

            end_punct = re.compile(r'[.!?]"?$')
            open_quotes = False
            quads = []
            max_lines = min(8, len(keys) - start_idx)  # Limit expansion

            for i in range(start_idx, start_idx + max_lines):
                try:
                    key = keys[i]
                    line = by_line[key]
                    if not line:
                        continue
                        
                    line_text = " ".join(tx for _, tx in line)
                    # update quote state
                    if '"' in line_text:
                        # toggle state per every quote char count on the line
                        if line_text.count('"') % 2 == 1:
                            open_quotes = not open_quotes

                    # compute line span rect (full breadth)
                    line_rects = [wr for wr,_ in line]
                    if not line_rects:
                        continue
                        
                    x0 = min(wr.x0 for wr in line_rects)
                    x1 = max(wr.x1 for wr in line_rects)
                    y0 = min(wr.y0 for wr in line_rects)
                    y1 = max(wr.y1 for wr in line_rects)
                    sr = self._safe_rect(fitz.Rect(x0, y0, x1, y1), page)
                    if sr:
                        quads.append(fitz.Quad(sr))

                    # stop if sentence ends and we're not inside an open quote
                    if end_punct.search(line_text) and not open_quotes:
                        break
                except Exception as e:
                    self.logger.debug(f"Error expanding line {i}: {e}")
                    break

            return quads if quads else [hit_quad]
        except Exception as e:
            self.logger.debug(f"Error in _expand_to_full_span: {e}")
            return [hit_quad]

    def _iou(self, a: fitz.Rect, b: fitz.Rect) -> float:
        """Calculate intersection over union of two rectangles."""
        inter = a & b
        if inter.is_empty: 
            return 0.0
        ia = inter.get_area()
        ua = a.get_area() + b.get_area() - ia
        return ia / max(ua, 1e-6)

    def _resolve_overlap(self, existing_rects: list, new_quads: list):
        """
        If new highlight overlaps a previous one a lot, split each line's quad in half
        so both remain visible. Alternate which side we keep based on how many
        overlaps we've resolved so far.
        """
        if not new_quads:
            return new_quads
        nb = fitz.Rect(min(q.rect.x0 for q in new_quads),
                       min(q.rect.y0 for q in new_quads),
                       max(q.rect.x1 for q in new_quads),
                       max(q.rect.y1 for q in new_quads))
        for i, er in enumerate(existing_rects):
            # high overlap with any prior box?
            inter = self._iou(er, nb)
            if inter >= 0.6:
                split_quads = []
                # alternate side by i parity
                keep_right = (i % 2 == 0)
                for q in new_quads:
                    r = q.rect
                    if r.width >= r.height:
                        # split horizontally (left/right)
                        midx = (r.x0 + r.x1) / 2.0
                        rr = fitz.Rect(midx, r.y0, r.x1, r.y1) if keep_right else fitz.Rect(r.x0, r.y0, midx, r.y1)
                    else:
                        # split vertically (top/bottom)
                        midy = (r.y0 + r.y1) / 2.0
                        rr = fitz.Rect(r.x0, midy, r.x1, r.y1) if keep_right else fitz.Rect(r.x0, r.y0, r.x1, midy)
                    sr = self._safe_rect(rr, None)
                    if sr:
                        split_quads.append(fitz.Quad(sr))
                    else:
                        split_quads.append(q)
                return split_quads
        return new_quads

    def split_quotes_cell(self, cell: str) -> List[str]:
        """Split multi-quote cells reliably (bullets with/without quote marks)."""
        if not cell:
            return []
        t = str(cell)
        # Normalize common punctuation first (so regex is simpler)
        t = (
            t.replace("\u201C", '"').replace("\u201D", '"')  # curly double quotes → straight
             .replace("\u2018", "'").replace("\u2019", "'")  # curly single quotes → straight
             .replace("\u2022", "\n- ")  # bullets to dash lines
             .replace("\u00A0", " ")     # nbsp
             .replace("\u00AD", "")      # soft hyphen
        )

        # Also split inline dash-delimited quotes: " ... " - " ... "
        # Turn them into separate lines so downstream logic treats each quote independently
        t = re.sub(r'"\s*-\s*"', '"\n"', t)

        # Split into candidate lines (dash-bullets or newlines)
        lines = re.split(r"\r?\n", t)
        items = []
        for ln in lines:
            ln = ln.strip()
            if not ln:
                continue
            # strip leading bullet markers
            ln = re.sub(r'^\s*[-*]\s*', '', ln)
            if not ln:
                continue
            # if it contains explicit quotes, take inside the first pair
            m = re.search(r'"([^"]+)"', ln)
            if m:
                items.append(m.group(1).strip())
            else:
                items.append(ln)

        # Clean, dedupe, drop trivial fragments
        out, seen = [], set()
        for q in items:
            q = re.sub(r"\s+", " ", q).strip()
            if len(q) < 6:
                continue
            k = q.lower()
            if k not in seen:
                seen.add(k)
                out.append(q)
        return out

    def _normalize_token(self, t: str) -> str:
        t = self._normalize_quotes(t).lower()
        # strip most punctuation but keep digits, letters, % and dots (for versions, decimals)
        t = re.sub(r'[^\w.%]+', '', t)
        return t

    def _tokenize_quote(self, s: str) -> List[str]:
        s = self._normalize_quotes(s)
        rough = re.findall(r'[A-Za-z0-9.%]+', s)
        return [self._normalize_token(w) for w in rough if self._normalize_token(w)]

    def _page_words(self, page):
        # cache words with rects + normalized tokens
        if hasattr(page, "_norm_words_cache"):
            return page._norm_words_cache
        flags = getattr(fitz, "TEXT_DEHYPHENATE", 0)
        wrds = page.get_text("words", flags=flags) or []
        wrds.sort(key=lambda w: (round(w[1], 1), w[0]))  # by line (y), then x
        norm = []
        for x0, y0, x1, y1, txt, *_ in wrds:
            tok = self._normalize_token(txt)
            if tok:
                norm.append((fitz.Rect(x0, y0, x1, y1), tok, txt))
        page._norm_words_cache = norm
        return norm

    def _find_quote_quads_on_page(self, page, quote: str) -> Optional[List[fitz.Quad]]:
        """Return quads covering the *entire* quote on this page; else None."""
        q_tokens = self._tokenize_quote(quote)
        if not q_tokens:
            return None

        words = self._page_words(page)
        n, m = len(words), len(q_tokens)
        if m == 0 or n == 0:
            return None

        i = 0
        while i <= n - m:
            if words[i][1] != q_tokens[0]:
                i += 1
                continue
            j = 0
            k = i
            while j < m and k < n and words[k][1] == q_tokens[j]:
                j += 1
                k += 1
            if j == m:
                rects = [words[t][0] for t in range(i, k)]
                quads = self._rects_to_quads_safe(rects, page)
                return quads if quads else None
            i += 1
        return None

    def _find_full_quote_any_page(self, doc, quote: str, clip_regions=None) -> Optional[tuple]:
        """Return (page_idx, [quads]) for the FULL quote on a single page if present."""
        for p in range(len(doc)):
            page = doc[p]
            # respect reference clipping
            page_regions = clip_regions.get(p, None) if clip_regions else None
            if page_regions == []:
                continue
            if page_regions is not None:
                # if regions are defined, only try if the quote hits inside them
                quads = self._find_quote_quads_on_page(page, quote)
                if not quads:
                    continue
                # keep only quads that intersect any allowed region
                kept = []
                for q in quads:
                    if any(q.rect.intersects(r) for r in page_regions):
                        kept.append(q)
                if kept:
                    return (p, kept)
            else:
                quads = self._find_quote_quads_on_page(page, quote)
                if quads:
                    return (p, quads)
        return None

    def _build_search_regions(self, doc) -> Dict[int, Optional[List[fitz.Rect]]]:
        """Build search regions, excluding reference sections."""
        regions: Dict[int, Optional[List[fitz.Rect]]] = {i: None for i in range(len(doc))}

        for i in range(len(doc)):
            page = doc[i]
            if i == 0:
                continue  # never treat the first page as references

            y0 = self._find_reference_heading_y(page)
            if y0 is not None:
                # On the first references page, allow searching only above the heading
                top_rect = fitz.Rect(0, 0, page.rect.width, max(0.0, y0 - 2))
                regions[i] = [top_rect]
                # Everything after this page is References-only: skip entirely
                for j in range(i + 1, len(doc)):
                    regions[j] = []
                break

        return regions

    def _norm_for_match(self, s: str) -> str:
        """Normalize text for matching."""
        if not s:
            return ""
        
        # Basic normalization
        s = unicodedata.normalize("NFKC", str(s))
        s = s.replace("\u00A0", " ").replace("\u00AD", "")  # nbsp, soft hyphen
        s = s.translate({0x2018: "'", 0x2019: "'", 0x201C: '"', 0x201D: '"'})  # quotes
        
        # Replace hyphens with spaces
        s = re.sub(r"[-\u2010\u2011\u2012\u2013\u2014]", " ", s)
        
        # Keep only letters, digits, basic punctuation
        s = re.sub(r"[^0-9A-Za-z.%+\-\s]+", " ", s)
        s = re.sub(r"\s+", " ", s).strip().lower()
        return s

    def find_first_occurrence_document_wide(self, doc, search_text, clip_regions=None):
        """Find the first occurrence of text across all pages in document.
        Returns (page_idx, hit_rect) or None for document-wide first-occurrence tracking.
        """
        if not search_text or len(search_text.strip()) < 2:
            return None
        
        search_text = search_text.strip()
        self.logger.debug(f"Searching document for: '{search_text[:60]}...'")
        
        try:
            # Search page by page for first occurrence
            for page_num in range(len(doc)):
                page = doc[page_num]
                
                # clip_regions should be a dict: {page_index: None | [] | [Rect,...]}
                page_clip_rects = clip_regions.get(page_num, None) if clip_regions else None
                # skip references-only pages
                if page_clip_rects == []:
                    continue
                
                # Use the existing page-level search method
                hit_rect = self.find_first_occurrence_only(page, search_text, page_clip_rects)
                
                if hit_rect:
                    self.logger.debug(f"✓ Found '{search_text[:30]}...' on page {page_num + 1}")
                    return (page_num, hit_rect)
            
            self.logger.debug(f"No matches found for: '{search_text[:50]}...'")
            return None
            
        except Exception as e:
            self.logger.debug(f"Document search failed for '{search_text[:50]}...': {e}")
            return None

    def _expand_quad_to_sentence_rect(self, page, quad):
        """Grow a found quad to cover the whole sentence (or quoted span) on that line."""
        if not hasattr(page, "_words_cache"):
            WORD_FLAGS = getattr(fitz, "TEXT_DEHYPHENATE", 0)
            wrds = page.get_text("words", flags=WORD_FLAGS) or []
            wrds.sort(key=lambda w: (round(w[1],1), w[0]))
            page._words_cache = [(fitz.Rect(w[0],w[1],w[2],w[3]), w[4]) for w in wrds]

        r = quad.rect if hasattr(quad, "rect") else fitz.Rect(quad)
        line = [(wr, tx) for (wr, tx) in page._words_cache if wr.y0 <= r.y1 and wr.y1 >= r.y0]
        if not line: 
            return r
        line.sort(key=lambda t: t[0].x0)
        left = min(wr.x0 for wr,_ in line)
        right = max(wr.x1 for wr,_ in line)
        full = fitz.Rect(left, min(wr.y0 for wr,_ in line), right, max(wr.y1 for wr,_ in line))

        end_punct = re.compile(r'[.!?]"?$')
        quote_open = any('"' in tx for _,tx in line)

        # scan down a few lines to close sentence / quote
        y = full.y1 + 0.6
        for _ in range(5):
            nxt = [(wr,tx) for (wr,tx) in page._words_cache if wr.y0 >= y and wr.y0 - y < 14]
            if not nxt: 
                break
            nxt.sort(key=lambda t: t[0].x0)
            full |= fitz.Rect(min(wr.x0 for wr,_ in nxt),
                              min(wr.y0 for wr,_ in nxt),
                              max(wr.x1 for wr,_ in nxt),
                              max(wr.y1 for wr,_ in nxt))
            line_text = " ".join(tx for _,tx in nxt).strip()
            if (quote_open and '"' in line_text) or end_punct.search(line_text):
                break
            y = full.y1 + 0.6
        return full

    def _lite(self, term: str) -> str:
        """Keep letters/digits/spaces only for forgiving matching."""
        return re.sub(r"[^0-9A-Za-z\s]+", " ", term).strip()

    def _regex_from_phrase(self, s: str) -> str:
        """Create regex pattern with flexible whitespace/hyphens between tokens."""
        toks = [re.escape(t) for t in s.split()]
        return r"\b" + r"[ \-\u2010\u2011\u2012\u2013\u2014]+".join(toks) + r"\b"

    def find_first_occurrence_only(self, page, term, clip_rects=None):
        term = (term or "").strip()
        if len(term) < 3:
            return None
        if term.lower() in {"not", "and", "the", "of", "in"}:
            return None

        SFLAGS = 1  # ignore case

        # 1) literal search
        try:
            hits = []
            if clip_rects:
                for rect in clip_rects:
                    hits.extend(self._safe_search(page, term, flags=SFLAGS, clip=rect))
            else:
                hits = self._safe_search(page, term, flags=SFLAGS)
            if hits:
                sr = self._safe_rect(hits[0], page)
                if sr: return sr
        except Exception:
            pass

        # 2) numeric variants (handles "6,283" vs thin spaces)
        if re.search(r"\d[, \u2009\u202F]\d", term):
            try:
                for cand in self._number_variants(term):
                    hits = []
                    if clip_rects:
                        for rect in clip_rects:
                            hits.extend(self._safe_search(page, cand, flags=SFLAGS, clip=rect))
                    else:
                        hits = self._safe_search(page, cand, flags=SFLAGS)
                    if hits:
                        sr = self._safe_rect(hits[0], page)
                        if sr: return sr
                pat = self._number_regex(term)
                if pat:
                    hits = []
                    if clip_rects:
                        for rect in clip_rects:
                            hits.extend(self._safe_search(page, pat, flags=SFLAGS|4, clip=rect))
                    else:
                        hits = self._safe_search(page, pat, flags=SFLAGS|4)
                    if hits:
                        sr = self._safe_rect(hits[0], page)
                        if sr: return sr
            except Exception:
                pass

        # 3) regex with flexible whitespace / dashes
        try:
            pattern = self._regex_from_phrase(self._clean_cell_value(term))
            hits = []
            if clip_rects:
                for rect in clip_rects:
                    hits.extend(self._safe_search(page, pattern, flags=SFLAGS|4, clip=rect))
            else:
                hits = self._safe_search(page, pattern, flags=SFLAGS|4)
            if hits:
                sr = self._safe_rect(hits[0], page)
                if sr: return sr
        except Exception:
            pass

        # 4) long-text sliding windows
        words = term.split()
        if len(words) > 12:
            for window_size in (12, 10, 8):
                for i in range(len(words) - window_size + 1):
                    for cand in {' '.join(words[i:i+window_size]), self._lite(' '.join(words[i:i+window_size]))}:
                        hits = []
                        if clip_rects:
                            for rect in clip_rects:
                                hits.extend(self._safe_search(page, cand, flags=SFLAGS, clip=rect))
                        else:
                            hits = self._safe_search(page, cand, flags=SFLAGS)
                        if hits:
                            sr = self._safe_rect(hits[0], page)
                            if sr: return sr

        # 5) token-span fallback → returns Rect
        tok = self._tokenize(term)
        if len(tok) >= 2:
            r = self._token_span_search(page, tok, clip_rects=clip_rects)
            if r: return r

        return None

    def highlight_text_instance(self, page, quad, color, variable_name, value):
        """Highlight exactly the matched text with full span and overlap resolution.
        Accepts either a fitz.Rect or a fitz.Quad as `quad`.
        """
        try:
            # Normalize input to a Quad for downstream logic
            if hasattr(quad, "rect"):
                base_rect = quad.rect
            else:
                base_rect = self._safe_rect(quad, page)
            if not base_rect:
                return 0
            quad = fitz.Quad(base_rect)
            # keep a page-level stash of existing rectangles
            if not hasattr(page, "_used_rects"):
                page._used_rects = []

            # If this looks like a quote variable, expand to enclosing quotes; else to sentence/span
            is_quote_var = not (variable_name in self.SKIP_SUPPORTING or variable_name in getattr(self, 'STRICT_EXACT', set()))
            if is_quote_var:
                quads = self._expand_to_enclosing_quotes(page, quad)
            else:
                quads = self._expand_to_full_span(page, quad)
            
            # avoid deep overlaps with earlier annotations
            quads = self._resolve_overlap(page._used_rects, quads)

            annot = page.add_highlight_annot(quads)
            annot.set_colors(stroke=color)
            annot.set_opacity(0.65)  # slightly lighter so split overlaps both show
            # title only; no quote content in popup
            try:
                annot.set_info(title=variable_name)
                try:
                    annot.set_contents("")  # empty content
                except AttributeError:
                    try:
                        annot.set_content("")
                    except Exception:
                        pass
            except:
                pass
            annot.update()
            
            # after successful creation, remember the box we actually used
            if quads:
                bx0 = min(q.rect.x0 for q in quads)
                by0 = min(q.rect.y0 for q in quads)
                bx1 = max(q.rect.x1 for q in quads)
                by1 = max(q.rect.y1 for q in quads)
                used = self._safe_rect(fitz.Rect(bx0, by0, bx1, by1), page)
                if used:
                    page._used_rects.append(used)
            
            return 1
        except Exception as e:
            # last-resort rectangle highlight so we never crash
            try:
                r = quad.rect if hasattr(quad, "rect") else fitz.Rect(quad)
                annot = page.add_rect_annot(r)
                annot.set_colors(stroke=color, fill=color)
                annot.set_opacity(0.25)
                try:
                    annot.set_info(title=variable_name)
                    annot.set_contents("")
                except: 
                    pass
                annot.update()
                return 1
            except Exception as ee:
                self.logger.warning(f"Failed to add highlight: {e} / {ee}")
                return 0

    def clean_and_extract_terms(self, value, variable_name):
        """Extract clean search terms from a CSV cell value."""
        if pd.isna(value) or not value:
            return []
        
        # Convert to string and basic cleanup
        text = str(value).strip()
        
        # Skip empty or NA-ish values
        na_patterns = ['n/a', 'not specified', 'not mentioned', 'not available', 'nan', '']
        if text.lower() in na_patterns or len(text) < 3:
            return []
        
        # Light cleanup: collapse whitespace, remove leading/trailing quotes only
        text = re.sub(r'\s+', ' ', text)
        text = text.strip('"\'')
        
        # For supporting quotes, extract inside-quotes text or use sentence windows
        if variable_name.startswith('Supporting_quotes_for__'):
            # Try to extract quoted text first
            quoted_matches = re.findall(r'"([^"]*)"', text)
            if quoted_matches:
                # Use the longest quoted text
                terms = [max(quoted_matches, key=len)]
            else:
                # Use first sentence, broken into 6-12 word windows
                first_sentence = text.split('.')[0].strip()
                if len(first_sentence) > 30:
                    words = first_sentence.split()
                    terms = []
                    for window_size in [12, 10, 8, 6]:
                        if len(words) >= window_size:
                            terms.append(' '.join(words[:window_size]))
                            break
                else:
                    terms = [first_sentence] if first_sentence else []
        else:
            # For direct values, use as-is
            terms = [text]
        
        return [t for t in terms if len(t.strip()) >= 3]

    def clean_text(self, text):
        """Clean and normalize text for matching."""
        if pd.isna(text) or text == '':
            return ''
        # Remove line breaks and normalize whitespace
        cleaned = re.sub(r'\s+', ' ', str(text).strip())
        # Remove common quote artifacts
        cleaned = cleaned.replace('- "', '"').replace('" -', '"')
        cleaned = cleaned.replace('\r\n  - "', ' "')
        return cleaned

    def extract_quote_chunks(self, quote_text, min_words=6, max_words=15):
        """Extract meaningful chunks from long quotes for better matching."""
        if not quote_text or len(quote_text.strip()) < 10:
            return []
        
        # Clean the quote
        cleaned = self.clean_text(quote_text)
        
        # Split into sentences
        sentences = re.split(r'[.!?]+\s*', cleaned)
        chunks = []
        
        for sentence in sentences:
            sentence = sentence.strip()
            if len(sentence) < 20:  # Skip very short fragments
                continue
                
            words = sentence.split()
            if len(words) >= min_words:
                if len(words) <= max_words:
                    # Use whole sentence if it's reasonable length
                    chunks.append(sentence)
                else:
                    # Break long sentences into overlapping chunks
                    for i in range(0, len(words) - min_words + 1, max_words - 2):
                        chunk_words = words[i:i + max_words]
                        chunk = ' '.join(chunk_words)
                        if len(chunk) >= 30:  # Ensure meaningful length
                            chunks.append(chunk)
                            
        # If no good chunks found, try the original text in parts
        if not chunks and len(cleaned) > 30:
            words = cleaned.split()
            if len(words) >= min_words:
                for i in range(0, len(words) - min_words + 1, max_words - 2):
                    chunk_words = words[i:i + max_words]
                    chunk = ' '.join(chunk_words)
                    if len(chunk) >= 30:
                        chunks.append(chunk)
        
        return chunks[:5]  # Return top 5 chunks to avoid too many attempts

    def process_study(self, study_id, pdf_folder="Review_articles", combined_csv=None, max_highlights=100):
        """Process a single study using only the combined dataset."""
        try:
            self.logger.info(f"🔍 Processing Study {study_id}")
            
            # Load data
            if combined_csv is None:
                combined_csv = "20250820_Analysis & Results/20250820_combined_dataset.csv"
            
            # Load combined dataset only
            combined_df = pd.read_csv(combined_csv)
            
            # Get study data
            study_data = combined_df[combined_df['Study_ID'] == int(study_id)]
            if study_data.empty:
                raise ValueError(f"Study {study_id} not found")
            
            study_row = study_data.iloc[0]
            
            # Open PDF and build search regions
            # Try a broader set of filename patterns to locate the PDF
            pdf_patterns = [
                f"01_{study_id}.pdf",
                (f"0{study_id}_*.pdf" if int(study_id) < 10 else f"{study_id}_*.pdf"),
                f"*{int(study_id):02d}*.pdf",
                f"*{study_id}*.pdf",
                f"*{study_id}_*.pdf",
            ]
            
            pdf_file = None
            for pattern in pdf_patterns:
                matches = list(Path(pdf_folder).glob(pattern))
                if matches:
                    pdf_file = matches[0]
                    break
            
            if pdf_file is None:
                if study_id == "1":
                    pdf_file = Path(pdf_folder) / "01_a_discrete_spatial_choice_model_of_burglary_target.pdf"
                else:
                    # Final fallbacks matching some repository naming conventions
                    pdf_file = Path(pdf_folder) / (f"0{study_id}_{study_id}.pdf" if int(study_id) < 10 else f"{study_id}_{study_id}.pdf")
            
            if not pdf_file.exists():
                raise FileNotFoundError(f"PDF not found: {pdf_file}")
            
            doc = fitz.open(pdf_file)
            self.logger.info(f"📖 Opened PDF: {pdf_file} ({len(doc)} pages)")
            
            # Build search regions to exclude references
            search_regions = self._build_search_regions(doc)
            
            # Collect all variables that have supporting quotes in the CSV for this row
            quote_vars = []
            for col in study_row.index:
                if col.startswith("Supporting_quotes_for__") and str(study_row[col]).strip():
                    # column format: Supporting_quotes_for__<NAME>_   (sometimes without trailing _)
                    name = col[len("Supporting_quotes_for__"):]
                    if name.endswith("_"):
                        name = name[:-1]
                    quote_vars.append(name)

            # Everything we will try to highlight with stable ordering
            direct_order = [
                'Country','State_or_Region','City',
                'Crime_Type','Crime_Type_Group','Crime_Incidents',
                'Model_Type','Software_Used','Data_Sources','Number_of_Data_Sources',
                'Spatial_Unit_Name','Unit_Size','Data_Collection_Period'
            ]
            direct_vars = [v for v in direct_order if v in self.SKIP_SUPPORTING]
            all_variables = direct_vars + [q for q in quote_vars if q not in direct_vars]
            self.logger.info(f"📋 Discovered {len(all_variables)} variables to process: {sorted(all_variables)}")
            
            highlights_created = 0
            processing_stats = {
                'direct_values': 0,
                'supporting_quotes': 0,
                'skipped_empty': 0,
                'first_only_applied': 0
            }
            
            # Define all variables to process - now dynamically discovered
            # all_variables already computed above from CSV discovery
            
            # Process each variable
            for variable_name in all_variables:
                if highlights_created >= max_highlights:
                    break
                try:
                    self.logger.debug(f"🔄 Processing variable: {variable_name}")
                    color = self.get_variable_color(variable_name)

                    # --- Special case: Number_of_Data_Sources anchors to co-occurring sources
                    if variable_name == "Number_of_Data_Sources":
                        src_text = str(study_row.get("Data_Sources", ""))
                        parts = [p.strip() for p in re.split(r'[;,\n]+', src_text) if len(p.strip()) >= 3][:3]
                        if parts:
                            for page_idx in range(len(doc)):
                                page = doc[page_idx]
                                page_regions = search_regions.get(page_idx, None)
                                if page_regions == []:
                                    continue
                                hits = []
                                for p in parts:
                                    h = self.find_first_occurrence_only(page, p, page_regions)
                                    if h: hits.append(h)
                                if len(hits) >= 2:
                                    r = hits[0].rect | hits[1].rect
                                    sr = self._safe_rect(r, page)
                                    if not sr:
                                        continue
                                    quad = fitz.Quad(sr)
                                    made = self.highlight_text_instance(page, quad, color, variable_name, "")
                                    highlights_created += made
                                    processing_stats['first_only_applied'] += made
                                    processing_stats['direct_values'] += made
                                    if made:
                                        self.logger.info("  🎯 Special highlighting: Number_of_Data_Sources (source co-occurrence)")
                                    break
                        continue  # don't search for raw "2"

                    # --- DIRECT VARIABLES ---
                    if variable_name in self.SKIP_SUPPORTING:
                        direct_value = study_row.get(variable_name, '')
                        search_terms = self._direct_term_variants(str(direct_value), variable_name)
                        if not search_terms:
                            self.logger.info(f"  ⚠️  Skipping {variable_name}: empty/short value")
                            processing_stats['skipped_empty'] += 1
                            continue

                        self.logger.info(f"  🎯 Direct highlighting: {variable_name} = '{str(direct_value)[:60]}...'")
                        self.logger.debug(f"    Search terms: {search_terms}")

                        made = 0
                        for term in search_terms:
                            if highlights_created >= max_highlights: break
                            hit = self.find_first_occurrence_document_wide(
                                doc, self._normalize_for_pdf(term), search_regions
                            )
                            if hit:
                                page_idx, quad = hit
                                page = doc[page_idx]
                                made += self.highlight_text_instance(page, quad, color, variable_name, term)
                                highlights_created += made
                                if made:
                                    processing_stats['first_only_applied'] += 1
                                    break  # first occurrence only
                        processing_stats['direct_values'] += made
                        self.logger.info("    ✅ 1 highlight created (first occurrence only)" if made else "    ❌ No matches found")
                        continue  # IMPORTANT: don't fall through

                    # --- QUOTE VARIABLES ---
                    quote_column_name = f"Supporting_quotes_for__{variable_name}_"
                    if quote_column_name not in study_row:
                        quote_column_name = f"Supporting_quotes_for__{variable_name}"
                    quote_value = study_row.get(quote_column_name, '')

                    if not (quote_value and not pd.isna(quote_value) and len(str(quote_value).strip()) > 5):
                        self.logger.info(f"  ⚠️  No quotes available for {variable_name}")
                        processing_stats['skipped_empty'] += 1
                        continue

                    self.logger.info(f"  📝 Quote highlighting: {variable_name}")
                    full_quotes = self.split_quotes_cell(str(quote_value))
                    self.logger.debug(f"    Found {len(full_quotes)} quotes in cell")

                    made = 0
                    for full in full_quotes:
                        if highlights_created >= max_highlights:
                            break
                        qnorm = self._normalize_for_pdf(full)

                        # 1) exact full-quote match on a single page (multi-line)
                        hit = self._find_full_quote_any_page(doc, qnorm, search_regions)

                        # 2) fallback: overlapping chunk windows (6–12 words) across the quote
                        if not hit:
                            chunks = self.extract_quote_chunks(qnorm, min_words=6, max_words=12)
                            attempts = 0
                            for chunk in chunks:
                                attempts += 1
                                hit2 = self.find_first_occurrence_document_wide(doc, chunk, search_regions)
                                if hit2:
                                    page_idx, quad = hit2
                                    page = doc[page_idx]
                                    cnt = self.highlight_text_instance(page, quad, color, variable_name, full)
                                    made += cnt; highlights_created += cnt
                                    if cnt:
                                        processing_stats['first_only_applied'] += 1
                                        self.logger.debug(f"    Found with chunk: {chunk[:60]}...")
                                    break
                            if not hit and made == 0:
                                # Log a compact miss summary with up to 2 attempted chunks
                                preview = "; ".join(c[:40] + ("..." if len(c) > 40 else "") for c in chunks[:2])
                                self.logger.debug(f"    No match for quote after {attempts} chunk attempts. Examples: {preview}")
                            if made:
                                continue

                        if hit:
                            page_idx, quads = hit
                            page = doc[page_idx]
                            # Bypass expansion: we already have line-by-line quads for the whole quote
                            # Keep overlap splitting for readability
                            quads = self._resolve_overlap(getattr(page, "_used_rects", []), quads)
                            annot = page.add_highlight_annot(quads)
                            annot.set_colors(stroke=color); annot.set_opacity(0.8)
                            try:
                                annot.set_info(title=variable_name)
                                try:
                                    annot.set_contents("")
                                except AttributeError:
                                    try:
                                        annot.set_content("")
                                    except Exception:
                                        pass
                            except:
                                pass
                            annot.update()

                            # remember used rect
                            bx0 = min(q.rect.x0 for q in quads); by0 = min(q.rect.y0 for q in quads)
                            bx1 = max(q.rect.x1 for q in quads); by1 = max(q.rect.y1 for q in quads)
                            if not hasattr(page, "_used_rects"):
                                page._used_rects = []
                            used = self._safe_rect(fitz.Rect(bx0, by0, bx1, by1), page)
                            if used:
                                page._used_rects.append(used)

                            made += 1; highlights_created += 1
                            processing_stats['first_only_applied'] += 1
                            self.logger.info(f"    ✅ Found match for quote: {full[:60]}...")

                    processing_stats['supporting_quotes'] += made
                    self.logger.info(f"    ✅ {made} highlights from quotes" if made else "    ❌ No quote matches found")

                except Exception as e:
                    self.logger.warning(f"  ❌ Error processing variable {variable_name}: {e}")
                    processing_stats['skipped_empty'] += 1
                    continue
            
            # Save result
            today = date.today().strftime("%Y%m%d")
            output_folder = Path(f"{today}_ModeBased_Highlighting")
            output_folder.mkdir(exist_ok=True)
            
            output_path = output_folder / f"Study_{study_id}_ENHANCED_HYBRID_HIGHLIGHTED.pdf"
            doc.save(str(output_path))
            doc.close()
            
            self.logger.info(f"✅ Saved: {output_path}")
            self.logger.info(f"✨ Total highlights: {highlights_created}")
            self.logger.info(f"📊 Stats - Direct: {processing_stats['direct_values']}, Quotes: {processing_stats['supporting_quotes']}, First-only: {processing_stats['first_only_applied']}, Skipped: {processing_stats['skipped_empty']}")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'output_file': str(output_path),
                'stats': processing_stats
            }
            
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {'success': False, 'error': str(e)}

def main():
    if len(sys.argv) < 2:
        print("Usage: python enhanced_hybrid_highlighter.py <study_id> [pdf_folder]")
        print("       python enhanced_hybrid_highlighter.py all [pdf_folder]")
        sys.exit(1)
    
    study_id = sys.argv[1]
    pdf_folder = sys.argv[2] if len(sys.argv) > 2 else "Review_articles"
    
    highlighter = EnhancedHybridHighlighter()
    
    if study_id.lower() == 'all':
        # Process all studies
        combined_csv = "20250820_combined_dataset.csv"
        
        try:
            df = pd.read_csv(combined_csv)
            study_ids = df['Study_ID'].unique()
            
            print(f"Processing {len(study_ids)} studies...")
            success_count = 0
            
            for sid in sorted(study_ids):
                result = highlighter.process_study(sid, pdf_folder)
                if result['success']:
                    success_count += 1
                    print(f"✅ Study {sid}: {result['highlights_created']} highlights")
                else:
                    print(f"❌ Study {sid}: {result['error']}")
            
            print(f"\n🎉 Completed: {success_count}/{len(study_ids)} studies processed successfully")
            
        except Exception as e:
            print(f"Error processing all studies: {e}")
    
    else:
        # Process single study
        result = highlighter.process_study(study_id, pdf_folder)
        if result['success']:
            print(f"✅ Successfully highlighted Study {study_id}")
            print(f"   Highlights created: {result['highlights_created']}")
            print(f"   Output file: {result['output_file']}")
        else:
            print(f"❌ Failed to process Study {study_id}: {result['error']}")

if __name__ == "__main__":
    main()
