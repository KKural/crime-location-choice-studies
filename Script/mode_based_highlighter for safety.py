#!/usr/bin/env python3
"""
Mode-Based Variable Highlighter
==============================

A smarter approach to highlighting variables using different modes:
1. KEYWORD mode: Match short 1-2 word anchors (e.g., Ghent, Belgium, street segments)
2. EXACT mode: Match the full phrase from the CSV 
3. QUOTE mode: Match only the text inside quotes from Supporting_* columns

Each mode uses appropriate matching strategies to optimize accuracy and relevance.
"""

import pandas as pd
import fitz
from pathlib import Path
import logging
import re
import unicodedata
import csv
from typing import Dict, List, Set, Optional, Tuple
from datetime import datetime
from collections import defaultdict

def safe_quad_from_rect(rect: fitz.Rect) -> fitz.Quad:
    # Construct quad manually to avoid PyMuPDF "Point: bad args"
    # (ul, ur, ll, lr) = (x0,y0), (x1,y0), (x0,y1), (x1,y1)
    x0, y0, x1, y1 = float(rect.x0), float(rect.y0), float(rect.x1), float(rect.y1)
    # tiny inflation if needed to avoid zero-height/width
    if x1 <= x0:
        x1 = x0 + 0.5
    if y1 <= y0:
        y1 = y0 + 0.5
    return fitz.Quad((x0, y0,  x1, y0,  x0, y1,  x1, y1))

def safe_quads_from_rects(rects: list[fitz.Rect]) -> list[fitz.Quad]:
    out = []
    for r in rects:
        try:
            out.append(safe_quad_from_rect(r))
        except Exception:
            # last resort: inflate a hair and try again
            try:
                rr = fitz.Rect(r.x0, r.y0, r.x1, r.y1)
                rr.x1 += 0.5; rr.y1 += 0.5
                out.append(safe_quad_from_rect(rr))
            except Exception:
                pass
    return out

def valid_rect(r: fitz.Rect) -> bool:
    try:
        return (r is not None) and (r.x1 > r.x0) and (r.y1 > r.y0)
    except Exception:
        return False

def snippet_windows(text: str, size: int = 7) -> list[str]:
    """Generate sliding windows of normalized text tokens."""
    toks = [t for t in re.split(r"\s+", _norm_for_match(text)) if t]
    if not toks:
        return []
    return [" ".join(toks[i:i+size]) for i in range(0, max(len(toks)-size+1, 1))]

def _find_reference_heading_y(page) -> Optional[float]:
    """
    If a References/Bibliography/Works Cited heading appears on *this* page,
    return the y0 of the heading line so we can ignore everything *below* it.
    Otherwise return None. Never treat page 1 as references.
    """
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

    # Fallback: text mode scan near top if dict parsing fails
    lines = (page.get_text("text") or "").splitlines()[:20]
    for ln in lines:
        t = ln.strip().lower()
        if any(re.match(p, t) for p in pats):
            # conservative fallback: treat top 20% as pre-heading content
            return page.rect.height * 0.20
    return None


def _build_search_regions(doc) -> Dict[int, Optional[List[fitz.Rect]]]:
    """
    For each page, return:
      - None  -> search the full page
      - []    -> skip this page completely
      - [rects] -> only search in those rects (used on the first References page:
                   search only *above* the heading line).
    Never skip page 1 (index 0).
    """
    regions: Dict[int, Optional[List[fitz.Rect]]] = {i: None for i in range(len(doc))}
    ref_seen = False

    for i in range(len(doc)):
        page = doc[i]
        if i == 0:
            continue  # never treat the first page as references

        y0 = _find_reference_heading_y(page)
        if y0 is not None:
            # On the *first* references page, allow searching only *above* the heading.
            top_rect = fitz.Rect(0, 0, page.rect.width, max(0.0, y0 - 2))
            regions[i] = [top_rect]
            ref_seen = True
            # Everything after this page is References-only: skip entirely.
            for j in range(i + 1, len(doc)):
                regions[j] = []
            break

    return regions

def _plural_variants(term: str) -> list[str]:
    """Generate singular/plural variants for better crime type matching."""
    t = term.strip()
    if not t:
        return []
    words = t.split()
    w = words[-1]

    # generate variants for the last token
    forms = {w}
    wl = w.lower()

    if wl.endswith("ies") and len(w) > 3:
        forms.add(w[:-3] + "y")               # burglaries -> burglary
    elif wl.endswith("y") and not wl.endswith(("ay","ey","iy","oy","uy")):
        forms.add(w[:-1] + "ies")             # burglary -> burglaries
    elif wl.endswith(("s","x","z","ch","sh")):
        forms.add(w + "es")                   # bus -> buses, match -> matches
        forms.add(w.rstrip("s"))              # buses -> buse (but also keep original)
    else:
        forms.add(w + "s")

    # rebuild phrases with each form
    variants = {" ".join(words[:-1] + [f]) for f in forms}
    # also keep original
    variants.add(t)

    # space/underscore forms
    expanded = []
    for v in variants:
        expanded.append(v)
        if " " in v: 
            expanded.append(v.replace(" ", "_"))
        if "_" in v: 
            expanded.append(v.replace("_", " "))
    # de-dup
    seen, out = set(), []
    for v in expanded:
        lv = v.lower()
        if lv not in seen:
            seen.add(lv)
            out.append(v)
    return out

def _looks_like_reference_line(text: str) -> bool:
    """Quick check if text looks like a bibliography entry."""
    t = text.strip()
    tlow = t.lower()
    # quick-and-dirty bibliography cues
    if "doi:" in tlow or "https://" in tlow or "http://" in tlow:
        return True
    # author-year pattern (Smith, 2015) or 2015.
    if re.search(r"\([A-Z][A-Za-z-]+(?:\s+et al\.)?,\s*(19|20)\d{2}\)", t):
        return True
    if re.search(r"\b(19|20)\d{2}[.\)]?$", t):
        return True
    return False

def _is_table_block(block) -> bool:
    """Check if a text block looks like a results table."""
    lines = block.get("lines")
    if not lines: 
        return False
    
    text = " ".join(span.get("text","") for line in lines for span in line.get("spans", []))
    low = text.lower()
    
    # Look for table indicators
    indicators = ("odds ratio","coefficient","effect size","ci","p-value","p value","95% ci",
                  "confidence interval","or =","β =","beta =","std. error","z-score","t-value","sig.")
    
    # Count numeric patterns (decimals, p-values, ORs)
    numeric = len(re.findall(r"\b\d+\.\d+\b", low))
    
    # Tables usually have many numbers and some table keywords
    return any(k in low for k in indicators) and numeric >= 3

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def rect_iou(a: fitz.Rect, b: fitz.Rect) -> float:
    """Calculate intersection over union for two rectangles."""
    inter = a & b
    if inter.is_empty or inter.x1 <= inter.x0 or inter.y1 <= inter.y0:
        return 0.0
    inter_area = inter.get_area()
    union_area = a.get_area() + b.get_area() - inter_area
    return inter_area / max(union_area, 1e-9)

HYPHENS = "-\u2010\u2011\u2012\u2013\u2014"

def _norm_for_match(s: str) -> str:
    import unicodedata, re
    HYPHENS = "-\u2010\u2011\u2012\u2013\u2014"
    s = unicodedata.normalize("NFKC", str(s or ""))
    s = s.replace("\u00A0", " ").replace("\u00AD", "")  # nbsp, soft hyphen
    s = s.translate({0x2018: "'", 0x2019: "'", 0x201C: '"', 0x201D: '"'})  # curly quotes
    s = (s.replace("ﬁ","fi").replace("ﬂ","fl").replace("ﬀ","ff")
           .replace("ﬃ","ffi").replace("ﬄ","ffl"))
    s = re.sub(rf"[{HYPHENS}]", " ", s)                 # hyphen family → space
    s = re.sub(r"[^0-9A-Za-z.%]+", " ", s)              # keep only letters/digits/%/.
    s = re.sub(r"\s+", " ", s).strip().lower()
    return s

def validate_bounding_box(rect: fitz.Rect) -> bool:
    """Ensure bounding box is valid and non-empty."""
    if rect.is_empty or rect.x1 <= rect.x0 or rect.y1 <= rect.y0:
        return False
    return True

def _page_word_stream(page) -> list[tuple[str, fitz.Rect]]:
    """One-time word extraction per page; normalized token + rect."""
    WORD_FLAGS = getattr(fitz, "TEXT_DEHYPHENATE", 0)
    words = page.get_text("words", flags=WORD_FLAGS) or []
    words.sort(key=lambda w: (round(w[1], 1), w[0]))  # reading order
    stream = []
    for x0, y0, x1, y1, text, *_ in words:
        # Validate coordinates
        if not all(isinstance(coord, (int, float)) for coord in [x0, y0, x1, y1]):
            logging.warning(f"Skipping invalid coordinates: {x0}, {y0}, {x1}, {y1}")
            continue
        try:
            r = fitz.Rect(x0, y0, x1, y1)
            if not validate_bounding_box(r):
                logging.warning(f"Skipping invalid bounding box: {r}")
                continue
            norm = _norm_for_match(text)
            if not norm or len(norm) <= 1:  # Skip empty or single-character tokens
                logging.debug(f"Skipping invalid or short normalized text for: {text}")
                continue
            for tok in norm.split():
                if len(tok) > 1:  # Ensure token length is valid
                    logging.debug(f"Adding token: {tok}, Rect: {r}")
                    stream.append((tok, r))
        except Exception as e:
            logging.error(f"Error creating Rect for: {x0}, {y0}, {x1}, {y1} - {e}")
    return stream

def validate_quad(quad: fitz.Quad) -> bool:
    """Ensure the quad is valid and non-empty."""
    # Check for None or empty values
    if quad is None:
        return False
    
    # Check for valid rectangle properties
    try:
        if quad.rect.is_empty or quad.rect.x1 <= quad.rect.x0 or quad.rect.y1 <= quad.rect.y0:
            return False
    except (AttributeError, TypeError):
        return False
        
    return True

def find_first_occurrence_rects_by_words(page, phrase: str) -> list[fitz.Rect]:
    target = _norm_for_match(phrase).split()
    if not target:
        return []

    # cache word stream
    stream = getattr(page, "_cached_stream", None)
    if stream is None:
        stream = page._cached_stream = _page_word_stream(page)

    n, m = len(stream), len(target)
    MAX_JOIN = 3  # allow joining e.g., "m^2" or broken hyphens

    i = 0
    while i <= n - m:
        j = i
        rects: list[fitz.Rect] = []
        ok = True
        for t in target:
            best_k = None
            for k in range(1, MAX_JOIN + 1):
                if j + k > n:
                    break
                joined = "".join(stream[j + p][0] for p in range(k))
                if joined == t:
                    best_k = k
                    break
            if best_k is None:
                ok = False
                break

            for p in range(best_k):
                rr = stream[j + p][1]
                if valid_rect(rr):
                    rects.append(rr)
                else:
                    ok = False
                    break
            if not ok:
                break
            j += best_k

        if ok and rects:
            return rects
        i += 1
    return []

# Constants for term generation and skipping
STOPWORDS = {"the","a","an","of","and","or","in","on","for","to","by","with"}
SKIP_PHRASES = ("not specified", "not mentioned", "no maup discussion",
                "no sensitivity analysis", "no alternative units",
                "not applicable", "n/a", "na")

class ModeBasedHighlighter:
    SUPPORTING_PREFIX = "supporting_quotes_for__"
    
    def __init__(self, study_id=1):
        """Initialize the highlighter with mode-based matching configuration."""
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_ModeBased_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Track what we've highlighted per variable
        self.seen = set()  # (variable, term_lower)
        
        # Policy: Only ONE hit per variable for cleaner results
        self.first_hit_per_variable = True
        self.MAX_HITS_PER_VARIABLE = 1
        
        # Configuration flags for highlighting behavior
        self.MERGE_OVERLAPS = False        # don't merge overlapping highlights
        self.OVERLAP_IOU = 0.65            # lower threshold for nearby matches (was 0.85)
        
        # Define which columns use which matching mode
        self.VAR_MODE = {
            # KEYWORDS (1–2 words): pick a crisp anchor
            "Country": "keyword",
            "City": "keyword",
            "Crime_Type": "keyword",
            "Crime_Type_Group": "keyword",
            "Spatial_Unit_Name": "keyword",
            "Model_Type": "keyword",              # e.g., "Poisson", "Conditional logit"
            "Software_Used": "keyword",
            "MAUP_Discussion": "keyword",
            "Sensitivity_Analysis": "keyword",

            # EXACT (full phrase from CSV)
            "Data_Collection_Period": "exact",
            "Total_Study_Area_Size": "exact",
            "Number_of_Units": "exact",
            "Crime_Incidents": "exact",
            "Model_Fit_Statistics": "exact",
            "Coefficients": "exact",
            "Effect_Sizes": "exact",
            "Spatial_Aggregation": "exact",
            "Alternative_Units": "exact",
            "Unit_Size": "exact",
            "Rationale_Category": "exact",
            "Unit_Selection_Rationale": "exact",
            "Computational_Constraints": "exact",
            "Data_Limitations": "exact",
            "Data_Sources": "exact",
            "Independent_Variables": "exact",
            "Future_Suggestions": "exact",
            "Average_Population_per_Unit": "exact",
            "Number_of_Data_Sources": "exact",
            "Number_of_Variables": "exact", 
            "Confidence_Intervals": "exact",

            # QUOTE (supporting text must match the inside of quotes verbatim)
            "Supporting_Quotes": "quote",
            "Supporting_Quotes_Methods": "quote",
            "Supporting_Quotes_Results": "quote",
            "Supporting_Theory": "quote",
            "Supporting_Rationale": "quote",
            "Supporting_Data": "quote",
        }
        
        # Skip these count columns
        self.skip_columns = {"Number_of_Data_Sources", "Number_of_Variables"}
        
        # 7 original categories + neutral gray - DARK COLORS for better visibility
        self.color_scheme = {
            'Geographic': (0.6, 0.0, 0.0),       # Dark Red
            'Study_Area': (0.0, 0.0, 0.6),       # Dark Blue  
            'Quantitative': (0.0, 0.5, 0.0),     # Dark Green
            'Crime_Data': (0.8, 0.3, 0.0),       # Dark Orange
            'Methodology': (0.6, 0.0, 0.6),      # Dark Magenta
            'Results': (0.7, 0.7, 0.0),          # Dark Yellow
            'Limitations': (0.0, 0.5, 0.5),      # Dark Cyan
            'Default': (0.3, 0.3, 0.3)           # Medium Gray
        }
        
        # Variable categorization (only for the target columns)
        self.variable_categories = {
            'Country': 'Geographic',
            'City': 'Geographic',
            'Total_Study_Area_Size': 'Study_Area',
            'Spatial_Unit_Name': 'Study_Area',
            'Unit_Size': 'Study_Area',
            'Number_of_Units': 'Quantitative',
            'Average_Population_per_Unit': 'Quantitative',
            'Crime_Incidents': 'Quantitative',
            'Number_of_Data_Sources': 'Quantitative',
            'Number_of_Variables': 'Quantitative',
            'Crime_Type': 'Crime_Data',
            'Crime_Type_Group': 'Crime_Data',
            'Data_Sources': 'Crime_Data',
            'Data_Collection_Period': 'Crime_Data',
            'Model_Type': 'Methodology',
            'Independent_Variables': 'Methodology',
            'Software_Used': 'Methodology',
            'Model_Fit_Statistics': 'Results',
            'Coefficients': 'Results',
            'Confidence_Intervals': 'Results',
            'Effect_Sizes': 'Results',
            'Data_Limitations': 'Limitations',
            'Computational_Constraints': 'Limitations',
            'Spatial_Aggregation': 'Study_Area',
            'Unit_Selection_Rationale': 'Study_Area',
            'Rationale_Category': 'Study_Area',
            'Alternative_Units': 'Study_Area',
            'MAUP_Discussion': 'Methodology',
            'Sensitivity_Analysis': 'Methodology',
            'Future_Suggestions': 'Methodology',
            'Supporting_Quotes': 'Default',
            'Supporting_Quotes_Methods': 'Methodology',
            'Supporting_Quotes_Results': 'Results',
            'Supporting_Theory': 'Default',
            'Supporting_Rationale': 'Study_Area',
            'Supporting_Data': 'Crime_Data',
        }

    def _norm_colname(self, name: str) -> str:
        # lower, strip spaces, collapse underscores, drop trailing underscores
        s = re.sub(r'_+$', '', str(name).strip())
        s = re.sub(r'__+', '__', s)
        return s

    def base_var_from_supporting(self, col: str) -> Optional[str]:
        s = self._norm_colname(col).lower()
        if not s.startswith(self.SUPPORTING_PREFIX): return None
        # take the part after prefix; strip leading/trailing underscores
        core = s[len(self.SUPPORTING_PREFIX):].strip('_')
        # turn double underscores around words into single underscores
        core = re.sub(r'__+', '_', core)
        # map back to your canonical variable casing (title-like)
        core = core.replace('_', ' ').strip()
        # try to match known variables by loose equality
        # fallbacks:
        return core.title().replace(' ', '_')

    def normalize_text(self, text: str) -> str:
        """Light normalization of text content."""
        if not text:
            return text
        
        # Strip whitespace
        s = str(text).strip()
        
        # unify quotes safely
        quote_map = {
            '\u2018': "'", '\u2019': "'",    # ' ' -> '
            '\u201C': '"', '\u201D': '"',    # " " -> "
            '\u2032': "'", '\u2033': '"',    # ′ ″ -> ' "
            '\u00AD': '',                    # soft hyphen
            '\u00A0': ' ',                   # nbsp -> space
        }
        for k, v in quote_map.items():
            s = s.replace(k, v)

        # normalize dashes/hyphens and ligatures
        s = s.replace('\u2013', '–').replace('\u2014', '—')
        for k, v in {'ﬁ':'fi','ﬂ':'fl','ﬀ':'ff','ﬃ':'ffi','ﬄ':'ffl'}.items():
            s = s.replace(k, v)

        # Collapse multiple spaces at the end to not break exact matching
        s = re.sub(r'\s+', ' ', s)
        
        return s

    def clean_list_numbering(self, text: str) -> str:
        """Remove list numbering and formatting from text."""
        # Remove common list patterns: 1., 2), 3:, a), -, •
        text = re.sub(r'^\s*(?:\(?\d+[\.\)\:]|[a-zA-Z][\.\)])\s*', '', text)  # 1. 2) 3: a)
        text = re.sub(r'^\s*[-•]\s*', '', text)  # - or •
        text = text.strip()
        return text

    # Helper functions for term generation
    def _take_keyword_anchors(self, text: str) -> list[str]:
        """Choose 1-2 word anchors for keyword mode."""
        # Try quoted bits first (e.g., "street segments")
        qs = re.findall(r'["""\'`](.+?)["""\'`]', text)
        if qs:
            # Take shortest reasonable quoted anchor
            qs = [q.strip() for q in qs if 1 <= len(q.split()) <= 3]
            if qs:
                return [qs[0]]
                
        # Fallback: pick best 1-2 word chunk
        words = [w for w in re.split(r'[^0-9A-Za-z%\.]+', text) if w]
        # Prefer 2-word combos first, then single words
        pairs = [" ".join(p) for p in zip(words, words[1:])]
        candidates = [*pairs, *words]
        
        # Filter stopwords-only, too short, etc.
        clean = []
        for c in candidates:
            toks = [t for t in c.split() if t.lower() not in STOPWORDS]
            if not toks:
                continue
            cc = " ".join(toks)
            if 1 <= len(cc.split()) <= 3:
                clean.append(cc)
                
        # Keep first good one
        return clean[:1] or []

    def _extract_quotes(self, text: str) -> list[str]:
        """Pull content inside straight or curly quotes."""
        qs = re.findall(r'["""\'`](.+?)["""\'`]', text)
        return [q.strip() for q in qs if len(q.strip()) >= 3]

    def _extract_enumerated_items(self, text: str) -> List[str]:
        """Extract items from enumerated lists like '1. item 2. item 3. item' without leakage."""
        if not text:
            return []
        t = self.normalize_text(text)
        # Replace newlines with space to catch inline enumerations
        t = t.replace('\n', ' ')
        # Pattern: number/letter followed by . or ) then item text up to next number/letter marker or end
        pattern = r'(?:^|\s)(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+(.+?)(?=\s+(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+|$)'
        items = [m.strip() for m in re.findall(pattern, t)]
        # Fallback: split by semicolon or bullet separators if regex found nothing
        if not items:
            rough = re.split(r'[;•\n]', t)
            items = [self.clean_list_numbering(x.strip()) for x in rough if x.strip()]
        # Remove trailing stray enumeration numbers at end of item
        items = [re.sub(r'\s*\d+\.?$', '', it).strip() for it in items if it.strip()]
        # De-duplicate preserving order
        seen: Set[str] = set()
        result: List[str] = []
        for it in items:
            low = it.lower()
            if low and low not in seen:
                seen.add(low)
                result.append(it)
        return result

    def generate_search_terms(self, value, variable: str) -> List[str]:
        if pd.isna(value): 
            return []
        raw = str(value).strip()
        if not raw: 
            return []

        # collapse doubled CSV quotes -> single
        raw = raw.replace('""', '"')

        # dynamic QUOTE mode: any Supporting_quotes_for__* column - return full quoted strings only
        base_from_support = self.base_var_from_supporting(variable)
        if base_from_support is not None or variable.startswith("Supporting_") or variable.lower().startswith("supporting_quotes_for__"):
            qs = re.findall(r'["\'`]{1,3}\s*(.+?)\s*["\'`]{1,3}', raw, flags=re.S)
            # Return only full quoted strings (length >= 3), no fuzzy snippets
            return [q.strip() for q in qs if len(q.strip()) >= 3]

        # screens out "not specified …"
        if any(p in raw.lower() for p in ("not specified","not mentioned","no maup discussion",
                                          "no sensitivity analysis","no alternative units",
                                          "not applicable","n/a"," na ")):
            return []

        # Independent variables: strip numbering + underscores -> spaces + short anchors
        if variable == "Independent_Variables":
            text = raw.replace("_", " ")
            text = re.sub(r'(?m)^\s*(?:\(?\d+[\.\)]|\(?[A-Za-z][\.\)])\s*', '', text)
            parts = re.split(r'[;•|]|\n', text)
            out = []
            for p in parts:
                p = p.strip(" -•;\n\t\r")
                if not p: 
                    continue
                p = re.sub(r'\s*\(?\d+[\.\)]\s*$', '', p).strip()
                p = re.sub(r'[:;,.\s]+$', '', p)  # end punctuation
                if p:
                    out.append(p)
                    # also keep 1–2 word anchors for long terms
                    toks = p.split()
                    if len(toks) > 3:
                        out.append(" ".join(toks[:2]))
            # also return underscore forms to catch literal underscores in PDFs if any
            out2 = out + [x.replace(' ', '_') for x in out]
            return list(dict.fromkeys(out2))

        # Coefficients / Effect_Sizes: anchor on label before colon + robust OR patterns
        if variable in {"Coefficients", "Effect_Sizes"}:
            lines = re.split(r'(?:\n|;|•)', raw)
            labels = []
            for ln in lines:
                m = re.match(r'\s*[-–—]?\s*([^:]+):', ln)
                if m:
                    lab = m.group(1).strip()
                    if lab:
                        labels.append(lab.replace("_", " "))
            
            # Generate robust alternates for OR patterns
            alts = []
            for lab in labels:
                base = lab.strip()
                alts += [
                    base,                       # Bars
                    base.lower(),
                    f"{base}:",                 # Bars:
                    f"{base} –", f"{base} -",   # Bars – / -
                ]
            
            # numeric ORs inside the value:
            for m in re.findall(r"OR\s*=\s*\d+(?:\.\d+)?", raw, flags=re.I):
                v = m.split("=")[1].strip()
                alts += [
                    m, m.replace(" ", ""),
                    f"odds ratio {v}", f"odds ratio of {v}",
                    f"or={v}", f"or = {v}", f"(or = {v})",
                ]
            
            # Paper-specific fallback for graffiti study if no labels extracted
            if not labels and variable == "Effect_Sizes":
                PAPER_EFFECT_LABELS = [
                    "Bars","Night shops","Nightclubs","Restaurants","Schools",
                    "Residential units","Trees","Street network centrality",
                    "Street segment surface size","Intersections","Regular street segments"
                ]
                alts.extend(PAPER_EFFECT_LABELS)
            
            # De-dup while preserving order
            return list(dict.fromkeys(alts)) or labels

        # Short keyword anchors with singular/plural variants
        if variable in {"Country","City","Crime_Type","Crime_Type_Group","Spatial_Unit_Name",
                        "Model_Type","Software_Used","Rationale_Category"}:
            s = raw.replace("_", " ").strip()
            qs = re.findall(r'["""\'`]{1,3}\s*(.+?)\s*["""\'`]{1,3}', s)
            
            # For Crime_Type and Crime_Type_Group, handle singular/plural variants
            if variable in {"Crime_Type", "Crime_Type_Group"}:
                base = (qs[0].strip() if qs else " ".join([t for t in re.split(r'[^0-9A-Za-z.%/+-]+', s) if t][:3]))
                if base:
                    variants = _plural_variants(base)
                    # Keep underscore + space forms for safety
                    expanded = []
                    for v in variants:
                        expanded.append(v)
                        if " " in v: 
                            expanded.append(v.replace(" ", "_"))
                        if "_" in v: 
                            expanded.append(v.replace("_", " "))
                    # de-dup while preserving order
                    seen = set()
                    terms = []
                    for v in expanded:
                        vl = v.lower()
                        if vl and vl not in seen:
                            seen.add(vl)
                            terms.append(v)
                    return terms
            else:
                # Standard keyword processing for other variables
                if qs:
                    cand = [q.strip() for q in qs if 1 <= len(q.split()) <= 3]
                    if cand: 
                        return [cand[0]]
                toks = [t for t in re.split(r'[^0-9A-Za-z.%/+-]+', s) if t]
                if len(toks) <= 3: 
                    return [" ".join(toks)]
                return [" ".join(toks[:3])]

        # Numeric-ish exacts - no variants, exact text only
        if variable in {"Data_Collection_Period","Total_Study_Area_Size","Number_of_Units",
                        "Average_Population_per_Unit","Crime_Incidents","Model_Fit_Statistics",
                        "Confidence_Intervals"}:
            return [self.normalize_text(raw)]  # no variants

        # List-like exacts
        if variable in {"Alternative_Units","Future_Suggestions","Unit_Selection_Rationale",
                        "Data_Limitations","Computational_Constraints","Spatial_Aggregation",
                        "Data_Sources"}:
            s = self.normalize_text(raw)
            parts = [p.strip() for p in re.split(r'[;•|]\s*', s) if p.strip()]
            return parts or [s]

        # default: exact phrase only (no variants)
        s = self.normalize_text(raw)
        return [s]

    def load_study_data(self):
        """Load study data from CSV with proper error handling."""
        try:
            df = pd.read_csv(self.csv_path, engine="python", quoting=csv.QUOTE_MINIMAL, keep_default_na=False)
            
            # Clean up doubled quotes from CSV export
            df = df.applymap(lambda x: x.replace('""','"') if isinstance(x, str) else x)
            
            # Coerce Study_ID to int if needed
            if 'Study_ID' in df.columns:
                df['Study_ID'] = df['Study_ID'].astype(int)
            
            # Find the study
            study_rows = df[df['Study_ID'] == int(self.study_id)]
            if study_rows.empty:
                raise ValueError(f"Study_ID {self.study_id} not found in CSV. Available IDs: {sorted(df['Study_ID'].unique())}")
            
            study_data = study_rows.iloc[0]
            logging.info(f"Loaded Study {self.study_id}: {study_data.get('Title', 'No Title')[:50]}...")
            return study_data
            
        except Exception as e:
            logging.error(f"Error loading study data: {e}")
            return None

    def sanitize_title_hint(self, title: str) -> str:
        """Sanitize title for filename matching (keep only [A-Za-z0-9_]+)."""
        return re.sub(r'[^A-Za-z0-9_]', '', str(title))

    def find_pdf_file(self, study_data) -> Optional[Path]:
        """Find PDF file using 4-tier fallback strategy."""
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if not pdf_files:
            raise FileNotFoundError(f"No PDF files found in {self.pdf_folder}")
        
        # Strategy 1: {study_id:02d}_*.pdf
        pattern1 = f"{self.study_id:02d}_*.pdf"
        matches = list(self.pdf_folder.glob(pattern1))
        if matches:
            logging.info(f"Found PDF using pattern {pattern1}: {matches[0].name}")
            return matches[0]
        
        # Strategy 2: {study_id}_*.pdf  
        pattern2 = f"{self.study_id}_*.pdf"
        matches = list(self.pdf_folder.glob(pattern2))
        if matches:
            logging.info(f"Found PDF using pattern {pattern2}: {matches[0].name}")
            return matches[0]
        
        # Strategy 3: *{sanitized_title_hint}*.pdf
        if 'Title' in study_data.index and not pd.isna(study_data['Title']):
            title_hint = self.sanitize_title_hint(study_data['Title'])[:30]  # Use first 30 chars
            if title_hint:  # Only if we have some sanitized content
                pattern3 = f"*{title_hint}*.pdf"
                matches = list(self.pdf_folder.glob(pattern3))
                if matches:
                    logging.info(f"Found PDF using title hint {pattern3}: {matches[0].name}")
                    return matches[0]
        
        # Strategy 4: First available PDF
        logging.warning(f"Using fallback PDF: {pdf_files[0].name}")
        return pdf_files[0]

    def highlight_pdf_mode_based(self, pdf_path: Path, variable_terms: Dict[str, List[str]]):
        """Highlight PDF using mode-based approach with optimized searching per mode."""
        try:
            doc = fitz.open(str(pdf_path))
            
            # NEW: Build search regions (never skip page 1, handle references properly)
            search_regions_by_page = _build_search_regions(doc)
            pages_to_scan = list(range(len(doc)))  # do not skip the first page
            
            highlights_created = 0
            match_details = []
            not_found_terms = []
            
            print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
            print(f"📄 Total pages: {len(doc)}")
            print(f"🔎 Using exact matching with reference-aware regions")
            
            # Track placed highlights for overlap detection AND variable tracking
            placed_by_page = defaultdict(list)  # page_num -> [{rect, annot, vars:set(), terms:set()}]
            highlighted_variables = set()  # Track which variables have been highlighted
            
            # Process each variable and its terms
            for variable, terms in variable_terms.items():
                # Skip if this variable has already been highlighted
                if variable in highlighted_variables:
                    print(f"\n--- {variable} (ALREADY HIGHLIGHTED) ---")
                    continue
                    
                category = self.variable_categories.get(variable, 'Default')
                color = self.color_scheme[category]
                mode = self.VAR_MODE.get(variable, "exact")
                
                # Use exact matching only (no tables, no fuzzy)
                restrict_to_tables = False  # search whole pages (minus reference regions)
                
                print(f"\n--- {variable} ({category}, {mode} mode, exact-only) ---")
                print(f"Terms to search: {len(terms)}")
                
                variable_hits = 0
                first_hit_page = None
                
                for term in terms:
                    found_match = False
                    search_term = term
                    log_term = (term if len(term) <= 50 else term[:50] + '…')
                    
                    # Search pages in ascending order for FIRST occurrence
                    for page_num in pages_to_scan:
                        page = doc[page_num]
                        
                        # -------- EXACT MATCH ONLY (no fuzzy) --------
                        FLAGS = (
                            getattr(fitz, "TEXT_IGNORECASE", 0)
                            | getattr(fitz, "TEXT_DEHYPHENATE", 0)
                            | getattr(fitz, "TEXT_PRESERVE_WHITESPACE", 0)
                        )

                        # Decide allowed regions: full page, restricted area (above "References"), or skip page.
                        allowed = search_regions_by_page.get(page_num, None)
                        if allowed == []:
                            continue  # skip this page entirely (pure references pages)

                        quads = []
                        if allowed is None:
                            hits = page.search_for(search_term, quads=True, flags=FLAGS)
                            if hits:
                                quads = [q for q in hits if validate_quad(q)][:1]  # take FIRST hit only
                        else:
                            for region in allowed:
                                hits = page.search_for(search_term, quads=True, clip=region, flags=FLAGS)
                                if hits:
                                    quads = [q for q in hits if validate_quad(q)][:1]  # take FIRST hit only
                                    break

                        # If still nothing, keep scanning
                        if not quads:
                            continue
                        # --------------------------------------------

                        # Simplified union rect since we only have one quad
                        union = quads[0].rect

                        # Check for overlaps with existing highlights
                        used = any(rect_iou(union, ph["rect"]) >= self.OVERLAP_IOU for ph in placed_by_page[page_num])
                        if used:
                            continue  # keep scanning next occurrences/pages

                        # Optional: Skip bibliography-looking lines
                        try:
                            clip_text = page.get_text("text", clip=union) or ""
                            if _looks_like_reference_line(clip_text):
                                continue  # skip bibliography-looking lines
                        except Exception:
                            pass  # If clip fails, continue anyway

                        # Create the highlight strictly from quads (no rect fallback)
                        try:
                            highlight = page.add_highlight_annot(quads)
                        except Exception as e:
                            logging.debug(f"add_highlight_annot failed for '{log_term}': {e}")
                            continue

                        highlight.set_colors(stroke=color)
                        highlight.set_opacity(0.25)
                        highlight.set_info(title=variable, content="")
                        highlight.update()
                        
                        placed_by_page[page_num].append({
                            "rect": union, "annot": highlight, "vars": {variable}, "terms": {search_term}
                        })
                        
                        # Mark this variable as highlighted so we don't highlight it again
                        highlighted_variables.add(variable)
                        
                        # Record the match
                        highlights_created += 1
                        variable_hits += 1
                        if first_hit_page is None:
                            first_hit_page = page_num + 1
                        
                        match_details.append({
                            "Page": page_num + 1,
                            "Variable": variable,
                            "Term": search_term,
                            "Mode": mode,
                            "HitsOnPage": 1,
                            "Color": color
                        })
                        
                        print(f"   ✅ FOUND: '{log_term}' on page {page_num + 1}")
                        found_match = True
                        break  # Stop after first match in the document
                    
                    if found_match:
                        break  # Break term loop after first variable hit (one highlight per variable)
                    
                    # Enforce MAX_HITS_PER_VARIABLE
                    if variable_hits >= self.MAX_HITS_PER_VARIABLE:
                        break  # stop adding more hits for this variable
                    
                    if not found_match:
                        not_found_terms.append((variable, search_term, mode))
                        print(f"   ❌ NOT FOUND: '{log_term}'")
                
                # Log per-variable summary
                if variable_hits > 0:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page={first_hit_page}")
                else:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page=NOT FOUND")
            
            # Save highlighted PDF with compression
            output_filename = f"Study_{self.study_id}_{pdf_path.stem}_MODEBASED.pdf"
            output_path = self.output_folder / output_filename
            doc.save(str(output_path), garbage=4, deflate=True)
            doc.close()
            
            print(f"\n✅ Saved highlighted PDF: {output_filename}")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'total_searched': sum(len(terms) for terms in variable_terms.values()),
                'match_details': match_details,
                'not_found_terms': not_found_terms,
                'output_file': output_path
            }
            
        except Exception as e:
            logging.error(f"Error highlighting PDF: {e}")
            return {'success': False, 'error': str(e)}

    def process_study(self):
        """Process study with mode-based highlighting approach."""
        print("="*80)
        print("MODE-BASED VARIABLE HIGHLIGHTER")
        print("="*80)
        print("✅ REQUIREMENTS:")
        print("🔑 KEYWORD mode: Short 1-2 word anchors")
        print("📝 EXACT mode: Full phrases from CSV")
        print("💬 QUOTE mode: Text inside quotes for Supporting_* columns")
        print("🚫 First non-overlapping occurrence per variable")
        print("🎨 7 original categories + neutral gray")
        print("="*80)
        
        # Load study data
        study_data = self.load_study_data()
        if study_data is None:
            print("❌ Failed to load study data")
            return False
        
        # Find PDF with robust search
        try:
            pdf_path = self.find_pdf_file(study_data)
        except FileNotFoundError as e:
            print(f"❌ {e}")
            return False
        
        print(f"\n📖 Processing PDF: {pdf_path.name}")
        print(f"🆔 Study ID: {self.study_id}")
        
        # Process all available columns that have modes defined
        variable_terms = {}
        total_columns_processed = 0
        total_terms_generated = 0
        
        print(f"\n🔍 PROCESSING COLUMNS BY MODE:")
        
        # Count columns by mode for summary
        mode_counts = {"keyword": 0, "exact": 0, "quote": 0}
        
        for column in study_data.index:
            # Skip columns without defined modes
            if column not in self.VAR_MODE:
                continue
                
            # Get the mode for this column
            mode = self.VAR_MODE[column]
            
            value = study_data[column]
            
            # Skip completely empty values
            if pd.isna(value) or str(value).strip() == '':
                print(f"  📋 {column}: EMPTY - skipped")
                continue
            
            total_columns_processed += 1
            mode_counts[mode] += 1
            
            # Generate search terms
            terms = self.generate_search_terms(value, column)
            if terms:
                variable_terms[column] = terms
                total_terms_generated += len(terms)
                print(f"  📋 {column} ({mode}): {len(terms)} terms - {str(value)[:60]}...")
            else:
                print(f"  📋 {column} ({mode}): No terms generated")
        
        print(f"\n📊 PROCESSING SUMMARY:")
        print(f"   Total columns processed: {total_columns_processed}")
        print(f"   Variables with terms: {len(variable_terms)}")
        print(f"   Total terms generated: {total_terms_generated}")
        print(f"   KEYWORD mode columns: {mode_counts['keyword']}")
        print(f"   EXACT mode columns: {mode_counts['exact']}")
        print(f"   QUOTE mode columns: {mode_counts['quote']}")
        
        if not variable_terms:
            print("❌ No terms to highlight!")
            return False
        
        # Highlight PDF with mode-based approach
        result = self.highlight_pdf_mode_based(pdf_path, variable_terms)
        
        if result['success']:
            success_rate = (result['highlights_created'] / result['total_searched']) * 100
            
            print(f"\n" + "="*80)
            print("MODE-BASED HIGHLIGHTING RESULTS")
            print("="*80)
            print(f"📊 Total Terms Searched: {result['total_searched']}")
            print(f"✨ Successfully Highlighted: {result['highlights_created']}")
            print(f"🎯 Success Rate: {success_rate:.1f}%")
            
            # Save CSV summary with mode information
            if result['match_details']:
                df_results = pd.DataFrame(result['match_details'])
                results_csv = self.output_folder / f"study_{self.study_id}_modebased_results.csv"
                df_results.to_csv(results_csv, index=False, encoding='utf-8')
                print(f"📋 CSV summary: {results_csv.name}")
                
                # Show category and mode breakdown
                if not df_results.empty:
                    category_summary = {}
                    mode_summary = {'keyword': 0, 'exact': 0, 'quote': 0}
                    
                    for _, row in df_results.iterrows():
                        var = row['Variable']
                        category = self.variable_categories.get(var, 'Default')
                        category_summary[category] = category_summary.get(category, 0) + 1
                        
                        mode = row.get('Mode', 'exact')
                        mode_summary[mode] = mode_summary.get(mode, 0) + 1
                    
                    print(f"\n📈 HIGHLIGHTS BY CATEGORY:")
                    for category, count in sorted(category_summary.items()):
                        print(f"   {category}: {count} highlights")
                        
                    print(f"\n📊 HIGHLIGHTS BY MODE:")
                    for mode, count in sorted(mode_summary.items()):
                        print(f"   {mode.upper()} mode: {count} highlights")
            
            # Diagnostics for not found terms by mode
            if result['not_found_terms']:
                not_found_by_mode = {'keyword': 0, 'exact': 0, 'quote': 0}
                for var, term, mode in result['not_found_terms']:
                    not_found_by_mode[mode] = not_found_by_mode.get(mode, 0) + 1
                
                print(f"\n🔍 NOT FOUND TERMS BY MODE:")
                for mode, count in sorted(not_found_by_mode.items()):
                    print(f"   {mode.upper()} mode: {count} terms not found")
            
            print(f"\n🎨 Highlighted PDF: {result['output_file'].name}")
            return True
        else:
            print(f"❌ HIGHLIGHTING FAILED: {result.get('error', 'Unknown error')}")
            return False

def main():
    """Main function with configurable study ID."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Mode-Based Variable Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID to process')
    
    args = parser.parse_args()
    
    highlighter = ModeBasedHighlighter(study_id=args.study_id)
    success = highlighter.process_study()
    
    if success:
        print(f"\n🎉 SUCCESS! Study {args.study_id} processed successfully.")
    else:
        print(f"\n❌ FAILED! Study {args.study_id} could not be processed.")
    
    return success

if __name__ == "__main__":
    # Use main() function to handle command line arguments
    main()
