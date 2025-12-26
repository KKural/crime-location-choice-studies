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
import time
from typing import Dict, List, Optional, Set, Union

# Enhanced normalization maps for scientific text
SUPERSCRIPT_MAP = str.maketrans({
    "¹": "1", "²": "2", "³": "3", "⁰": "0", "⁴": "4", "⁵": "5",
    "⁶": "6", "⁷": "7", "⁸": "8", "⁹": "9",
})

GREEK_MAP = {
    "β": "beta", "μ": "mu", "±": "+-", "·": ".", "×": "x",
}
from typing import Dict, List, Set, Optional, Tuple
from datetime import datetime
from collections import defaultdict, deque

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

    for i in range(len(doc)):
        page = doc[i]
        if i == 0:
            continue  # never treat the first page as references

        y0 = _find_reference_heading_y(page)
        if y0 is not None:
            # On the *first* references page, allow searching only *above* the heading.
            top_rect = fitz.Rect(0, 0, page.rect.width, max(0.0, y0 - 2))
            regions[i] = [top_rect]
            # Everything after this page is References-only: skip entirely.
            for j in range(i + 1, len(doc)):
                regions[j] = []
            break

    return regions

def _to_singular(word: str) -> str:
    """Convert a word to its singular form."""
    w = word.strip()
    if re.search(r"ies$", w, flags=re.I):
        return re.sub(r"ies$", "y", w, flags=re.I)
    if re.search(r"(ses|xes|zes|ches|shes)$", w, flags=re.I):
        return re.sub(r"es$", "", w, flags=re.I)
    if re.search(r"s$", w, flags=re.I) and not re.search(r"ss$", w, flags=re.I):
        return re.sub(r"s$", "", w, flags=re.I)
    return w

def _split_values(val: str) -> list[str]:
    """Split 'bars and restaurants, nightclubs / schools' into separate values."""
    parts = re.split(r"\s*(?:,|/|;| and )\s*", val, flags=re.I)
    return [p.strip() for p in parts if p.strip()]

def extract_core_value_anchors(variable_text: str) -> list[str]:
    """
    From 'Presence of Bars' or 'Count of Bars' return ['bar', 'bars'].
    From 'Crime Type: Robbery' return ['robbery'].
    From 'Location Type: Street Segment' return ['street segment', 'street segments'].
    """
    t = variable_text.strip()

    # Normalize punctuation and spaces
    t = re.sub(r"\s+", " ", t)

    # Patterns like "Presence of X", "Count of X", "Density of X", "Number of X", "Rate of X"
    m = re.match(r"^(presence|count|density|number|rate)\s+of\s+(.+)$", t, flags=re.I)
    if m:
        val = m.group(2).strip()
    else:
        # Patterns like "Crime Type: X" or "Location Type: Y"
        m2 = re.match(r"^(crime\s*type|location\s*type)\s*[:\-]\s*(.+)$", t, flags=re.I)
        if m2:
            val = m2.group(2).strip()
        else:
            # Fallback: use the whole thing as value
            val = t

    # Values can contain multiple categories
    raw_values = _split_values(val)

    anchors: list[str] = []
    for rv in raw_values:
        # keep multi-word values (e.g., "street segment")
        sing = " ".join(_to_singular(w) for w in rv.split())
        # generate singular + plural form for the last token only
        if " " in sing:
            head, last = sing.rsplit(" ", 1)
            last_plurals = _plural_variants(last)  # already in your code
            for lp in {last, *[p.split()[-1] for p in last_plurals]}:
                anchors.append(f"{head} {lp}".strip())
        else:
            # single token
            for v in _plural_variants(sing):
                anchors.append(v)

    # de-duplicate preserving order, and prefer shorter anchors first
    out, seen = [], set()
    for a in sorted(anchors, key=len):
        al = a.lower()
        if al not in seen:
            seen.add(al)
            out.append(a)
    return out

def _is_supporting_col(name: str) -> bool:
    """Check if a column name represents a supporting quotes column."""
    s = str(name or "").strip().lower()
    return s.startswith("supporting_quotes_for__") or s.startswith("supporting_")

def _quote_windows(q: str, min_w: int = 4, max_w: int = 8) -> list[str]:
    """Split long quotes into overlapping windows for robust PDF matching."""
    tokens = [t for t in re.split(r"\s+", q.strip()) if t]
    wins = []
    for w in range(max_w, min_w - 1, -1):
        for i in range(0, max(0, len(tokens) - w + 1)):
            frag = " ".join(tokens[i:i+w])
            if len(_norm_for_match(frag)) >= 10:  # ensure some substance post-normalization
                wins.append(frag)
    return list(dict.fromkeys(wins)) or ([q] if q else [])

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

    # superscripts → digits
    s = s.translate(SUPERSCRIPT_MAP)

    # curly quotes, nbsp/soft hyphen
    s = s.replace("\u00A0", " ").replace("\u00AD", "")
    s = s.translate({0x2018: "'", 0x2019: "'", 0x201C: '"', 0x201D: '"'})

    # common scientific glyphs
    for k, v in GREEK_MAP.items():
        s = s.replace(k, v)

    # ligatures
    s = (s.replace("ﬁ","fi").replace("ﬂ","fl").replace("ﬀ","ff")
           .replace("ﬃ","ffi").replace("ﬄ","ffl"))

    # hyphen family → space
    s = re.sub(rf"[{HYPHENS}]", " ", s)

    # keep letters/digits/%/./+/-  (retain + and - for OR=1.23, +- etc.)
    s = re.sub(r"[^0-9A-Za-z.%+\-]+", " ", s)

    s = re.sub(r"\s+", " ", s).strip().lower()
    return s

def _safe_search_for(page, st, *, allowed=None, flags=0, time_budget_s=0.50, hit_max=40):
    """
    Robust wrapper around page.search_for that:
      - Skips / chunks very long strings (PDFs choke on these)
      - Enforces a tiny time budget per term to avoid "freezes"
      - Never throws; always returns a list (possibly empty)
      - Falls back to rect search if quad search fails
    """
    t0 = time.perf_counter()

    # Hard trim absurdly long search strings (let fallback handle them)
    st_norm = st.strip()
    if len(st_norm) > 160:
        st_norm = st_norm[:160]

    results = []
    
    def _convert_rects(rects):
        """Convert rects to quads safely."""
        out = []
        for r in rects or []:
            try:
                rr = r if isinstance(r, fitz.Rect) else fitz.Rect(r)
                out.append(safe_quad_from_rect(rr))
            except Exception:
                pass
        return out

    try:
        # Try fast path first (optionally clipped to allowed regions)
        if allowed is None:
            hits = page.search_for(st_norm, quads=True, flags=flags) or []
            results.extend(hits)
        else:
            for region in allowed:
                if time.perf_counter() - t0 > time_budget_s:
                    break
                hits = page.search_for(st_norm, quads=True, clip=region, flags=flags) or []
                results.extend(hits)
    except Exception:
        # try rect search then convert
        try:
            if allowed is None:
                rects = page.search_for(st_norm, flags=flags) or []
                results.extend(_convert_rects(rects))
            else:
                for region in allowed:
                    if time.perf_counter() - t0 > time_budget_s:
                        break
                    rects = page.search_for(st_norm, clip=region, flags=flags) or []
                    results.extend(_convert_rects(rects))
        except Exception:
            results = []

    # Enforce time budget strictly
    if time.perf_counter() - t0 > time_budget_s:
        return results[:hit_max]  # cheap cap

    return results[:hit_max]

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

def _quote_windows(quote_text, min_window=6, max_window=10):
    """
    Create overlapping sliding windows from a long quote for robust matching.
    Handles cases where PDF formatting might break up the exact quote text.
    """
    words = quote_text.split()
    if len(words) <= max_window:
        return [quote_text]
    
    windows = []
    
    # Create overlapping windows of different sizes
    for window_size in range(min_window, min(max_window + 1, len(words) + 1)):
        for start in range(len(words) - window_size + 1):
            window = " ".join(words[start:start + window_size])
            if window not in windows:  # avoid duplicates
                windows.append(window)
    
    # Always include the full quote as well
    if quote_text not in windows:
        windows.append(quote_text)
    
    return windows

def _group_overlapping_highlights(all_highlights):
    """
    Group highlights that overlap using IoU (Intersection over Union).
    Returns list of groups, where each group contains overlapping highlights.
    """
    if not all_highlights:
        return []
    
    def quad_to_rect(quad):
        """Convert quad to simple rect for IoU calculation"""
        if hasattr(quad, 'rect'):
            return quad.rect
        # Assume quad is already a rect or similar
        return quad
    
    def rect_iou(rect1, rect2):
        """Calculate Intersection over Union for two rectangles"""
        # Get intersection
        x1 = max(rect1.x0, rect2.x0)
        y1 = max(rect1.y0, rect2.y0) 
        x2 = min(rect1.x1, rect2.x1)
        y2 = min(rect1.y1, rect2.y1)
        
        if x2 <= x1 or y2 <= y1:
            return 0.0
        
        intersection = (x2 - x1) * (y2 - y1)
        area1 = (rect1.x1 - rect1.x0) * (rect1.y1 - rect1.y0)
        area2 = (rect2.x1 - rect2.x0) * (rect2.y1 - rect2.y0)
        union = area1 + area2 - intersection
        
        return intersection / union if union > 0 else 0.0
    
    groups = []
    processed = set()
    
    for i, highlight1 in enumerate(all_highlights):
        if i in processed:
            continue
            
        # Start new group
        group = [highlight1]
        processed.add(i)
        
        # Find all overlapping highlights
        for j, highlight2 in enumerate(all_highlights):
            if j in processed or i == j:
                continue
                
            rect1 = quad_to_rect(highlight1[1])  # quad is at index 1
            rect2 = quad_to_rect(highlight2[1])
            
            if rect_iou(rect1, rect2) > 0.1:  # 10% overlap threshold
                group.append(highlight2)
                processed.add(j)
        
        groups.append(group)
    
    return groups

def find_first_occurrence_rects_by_words(page, phrase: str) -> list[fitz.Rect]:
    target = _norm_for_match(phrase).split()
    if not target:
        return []

    stream = getattr(page, "_cached_stream", None)
    if stream is None:
        stream = page._cached_stream = _page_word_stream(page)

    n = len(stream)
    if n == 0:
        return []

    MAX_JOIN = 5          # allow re-joining e.g., hyphen breaks, m^2, etc.
    MAX_GAPS = 1          # allow up to 1 stray token skipped in the PDF stream

    i = 0
    while i < n:
        j = i
        ti = 0
        gaps_used = 0
        rects: list[fitz.Rect] = []
        ok = True

        while ti < len(target) and j < n:
            t = target[ti]
            best_k = None

            # try to match this target token by joining up to MAX_JOIN stream tokens
            for k in range(1, MAX_JOIN + 1):
                if j + k > n:
                    break
                joined = "".join(stream[j + p][0] for p in range(k))
                if joined == t:
                    best_k = k
                    break

            if best_k is not None:
                # consume k tokens and collect rects
                for p in range(best_k):
                    rr = stream[j + p][1]
                    if valid_rect(rr):
                        rects.append(rr)
                j += best_k
                ti += 1
            else:
                # allow skipping one stray token (e.g., page number, figure label)
                if gaps_used < MAX_GAPS:
                    gaps_used += 1
                    j += 1
                    continue
                ok = False
                break

        if ok and ti == len(target) and rects:
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
        self.output_folder = self.base_dir / "20250825_ModeBased_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Track what we've highlighted per variable
        self.seen = set()  # (variable, term_lower)
        
        # Policy: Highlight ALL occurrences per variable (no cap)
        self.first_hit_per_variable = False
        self.MAX_HITS_PER_VARIABLE = None  # unlimited
        
        # Configuration flags for highlighting behavior
        self.MERGE_OVERLAPS = False        # don't merge overlapping highlights
        self.OVERLAP_IOU = 0.65            # lower threshold for nearby matches (was 0.85)
        self.DEFAULT_CAP = 25              # default highlight cap per variable
        
        # Per-variable highlight policies
        self.HIT_POLICY = {
            # Document-level variables (first occurrence only)
            "Country": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            "City": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            "Number_of_Units": {"cap": 1, "first_only": True, "mode_override": "exact"},
            "Crime_Incidents": {"cap": 1, "first_only": True, "mode_override": "exact"},
            "Model_Type": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            "Software_Used": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            "Crime_Type": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            "Crime_Type_Group": {"cap": 1, "first_only": True, "mode_override": "keyword"},
            
            # Repetitive variables with controlled caps
            "Spatial_Unit_Name": {"cap": 1, "first_only": False, "mode_override": "keyword"},  # highlight "house" only once
            
            # Heavy explanatory variables
            "Unit_Selection_Rationale": {"cap": 40, "first_only": False, "mode_override": None},
            "Rationale_Category": {"cap": 40, "first_only": False, "mode_override": None},
            "MAUP_Discussion": {"cap": 40, "first_only": False, "mode_override": None},
            "Sensitivity_Analysis": {"cap": 40, "first_only": False, "mode_override": None},
            
            # Default for Supporting_* columns (applied dynamically)
            "_supporting_default": {"cap": 40, "first_only": False, "mode_override": None},
        }
        
        # === NEW: per-variable caps (None = unlimited) ===
        self.MAX_HITS_CAP = {
            "Spatial_Unit_Name": 1,   # highlight "house" only once
            "Country": 1,             # one country mention is enough
            "City": 1,                # one city mention is enough
            "Number_of_Units": 1,     # document-level stat
            "Crime_Incidents": 1,     # document-level stat
            "Model_Type": 1,          # usually mentioned once
            "Sample_Size": 1,         # document-level stat
            "Software_Used": 1,       # usually mentioned once
            "Number_of_Data_Sources": 1,  # prevent highlighting every "2"
            "Unit_Size_km2": 0,       # skip "NA" entirely - too generic
            "Independent_Variables": 8,   # allow more matches for statistical tables
        }
        
        # === POLICY BUCKETS for evidence vs factual highlighting ===
        # Variables that do NOT need Supporting quotes (highlight only the value)
        self.SKIP_SUPPORTING = {
            "Country","City","Data_Collection_Period","Crime_Type","Crime_Type_Group",
            "Crime_Incidents","Spatial_Unit_Name","Number_of_Units","Number_of_Data_Sources",
            "Software_Used","Total_Study_Area_Size","Total_Study_Area_km2",
            "Unit_Size_km2","Number_of_Variables","Verified_by_Kural","Verified_by_Stephanie"
        }

        # Variables that MUST use Supporting quotes (verbatim, expand to sentence)
        self.REQUIRE_SUPPORTING = {
            "Unit_Selection_Rationale","Rationale_Category","Data_Limitations","Computational_Constraints",
            "Alternative_Units","Independent_Variables","Model_Fit_Statistics","Coefficients",
            "Confidence_Intervals","Effect_Sizes","MAUP_Discussion","Sensitivity_Analysis","Future_Suggestions"
        }

        # Variables where summary is enough, quotes optional
        self.SUMMARY_OK = {"Spatial_Aggregation","Average_Population_per_Unit","Unit_Size"}
        
        # Create lowercase set for efficient comparison
        self._skip_supporting_lower = {s.lower() for s in self.SKIP_SUPPORTING}

        # === NEW: priority buckets for 2-pass highlighting ===
        self.HIGH_PRIORITY_VARS = {
            "Supporting_Quotes", "Supporting_Quotes_Methods", "Supporting_Quotes_Results",
            "Supporting_Theory", "Supporting_Rationale", "Supporting_Data",
            "Effect_Sizes", "Coefficients", "Model_Fit_Statistics", "Confidence_Intervals",
            "Independent_Variables", "Dependent_Variables"  # Important analysis vars
        }
        
        # Default policy for unlisted variables
        self.DEFAULT_POLICY = {"cap": self.DEFAULT_CAP, "first_only": False, "mode_override": None}
        
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
            
            # Allow core-value anchoring from the Variables column
            "Variables": "keyword",

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
        
        # Skip these count columns - remove Number_of_Data_Sources to allow contextual search
        self.skip_columns = {"Number_of_Variables"}
        
        # 7 original categories + neutral gray - IMPROVED VIBRANT COLORS for maximum visibility
        self.color_scheme = {
            'Geographic': (1.0, 0.2, 0.2),       # Bright Red
            'Study_Area': (0.2, 0.4, 1.0),       # Bright Blue  
            'Quantitative': (0.0, 0.8, 0.2),     # Bright Green
            'Crime_Data': (1.0, 0.6, 0.0),       # Bright Orange
            'Methodology': (0.8, 0.2, 0.8),      # Bright Magenta
            'Results': (0.9, 0.9, 0.0),          # Bright Yellow
            'Limitations': (0.0, 0.8, 0.8),      # Bright Cyan
            'Default': (0.5, 0.5, 0.5)           # Medium Gray
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
    
    def _should_expand_sentence(self, variable: str) -> bool:
        """Decide if we should expand a match to full sentence for this variable."""
        if _is_supporting_col(variable):  # any Supporting_* column
            return True
        return variable in {"Effect_Sizes","Coefficients","Model_Fit_Statistics","Confidence_Intervals"}
    
    def _expand_quad_to_sentence_rect(self, page, quad: fitz.Quad) -> fitz.Rect:
        """Expand a quad to cover the full sentence for better context."""
        # get words once
        stream = getattr(page, "_cached_stream", None)
        if stream is None:
            stream = page._cached_stream = _page_word_stream(page)
        r = quad.rect
        # collect nearby words on same lines that touch the quad vertically
        words_on_lines = [(tok, rr) for tok, rr in stream if abs(rr.y0 - r.y0) < 6 or (rr & r).height > 0]
        if not words_on_lines:
            return r
        # grow left/right until punctuation .,;:!? or line ends
        line_y = sorted(words_on_lines, key=lambda x: x[1].x0)
        left = min((rr.x0 for _, rr in line_y), default=r.x0)
        right = max((rr.x1 for _, rr in line_y), default=r.x1)
        # make a slightly taller box to cover asc/descenders
        return fitz.Rect(left, r.y0 - 1.5, right, r.y1 + 1.5)

    def _split_multiline_items(self, text: str) -> List[str]:
        """Split multi-line / bulleted / semi-colon lists into clean items.
        Enhanced to handle numbered lists and CSV quote formats properly."""
        if not text:
            return []
        
        # Normalize the text and standardize line breaks
        t = self.normalize_text(text).replace("\r", "\n")
        
        # Handle CSV quote format: '- "Quote 1"\n  - "Quote 2"' etc.
        if '- "' in t or '"\n' in t:
            # Split on the quote pattern: \n  - " or just - "
            quote_parts = re.split(r'(?:\n\s*)?-\s*"([^"]*)"', t)
            items = []
            for part in quote_parts:
                part = part.strip()
                if part and len(part) > 10:  # Avoid very short fragments
                    # Clean up the quote content
                    part = re.sub(r'^["\s]+|["\s]+$', '', part)  # Remove quotes and whitespace
                    part = re.sub(r'\s+', ' ', part)  # Normalize whitespace
                    if part:
                        items.append(part)
            
            if items:
                # De-duplicate while preserving order
                seen = set(); out = []
                for it in items:
                    low = it.lower()
                    if low not in seen and len(it.strip()) > 5:  # Minimum length filter
                        seen.add(low); out.append(it)
                return out
        
        # First check for standard numbered list pattern: 1. Item, 2. Item, etc.
        numbered_items = re.findall(r'(\d+\.\s*[^0-9\n]+?)(?=\d+\.|$)', t, re.DOTALL)
        if numbered_items and len(numbered_items) >= 2:
            # Process each numbered item to clean it up
            items = []
            for item in numbered_items:
                # Remove the number prefix
                clean_item = re.sub(r'^\d+\.\s*', '', item.strip())
                # Clean up any other formatting
                clean_item = re.sub(r"\s*\([^)]*\)\s*$", "", clean_item).strip()
                clean_item = re.sub(r"[:;,\.\s]+$", "", clean_item)
                if clean_item:
                    items.append(clean_item)
            
            # De-duplicate while preserving order
            seen = set(); out = []
            for it in items:
                low = it.lower()
                if low not in seen:
                    seen.add(low); out.append(it)
            return out
        
        # Fallback to the original splitting logic if numbered list pattern doesn't match
        t = re.sub(r"[•\u2022\u25CF\u2043]", "-", t)  # bullets -> hyphen
        raw_parts = re.split(r"(?:\n|\r|\r\n|;|\||—|-)\s*", t)
        items = []
        for p in raw_parts:
            p = self.clean_list_numbering(p)              # drop "1.", "a)", etc.
            p = re.sub(r"\s*\([^)]*\)\s*$", "", p).strip()# strip trailing ( ... )
            p = re.sub(r"[:;,\.\s]+$", "", p)
            if p:
                items.append(p)
                
        # De-duplicate while preserving order
        seen = set(); out = []
        for it in items:
            low = it.lower()
            if low not in seen:
                seen.add(low); out.append(it)
        return out

    def _reduce_to_core_anchor(self, term: str) -> List[str]:
        """Reduce phrases like 'Presence_of_garage' -> 'garage' (+ variants)."""
        t = term.replace("_", " ").strip()
        # remove leading descriptors
        m = re.match(r"(presence|count|density|number|rate)\s+of\s+(.+)", t, flags=re.I)
        if m:
            t = m.group(2).strip()
        # drop boilerplate words frequently seen in your domain (tune as needed)
        t = re.sub(r"\b(house|level|target|selection|property|residential)\b", "", t, flags=re.I).strip()
        t = re.sub(r"\s+", " ", t)
        # produce variants (singular/plural on last token; underscore/space)
        variants = [t]
        variants += _plural_variants(t)
        expanded = []
        for v in variants:
            expanded.append(v)
            if " " in v: expanded.append(v.replace(" ", "_"))
            if "_" in v: expanded.append(v.replace("_", " "))
        # keep short anchors as well (first 1–2 non-stopwords)
        toks = [w for w in re.split(r"[^0-9A-Za-z]+", t) if w and w.lower() not in STOPWORDS]
        if toks:
            short1 = toks[0]
            if short1.lower() not in {x.lower() for x in expanded}:
                expanded.append(short1)
            if len(toks) >= 2:
                short2 = " ".join(toks[:2])
                if short2.lower() not in {x.lower() for x in expanded}:
                    expanded.append(short2)
        # de-dup
        seen = set(); out = []
        for v in expanded:
            lv = v.lower().strip()
            if lv and lv not in seen:
                seen.add(lv); out.append(v.strip())
        return out

    def base_var_from_supporting(self, column_str: str) -> str:
        """Extract base variable from Supporting_* column name."""
        # Handle Supporting_quotes_for__VariableName_ format
        if column_str.lower().startswith("supporting_quotes_for__"):
            base = column_str[len("supporting_quotes_for__"):].rstrip("_")
            return base
        # Handle Supporting_* format  
        if column_str.startswith("Supporting_"):
            # Remove Supporting_ prefix and any trailing descriptors
            base = column_str[len("Supporting_"):].split("_")[0]
            return base
        return column_str

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
        text = re.sub(r'^\s*(?:\(?\d+[\.\)\:]|[a-zA-Z][\.\)]|\(\w+\))\s*', '', text)  # 1. 2) 3: a) (i)
        text = re.sub(r'^\s*[-•·•‣⁃◦⦿⁌⁍]\s*', '', text)  # Handle more bullet types
        return text.strip()
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

        # Filter out generic/problematic terms
        generic_terms = {
            "na", "n/a", "not applicable", "2", "1", "0", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "0.",
            "the", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "from", "up", "about", "into",
            "through", "during", "before", "after", "above", "below", "over", "under", "between", "among", "within", "without",
            "a", "an", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "do", "does", "did", "will", "would",
            "could", "should", "may", "might", "must", "can", "shall", "this", "that", "these", "those", "i", "you", "he", "she", "it",
            "we", "they", "me", "him", "her", "us", "them", "my", "your", "his", "her", "its", "our", "their"
        }
        if raw.lower() in generic_terms:
            return []
            
        # Filter out very short terms that are likely noise (but keep important abbreviations)
        important_short_terms = {"R", "GIS", "ArcGIS", "SAS", "SPSS", "UK", "US", "USA", "EU", "GDP", "FBI", "CIA", "UCR", "NCVS"}
        if len(raw.strip()) <= 2 and raw.strip() not in important_short_terms:
            return []
            
        # Filter out pure numbers or simple punctuation
        if re.match(r'^[\d\s\.,;:\-_\(\)\[\]]+$', raw.strip()):
            return []

        # NEW: global NA-ish guard (works for long sentences too)
        if re.search(r'\b(not\s+specified|not\s+mentioned|no\s+(maup\s+discussion|sensitivity\s+analysis|alternative\s+units)\b)', raw, flags=re.I):
            return []

        # NEW: only highlight the value for each entry in the "Variables" column
        if variable.strip().lower() == "variables":
            return extract_core_value_anchors(raw)

        # dynamic QUOTE mode: any Supporting_quotes_for__* column - return full quoted strings only
        base_from_support = self.base_var_from_supporting(variable)
        if base_from_support is not None or variable.startswith("Supporting_") or variable.lower().startswith("supporting_quotes_for__"):
            qs = re.findall(r'["\'`]{1,3}\s*(.+?)\s*["\'`]{1,3}', raw, flags=re.S)
            quotes = [q.strip() for q in qs if len(q.strip()) >= 3]
            if quotes:
                return quotes[:6]  # at most six exact quotes
            else:
                # take the first sentence to avoid explosion
                first_sentence = re.split(r'(?<=[.!?])\s+', raw.strip(), maxsplit=1)[0]
                windows = _quote_windows(first_sentence, 4, 8)
                return windows[:8]

        # Safe NA filter using word boundaries
        LOW = raw.lower()
        NEG_PATTERNS = [r"\bnot\s+specified\b", r"\bnot\s+mentioned\b", r"\bno\s+maup\s+discussion\b",
                        r"\bno\s+sensitivity\s+analysis\b", r"\bno\s+alternative\s+units\b",
                        r"\bnot\s+applicable\b", r"\bn/?a\b"]
        if any(re.search(p, LOW) for p in NEG_PATTERNS):
            return []

        # Special anchors for explanatory variables
        if variable == "MAUP_Discussion":
            return ["MAUP", "modifiable areal unit problem"]

        if variable == "Sensitivity_Analysis":
            return ["sensitivity analysis", "robustness check", "robustness checks", "robustness test", "robustness tests"]

        if variable == "Rationale_Category":
            # split like "Theory, previous research, and data availability"
            items = [re.sub(r"[^a-zA-Z ]+", "", x).strip() for x in re.split(r"[;,/]| and ", raw)]
            items = [x for x in items if x]
            return list(dict.fromkeys(items)) or [raw]

        if variable == "Unit_Selection_Rationale":
            # prefer supporting quotes if present (handled elsewhere),
            # plus extract enumerated items/keywords
            items = self._extract_enumerated_items(raw)
            if not items:
                # fallback keywords
                items = ["unit of analysis", "spatial unit", "house level", "street segment", "justif", "rationale"]
            return list(dict.fromkeys(items))

        if variable == "Number_of_Data_Sources":
            return ["2 data sources", "two data sources", "two sources of data", "2 sources of data"]

        # Independent_Variables — enhanced statistical table recognition
        if variable == "Independent_Variables":
            # 1) split multi-line content safely
            raw_items = self._split_multiline_items(raw)
            
            # If split_multiline_items failed to parse the numbered list properly,
            # we'll try a direct approach to extract numbered items
            if len(raw_items) <= 1 and "1." in raw and "2." in raw:
                # Extract numbered items directly with regex
                numbered_pattern = r'\d+\.\s*([^0-9\r\n]+?)(?=\d+\.|$)'
                direct_matches = re.findall(numbered_pattern, raw, re.DOTALL)
                if direct_matches:
                    raw_items = [m.strip() for m in direct_matches if m.strip()]
            
            out_terms = []
            
            # Enhanced mapping from CSV variable names to statistical table terms
            variable_mappings = {
                "Built_surface_area": ["Built surface", "surface area", "built area", "Surface (1000 m2)", 
                                      "Built surface (1000 m2)", "building size"],
                "Presence_of_garage": ["Garage", "garage present", "Garage (yes/no)", "Garage (ref: no)",
                                      "Garage present", "Has garage"],
                "Central_heating": ["Central heating", "heating", "central heat", "Heating system", 
                                   "Air conditioning", "HVAC"],
                "Distance_from_offender": ["Distance to house", "Distance", "distance (km)", "Distance to target",
                                          "Distance to offender", "Distance from", "Distance in km"],
                "Construction_type": ["Semi-detached", "Detached", "Terraced", "Construction type", 
                                     "House type", "Semi-detached (ref: terraced)", "Building type"],
                "Number_of_floors": ["floors", "number of floors", "Number floors", "Floor count",
                                    "Multiple floors", "Single floor"],
                "Presence_of_rooftop": ["Rooftop", "rooftop living", "Roof living", "rooftop floor",
                                       "Attic", "Top floor", "Loft"],
                "air_conditioning": ["air conditioning", "AC", "cooling system", "HVAC system"]
            }
            
            # Add each cleaned variable and its table format mappings
            for item in raw_items:
                # Clean up the item - remove numbers and underscores
                clean_item = re.sub(r'^\d+\.\s*', '', item).replace('_', ' ').strip()
                
                # Skip if it's just a number
                if re.match(r'^\d+\.?$', clean_item.strip()):
                    continue
                    
                # Add the clean item
                if _meaningful(clean_item):
                    out_terms.append(clean_item)
                
                # Check for partial matches in variable_mappings and add corresponding table terms
                for csv_var, table_terms in variable_mappings.items():
                    csv_parts = csv_var.lower().replace('_', ' ').split()
                    item_lower = clean_item.lower()
                    
                    # Check if any significant part of the CSV variable is in the item
                    if any(part in item_lower for part in csv_parts if len(part) > 3):
                        out_terms.extend(table_terms)
                
                # Add reduced core anchors (more focused terms)
                core_terms = self._reduce_to_core_anchor(item)
                for term in core_terms:
                    if _meaningful(term):
                        out_terms.append(term)
            
            # Add general statistical table terms that commonly appear
            general_table_terms = [
                "Semi-detached", "Detached", "Terraced", "Floors", "Rooftop", 
                "Garage", "Central heating", "Built surface", "Distance to house",
                "Surface area", "Garage present", "Heating", "Construction type",
                "House type", "Building type", "Property type", "Distance (km)",
                "Distance", "Surface (m2)", "Residential"
            ]
            out_terms.extend(general_table_terms)
            
            # Filter out generic numbers and very short terms
            filtered = []
            for term in out_terms:
                # Skip pure numbers or generic terms
                if re.match(r'^\d+\.?$', term.strip()) or term.strip().lower() in {'1', '2', '3', '4', '5', '6', '7', '8', '9', '0'}:
                    continue
                    
                toks = [t for t in term.split() if t.lower() not in STOPWORDS and len(t) >= 2]
                if toks and len(term.strip()) > 2:
                    filtered.append(term)
            
            # De-dup preserving order
            seen = set(); final = []
            for t in filtered:
                lt = t.lower()
                if lt not in seen:
                    seen.add(lt); final.append(t)
            
            # Add label forms for table/header rescue
            label_forms = []
            for t in final[:10]:  # Add label forms only for the most important terms
                if len(t) > 2:
                    label_forms += [f"{t}:", f"{t} -", f"{t} –"]
            
            return list(dict.fromkeys(final + label_forms))[:60]
            label_forms = []
            for t in final:
                if len(t) > 2:  # avoid short terms
                    label_forms += [f"{t}:", f"{t} -", f"{t} –"]
            
            return list(dict.fromkeys(final + label_forms))[:60]  # cap for sanity

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
                base = (qs[0].strip() if qs else " ".join([t for t in re.split(r'[^0-9A-Za-z.%/+-]+', s) if t][:2]))
                if base:
                    variants = _plural_variants(base)
                    # Keep only 1-2 word anchors, filter long phrases
                    short_variants = [v for v in variants if len(v.split()) <= 2]
                    # Keep underscore + space forms for safety
                    expanded = []
                    for v in short_variants:
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
                        if vl and vl not in seen and len(v.split()) <= 2:  # enforce short anchors
                            seen.add(vl)
                            terms.append(v)
                    return terms[:3]  # limit to top 3 variants
            else:
                # Standard keyword processing for other variables - keep short
                if qs:
                    cand = [q.strip() for q in qs if 1 <= len(q.split()) <= 2]  # max 2 words
                    if cand: 
                        return [cand[0]]
                toks = [t for t in re.split(r'[^0-9A-Za-z.%/+-]+', s) if t]
                if len(toks) <= 2: 
                    return [" ".join(toks)]
                return [" ".join(toks[:2])]  # max 2 words

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

    def highlight_pdf_mode_based(self, pdf_path: Path, variable_entries: List[tuple]):
        """
        Highlight PDF using mode-based approach with smart per-variable policies.
        
        Args:
            variable_entries: List of (variable, terms, mode) triplets
        
        Features:
        - Hit caps per variable to reduce noise
        - 2-pass priority system with preserved mode decisions
        - Policy-based first-only restrictions
        - Comprehensive overlap detection and management
        """
        try:
            doc = fitz.open(str(pdf_path))
            
            # Quick doc text sniff to warn about OCR needs
            doc_text_len = sum(len(_norm_for_match((doc[i].get_text("text") or ""))) for i in range(min(len(doc), 5)))
            if doc_text_len < 200:
                print("⚠️ PDF seems to have very little extractable text. "
                      "If this is a scanned PDF, run OCR (e.g., OCRmyPDF) and retry.")
            else:
                print(f"📄 Document text check: {doc_text_len} normalized chars in first 5 pages")
            
            # Build search regions (never skip page 1, handle references properly)
            search_regions_by_page = _build_search_regions(doc)
            pages_to_scan = list(range(len(doc)))  # do not skip the first page
            
            print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
            print(f"📄 Total pages: {len(doc)}")
            print(f"🔎 Using 2-pass priority system with sentence expansion and caps")
            
            # Sanity-check on easy anchors first
            FLAGS = getattr(fitz, "TEXT_IGNORECASE", 0) | getattr(fitz, "TEXT_DEHYPHENATE", 0)
            probe_terms = ["Ghent", "Belgium"]
            for pt in probe_terms:
                any_hit = False
                for p in range(min(5, len(doc))):
                    if _safe_search_for(doc[p], pt, allowed=search_regions_by_page.get(p), flags=FLAGS):
                        any_hit = True; break
                print(f"🔎 Probe '{pt}':", "FOUND" if any_hit else "not found")
            
            # Debug sanity highlight to prove annotation creation works
            for p in range(min(5, len(doc))):
                test = _safe_search_for(doc[p], "Belgium", allowed=search_regions_by_page.get(p), flags=FLAGS)
                if test:
                    try:
                        doc[p].add_highlight_annot([test[0]])  # should succeed
                        print(f"🟩 Debug: inserted test highlight on page {p+1}")
                        break
                    except Exception as e:
                        print(f"🟥 Debug: add_highlight_annot failed on page {p+1}: {e}")
            
            # Split variables into two passes, preserving mode
            vars_pass_A = []  # High priority: supporting quotes, analysis results
            vars_pass_B = []  # Low priority: simple keywords like country, city, spatial unit
            
            for variable, terms, mode in variable_entries:
                # Prioritize evidence variables and supporting quotes in Pass A
                if (variable in self.HIGH_PRIORITY_VARS or 
                    variable in self.REQUIRE_SUPPORTING or
                    _is_supporting_col(variable) or 
                    variable.lower().startswith("supporting_quotes_for__")):
                    vars_pass_A.append((variable, terms, mode))
                else:
                    vars_pass_B.append((variable, terms, mode))

            ordered_vars = [*vars_pass_A, *vars_pass_B]
            
            print(f"🎯 Pass A (high priority): {len(vars_pass_A)} variables")
            print(f"🎯 Pass B (low priority): {len(vars_pass_B)} variables")
            
            # Track placed highlights for overlap detection
            placed_by_page = defaultdict(list)  # page_num -> [{rect, annot, vars:set(), terms:set()}]
            
            highlights_created = 0
            match_details = []
            not_found_terms = []
            
            # Process each variable with 2-pass priority system
            for variable, terms, mode in ordered_vars:
                category = self.variable_categories.get(variable, 'Default')
                color = self.color_scheme[category]
                
                # Time budgets to prevent combinatorial explosion (loosened for debugging)
                VAR_TIME_BUDGET_S = 12.0    # total per variable
                TERM_TIME_BUDGET_S = 3.0    # per term across all pages
                PAGES_PER_TERM_CAP = len(doc)  # search all pages for each term
                var_t0 = time.perf_counter()
                
                # Get cap for this variable (None = unlimited)
                cap = self.MAX_HITS_CAP.get(variable, None)
                hits_for_var = 0
                
                print(f"\n--- {variable} ({category}, {mode} mode, cap={cap}) ---")
                print(f"Terms to search: {len(terms)}")
                
                variable_hits = 0
                first_hit_page = None
                
                for term in terms:
                    # Check variable time budget
                    if time.perf_counter() - var_t0 > VAR_TIME_BUDGET_S:
                        print(f"⏰ Variable time budget exceeded, moving to next variable")
                        break  # move to next variable quickly
                    
                    term_t0 = time.perf_counter()
                    search_term = term
                    log_term = (term if len(term) <= 50 else term[:50] + '…')
                    
                    # Handle search terms (windows already prepared upstream when needed)
                    search_terms = [search_term]  # windows already prepared upstream when needed
                    
                    # Search pages in ascending order with caps
                    pages_scanned = 0
                    for st in search_terms:
                        # Check term time budget
                        if time.perf_counter() - term_t0 > TERM_TIME_BUDGET_S:
                            print(f"⏰ Term time budget exceeded, skipping to next term")
                            continue
                            
                        for page_num in pages_to_scan:
                            if pages_scanned >= PAGES_PER_TERM_CAP:
                                break
                            pages_scanned += 1
                            page = doc[page_num]
                            
                            # Skip pages without allowed regions (pure references)
                            allowed = search_regions_by_page.get(page_num, None)
                            if allowed == []:
                                continue
                            
                            # Cheap page pre-filter before heavy search (only on text-rich pages)
                            if not hasattr(page, "_norm_text_cache"):
                                # add flags for better extraction and normalize
                                raw_text = page.get_text("text", flags=getattr(fitz, "TEXT_DEHYPHENATE", 0)) or ""
                                page._norm_text_cache = _norm_for_match(raw_text)
                            
                            use_prefilter = len(page._norm_text_cache) >= 80  # only when page is text-rich
                            if use_prefilter:
                                norm_st = _norm_for_match(st)
                                if norm_st and norm_st not in page._norm_text_cache:
                                    continue
                            # if page text is short/noisy, fall through and try real search anyway
                            
                            # EXACT MATCH with flags
                            FLAGS = (
                                getattr(fitz, "TEXT_IGNORECASE", 0)
                                | getattr(fitz, "TEXT_DEHYPHENATE", 0)
                                # intentionally NOT using PRESERVE_WHITESPACE to be more forgiving
                            )

                            # 1) Try fast text search with time protection
                            hits = _safe_search_for(page, st, allowed=allowed, flags=FLAGS) or []
                            valid_quads = [q for q in hits if validate_quad(q)]

                            # 2) Fallback: normalized word-stream matching (handles hyphens/line wraps)
                            if not valid_quads:
                                # Check term time budget before fallback
                                if time.perf_counter() - term_t0 > TERM_TIME_BUDGET_S:
                                    continue
                                    
                                # use search term as-is (no more windowing)
                                fallback_terms = [st]
                                for ft in fallback_terms:
                                    rects = find_first_occurrence_rects_by_words(page, ft)
                                    if rects:
                                        # merge the contiguous word rects into one line box (then expand to sentence if needed)
                                        union = fitz.Rect(rects[0])
                                        for rr in rects[1:]:
                                            union |= rr
                                        if allowed not in (None, []):
                                            # make sure union is within allowed region(s)
                                            if not any((union & reg).get_area() > 0 for reg in allowed):
                                                continue
                                        # Validate union rect before creating quad
                                        if union.get_area() > 0:
                                            try:
                                                valid_quads = [safe_quad_from_rect(union)]
                                                break
                                            except Exception as e:
                                                # Skip if quad creation fails
                                                continue

                            new_hits = 0
                            for q in valid_quads:
                                # allow both Quad and Rect from the fallback
                                union = q.rect if hasattr(q, "rect") else fitz.Rect(q)
                                if any(rect_iou(union, ph["rect"]) >= self.OVERLAP_IOU for ph in placed_by_page[page_num]):
                                    continue

                                # Sentence expansion only for selected vars
                                if self._should_expand_sentence(variable):
                                    # if we created a Rect, wrap it into a dummy quad for the expander
                                    qq = q if hasattr(q, "rect") else safe_quad_from_rect(union)
                                    rect_to_use = self._expand_quad_to_sentence_rect(page, qq)
                                else:
                                    rect_to_use = union

                                # Recheck overlap using the expanded rect
                                if any(rect_iou(rect_to_use, ph["rect"]) >= self.OVERLAP_IOU for ph in placed_by_page[page_num]):
                                    continue

                                # Create highlight with fallback
                                try:
                                    quad = safe_quad_from_rect(rect_to_use)
                                    annot = page.add_highlight_annot([quad])
                                except Exception as e1:
                                    try:
                                        # fallback: let PyMuPDF build the quad from a tight text search bbox
                                        annot = page.add_highlight_annot(rect_to_use)
                                    except Exception as e2:
                                        print(f"   ⚠️ highlight failed on p{page_num+1}: {e1} / {e2}")
                                        continue

                                annot.set_colors(stroke=color)              # highlight annotations only support stroke colors
                                annot.set_opacity(0.80)                     # darker but readable
                                annot.set_info(title=variable, content="")
                                annot.update()

                                placed_by_page[page_num].append({"rect": rect_to_use, "annot": annot, "vars": {variable}, "terms": {st}})
                                highlights_created += 1
                                variable_hits += 1
                                new_hits += 1
                                hits_for_var += 1
                                
                                if first_hit_page is None:
                                    first_hit_page = page_num + 1
                                
                                match_details.append({
                                    "Page": page_num + 1,
                                    "Variable": variable,
                                    "Term": st,
                                    "Mode": mode,
                                    "HitsOnPage": 1,
                                    "Color": color,
                                    "Expanded": self._should_expand_sentence(variable)
                                })

                                # === enforce per-variable cap (e.g., Spatial_Unit_Name = 1) ===
                                if cap is not None and hits_for_var >= cap:
                                    break
                            
                            # Log hits found on this page
                            if new_hits > 0:
                                window_indicator = f" (window of '{log_term}')" if mode == "quote" and st != search_term else ""
                                print(f"   ✅ FOUND: '{st[:50]}{'…' if len(st) > 50 else ''}'{window_indicator} on page {page_num + 1} ({new_hits} hits)")
                            
                            if cap is not None and hits_for_var >= cap:
                                break
                        if cap is not None and hits_for_var >= cap:
                            break
                    if cap is not None and hits_for_var >= cap:
                        break
                
                # Log per-variable summary
                if variable_hits > 0:
                    cap_msg = f" (hit cap of {cap})" if cap and hits_for_var >= cap else ""
                    print(f"📊 {variable}: terms_generated={len(terms)}, total_hits={variable_hits}, first_hit_page={first_hit_page}{cap_msg}")
                else:
                    print(f"📊 {variable}: terms_generated={len(terms)}, total_hits=0, first_hit_page=NOT FOUND")
                    # Add to not found terms for all terms of this variable
                    for term in terms:
                        not_found_terms.append((variable, term, mode))
            
            # Save highlighted PDF with compression
            output_filename = f"Study_{self.study_id}_{pdf_path.stem}_PRIORITY2PASS.pdf"
            output_path = self.output_folder / output_filename
            doc.save(str(output_path), garbage=4, deflate=True)
            doc.close()
            
            print(f"\n✅ Saved highlighted PDF: {output_filename}")
            print(f"✨ Total highlights created: {highlights_created}")
            print(f"🎯 Pass A (high priority) completed first")
            print(f"🎯 Pass B (low priority) respects Pass A highlights")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'total_searched': sum(len(terms) for _, terms, _ in variable_entries),
                'match_details': match_details,
                'not_found_terms': not_found_terms,
                'output_file': output_path
            }
            
        except Exception as e:
            logging.error(f"Error highlighting PDF: {e}")
            return {'success': False, 'error': str(e)}

    def process_study(self):
        """Process study with 2-pass priority highlighting approach."""
        print("="*80)
        print("2-PASS PRIORITY HIGHLIGHTER")
        print("="*80)
        print("✅ NEW FEATURES:")
        print("🎯 Pass A: High priority (Supporting quotes, analysis results)")
        print("🎯 Pass B: Low priority (Keywords like country, city, spatial unit)")
        print("📏 Sentence expansion for evidence variables")
        print("🔒 Per-variable caps (house = 1, etc.)")
        print("🚫 Pass B respects Pass A highlights (no overlap)")
        print("="*80)
        
        # Load study data
        study_data = self.load_study_data()
        if study_data is None:
            print("❌ Failed to load study data")
            return False
        
        # Find PDF file
        try:
            pdf_path = self.find_pdf_file(study_data)
        except FileNotFoundError as e:
            print(f"❌ {e}")
            return False
        
        print(f"\n📖 Processing PDF: {pdf_path.name}")
        print(f"🆔 Study ID: {self.study_id}")
        
        # Generate search terms for all defined columns
        variable_entries = []  # List of (variable, terms, mode) triplets
        total_columns_processed = 0
        total_terms_generated = 0
        
        print(f"\n🔍 GENERATING SEARCH TERMS:")
        
        # Process all columns that have modes defined
        for column in study_data.index:
            column_str = str(column).strip()
            
            # Check if this is a Supporting_* column
            is_supporting = _is_supporting_col(column_str) or column_str.lower().startswith("supporting_quotes_for__")

            # NEW: if it's supporting AND its base variable is in SKIP_SUPPORTING, skip it
            if is_supporting:
                base = self.base_var_from_supporting(column_str)  # e.g., Supporting_quotes_for__Country_ -> Country
                # normalize for robust comparison
                base_key = base.replace(" ", "_").strip("_").lower()
                if base_key in self._skip_supporting_lower:
                    # hard skip: we don't want supporting quotes for these factual vars
                    print(f"  📋 {column_str}: SKIPPED (base '{base}' is SKIP_SUPPORTING)")
                    continue

            if column_str in self.skip_columns:
                continue

            # Policy-driven mode selection
            if column_str in self.SKIP_SUPPORTING:
                mode = self.VAR_MODE.get(column_str, "keyword")  # highlight value only
            elif column_str in self.REQUIRE_SUPPORTING or is_supporting:
                mode = "quote"  # pull direct quotes/windows  
            elif column_str in self.SUMMARY_OK:
                mode = self.VAR_MODE.get(column_str, "exact")    # summary fine; quotes optional
            elif column_str == "Variables":
                mode = "keyword"
            else:
                # ignore columns we don't handle
                continue
                
            # Skip completely empty values
            value = study_data[column]
            if pd.isna(value) or str(value).strip() == '':
                print(f"  📋 {column_str}: EMPTY - skipped")
                continue
            
            # Generate search terms
            terms = self.generate_search_terms(value, column_str)
            
            if terms:
                # collect triplets (var, terms, mode)
                variable_entries.append((column_str, terms, mode))
                total_columns_processed += 1
                total_terms_generated += len(terms)
                print(f"  📋 {column_str} ({mode}): {len(terms)} terms - {str(value)[:60]}...")
            else:
                print(f"  📋 {column_str} ({mode}): No terms generated")
        
        print(f"\n📊 TERM GENERATION SUMMARY:")
        print(f"   Total columns processed: {total_columns_processed}")
        print(f"   Variables with terms: {len(variable_entries)}")
        print(f"   Total terms generated: {total_terms_generated}")
        
        if not variable_entries:
            print("❌ No terms to highlight!")
            return False
        
        # Apply 2-pass priority highlighting
        result = self.highlight_pdf_mode_based(pdf_path, variable_entries)
        
        if result['success']:
            print(f"\n" + "="*80)
            print("2-PASS PRIORITY HIGHLIGHTING RESULTS")
            print("="*80)
            print(f"✨ Successfully Highlighted: {result['highlights_created']}")
            print(f"📊 Total Terms Processed: {result['total_searched']}")
            
            # Save results
            if result['match_details']:
                df_results = pd.DataFrame(result['match_details'])
                results_csv = self.output_folder / f"study_{self.study_id}_priority2pass_results.csv"
                df_results.to_csv(results_csv, index=False, encoding='utf-8')
                print(f"📋 CSV summary: {results_csv.name}")
            
            return True
        else:
            print(f"❌ Highlighting failed: {result.get('error', 'Unknown error')}")
            return False


def main():
    """Main entry point for the 2-pass priority highlighter."""
    print("🎯 2-Pass Priority PDF Highlighter")
    print("=" * 60)
    
    # Process specific study or interactive mode
    import sys
    if len(sys.argv) > 1:
        try:
            study_id = int(sys.argv[1])
        except ValueError:
            print("❌ Invalid study ID. Please provide a number.")
            return False
    else:
        # Interactive mode
        try:
            study_id = int(input("Enter Study ID: "))
        except ValueError:
            print("❌ Invalid input. Please enter a number.")
            return False
    
    # Create and run highlighter
    highlighter = ModeBasedHighlighter()
    highlighter.study_id = study_id
    
    success = highlighter.process_study()
    
    if success:
        print("\n🎉 Processing completed successfully!")
        return True
    else:
        print("\n❌ Processing failed!")
        return False


if __name__ == "__main__":
    main()
if __name__ == "__main__":
    # Use main() function to handle command line arguments
    main()
