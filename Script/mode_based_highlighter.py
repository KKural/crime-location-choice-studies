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
    words = page.get_text("words") or []
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
    
    # Check if any of the points are invalid
    try:
        for point in [quad.ul, quad.ur, quad.ll, quad.lr]:
            if not isinstance(point.x, (int, float)) or not isinstance(point.y, (int, float)):
                return False
            if not (0 <= point.x <= 1000) or not (0 <= point.y <= 1000):  # Reasonable bounds check
                return False
    except (AttributeError, TypeError):
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
        
        # Policy: Only highlight first occurrence per variable (not per term)
        self.first_hit_per_variable = True
        
        # Configuration flags for highlighting behavior
        self.MERGE_OVERLAPS = False        # don't merge overlapping highlights
        self.OVERLAP_IOU = 0.85            # how similar two rectangles must be to count as "same spot"
        
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
        
        # 7 original categories + neutral gray for unmapped
        self.color_scheme = {
            'Geographic': (1.0, 0.0, 0.0),      # Red
            'Study_Area': (0.0, 0.0, 1.0),      # Blue  
            'Quantitative': (0.0, 1.0, 0.0),    # Green
            'Crime_Data': (1.0, 0.65, 0.0),     # Orange
            'Methodology': (0.5, 0.0, 0.5),     # Purple
            'Results': (0.0, 1.0, 1.0),         # Cyan
            'Limitations': (1.0, 0.0, 1.0),     # Magenta
            'Default': (0.6, 0.6, 0.6)          # Neutral Gray
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
        text = str(text).strip()
        
        # Convert smart quotes to straight quotes
        text = (
            text
            .replace('\u2018', "'").replace('\u2019', "'")  # left/right single quotes
            .replace('\u201C', '"').replace('\u201D', '"')  # left/right double quotes
            .replace('"', '"').replace('"', '"')
            .replace(''', "'").replace(''', "'")
        )
        
        # Normalize dashes and hyphens
        text = text.replace('\u2013', '–').replace('\u2014', '—')  # ensure canonical forms
        
        # Remove soft hyphen
        text = text.replace('\u00AD', '')
        
        # Replace non-breaking space with normal space
        text = text.replace('\u00A0', ' ')
        
        # Map common ligatures
        ligature_map = {
            'ﬁ': 'fi', 'ﬂ': 'fl', 'ﬀ': 'ff', 'ﬃ': 'ffi', 'ﬄ': 'ffl'
        }
        for ligature, replacement in ligature_map.items():
            text = text.replace(ligature, replacement)
        
        # Collapse multiple spaces at the end to not break exact matching
        text = re.sub(r'\s+', ' ', text)
        
        return text

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

        # Skip obvious "nothing here" cells
        if any(s in raw.lower() for s in ("not specified", "not mentioned", "no maup discussion",
                                          "no sensitivity analysis", "no alternative units",
                                          "not applicable", "n/a", "na")):
            return []

        # Quote-mode for Supporting_* (verbatim)
        if variable.startswith("Supporting_"):
            qs = re.findall(r'["""\'`]{1,3}\s*(.+?)\s*["""\'`]{1,3}', raw, flags=re.S)
            return list(dict.fromkeys([q.strip() for q in qs if q.strip()]))

        # Independent variables: strip numbering + underscores -> spaces
        if variable == "Independent_Variables":
            text = raw.replace("_", " ")
            # remove leading enumeration like "1. ", "(a) ", "2) "
            text = re.sub(r'(?m)^\s*(?:\(?\d+[\.\)]|\(?[A-Za-z][\.\)])\s*', '', text)
            # split on separators
            parts = re.split(r'[;•|]|(?:\s{2,})|\n', text)
            out = []
            for p in parts:
                p = p.strip(" -•;\n\t\r")
                if not p:
                    continue
                # drop trailing enumeration fragments
                p = re.sub(r'\s*\(?\d+[\.\)]\s*$', '', p).strip()
                if p:
                    out.append(p)
            return list(dict.fromkeys(out))

        # Coefficients / Effect_Sizes: anchor on label before colon
        if variable in {"Coefficients", "Effect_Sizes"}:
            lines = re.split(r'(?:\n|;|•)', raw)
            labels = []
            for ln in lines:
                m = re.match(r'\s*[-–—]?\s*([^:]+):', ln)
                if m:
                    lab = m.group(1).strip()
                    # keep short, readable anchor (e.g., "Bars", "Night shops")
                    # also accept underscore→space variant if any
                    labels.append(lab.replace("_", " "))
            return list(dict.fromkeys([l for l in labels if l]))

        # Short keyword anchors (1–3 words)
        if variable in {"Country", "City", "Crime_Type", "Crime_Type_Group",
                        "Spatial_Unit_Name", "Model_Type", "Software_Used"}:
            s = raw.replace("_", " ").strip()
            # prefer quoted phrase if present
            qs = re.findall(r'["""\'`]{1,3}\s*(.+?)\s*["""\'`]{1,3}', s)
            if qs:
                cand = [q.strip() for q in qs if 1 <= len(q.split()) <= 3]
                if cand:
                    return [cand[0]]
            # else take 1–3 word anchor
            toks = [t for t in re.split(r'[^0-9A-Za-z.%]+', s) if t]
            if len(toks) <= 3:
                return [" ".join(toks)]
            # fall back to first 2–3 tokens
            return [" ".join(toks[:3])]

        # Numeric exacts that appear verbatim
        if variable in {"Data_Collection_Period", "Total_Study_Area_Size", "Number_of_Units",
                        "Average_Population_per_Unit", "Crime_Incidents"}:
            return [self.normalize_text(raw)]

        # List-like exacts (split if clearly multiple)
        if variable in {"Alternative_Units", "Future_Suggestions",
                        "Unit_Selection_Rationale", "Data_Limitations",
                        "Computational_Constraints", "Model_Fit_Statistics",
                        "Confidence_Intervals", "Spatial_Aggregation", "Data_Sources"}:
            s = self.normalize_text(raw)
            parts = [p.strip() for p in re.split(r'[;•|]\s*', s) if p.strip()]
            return parts or [s]

        # Default: exact phrase (normalized) + underscore→space variant
        s = self.normalize_text(raw)
        out = [s]
        if "_" in raw:
            out.insert(0, raw.replace("_", " ").strip())
        return list(dict.fromkeys(out))

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
            highlights_created = 0
            match_details = []
            not_found_terms = []
            
            print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
            print(f"📄 Total pages: {len(doc)}")
            
            # Track placed highlights for overlap detection
            placed_by_page = defaultdict(list)  # page_num -> [{rect, annot, vars:set(), terms:set()}]
            
            # Process each variable and its terms
            for variable, terms in variable_terms.items():
                category = self.variable_categories.get(variable, 'Default')
                color = self.color_scheme[category]
                mode = self.VAR_MODE.get(variable, "exact")
                
                print(f"\n--- {variable} ({category}, {mode} mode) ---")
                print(f"Terms to search: {len(terms)}")
                
                variable_hits = 0
                first_hit_page = None
                
                for term in terms:
                    found_match = False
                    search_term = term
                    log_term = (term if len(term) <= 50 else term[:50] + '…')
                    
                    # Search all pages in ascending order for FIRST occurrence
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        
                        # 1) try word-stream (rects)
                        rects = find_first_occurrence_rects_by_words(page, search_term)

                        # 2) fall back to page.search_for
                        if not rects:
                            try:
                                hits = page.search_for(search_term, quads=False, flags=fitz.TEXT_DEHYPHENATE | getattr(fitz, "TEXT_IGNORECASE", 0))
                            except Exception:
                                hits = page.search_for(search_term)  # older versions
                            rects = [hits[0]] if isinstance(hits, list) and hits else []

                        if not rects:
                            continue

                        # overlap check uses a union rect
                        union = rects[0] if len(rects) == 1 else fitz.Rect(min(r.x0 for r in rects), min(r.y0 for r in rects),
                                                                           max(r.x1 for r in rects), max(r.y1 for r in rects))

                        # Check for overlaps with existing highlights
                        used = any(rect_iou(union, ph["rect"]) >= self.OVERLAP_IOU for ph in placed_by_page[page_num])
                        if used:
                            continue  # keep scanning next occurrences/pages

                        # build safe quads and add annotation
                        quads = safe_quads_from_rects(rects)
                        try:
                            if quads:
                                highlight = page.add_highlight_annot(quads)
                            else:
                                highlight = page.add_highlight_annot(union)
                        except Exception:
                            highlight = page.add_highlight_annot(union)

                        highlight.set_colors(stroke=color)
                        highlight.set_info(title=variable, content=f"{variable} ({mode})\n{search_term[:250]}")
                        highlight.update()
                        
                        placed_by_page[page_num].append({
                            "rect": union, "annot": highlight, "vars": {variable}, "terms": {search_term}
                        })
                        
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
                    
                    if found_match and self.first_hit_per_variable:
                        break  # Break term loop after first variable hit when policy is enabled
                    
                    if not found_match:
                        not_found_terms.append((variable, search_term, mode))
                        print(f"   ❌ NOT FOUND: '{log_term}'")
                
                # Log per-variable summary
                if variable_hits > 0:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page={first_hit_page}")
                else:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page=NOT FOUND")
            
            # Save highlighted PDF
            output_filename = f"Study_{self.study_id}_{pdf_path.stem}_MODEBASED.pdf"
            output_path = self.output_folder / output_filename
            doc.save(str(output_path))
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
