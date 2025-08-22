#!/usr/bin/env python3
"""
First-Occurrence Variable Highlighter
=====================================

Updates to meet exact requirements:
1. Only 30 target columns (excludSKIP_PHRASES = (
    "not specified", "not mentioned", "no maup discussion",
    "no sensitivity analysis", "no alternative units",
    "not applicable", "n/a", "na"
)g_/Reasoning_)
2. First occurrence only in entire PDF per term
3. Exact content with light normalization
4. 7 original categories + neutral gray
5. Robust PDF selection with 4-tier fallback
6. Quads + TEXT_DEHYPHENATE for better matching
7. Comprehensive diagnostics and CSV output
"""

import pandas as pd
import fitz
from pathlib import Path
import logging
import re
import unicodedata
from typing import Dict, List, Set, Optional
from datetime import datetime
from collections import defaultdict

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
                logging.info(f"Skipping invalid or short normalized text for: {text}")
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

def find_first_occurrence_quads_by_words(page, phrase: str) -> list[fitz.Quad]:
    """Exact phrase match with hyphen/line-break joins (full-span quads)."""
    target = _norm_for_match(phrase).split()
    if not target:
        return []

    stream = getattr(page, "_cached_stream", None)
    if stream is None:
        stream = page._cached_stream = _page_word_stream(page)

    n, m = len(stream), len(target)
    MAX_JOIN = 3  # allow joining up to 3 adjacent tokens for one CSV word

    i = 0
    while i <= n - m:
        j = i
        quads = []
        ok = True
        for t in target:
            best = None
            # try consuming 1..MAX_JOIN tokens to equal t when concatenated
            for k in range(1, MAX_JOIN + 1):
                if j + k > n:
                    break
                joined = "".join(stream[j + p][0] for p in range(k))
                if joined == t:
                    best = k
                    break
            if best is None:
                ok = False
                break
            for p in range(best):
                try:
                    rect = stream[j + p][1]
                    # Additional validation of rectangle coordinates
                    if not validate_bounding_box(rect):
                        logging.warning(f"Invalid bounding box for token {stream[j + p][0]}: {rect}")
                        ok = False
                        break
                    
                    # Ensure coordinates are reasonable before creating quad
                    if not (0 <= rect.x0 <= rect.x1 <= 1000) or not (0 <= rect.y0 <= rect.y1 <= 1000):
                        logging.warning(f"Rectangle coordinates out of reasonable bounds: {rect}")
                        ok = False
                        break
                        
                    # Safe creation of quad with error handling
                    try:
                        quad = fitz.Quad(rect)
                        if validate_quad(quad):
                            quads.append(quad)
                        else:
                            logging.warning(f"Created quad failed validation for token {stream[j + p][0]}: {rect}")
                            ok = False
                            break
                    except Exception as e:
                        logging.error(f"Error creating Quad for token {stream[j + p][0]} with rect {rect}: {e}")
                        ok = False
                        break
                except Exception as e:
                    logging.error(f"General error processing token at position {j+p}: {e}")
                    ok = False
                    break
            j += best
        if ok and quads:
            return quads
        i += 1
    return []

SKIP_PHRASES = (
    "not specified", "not mentioned", "no maup discussion",
    "no sensitivity analysis", "no alternative units",
    "not applicable", "n/a", "na"
)

def generate_search_terms(raw):
    s = self.normalize_text(raw)
    ls = s.lower()
    if any(p in ls for p in SKIP_PHRASES):
        return []  # don’t waste time; this wording isn’t in the PDF
    # ...existing code...
    return terms

class FirstOccurrenceHighlighter:
    def __init__(self, study_id=1):
        """Initialize the highlighter for first occurrence highlighting."""
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_FirstOnly_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Track what we've highlighted per variable
        self.seen = set()  # (variable, term_lower)
        
        # Policy: Only highlight first occurrence per variable (not per term)
        self.first_hit_per_variable = True
        
        # Configuration flags for exact highlighting behavior
        self.EXACT_TERMS_ONLY = True           # only search exactly what's in the cell
        self.MERGE_OVERLAPS = True             # merge if two variables land on the same spot
        self.OVERLAP_IOU = 0.60                # how similar two rectangles must be to count as "same spot"
        
        # Skip these count columns
        self.skip_columns = {"Number_of_Data_Sources", "Number_of_Variables"}
        
        # Exact 30 columns to process (excluding Supporting_/Reasoning_)
        self.target_columns = {
            'Country', 'City', 'Total_Study_Area_Size', 'Spatial_Unit_Name', 'Unit_Size', 
            'Number_of_Units', 'Average_Population_per_Unit', 'Spatial_Aggregation', 
            'Unit_Selection_Rationale', 'Rationale_Category', 'Data_Limitations', 
            'Computational_Constraints', 'Alternative_Units', 'Data_Collection_Period', 
            'Data_Sources', 'Number_of_Data_Sources', 'Crime_Type', 'Crime_Type_Group', 
            'Crime_Incidents', 'Model_Type', 'Independent_Variables', 'Number_of_Variables', 
            'Model_Fit_Statistics', 'Coefficients', 'Confidence_Intervals', 'Effect_Sizes', 
            'Software_Used', 'MAUP_Discussion', 'Sensitivity_Analysis', 'Future_Suggestions'
        }
        
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
        
        # Variable categorization (only for the 30 target columns)
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
            'Future_Suggestions': 'Methodology'
        }

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
            .replace('“', '"').replace('”', '"')
            .replace('‘', "'").replace('’', "'")
        )
        
        # Normalize dashes and hyphens
        text = text.replace('\u2013', '–').replace('\u2014', '—')  # ensure canonical forms
        # We'll expand variants later; keep original too
        
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

    def extract_keywords_from_summary(self, text: str, variable: str) -> List[str]:
        """Extract searchable keywords from summary/paraphrase cells."""
        keywords = []
        text_lower = text.lower()
        
        # Variable-specific keyword extraction
        if variable == 'Sensitivity_Analysis':
            if 'no sensitivity' in text_lower or 'not performed' in text_lower:
                keywords.extend(['sensitivity analysis', 'sensitivity'])
            else:
                keywords.append('sensitivity analysis')
        
        elif variable == 'Confidence_Intervals':
            keywords.extend(['confidence interval', '95% confidence', 'CI'])
        
        elif variable == 'MAUP_Discussion':
            keywords.extend(['MAUP', 'modifiable areal unit', 'aggregation'])
        
        elif variable in ['Data_Limitations', 'Computational_Constraints']:
            # Extract key constraint/limitation terms
            constraint_terms = ['limited', 'constraint', 'issue', 'problem', 'lack of data', 'computational']
            for term in constraint_terms:
                if term in text_lower:
                    keywords.append(term)
        
        elif 'crime_type' in variable.lower():
            if 'single' in text_lower:
                keywords.extend(['burglary', 'residential burglary'])
            else:
                keywords.append('crime type')
        
        elif variable == 'Average_Population_per_Unit':
            if 'not specified' in text_lower:
                keywords.extend(['population', 'inhabitants', 'residents'])
            else:
                keywords.append('population per unit')
        
        elif variable == 'Rationale_Category':
            # Anchor to broad rationale cues
            keywords.extend(['theory', 'previous research', 'data availability'])
        
        return keywords

    def expand_unit_variants(self, term: str) -> List[str]:
        """Expand terms with common unit/symbol variants."""
        variants = [term]  # Original term first
        
        # Common unit/symbol substitutions and synonyms
        unit_map = {
            'km²': ['km2', 'km^2', 'square kilometers', 'square kilometres', 'sq km', 'sq. km'],
            'km2': ['km²', 'km^2', 'square kilometers', 'square kilometres', 'sq km', 'sq. km'],
            'km^2': ['km²', 'km2', 'square kilometers', 'square kilometres', 'sq km', 'sq. km'],
            'R²': ['R2', 'R^2', 'R-squared', 'R squared', "McFadden's R2", 'pseudo R2', 'pseudo-R2', 'Pseudo R-squared'],
            'R2': ['R²', 'R^2', 'R-squared', 'R squared', "McFadden's R2", 'pseudo R2', 'pseudo-R2'],
            'R^2': ['R²', 'R2', 'R-squared', 'R squared'],
            'CI': ['confidence interval', '95% CI', '95 % CI', 'confidence intervals'],
            '%': [' percent '],
        }
        
        for original, replacements in unit_map.items():
            if original in term:
                for replacement in replacements:
                    variants.append(term.replace(original, replacement))
        
        # Add some well-known model fit anchors if we see hints
        hints = ['pseudo r', 'mcfadden', 'likelihood ratio', 'aic', 'bic', 'log-likelihood']
        lower = term.lower()
        if any(h in lower for h in hints):
            variants.extend(['pseudo r', 'mcfadden', 'likelihood ratio', 'aic', 'bic', 'log-likelihood'])
        
        return list(dict.fromkeys(variants))  # Remove duplicates while preserving order

    def clean_list_numbering(self, text: str) -> str:
        """Remove list numbering and formatting from text."""
        # Remove common list patterns: 1., 2), 3:, a), -, •
        text = re.sub(r'^\s*(?:\(?\d+[\.\)\:]|[a-zA-Z][\.\)])\s*', '', text)  # 1. 2) 3: a)
        text = re.sub(r'^\s*[-•]\s*', '', text)  # - or •
        text = text.strip()
        return text

    def fix_underscores(self, text: str) -> List[str]:
        """Generate variants with underscores replaced by spaces."""
        variants = [text]
        if '_' in text:
            # Prefer space-variant first
            space_variant = text.replace('_', ' ')
            variants = [space_variant, text]
        return variants

    def expand_dash_variants(self, term: str) -> List[str]:
        """Expand ASCII hyphen to en/em dash variants and with/without spaces around dash."""
        variants = [term]
        dash_patterns = [('-', '-'), ('-', '–'), ('-', '—')]
        for ascii_dash, repl in dash_patterns:
            if ascii_dash in term:
                base = term.replace(ascii_dash, repl)
                variants.append(base)
                # Variants with spaces around dash
                variants.append(base.replace(repl, f' {repl} '))
        return list(dict.fromkeys(variants))

    def extract_enumerated_items(self, text: str) -> List[str]:
        """Extract items from enumerated lists like '1. item 2. item 3. item' without leakage."""
        if not text:
            return []
        t = self.normalize_text(text)
        # Replace newlines with space to catch inline enumerations
        t = t.replace('\n', ' ')
        # Pattern: number/letter followed by . or ) then item text up to next number/letter marker or end
        pattern = r'(?:^|\s)(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+(.+?)(?=\s+(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+|$)'
        items = [self.clean_list_numbering(m.strip()) for m in re.findall(pattern, t)]
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

    def split_semicolon_items(self, text: str) -> List[str]:
        """Always split items by semicolon for specific columns."""
        if not text:
            return []
        t = self.normalize_text(text)
        parts = [p.strip() for p in re.split(r'[;|]', t) if p.strip()]
        # Also split on ' and ' if both sides look like sources/tools
        refined = []
        for p in parts:
            if ' and ' in p and len(p) > 20:
                refined.extend([x.strip() for x in p.split(' and ') if x.strip()])
            else:
                refined.append(p)
        return refined

    def smart_chunk_split(self, text: str) -> List[str]:
        """Split long text into searchable chunks intelligently."""
        if len(text) <= 200:
            return [text]
        
        chunks = []
        
        # Try different delimiters in order of preference
        delimiters = [';', '\n', '.', '|', ' - ', ' and ', ' & ', ',']
        
        for delimiter in delimiters:
            if delimiter in text:
                parts = [p.strip() for p in text.split(delimiter)]
                for part in parts:
                    if 10 <= len(part) <= 200:
                        chunks.append(part)
                if chunks:  # If we found good chunks, use them
                    break
        
        # If no good chunks found, try sentence-like splits
        if not chunks:
            # Split on sentence boundaries but keep reasonable length
            sentences = re.split(r'[.!?]\s+', text)
            for sentence in sentences:
                sentence = sentence.strip()
                if 10 <= len(sentence) <= 200:
                    chunks.append(sentence)
                elif len(sentence) > 200:
                    # Further split long sentences on commas
                    sub_parts = [p.strip() for p in sentence.split(',')]
                    for part in sub_parts:
                        if 10 <= len(part) <= 200:
                            chunks.append(part)
        
        return chunks if chunks else [text]  # Fallback to original

    def expand_term_variants(self, base: str) -> List[str]:
        """Compose all variants: underscores, units/symbols, and dash variants."""
        variants: List[str] = []
        for v1 in self.fix_underscores(base):
            for v2 in self.expand_unit_variants(v1):
                for v3 in self.expand_dash_variants(v2):
                    variants.append(v3)
        # unique preserve order
        seen = set()
        result = []
        for v in variants:
            if v.lower() not in seen:
                seen.add(v.lower())
                result.append(v)
        return result

    def generate_search_terms(self, value, variable: str) -> List[str]:
        """Exact terms only: use the cell text as-is (with light normalization).
           Split only where the cell clearly contains multiple items.
        """
        if pd.isna(value):
            return []
        raw = str(value).strip()
        if not raw:
            return []

        s = self.normalize_text(raw)

        # Always split these as lists
        if variable in {"Data_Sources", "Software_Used"}:
            parts = [p.strip() for p in re.split(r'[;|]', s) if p.strip()]
            # keep comma-split for Software_Used (versions etc.)
            if variable == "Software_Used":
                more = []
                for p in parts:
                    more += [x.strip() for x in p.split(',') if x.strip()]
                parts = more or parts
            terms = parts

        # Enumerated items for variables that are list-like
        elif variable in {
            "Independent_Variables","Coefficients","Effect_Sizes",
            "Alternative_Units","Future_Suggestions","Unit_Selection_Rationale",
            "Data_Limitations","Computational_Constraints"
        }:
            t = s.replace("\n", " ")
            pat = r'(?:^|\s)(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+(.+?)(?=\s+(?:\(?\d+[\.\)]|\(?[a-zA-Z][\.\)])\s+|$)'
            items = [m.strip() for m in re.findall(pat, t)]
            if not items:
                items = [x.strip() for x in re.split(r'[;•]', t) if x.strip()]
            terms = items or [s]

        else:
            # Single exact term
            terms = [s]

        # De-dupe per variable, but keep per-variable independence
        out = []
        seen_local = set()
        for term in terms:
            term = term.strip()
            if not term:
                continue
            if term.lower() in seen_local:
                continue
            seen_local.add(term.lower())

            # Keep a *space* variant if the CSV uses underscores (your cells sometimes do)
            if "_" in term:
                out.append(term.replace("_", " "))
            out.append(term)   # exact as-is

        # record per-variable de-dup globally
        unique_terms = []
        for t in out:
            key = (variable, t.lower())
            if key not in self.seen:
                self.seen.add(key)
                unique_terms.append(t)

        return unique_terms

    def load_study_data(self):
        """Load study data from CSV with proper error handling."""
        try:
            df = pd.read_csv(self.csv_path)
            
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

    def highlight_pdf_first_occurrence(self, pdf_path: Path, variable_terms: Dict[str, List[str]]):
        """Highlight PDF with first occurrence only using improved search."""
        try:
            doc = fitz.open(str(pdf_path))
            highlights_created = 0
            match_details = []
            not_found_terms = []
            image_only_warning_shown = False
            
            print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
            print(f"📄 Total pages: {len(doc)}")
            
            # Set up search flags
            search_flags = fitz.TEXT_DEHYPHENATE
            try:
                search_flags |= fitz.TEXT_IGNORECASE  # Add if available
            except AttributeError:
                pass
            
            # Track placed highlights for overlap detection
            placed_by_page = defaultdict(list)  # page_num -> [{rect, annot, vars:set(), terms:set()}]
            
            # Process each variable and its terms
            for variable, terms in variable_terms.items():
                category = self.variable_categories.get(variable, 'Default')
                color = self.color_scheme[category]
                
                print(f"\n--- {variable} ({category}) ---")
                print(f"Terms to search: {len(terms)}")
                
                variable_hits = 0
                first_hit_page = None
                
                for term in terms:  # no cap — exact terms only
                    found_match = False
                    # Keep a clean copy of the term for searching, and a truncated one for logging
                    search_term = term
                    log_term = (term if len(term) <= 50 else term[:50] + '…')
                    
                    # Search all pages in ascending order for FIRST occurrence
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        
                        # Check if page has selectable text
                        if not image_only_warning_shown:
                            page_text = page.get_text()
                            if not page_text.strip():
                                print(f"⚠️  WARNING: Page {page_num + 1} may be scanned/OCR needed")
                                image_only_warning_shown = True
                        
                        # Try word-based exact phrase match first (best for full-phrase highlight)
                        quads = []
                        try:
                            quads = find_first_occurrence_quads_by_words(page, search_term)
                        except Exception as e:
                            logging.error(f"Error in word-based matching for '{search_term}': {e}")
                            # Continue with fallback methods

                        # First fallback: Use simple PyMuPDF search if word-based matching failed
                        if not quads:
                            try:
                                hits = page.search_for(search_term, quads=True, 
                                                     flags=fitz.TEXT_DEHYPHENATE | getattr(fitz, "TEXT_IGNORECASE", 0))
                                if isinstance(hits, list) and hits:
                                    first = hits[0]
                                    if hasattr(first, "rect"):
                                        quads = [first]
                                    else:
                                        try:
                                            quads = [fitz.Quad(first)]
                                        except Exception as quad_err:
                                            logging.error(f"Error creating quad from search hit: {quad_err}")
                            except (TypeError, AttributeError, Exception) as e:
                                logging.error(f"Error in basic search for '{search_term}': {e}")
                        
                        # Second fallback: If both approaches failed, try rectangle-based highlighting
                        if not quads:
                            try:
                                # Simple text search with rectangles
                                rects = page.search_for(search_term)
                                if isinstance(rects, list) and rects and rects[0].is_valid:
                                    # Directly use rectangles for highlighting instead of quads
                                    rect = rects[0]
                                    # Skip the quad creation entirely - we'll handle this special case later
                                    quads = ["USE_RECT_FALLBACK"]
                                    # Store the rectangle for later use
                                    page._special_rect = rect
                            except Exception as e:
                                logging.error(f"Error in rectangle fallback search for '{search_term}': {e}")
                        
                        if not quads:
                            continue
                        
                        # Build a single bounding rect for overlap checks
                        rect = quads[0].rect if len(quads) == 1 else fitz.Rect(
                            min(q.ul.x for q in quads), min(q.ul.y for q in quads),
                            max(q.lr.x for q in quads), max(q.lr.y for q in quads)
                        )
                        
                        # Optional: merge same-spot highlights
                        merged = False
                        if self.MERGE_OVERLAPS:
                            for ph in placed_by_page[page_num]:
                                if rect_iou(rect, ph["rect"]) >= self.OVERLAP_IOU:
                                    ph["vars"].add(variable)
                                    ph["terms"].add(search_term)
                                    title = " | ".join(sorted(ph["vars"]))
                                    content = f"{title}\n{'; '.join(sorted(ph['terms']))[:250]}"
                                    ph["annot"].set_info(title=title, content=content)
                                    ph["annot"].update()
                                    merged = True
                                    break
                        
                        if merged:
                            variable_hits += 1
                            if first_hit_page is None:
                                first_hit_page = page_num + 1
                            match_details.append({
                                'Page': page_num + 1,
                                'Variable': variable,
                                'Term': search_term,
                                'HitsOnPage': 1,
                                'Color': color
                            })
                            found_match = True
                            break
                        
                        # Create highlight with ALL quads (full phrase) — no sticky note icon
                        try:
                            # Handle special case for rectangle fallback
                            if quads and quads[0] == "USE_RECT_FALLBACK" and hasattr(page, "_special_rect"):
                                highlight = page.add_highlight_annot(page._special_rect)
                                # Clean up the special attribute
                                delattr(page, "_special_rect")
                            else:
                                try:
                                    highlight = page.add_highlight_annot(quads)
                                except Exception:
                                    # If highlighting with quads fails, try the first quad's rectangle
                                    if quads and hasattr(quads[0], "rect") and quads[0].rect.is_valid:
                                        highlight = page.add_highlight_annot(quads[0].rect)
                                    else:
                                        logging.error(f"Failed to create highlight for '{search_term}' - no valid quads or rectangles")
                                        continue
                            
                            highlight.set_colors(stroke=color)
                            highlight.set_info(title=variable, content=f"{variable}\n{search_term[:250]}")
                            highlight.update()
                            
                            placed_by_page[page_num].append({
                                "rect": rect, "annot": highlight, "vars": {variable}, "terms": {search_term}
                            })
                        except Exception as e:
                            logging.error(f"Error creating highlight annotation: {e}")
                            continue
                        
                        # Record the match
                        highlights_created += 1
                        variable_hits += 1
                        if first_hit_page is None:
                            first_hit_page = page_num + 1
                        
                        match_details.append({
                            "Page": page_num + 1,
                            "Variable": variable,
                            "Term": search_term,
                            "HitsOnPage": 1,
                            "Color": color
                        })
                        
                        print(f"   ✅ FOUND: '{log_term}' on page {page_num + 1}")
                        found_match = True
                        break  # Stop after first match in the document
                    
                    if found_match and self.first_hit_per_variable:
                        break  # Break term loop after first variable hit when policy is enabled
                    
                    if not found_match:
                        not_found_terms.append((variable, search_term))
                        print(f"   ❌ NOT FOUND: '{log_term}'")
                
                # Log per-variable summary
                if variable_hits > 0:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page={first_hit_page}")
                else:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page=NOT FOUND")
            
            # Save highlighted PDF
            output_filename = f"Study_{self.study_id}_{pdf_path.stem}_FIRSTONLY.pdf"
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
        """Process study with first occurrence highlighting for 30 target columns only."""
        print("="*80)
        print("FIRST-OCCURRENCE VARIABLE HIGHLIGHTER")
        print("="*80)
        print("✅ REQUIREMENTS:")
        print("🎯 Only 30 target columns (excluding Supporting_/Reasoning_)")
        print("🔍 First occurrence only in entire PDF")
        print("📝 Exact content with light normalization")
        print("🎨 7 original categories + neutral gray")
        print("🧩 Quads + TEXT_DEHYPHENATE for better matching")
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
        print(f"📋 Target columns: {len(self.target_columns)}")
        
        # Process only the 30 target columns (exclude Supporting_/Reasoning_)
        variable_terms = {}
        total_columns_processed = 0
        total_terms_generated = 0
        
        print(f"\n🔍 PROCESSING TARGET COLUMNS:")
        
        for column in self.target_columns:
            # Skip Supporting_/Reasoning_ columns explicitly
            if column.startswith('Supporting_') or column.startswith('Reasoning_'):
                continue
            
            # Skip count columns
            if column in self.skip_columns:
                print(f"  📋 {column}: skipped (count field)")
                continue
                
            if column not in study_data.index:
                print(f"  ⚠️  {column} not found in CSV")
                continue
            
            value = study_data[column]
            
            # Skip completely empty values
            if pd.isna(value) or str(value).strip() == '':
                print(f"  📋 {column}: EMPTY - skipped")
                continue
            
            total_columns_processed += 1
            
            # Generate search terms
            terms = self.generate_search_terms(value, column)
            if terms:
                variable_terms[column] = terms
                total_terms_generated += len(terms)
                print(f"  📋 {column}: {len(terms)} terms - {str(value)[:60]}...")
            else:
                print(f"  📋 {column}: No terms generated")
        
        print(f"\n📊 PROCESSING SUMMARY:")
        print(f"   Target columns available: {total_columns_processed}")
        print(f"   Variables with terms: {len(variable_terms)}")
        print(f"   Total terms generated: {total_terms_generated}")
        
        if not variable_terms:
            print("❌ No terms to highlight!")
            return False
        
        # Highlight PDF
        result = self.highlight_pdf_first_occurrence(pdf_path, variable_terms)
        
        if result['success']:
            success_rate = (result['highlights_created'] / result['total_searched']) * 100
            
            print(f"\n" + "="*80)
            print("FIRST-OCCURRENCE HIGHLIGHTING RESULTS")
            print("="*80)
            print(f"📊 Total Terms Searched: {result['total_searched']}")
            print(f"✨ Successfully Highlighted: {result['highlights_created']}")
            print(f"🎯 Success Rate: {success_rate:.1f}%")
            
            # Diagnostics: If success rate < 25%, show top 20 not-found terms
            if success_rate < 25.0 and result['not_found_terms']:
                print(f"\n🔍 DIAGNOSTICS (Success rate < 25%):")
                print("Top 20 not-found terms (longest first):")
                sorted_not_found = sorted(result['not_found_terms'], 
                                        key=lambda x: len(x[1]), reverse=True)[:20]
                for variable, term in sorted_not_found:
                    print(f"   {variable}: '{term[:80]}...'")
            
            # Save CSV summary
            if result['match_details']:
                df_results = pd.DataFrame(result['match_details'])
                results_csv = self.output_folder / f"study_{self.study_id}_firstonly_results.csv"
                df_results.to_csv(results_csv, index=False, encoding='utf-8')
                print(f"📋 CSV summary: {results_csv.name}")
                
                # Show category breakdown
                if not df_results.empty:
                    category_summary = {}
                    for _, row in df_results.iterrows():
                        var = row['Variable']
                        category = self.variable_categories.get(var, 'Default')
                        category_summary[category] = category_summary.get(category, 0) + 1
                    
                    print(f"\n📈 HIGHLIGHTS BY CATEGORY:")
                    for category, count in sorted(category_summary.items()):
                        print(f"   {category}: {count} highlights")
            
            print(f"\n🎨 Highlighted PDF: {result['output_file'].name}")
            return True
        else:
            print(f"❌ HIGHLIGHTING FAILED: {result.get('error', 'Unknown error')}")
            return False

def main():
    """Main function with configurable study ID."""
    import argparse
    
    parser = argparse.ArgumentParser(description='First-Occurrence Variable Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID to process')
    
    args = parser.parse_args()
    
    highlighter = FirstOccurrenceHighlighter(study_id=args.study_id)
    success = highlighter.process_study()
    
    if success:
        print(f"\n🎉 SUCCESS! Study {args.study_id} processed successfully.")
    else:
        print(f"\n❌ FAILED! Study {args.study_id} could not be processed.")
    
    return success

if __name__ == "__main__":
    # Use main() function to handle command line arguments
    main()
