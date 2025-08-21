#!/usr/bin/env python3
"""
First-Occurrence Variable Highlighter
=====================================

Updates to meet exact requirements:
1. Only 30 target columns (excluding Supporting_/Reasoning_)
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
from typing import Dict, List, Set, Optional
from datetime import datetime

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

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
        """Generate search terms from cell value with improved processing and anchors."""
        if pd.isna(value) or str(value).strip() == '':
            return []
        
        # Normalize the text
        normalized = self.normalize_text(value)
        terms = []
        lower_norm = normalized.lower()

        # Variable-specific handling first
        if variable in ['Data_Sources', 'Software_Used']:
            # Always split by semicolons (and pipes) regardless of length
            items = self.split_semicolon_items(normalized)
            # For software, also split commas because tools often comma-separated
            if variable == 'Software_Used':
                extra = []
                for it in items:
                    extra.extend([x.strip() for x in it.split(',') if x.strip()])
                if extra:
                    items = extra
            for item in items:
                item = self.clean_list_numbering(item)
                terms.extend(self.expand_term_variants(item))

        elif variable == 'Independent_Variables':
            items = self.extract_enumerated_items(normalized)
            for item in items:
                terms.extend(self.expand_term_variants(item))

        elif variable == 'Model_Fit_Statistics':
            # Use anchors only; stitched sentences won't match
            anchors = ['pseudo r', 'mcfadden', 'likelihood ratio', 'aic', 'bic', 'log-likelihood']
            terms.extend(anchors)

        elif variable == 'Data_Collection_Period':
            # Generate range and year anchors with dash variants
            m = re.search(r'\b((?:19|20)\d{2})\s*[-–—]\s*((?:19|20)\d{2})\b', normalized)
            if m:
                y1, y2 = m.group(1), m.group(2)
                # Range variants
                ranges = [f"{y1}-{y2}", f"{y1}–{y2}", f"{y1}—{y2}", f"{y1} – {y2}", f"{y1} — {y2}"]
                terms.extend(ranges)
                # Individual years
                terms.extend([y1, y2])
            else:
                # Try 'YYYY to YYYY'
                m2 = re.search(r'\b((?:19|20)\d{2})\s+to\s+((?:19|20)\d{2})\b', normalized, flags=re.IGNORECASE)
                if m2:
                    y1, y2 = m2.group(1), m2.group(2)
                    terms.extend([f"{y1} to {y2}", f"{y1}-{y2}", f"{y1}–{y2}"])
                else:
                    # Fallback: add any year-looking tokens
                    years = re.findall(r'\b(?:19|20)\d{2}\b', normalized)
                    terms.extend(years)

        elif variable in ['Confidence_Intervals']:
            # Avoid bare 'CI' to prevent false positives
            terms.extend(['confidence interval', '95% CI', '95 % CI', '95% confidence'])

        elif variable == 'MAUP_Discussion':
            terms.extend(['MAUP', 'modifiable areal unit', 'aggregation'])

        elif variable == 'Average_Population_per_Unit':
            # Use anchors rather than prose
            terms.extend(['population', 'inhabitants', 'residents'])

        elif variable in ['Data_Limitations', 'Computational_Constraints']:
            # Extract keywords from summary instead of using full text
            terms.extend(self.extract_keywords_from_summary(normalized, variable))

        elif variable == 'Alternative_Units':
            # If colon exists, take left anchor part
            left = normalized.split(':', 1)[0].strip()
            if left:
                terms.extend(self.expand_term_variants(left))

        elif variable == 'Rationale_Category':
            terms.extend(['theory', 'previous research', 'data availability'])

        elif variable == 'Crime_Type_Group':
            if 'single' in lower_norm:
                terms.extend(['burglary', 'residential burglary'])
            else:
                terms.append('crime type')

        elif variable == 'Future_Suggestions':
            # Use anchors typical in suggestion/recommendation sections
            anchors = [
                'fine-grained', 'fine grained',
                'house-level', 'house level',
                'nested logit', 'nested-logit',
                'multiple levels', 'multi-level', 'multilevel',
                'aggregation', 'spatial aggregation',
                'unit of analysis', 'smaller spatial units',
            ]
            # Include dash variants for hyphenated forms
            for a in anchors:
                terms.extend(self.expand_term_variants(a))

        elif variable == 'Coefficients':
            # Short anchors more likely to exist than stitched sentences
            anchors = [
                'odds ratio', 'OR', 'coefficient', 'estimate', 'beta', 'SE',
                'distance', 'terraced', 'semi-detached', 'garage',
                'central heating', 'air-conditioning', 'construction type',
            ]
            for a in anchors:
                terms.extend(self.expand_term_variants(a))

        elif variable == 'Effect_Sizes':
            anchors = [
                'odds ratio', 'OR', 'increase', 'decrease', 'per kilometer', 'km',
                'percentage', '%',
            ]
            for a in anchors:
                terms.extend(self.expand_term_variants(a))

        else:
            # Generic handling
            is_summary = any(phrase in lower_norm for phrase in [
                'not specified', 'not mentioned', 'the authors', 'the study', 'no sensitivity'
            ])
            if is_summary:
                terms.extend(self.extract_keywords_from_summary(normalized, variable))
            elif len(normalized) <= 120:
                terms.extend(self.expand_term_variants(normalized))
            else:
                chunks = self.smart_chunk_split(normalized)
                for chunk in chunks:
                    clean_chunk = self.clean_list_numbering(chunk)
                    if len(clean_chunk) >= 5:
                        terms.extend(self.expand_term_variants(clean_chunk))
        
        # Filter hazardous very short tokens and exact bad abbreviations
        def _filter_terms(var: str, items: List[str]) -> List[str]:
            bad = {"or", "se", "ci"}
            allow_short = {"km", "%"}
            out = []
            for s in items:
                raw = s.strip()
                low = raw.lower()
                if low in bad:
                    continue
                if len(raw) < 3 and low not in allow_short and not re.search(r"\d", raw):
                    continue
                out.append(raw)
            return out

        terms = _filter_terms(variable, terms)

        # De-duplicate terms per variable
        unique_terms = []
        for term in terms:
            term_key = (variable, term.lower())
            if term_key not in self.seen:
                self.seen.add(term_key)
                unique_terms.append(term)
        
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
            
            # Process each variable and its terms
            for variable, terms in variable_terms.items():
                category = self.variable_categories.get(variable, 'Default')
                color = self.color_scheme[category]
                
                print(f"\n--- {variable} ({category}) ---")
                print(f"Terms to search: {len(terms)}")
                
                variable_hits = 0
                first_hit_page = None
                
                for term in terms:
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
                        
                        # Search with improved flags and quads
                        try:
                            text_instances = page.search_for(search_term, quads=True, hit_max=1, flags=search_flags)
                        except (TypeError, AttributeError):
                            # Fallback for older PyMuPDF versions
                            text_instances = page.search_for(search_term)
                        
                        if text_instances:
                            # Always highlight only the first match to avoid confetti
                            first = text_instances[0]
                            rect = getattr(first, 'rect', first)
                            try:
                                # Prefer single-rect highlight to avoid multi-hit annotations
                                highlight = page.add_highlight_annot(rect)
                            except Exception:
                                # Fallback: if rect fails, try with first item as list
                                try:
                                    highlight = page.add_highlight_annot([first])
                                except Exception:
                                    highlight = page.add_highlight_annot(rect)
                            
                            highlight.set_colors(stroke=color)
                            
                            # Add small text annotation
                            note_text = f"{variable}\n{search_term[:250]}"
                            annotation_point = fitz.Point(rect.x0, rect.y0)
                            annotation = page.add_text_annot(annotation_point, note_text)
                            annotation.set_info(content=note_text, title=variable)
                            annotation.update()
                            highlight.update()
                            
                            # Record the match
                            highlights_created += 1
                            variable_hits += 1
                            if first_hit_page is None:
                                first_hit_page = page_num + 1
                            
                            match_details.append({
                                'Page': page_num + 1,
                                'Variable': variable,
                                'Term': term,
                                'HitsOnPage': len(text_instances),
                                'Color': color
                            })
                            
                            print(f"   ✅ FOUND: '{log_term}' on page {page_num + 1}")
                            found_match = True
                            break  # Stop after first match in the document
                    
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
    # For direct execution, use defaults
    highlighter = FirstOccurrenceHighlighter(study_id=1)
    highlighter.process_study()
