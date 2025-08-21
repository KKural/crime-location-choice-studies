#!/usr/bin/env python3
"""
Fixed Comprehensive Variable Highlighter
========================================

FIXES APPLIED:
1. First occurrence only - scans all pages in order, stops after first match
2. Configurable deduplication policy instead of global blocking
3. Uses exact cell content with minimal splitting for long values
4. Processes ALL 98 columns from    def process_study(self):
        """Process study with first occurrence highlighting for 30 target columns only."""
        print("="*80)
        print("FIRST-OCCURRENCE VARIABLE HIGHLIGHTER")
        print("="*80)
        print("✅ REQUIREMENTS:")
        print("🎯 Only 30 target columns (excluding Supporting_/Reasoning_)")
        print("🔍 First occurrence only in entire PDF")
        print("📝 Exact content with light normalization")
        print("🎨 7 original categories + neutral gray")
        print("🧩 Quads + TEXT_DEHYPHENATE for better matching")art categorization
5. Robust PDF selection with multiple fallback strategies
6. Uses quads=True and TEXT_DEHYPHENATE|TEXT_IGNORECASE for better matching
"""

import pandas as pd
import fitz
from pathlib import Path
import logging
import re
from typing import Dict, List, Tuple, Set, Optional
from datetime import datetime

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class OptimizedVariableHighlighter:
    def __init__(self, study_id=1):
        """
        Initialize the highlighter.
        
        Args:
            study_id: Study ID to process
        """
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
        """
        Light normalization of text content.
        """
        if not text:
            return text
        
        # Strip whitespace
        text = str(text).strip()
        
        # Collapse multiple spaces
        text = re.sub(r'\s+', ' ', text)
        
        # Convert smart quotes to straight quotes
        text = text.replace('"', '"').replace('"', '"').replace(''', "'").replace(''', "'")
        
        # Convert en/em dashes to regular dash
        text = text.replace('–', '-').replace('—', '-')
        
        # Remove soft hyphen
        text = text.replace('\u00AD', '')
        
        # Replace non-breaking space with normal space
        text = text.replace('\u00A0', ' ')
        
        # Map common ligatures
        ligature_map = {
            'ﬁ': 'fi',
            'ﬂ': 'fl',
            'ﬀ': 'ff',
            'ﬃ': 'ffi',
            'ﬄ': 'ffl'
        }
        for ligature, replacement in ligature_map.items():
            text = text.replace(ligature, replacement)
        
        return text

    def generate_search_terms(self, value, variable: str) -> List[str]:
        """
        Generate search terms from cell value with exact content processing.
        Returns list of terms (de-duplicated per variable).
        """
        if pd.isna(value) or str(value).strip() == '':
            return []
        
        # Normalize the text
        normalized = self.normalize_text(value)
        terms = []
        
        # If length ≤ 120 chars → use the whole cell as single search term
        if len(normalized) <= 120:
            terms.append(normalized)
        else:
            # Split on ; first, otherwise on sentence punctuation
            segments = []
            
            # Try semicolon first
            if ';' in normalized:
                segments = [s.strip() for s in normalized.split(';')]
            else:
                # Split on sentence punctuation (. ; : \n)
                segments = re.split(r'[.;:\n]', normalized)
                segments = [s.strip() for s in segments]
            
            # Keep segments with length 10..200 chars
            valid_segments = [s for s in segments if 10 <= len(s) <= 200]
            
            if valid_segments:
                terms.extend(valid_segments)
            else:
                # Fall back to the full string
                terms.append(normalized)
        
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
        """
        Find PDF file using 4-tier fallback strategy.
        """
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
        """
        Highlight PDF with first occurrence only using improved search.
        """
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
                            text_instances = page.search_for(term, quads=True, hit_max=1, flags=search_flags)
                        except (TypeError, AttributeError):
                            # Fallback for older PyMuPDF versions
                            text_instances = page.search_for(term)
                        
                        if text_instances:
                            # Use quads if available, otherwise convert to rect
                            if hasattr(text_instances[0], 'rect'):
                                rect = text_instances[0].rect
                            else:
                                rect = text_instances[0]
                            
                            # Create highlight with quads if supported
                            try:
                                highlight = page.add_highlight_annot(text_instances)
                            except:
                                highlight = page.add_highlight_annot(rect)
                            
                            highlight.set_colors(stroke=color)
                            
                            # Add small text annotation
                            note_text = f"{variable}\n{term[:250]}"
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
                            
                            print(f"   ✅ FOUND: '{term[:50]}...' on page {page_num + 1}")
                            found_match = True
                            break  # Stop after first match in the document
                    
                    if not found_match:
                        not_found_terms.append((variable, term))
                        print(f"   ❌ NOT FOUND: '{term[:50]}...'")
                
                # Log per-variable summary
                if variable_hits > 0:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page={first_hit_page}")
                else:
                    print(f"📊 {variable}: terms_generated={len(terms)}, first_hit_page=NOT FOUND")
            
            # Save highlighted PDF
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
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
        """Process a single study with comprehensive column coverage and robust matching."""
        print("="*80)
        print("FIXED COMPREHENSIVE VARIABLE-SPECIFIC PDF HIGHLIGHTER")
        print("="*80)
        print("✅ FIXES APPLIED:")
        print("🎯 First occurrence only - scans pages in order")
        print("� Configurable deduplication instead of global blocking") 
        print("📝 Uses exact cell content with minimal splitting")
        print("📊 Processes ALL 98 columns with smart categorization")
        print("🔍 Robust PDF selection with fallback strategies")
        print("🧩 Better text matching with flags and quads")
        print("="*80)
        
        # Load study data
        study_data = self.load_study_data()
        if study_data is None:
            print("❌ Failed to load study data")
            return False
        
        # Find PDF with robust search
        pdf_path = self.find_pdf_file(study_data)
        if not pdf_path:
            print("❌ PDF file not found")
            return False
        
        print(f"\n📖 Processing PDF: {pdf_path.name}")
        print(f"🆔 Study ID: {self.study_id}")
        print(f"🔄 Dedup policy: {self.dedup_policy}")
        
        # Extract highlight values for ALL columns
        highlight_values = {category: [] for category in self.color_scheme.keys()}
        processed_columns = 0
        skipped_columns = 0
        
        print(f"\n🔍 PROCESSING ALL AVAILABLE COLUMNS:")
        
        for column in study_data.index:
            if column in ['Study_ID']:  # Skip ID columns
                continue
                
            processed_columns += 1
            value = study_data[column]
            
            # Skip completely empty values
            if pd.isna(value) or str(value).strip() == '':
                skipped_columns += 1
                continue
            
            # Get category for this column (with default fallback)
            category = self.variable_categories.get(column, 'Default')
            
            print(f"  📋 {column} → {category}: {str(value)[:60]}...")
            
            # Extract search terms
            terms = self.extract_search_terms(value, column)
            
            for term, term_type in terms:
                highlight_values[category].append((term, column, term_type))
        
        # Summary of extraction
        total_terms = sum(len(values) for values in highlight_values.values())
        non_empty_categories = {cat: len(vals) for cat, vals in highlight_values.items() if vals}
        
        print(f"\n📊 EXTRACTION SUMMARY:")
        print(f"   Columns processed: {processed_columns}")
        print(f"   Columns skipped (empty): {skipped_columns}")
        print(f"   Total search terms: {total_terms}")
        print(f"   Categories with data: {len(non_empty_categories)}")
        
        for category, count in non_empty_categories.items():
            print(f"     {category}: {count} terms")
        
        if total_terms == 0:
            print("❌ No terms to highlight!")
            return False
        
        # Highlight PDF
        result = self.highlight_pdf_with_annotations(pdf_path, highlight_values)
        
        if result['success']:
            success_rate = (result['highlights_created'] / result['total_searched']) * 100
            
            print(f"\n" + "="*80)
            print("COMPREHENSIVE HIGHLIGHTING RESULTS")
            print("="*80)
            print(f"📊 Total Terms Searched: {result['total_searched']}")
            print(f"✨ Successfully Highlighted: {result['highlights_created']}")
            print(f"🎯 Success Rate: {success_rate:.1f}%")
            print(f"🔄 Deduplication Policy: {result['dedup_policy']}")
            
            # Save detailed results
            if result['match_details']:
                df_results = pd.DataFrame(result['match_details'])
                results_csv = self.output_folder / f"study_{self.study_id}_comprehensive_results.csv"
                df_results.to_csv(results_csv, index=False, encoding='utf-8')
                print(f"📋 Detailed results: {results_csv.name}")
                
                # Show category breakdown
                category_summary = df_results.groupby('Category').size().sort_values(ascending=False)
                print(f"\n📈 HIGHLIGHTS BY CATEGORY:")
                for category, count in category_summary.items():
                    print(f"   {category}: {count} highlights")
            
            print(f"\n🎨 Highlighted PDF: {result['output_file'].name}")
            
            return True
        else:
            print(f"❌ HIGHLIGHTING FAILED: {result.get('error', 'Unknown error')}")
            return False

def main():
    """Main function with configurable options."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Fixed Comprehensive Variable Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID to process')
    parser.add_argument('--dedup-policy', choices=['none', 'per_variable', 'global'], 
                       default='per_variable', help='Deduplication policy')
    
    args = parser.parse_args()
    
    highlighter = OptimizedVariableHighlighter(
        study_id=args.study_id, 
        dedup_policy=args.dedup_policy
    )
    
    success = highlighter.process_study()
    
    if success:
        print(f"\n🎉 SUCCESS! Study {args.study_id} processed successfully.")
    else:
        print(f"\n❌ FAILED! Study {args.study_id} could not be processed.")
    
    return success

if __name__ == "__main__":
    # For direct execution, use defaults
    highlighter = OptimizedVariableHighlighter(study_id=1, dedup_policy='per_variable')
    highlighter.process_study()
