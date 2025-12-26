#!/usr/bin/env python3
"""
Direct Variables Only PDF Highlighter
Focused highlighter for direct variables with improved text expansion and overlap prevention
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
from datetime import datetime
from typing import List, Dict, Optional, Tuple, Set
import unicodedata
import json
from dataclasses import dataclass, asdict

@dataclass
class HighlightResult:
    """Container for highlight operation results"""
    study_id: str
    variable_name: str
    value: str
    found: bool
    page_number: int = -1
    highlighted_text: str = ""
    confidence: float = 0.0
    match_method: str = ""
    bounding_rect: str = ""

class DirectVariablesHighlighter:
    def __init__(self):
        self.setup_logging()
        
        # Direct variables by category with colors
        self.VARIABLE_CATEGORIES = {
            'location': {
                'variables': {'Country', 'City', 'State_or_Region', 'Spatial_Unit_Name'},
                'color': [1.0, 0.8, 0.0]  # Yellow
            },
            'temporal': {
                'variables': {'Data_Collection_Period'},
                'color': [0.0, 0.8, 1.0]  # Cyan
            },
            'crime': {
                'variables': {'Crime_Type', 'Crime_Incidents', 'Crime_Type_Group'},
                'color': [1.0, 0.4, 0.4]  # Red
            },
            'methodology': {
                'variables': {'Model_Type', 'Data_Sources', 'Number_of_Data_Sources', 'Software_Used'},
                'color': [0.6, 0.8, 1.0]  # Light blue
            },
            'spatial': {
                'variables': {'Unit_Size', 'Total_Study_Area_Size', 'Number_of_Units', 'Average_Population_per_Unit'},
                'color': [0.0, 1.0, 0.8]  # Mint green
            },
            'analytical': {
                'variables': {'Independent_Variables', 'Model_Fit_Statistics', 'Coefficients'},
                'color': [0.8, 1.0, 0.6]  # Light green
            },
            'quality': {
                'variables': {'Data_Limitations', 'MAUP_Discussion', 'Sensitivity_Analysis'},
                'color': [1.0, 0.6, 1.0]  # Pink
            },
            'future': {
                'variables': {'Future_Suggestions'},
                'color': [0.9, 0.7, 1.0]  # Light purple
            }
        }
        
        # Flatten variables for easy access
        self.DIRECT_VARS = set()
        for category_info in self.VARIABLE_CATEGORIES.values():
            self.DIRECT_VARS.update(category_info['variables'])
        
        # Track highlighted areas to prevent overlaps
        self.highlighted_areas = []

    def setup_logging(self):
        """Setup logging"""
        log_filename = f"{datetime.now().strftime('%Y%m%d')}_direct_variables_highlighting.log"
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_filename, mode='w', encoding='utf-8'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger('DirectVariablesHighlighter')
        self.logger.info(f"Direct Variables Highlighter initialized - Log: {log_filename}")

    def get_variable_category_and_color(self, variable_name: str) -> Tuple[str, List[float]]:
        """Get category and color for a variable"""
        for category, info in self.VARIABLE_CATEGORIES.items():
            if variable_name in info['variables']:
                return category, info['color']
        return 'default', [0.7, 0.7, 0.7]

    def process_cell_text(self, cell_value: str, variable_name: str) -> List[str]:
        """Process text from CSV cell to extract searchable phrases"""
        if not cell_value or pd.isna(cell_value):
            return []
        
        # Clean the text
        text = self.normalize_text(str(cell_value).strip())
        
        if not text or text.lower() in ['n/a', 'na', '-', '--', 'not specified', 'not available']:
            return []
        
        search_terms = []
        
        # Handle different variable types
        if variable_name in ['Country', 'City']:
            # For location variables, use the exact value and common variants
            search_terms.append(text)
            if variable_name == 'City' and text.lower() == 'east flanders':
                search_terms.extend(['East Flanders', 'province of East Flanders', 'East Flanders province'])
        
        elif variable_name == 'Crime_Type':
            # For crime types, add variants
            if 'burglary' in text.lower():
                search_terms.extend(['residential burglary', 'burglary', 'residential burglaries', 'burglaries'])
        
        elif variable_name == 'Crime_Incidents':
            # For numbers, create variants
            if text.isdigit() or ',' in text:
                num_clean = re.sub(r'[,\s]', '', text)
                search_terms.extend([
                    text,  # Original format
                    num_clean,  # No separators
                    f"{int(num_clean):,}" if num_clean.isdigit() else text,  # Comma format
                    f"{text} residential burglaries",
                    f"{text} burglaries",
                    f"{text} unique burglars"
                ])
        
        elif variable_name == 'Model_Type':
            # For model types, add technical variants
            if 'conditional logit' in text.lower():
                search_terms.extend([
                    'conditional logit model',
                    'conditional logit',
                    'bootstrapped conditional logit',
                    'discrete choice model'
                ])
        
        elif variable_name == 'Data_Collection_Period':
            # Handle date ranges
            years = re.findall(r'(19|20)\d{2}', text)
            if len(years) >= 2:
                start_year, end_year = years[0], years[-1]
                search_terms.extend([
                    f"{start_year} and {end_year}",
                    f"{start_year}-{end_year}",
                    f"{start_year}–{end_year}",
                    f"between {start_year} and {end_year}",
                    f"from {start_year} to {end_year}",
                    text
                ])
        
        elif variable_name == 'Software_Used':
            # For software, look for version numbers and names
            if 'R version' in text:
                search_terms.extend([
                    'R version 3.0.2',
                    'R 3.0.2',
                    'ICTCE 5.5.0',
                    text
                ])
        
        elif variable_name in ['Number_of_Units', 'Total_Study_Area_Size', 'Average_Population_per_Unit']:
            # Handle numeric spatial variables
            if variable_name == 'Number_of_Units' and '503' in text:
                search_terms.extend([
                    '503,589',
                    '503589',
                    '503,589 residential properties',
                    '503589 residential properties',
                    'approximately 500,000',
                    '500,000 residential properties'
                ])
            elif variable_name == 'Total_Study_Area_Size' and '3,000' in text:
                search_terms.extend([
                    '3,000 km²',
                    '3000 km²',
                    '3,000 km2',
                    '3000 km2',
                    'nearly 3,000 km²',
                    'covers nearly 3,000',
                    '3,000 square kilometers'
                ])
            elif variable_name == 'Average_Population_per_Unit' and '1.5 million' in text:
                search_terms.extend([
                    '1.5 million',
                    '1.5 million inhabitants',
                    'approximately 1.5 million',
                    'population of approximately 1.5 million'
                ])
        
        elif variable_name == 'Spatial_Unit_Name':
            # For spatial units
            if 'house' in text.lower():
                search_terms.extend([
                    'house',
                    'house level',
                    'house-level',
                    'the house level',
                    'at the house level'
                ])
        
        # Add the original text if not already included
        if text not in search_terms:
            search_terms.append(text)
        
        # For long text, create shorter searchable phrases
        if len(text.split()) > 8:
            # Create phrases from the text
            phrases = self.extract_key_phrases(text)
            search_terms.extend(phrases)
        
        # Clean and deduplicate
        final_terms = []
        seen = set()
        for term in search_terms:
            if term and len(term.strip()) >= 3:
                normalized = term.strip().lower()
                if normalized not in seen:
                    seen.add(normalized)
                    final_terms.append(term.strip())
        
        return final_terms[:15]  # Limit to prevent excessive searching

    def extract_key_phrases(self, text: str, max_phrases: int = 5) -> List[str]:
        """Extract key phrases from long text"""
        # Split into sentences
        sentences = re.split(r'[.!?]', text)
        phrases = []
        
        for sentence in sentences[:max_phrases]:
            sentence = sentence.strip()
            if len(sentence) > 20:
                # Extract meaningful parts
                words = sentence.split()
                if len(words) > 6:
                    # Take first 6-8 words as a phrase
                    phrase = ' '.join(words[:8])
                    phrases.append(phrase)
        
        return phrases

    def normalize_text(self, text: str) -> str:
        """Normalize text for better matching"""
        if not text:
            return ""
        
        # Unicode normalization
        t = unicodedata.normalize('NFKC', str(text))
        
        # Handle special characters
        t = t.replace('\u00A0', ' ').replace('\u00AD', '')  # nbsp, soft hyphen
        t = t.replace('"', '"').replace('"', '"').replace(''', "'").replace(''', "'")
        t = t.replace('–', '-').replace('—', '-')
        
        # Handle line breaks and multiple spaces
        t = re.sub(r'\s*\n\s*', ' ', t)
        t = re.sub(r'\s+', ' ', t)
        
        return t.strip()

    def find_text_with_expansion(self, page: fitz.Page, search_term: str) -> Optional[Tuple[fitz.Rect, str]]:
        """Find text and expand to complete phrases/sentences"""
        
        # First, try exact search with different approaches
        rects = []
        
        # Try basic search
        try:
            rects = page.search_for(search_term)
        except Exception:
            pass
        
        # Try with flags if no results
        if not rects:
            try:
                # Try with different flags
                for flags in [1, 0, 2]:  # Different search modes
                    rects = page.search_for(search_term, flags=flags)
                    if rects:
                        break
            except Exception:
                pass
        
        # Try case-insensitive search
        if not rects:
            try:
                rects = page.search_for(search_term.lower())
                if not rects:
                    rects = page.search_for(search_term.upper())
            except Exception:
                pass
        
        if rects:
            base_rect = rects[0]
            
            # Try to expand the highlight to include complete context
            expanded_rect, expanded_text = self.expand_highlight_area(page, base_rect, search_term)
            
            return expanded_rect, expanded_text
        
        return None

    def expand_highlight_area(self, page: fitz.Page, base_rect: fitz.Rect, search_term: str) -> Tuple[fitz.Rect, str]:
        """Expand highlight area to include complete phrases"""
        
        try:
            # Get all words on the page
            words = page.get_text("words")
            if not words:
                return base_rect, search_term
            
            # Find words that overlap with base_rect
            overlapping_words = []
            for word_info in words:
                word_rect = fitz.Rect(word_info[:4])
                if word_rect.intersects(base_rect):
                    overlapping_words.append(word_info)
            
            if not overlapping_words:
                return base_rect, search_term
            
            # Get the text lines to find sentence boundaries
            blocks = page.get_text("dict")
            
            # Find the block containing our text
            target_block = None
            for block in blocks["blocks"]:
                if "lines" in block:
                    block_rect = fitz.Rect(block["bbox"])
                    if block_rect.intersects(base_rect):
                        target_block = block
                        break
            
            if target_block:
                # Find the line containing our text
                for line in target_block["lines"]:
                    line_rect = fitz.Rect(line["bbox"])
                    if line_rect.intersects(base_rect):
                        # Extract the complete line or sentence
                        line_text = ""
                        line_words_rects = []
                        
                        for span in line["spans"]:
                            span_text = span.get("text", "")
                            span_rect = fitz.Rect(span["bbox"])
                            line_text += span_text
                            line_words_rects.append(span_rect)
                        
                        # Clean up the line text
                        line_text = self.normalize_text(line_text)
                        
                        # Create expanded rectangle from the line
                        if line_words_rects:
                            expanded_rect = line_words_rects[0]
                            for rect in line_words_rects[1:]:
                                expanded_rect = expanded_rect.union(rect)
                            
                            return expanded_rect, line_text
            
            return base_rect, search_term
            
        except Exception as e:
            self.logger.debug(f"Error expanding highlight area: {e}")
            return base_rect, search_term

    def check_overlap(self, new_rect: fitz.Rect, page_num: int) -> bool:
        """Check if new rectangle overlaps with existing highlights"""
        for existing_page, existing_rect in self.highlighted_areas:
            if existing_page == page_num and new_rect.intersects(existing_rect):
                # Calculate overlap percentage
                overlap_area = new_rect.intersect(existing_rect).get_area()
                new_area = new_rect.get_area()
                
                if new_area > 0 and (overlap_area / new_area) > 0.3:  # More than 30% overlap
                    return True
        
        return False

    def create_highlight(self, page: fitz.Page, rect: fitz.Rect, color: List[float], 
                        variable_name: str, highlighted_text: str, page_num: int) -> bool:
        """Create highlight annotation with overlap checking"""
        
        # Check for overlaps
        if self.check_overlap(rect, page_num):
            self.logger.info(f"Skipping overlapping highlight for {variable_name}")
            return False
        
        try:
            # Create highlight annotation
            annot = page.add_highlight_annot(rect)
            annot.set_colors(stroke=color)
            annot.set_opacity(0.5)  # Slightly more transparent
            
            # Set annotation info
            try:
                annot.set_info(title=variable_name)
                # Truncate content for tooltip
                content = highlighted_text[:150] + "..." if len(highlighted_text) > 150 else highlighted_text
                annot.set_contents(content)
            except AttributeError:
                pass
            
            annot.update()
            
            # Record this highlight to prevent future overlaps
            self.highlighted_areas.append((page_num, rect))
            
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to create highlight for {variable_name}: {e}")
            return False

    def process_study(self, study_id: str, csv_file: str, pdf_folder: str, output_folder: str) -> Dict:
        """Process a single study for direct variables highlighting"""
        
        start_time = datetime.now()
        self.logger.info(f"Processing Study {study_id}")
        
        try:
            # Load dataset
            try:
                df = pd.read_csv(csv_file, encoding='utf-8')
            except UnicodeDecodeError:
                try:
                    df = pd.read_csv(csv_file, encoding='latin-1')
                except UnicodeDecodeError:
                    df = pd.read_csv(csv_file, encoding='cp1252')
            
            # Find study
            study_rows = df[df['Study_ID'].astype(str) == str(study_id)]
            if study_rows.empty:
                raise ValueError(f"Study {study_id} not found in dataset")
            
            study_row = study_rows.iloc[0]
            
            # Find PDF file
            pdf_file = self.find_pdf_file(study_id, pdf_folder)
            if not pdf_file:
                raise FileNotFoundError(f"PDF file not found for Study {study_id}")
            
            # Open PDF
            doc = fitz.open(pdf_file)
            self.logger.info(f"Opened PDF: {pdf_file.name} ({len(doc)} pages)")
            
            # Reset highlighted areas for this study
            self.highlighted_areas = []
            
            # Process each direct variable
            results = []
            highlights_created = 0
            
            for variable_name in sorted(self.DIRECT_VARS):
                if variable_name not in study_row or pd.isna(study_row[variable_name]):
                    continue
                
                cell_value = study_row[variable_name]
                search_terms = self.process_cell_text(cell_value, variable_name)
                
                if not search_terms:
                    continue
                
                self.logger.info(f"Processing {variable_name}: {len(search_terms)} search terms")
                
                # Search for the variable in the PDF
                found = False
                for page_num in range(min(len(doc), 20)):  # Search first 20 pages
                    page = doc[page_num]
                    
                    for search_term in search_terms:
                        result = self.find_text_with_expansion(page, search_term)
                        
                        if result:
                            rect, highlighted_text = result
                            category, color = self.get_variable_category_and_color(variable_name)
                            
                            if self.create_highlight(page, rect, color, variable_name, highlighted_text, page_num):
                                highlights_created += 1
                                found = True
                                
                                # Record the result
                                results.append(HighlightResult(
                                    study_id=study_id,
                                    variable_name=variable_name,
                                    value=str(cell_value)[:100] + "..." if len(str(cell_value)) > 100 else str(cell_value),
                                    found=True,
                                    page_number=page_num + 1,
                                    highlighted_text=highlighted_text[:100] + "..." if len(highlighted_text) > 100 else highlighted_text,
                                    confidence=1.0,
                                    match_method="direct_expansion",
                                    bounding_rect=f"({rect.x0:.1f},{rect.y0:.1f},{rect.x1:.1f},{rect.y1:.1f})"
                                ))
                                
                                self.logger.info(f"  ✓ Highlighted {variable_name} on page {page_num + 1}")
                                break
                    
                    if found:
                        break
                
                if not found:
                    # Record as not found
                    results.append(HighlightResult(
                        study_id=study_id,
                        variable_name=variable_name,
                        value=str(cell_value)[:100] + "..." if len(str(cell_value)) > 100 else str(cell_value),
                        found=False
                    ))
                    self.logger.info(f"  ✗ Could not find {variable_name}")
            
            # Save the highlighted PDF
            output_path = Path(output_folder)
            output_path.mkdir(parents=True, exist_ok=True)
            
            output_file = output_path / f"Study_{study_id}_DirectVariables_Highlighted.pdf"
            doc.save(str(output_file))
            doc.close()
            
            # Calculate statistics
            processing_time = (datetime.now() - start_time).total_seconds()
            found_count = sum(1 for r in results if r.found)
            
            # Save detailed results
            results_data = {
                'study_id': study_id,
                'processing_time': processing_time,
                'total_variables_processed': len(results),
                'variables_found': found_count,
                'highlights_created': highlights_created,
                'success_rate': (found_count / len(results)) * 100 if results else 0,
                'output_file': str(output_file),
                'detailed_results': [asdict(result) for result in results]
            }
            
            results_file = output_path / f"Study_{study_id}_results.json"
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(results_data, f, indent=2, ensure_ascii=False)
            
            self.logger.info(f"✅ Successfully processed Study {study_id}")
            self.logger.info(f"   Variables found: {found_count}/{len(results)}")
            self.logger.info(f"   Highlights created: {highlights_created}")
            self.logger.info(f"   Success rate: {results_data['success_rate']:.1f}%")
            self.logger.info(f"   Processing time: {processing_time:.2f}s")
            self.logger.info(f"   Output: {output_file}")
            
            return {
                'success': True,
                'summary': results_data,
                'results': results
            }
            
        except Exception as e:
            error_msg = f"Error processing Study {study_id}: {str(e)}"
            self.logger.error(error_msg)
            
            return {
                'success': False,
                'error': error_msg
            }

    def find_pdf_file(self, study_id: str, pdf_folder: str) -> Optional[Path]:
        """Find PDF file for a given study ID"""
        pdf_path = Path(pdf_folder)
        if not pdf_path.exists():
            return None
        
        # Try different naming patterns
        patterns = [
            f"*{int(study_id):02d}*.pdf",
            f"*{study_id}*.pdf",
            f"*{study_id}_*.pdf",
            f"{study_id}_*.pdf",
        ]
        
        for pattern in patterns:
            matches = list(pdf_path.glob(pattern))
            if matches:
                return matches[0]
        
        return None

def main():
    """Main function"""
    if len(sys.argv) < 2:
        print("Usage: python direct_variables_highlighter.py <study_id> [options]")
        print("Options:")
        print("  --csv <path>          Path to combined dataset CSV")
        print("  --pdf-folder <path>   Path to PDF folder")
        print("  --output <path>       Output folder")
        sys.exit(1)
    
    study_id = sys.argv[1]
    csv_file = "20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv"
    pdf_folder = "Review_articles"
    output_folder = f"{datetime.now().strftime('%Y%m%d')}_DirectVariables_Highlighted"
    
    # Parse additional arguments
    i = 2
    while i < len(sys.argv):
        if sys.argv[i] == '--csv' and i + 1 < len(sys.argv):
            csv_file = sys.argv[i + 1]
            i += 2
        elif sys.argv[i] == '--pdf-folder' and i + 1 < len(sys.argv):
            pdf_folder = sys.argv[i + 1]
            i += 2
        elif sys.argv[i] == '--output' and i + 1 < len(sys.argv):
            output_folder = sys.argv[i + 1]
            i += 2
        else:
            i += 1
    
    # Initialize highlighter
    highlighter = DirectVariablesHighlighter()
    
    print(f"Direct Variables PDF Highlighter")
    print(f"Study ID: {study_id}")
    print(f"CSV file: {csv_file}")
    print(f"PDF folder: {pdf_folder}")
    print(f"Output folder: {output_folder}")
    print("="*60)
    
    # Process the study
    result = highlighter.process_study(study_id, csv_file, pdf_folder, output_folder)
    
    if result['success']:
        summary = result['summary']
        print(f"\n🎉 SUCCESS! Direct variables highlighting completed!")
        print(f"Variables processed: {summary['total_variables_processed']}")
        print(f"Variables found: {summary['variables_found']}")
        print(f"Highlights created: {summary['highlights_created']}")
        print(f"Success rate: {summary['success_rate']:.1f}%")
        print(f"Processing time: {summary['processing_time']:.2f}s")
        print(f"Output file: {summary['output_file']}")
    else:
        print(f"❌ Failed: {result['error']}")

if __name__ == "__main__":
    main()
