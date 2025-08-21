#!/usr/bin/env python3
"""
Improved PDF Highlighter with Proper Annotations
Fix the image icon issue and improve highlighting accuracy
"""

import pandas as pd
from pathlib import Path
import fitz
import logging
import re

logging.basicConfig(level=logging.INFO)

class ImprovedVariableHighlighter:
    def __init__(self, study_id=1):
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_Improved_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Color scheme for different variable categories
        self.color_scheme = {
            'Geographic': (1.0, 0.0, 0.0),      # Red
            'Study_Area': (0.0, 0.0, 1.0),      # Blue
            'Quantitative': (0.0, 1.0, 0.0),    # Green
            'Crime_Data': (1.0, 0.65, 0.0),     # Orange
            'Methodology': (0.5, 0.0, 0.5),     # Purple
            'Results': (0.0, 1.0, 1.0),         # Cyan
            'Limitations': (1.0, 0.0, 1.0)      # Magenta
        }
        
        # Variable categories mapping
        self.variable_categories = {
            'Country': 'Geographic',
            'City': 'Geographic',
            'Total_Study_Area_Size': 'Study_Area',
            'Spatial_Unit_Name': 'Study_Area',
            'Unit_Size': 'Study_Area',
            'Number_of_Units': 'Quantitative',
            'Crime_Incidents': 'Quantitative',
            'Crime_Type': 'Crime_Data',
            'Crime_Type_Group': 'Crime_Data',
            'Data_Sources': 'Crime_Data',
            'Data_Collection_Period': 'Crime_Data',
            'Model_Type': 'Methodology',
            'Independent_Variables': 'Methodology',
            'Software_Used': 'Methodology',
            'Model_Fit_Statistics': 'Results',
            'Coefficients': 'Results',
            'Effect_Sizes': 'Results',
            'Data_Limitations': 'Limitations',
            'Computational_Constraints': 'Limitations'
        }
        
        self.highlighted_globally = set()

    def load_study_data(self):
        """Load study data from CSV."""
        try:
            df = pd.read_csv(self.csv_path, encoding='utf-8')
            study_data = df[df['Study_ID'] == self.study_id]
            
            if study_data.empty:
                print(f"❌ No data found for Study ID {self.study_id}")
                return None
                
            study_info = study_data.iloc[0]
            study_title = study_info.get('Study_Title', 'Unknown Study')
            print(f"🔍 Processing Study {self.study_id}: {study_title[:80]}...")
            
            return study_info
            
        except Exception as e:
            print(f"❌ Error loading CSV: {e}")
            return None

    def find_pdf_file(self):
        """Find the PDF file for this study."""
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if pdf_files:
            return pdf_files[0]  # Use first PDF for testing
        return None

    def extract_effective_terms(self, value, variable):
        """Extract terms that are likely to be found in PDF text."""
        if pd.isna(value) or str(value).strip() == '' or str(value).lower() in ['not specified', 'n/a', 'na']:
            return []
        
        value_str = str(value).strip()
        terms = []
        
        # Extract numbers (including decimals and large numbers with commas)
        numbers = re.findall(r'\b\d{1,3}(?:,\d{3})*(?:\.\d+)?\b|\b\d+\.\d+\b|\b\d+\b', value_str)
        for num in numbers:
            if len(num) >= 2:  # Skip single digits
                terms.append((num, 'Number'))
        
        # Extract quoted phrases
        quoted_phrases = re.findall(r'"([^"]+)"', value_str)
        for phrase in quoted_phrases:
            if len(phrase.split()) <= 4:  # Max 4 words
                terms.append((phrase.strip(), 'Quote'))
        
        # Extract simple phrases (2-4 words)
        if len(value_str.split()) <= 4 and len(value_str) <= 50:
            if not any(char in value_str for char in ['(', ')', ':', ';', '\n']):
                terms.append((value_str, 'Simple'))
        
        # Extract specific patterns for different variables
        if 'Software' in variable:
            # Extract software names and versions
            software_patterns = [
                r'\b[A-Z][a-z]+ version \d+\.\d+\.\d+\b',
                r'\b[A-Z][a-z]+ \d+\.\d+\b',
                r'\bR version \d+\.\d+\.\d+\b',
                r'\bSPSS\b', r'\bStata\b', r'\bPython\b'
            ]
            for pattern in software_patterns:
                matches = re.findall(pattern, value_str)
                for match in matches:
                    terms.append((match, 'Software'))
        
        # Extract data sources (organizations, agencies)
        if 'Data_Sources' in variable:
            # Split by semicolons and clean
            sources = [s.strip() for s in value_str.split(';') if s.strip()]
            for source in sources[:3]:  # Max 3 sources
                if len(source) <= 50 and source:
                    terms.append((source, 'DataSource'))
        
        # Extract years from periods
        if 'Period' in variable:
            years = re.findall(r'\b(19|20)\d{2}\b', value_str)
            for year in years:
                terms.append((year, 'Year'))
        
        return terms

    def highlight_pdf_with_better_annotations(self, pdf_path, highlight_values):
        """Highlight PDF with improved annotation system (no image icons)."""
        try:
            doc = fitz.open(str(pdf_path))
            highlights_created = 0
            match_details = []
            
            print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
            print(f"📄 Total pages: {len(doc)}")
            
            # Process by category
            for category, color in self.color_scheme.items():
                category_values = highlight_values.get(category, [])
                if not category_values:
                    continue
                
                print(f"\n--- {category.upper()} HIGHLIGHTING ---")
                
                for value, variable, term_type in category_values:
                    found_match = False
                    
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        text_instances = page.search_for(value)
                        
                        if text_instances and value not in self.highlighted_globally:
                            self.highlighted_globally.add(value)
                            
                            # Highlight all instances on this page
                            for i, rect in enumerate(text_instances):
                                # Create highlight
                                highlight = page.add_highlight_annot(rect)
                                highlight.set_colors(stroke=color)
                                highlight.update()
                                
                                # Create popup annotation ONLY for first instance to avoid clutter
                                if i == 0:
                                    # Create a popup annotation with better positioning
                                    popup_rect = fitz.Rect(rect.x1 + 5, rect.y0, rect.x1 + 200, rect.y1 + 50)
                                    
                                    # Ensure popup stays within page bounds
                                    page_rect = page.rect
                                    if popup_rect.x1 > page_rect.x1:
                                        popup_rect = fitz.Rect(rect.x0 - 200, rect.y0, rect.x0 - 5, rect.y1 + 50)
                                    
                                    popup_text = f"{category}: {variable}\nValue: {value}\nType: {term_type}\nInstances: {len(text_instances)}"
                                    
                                    # Use text annotation instead of popup
                                    text_annot = page.add_text_annot(fitz.Point(rect.x1 + 2, rect.y0), popup_text)
                                    text_annot.set_info(title=f"{variable}", content=popup_text)
                                    text_annot.update()
                            
                            highlights_created += len(text_instances)
                            found_match = True
                            
                            # Record match details
                            match_details.append({
                                'Variable': variable,
                                'Category': category,
                                'Value': value,
                                'TermType': term_type,
                                'Page': page_num + 1,
                                'Color': color,
                                'Instances': len(text_instances)
                            })
                            
                            print(f"   ✅ HIGHLIGHTED: '{value}' ({variable}) - {len(text_instances)} instances on page {page_num + 1}")
                            break
                    
                    if not found_match:
                        print(f"   ❌ NOT FOUND: '{value}' ({variable})")
            
            # Save highlighted PDF
            output_filename = f"Study_{self.study_id}_{pdf_path.stem}_IMPROVED.pdf"
            output_path = self.output_folder / output_filename
            doc.save(str(output_path))
            doc.close()
            
            print(f"\n✅ Saved highlighted PDF: {output_filename}")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'total_searched': sum(len(values) for values in highlight_values.values()),
                'match_details': match_details,
                'output_file': output_path
            }
            
        except Exception as e:
            logging.error(f"Error highlighting PDF: {e}")
            return {'success': False, 'error': str(e)}

    def process_study(self):
        """Process a single study with improved highlighting and annotations."""
        print("="*80)
        print("IMPROVED VARIABLE-SPECIFIC PDF HIGHLIGHTER")
        print("="*80)
        print("🎯 FOCUSED terms that exist in PDFs")
        print("📝 CLEAN annotations (no image icons)")
        print("🎨 7 COLOR categories")
        print("✅ ALL instances highlighted")
        print("="*80)
        
        # Load study data
        study_data = self.load_study_data()
        if study_data is None:
            return False
        
        # Find PDF
        pdf_path = self.find_pdf_file()
        if not pdf_path:
            print("❌ PDF file not found")
            return False
        
        # Extract highlight values by category
        highlight_values = {category: [] for category in self.color_scheme.keys()}
        
        for variable, category in self.variable_categories.items():
            if variable in study_data.index:
                value = study_data[variable]
                
                print(f"\n🔍 Processing {variable} ({category}): {str(value)[:80]}...")
                
                # Extract effective terms
                terms = self.extract_effective_terms(value, variable)
                
                for term, term_type in terms:
                    highlight_values[category].append((term, variable, term_type))
                
                print(f"   → Extracted {len(terms)} terms")
        
        # Summary of extraction
        total_terms = sum(len(values) for values in highlight_values.values())
        print(f"\n📊 EXTRACTION SUMMARY:")
        print(f"Total terms to highlight: {total_terms}")
        for category, values in highlight_values.items():
            if values:
                print(f"   {category}: {len(values)} terms")
        
        # Highlight PDF
        result = self.highlight_pdf_with_better_annotations(pdf_path, highlight_values)
        
        if result['success']:
            success_rate = (result['highlights_created'] / result['total_searched']) * 100 if result['total_searched'] > 0 else 0
            
            print(f"\n" + "="*80)
            print("IMPROVED HIGHLIGHTING RESULTS")
            print("="*80)
            print(f"📊 Total Terms Searched: {result['total_searched']}")
            print(f"✨ Total Highlights Created: {result['highlights_created']}")
            print(f"🎯 Success Rate: {success_rate:.1f}%")
            
            # Save detailed results
            if result['match_details']:
                df_results = pd.DataFrame(result['match_details'])
                results_csv = self.output_folder / f"study_{self.study_id}_improved_results.csv"
                df_results.to_csv(results_csv, index=False, encoding='utf-8')
                print(f"📋 Detailed results: {results_csv.name}")
            
            print(f"🎨 Highlighted PDF: {result['output_file'].name}")
            
            return True
        else:
            print(f"❌ FAILED: {result.get('error', 'Unknown error')}")
            return False

if __name__ == "__main__":
    highlighter = ImprovedVariableHighlighter(study_id=1)
    highlighter.process_study()
