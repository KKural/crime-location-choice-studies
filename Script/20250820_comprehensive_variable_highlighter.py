#!/usr/bin/env python3
"""
Comprehensive Variable-Specific PDF Highlighter
Highlights ALL important variables with their own color categories.
Handles complex fields like Independent_Variables with individual parsing.
"""

import pandas as pd
import fitz  # PyMuPDF
import logging
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
import re

class ComprehensiveVariableHighlighter:
    """PDF highlighter with variable-specific color coding and comprehensive coverage."""
    
    def __init__(self, study_id: int = 1):
        self.study_id = study_id
        self.base_dir = Path(__file__).parent
        self.csv_path = self.base_dir / "../20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_dir = self.base_dir / "../Review_articles"
        
        # Tracking for highlights (once per paper)
        self.highlighted_globally = set()
        
        # Color scheme by variable category
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
            # Geographic & Location
            'Country': 'Geographic',
            'City': 'Geographic',
            
            # Study Area & Spatial  
            'Total_Study_Area_Size': 'Study_Area',
            'Spatial_Unit_Name': 'Study_Area',
            'Unit_Size': 'Study_Area',
            'Spatial_Aggregation': 'Study_Area',
            
            # Quantitative & Numbers
            'Number_of_Units': 'Quantitative',
            'Average_Population_per_Unit': 'Quantitative', 
            'Crime_Incidents': 'Quantitative',
            'Number_of_Data_Sources': 'Quantitative',
            'Number_of_Variables': 'Quantitative',
            
            # Crime & Data
            'Crime_Type': 'Crime_Data',
            'Crime_Type_Group': 'Crime_Data',
            'Data_Collection_Period': 'Crime_Data',
            'Data_Sources': 'Crime_Data',
            
            # Methodology & Analysis
            'Model_Type': 'Methodology',
            'Independent_Variables': 'Methodology',
            'Software_Used': 'Methodology',
            'Unit_Selection_Rationale': 'Methodology',
            'Rationale_Category': 'Methodology',
            
            # Results & Statistics
            'Model_Fit_Statistics': 'Results',
            'Coefficients': 'Results',
            'Confidence_Intervals': 'Results',
            'Effect_Sizes': 'Results',
            
            # Limitations & Future Work
            'Data_Limitations': 'Limitations',
            'Computational_Constraints': 'Limitations',
            'Alternative_Units': 'Limitations',
            'MAUP_Discussion': 'Limitations',
            'Sensitivity_Analysis': 'Limitations',
            'Future_Suggestions': 'Limitations'
        }
        
        self.setup_logging()
    
    def setup_logging(self):
        """Configure logging."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = self.base_dir / f"20250820_Comprehensive_Highlighting/comprehensive_highlighting_{timestamp}.log"
        log_file.parent.mkdir(exist_ok=True)
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file, encoding='utf-8'),
                logging.StreamHandler(sys.stdout)
            ]
        )
    
    def load_study_data(self) -> Dict:
        """Load specific study data from CSV."""
        try:
            df = pd.read_csv(self.csv_path)
            study_data = df[df['Study_ID'] == self.study_id].iloc[0]
            logging.info(f"Loaded Study {self.study_id}: {study_data['Title'][:50]}...")
            return study_data.to_dict()
        except Exception as e:
            logging.error(f"Failed to load study data: {e}")
            return {}
    
    def parse_independent_variables(self, variables_text: str) -> List[str]:
        """Parse the Independent_Variables field into individual variables."""
        if not variables_text or variables_text == 'nan':
            return []
        
        # Parse numbered list format
        variables = []
        lines = variables_text.split('\n')
        
        for line in lines:
            line = line.strip()
            if line:
                # Remove numbering (1., 2., etc.)
                clean_line = re.sub(r'^\d+\.\s*', '', line)
                if clean_line:
                    variables.append(clean_line)
        
        return variables
    
    def extract_searchable_terms(self, value, variable):
        """Extract searchable terms from complex CSV values."""
        if pd.isna(value) or str(value).strip() == '':
            return []
        
        value_str = str(value).strip()
        terms = []
        
        # Always add the original value first
        terms.append((value_str, 'Main'))
        
        # For long descriptive text, extract key numbers and short phrases
        if len(value_str) > 50:
            import re
            
            # Extract numbers (including decimals, commas, percentages)
            numbers = re.findall(r'\b\d+[,.]?\d*\.?\d*%?\b', value_str)
            for num in numbers:
                if len(num) > 1:  # Skip single digits
                    terms.append((num, 'Number'))
            
            # Extract years (4-digit numbers between 1900-2030)
            years = re.findall(r'\b(19|20)\d{2}\b', value_str)
            for year in years:
                terms.append((year, 'Year'))
            
            # Extract quoted text
            quoted = re.findall(r'"([^"]+)"', value_str)
            for quote in quoted:
                if 5 <= len(quote) <= 50:  # Reasonable length quotes
                    terms.append((quote, 'Quote'))
            
            # Extract parenthetical information
            parens = re.findall(r'\(([^)]+)\)', value_str)
            for paren in parens:
                if 3 <= len(paren) <= 30:  # Reasonable length
                    terms.append((paren, 'Parenthetical'))
            
            # For specific variables, extract key terms
            if 'Model' in variable:
                # Look for model names
                model_patterns = ['logit', 'regression', 'choice', 'conditional', 'nested', 'mixed']
                for pattern in model_patterns:
                    if pattern.lower() in value_str.lower():
                        # Find the full model name around this pattern
                        words = value_str.split()
                        for i, word in enumerate(words):
                            if pattern.lower() in word.lower():
                                # Take 1-3 words around it
                                start = max(0, i-1)
                                end = min(len(words), i+3)
                                model_name = ' '.join(words[start:end])
                                terms.append((model_name, 'Model'))
                                break
        
        else:
            # For short values, try some simple variations
            if ',' in value_str:
                parts = [p.strip() for p in value_str.split(',')]
                for part in parts:
                    if len(part) > 1:
                        terms.append((part, 'Part'))
        
        return terms

    def extract_all_highlight_values(self, study_data: Dict) -> Dict[str, List[Tuple[str, str, str]]]:
        """Extract ALL values from all important variables, organized by category."""
        values_by_category = {category: [] for category in self.color_scheme.keys()}
        
        # Process each variable
        for variable, category in self.variable_categories.items():
            value = str(study_data.get(variable, '')).strip()
            if not value or value == 'nan':
                continue
            
            logging.info(f"\nProcessing {variable} ({category}): {value[:100]}...")
            
            # Special handling for Independent_Variables
            if variable == 'Independent_Variables':
                individual_vars = self.parse_independent_variables(value)
                logging.info(f"  Parsed {len(individual_vars)} independent variables:")
                for i, var in enumerate(individual_vars, 1):
                    logging.info(f"    {i}. {var}")
                    # Add each variable with flexible matching
                    variations = self.create_value_variations(var, variable)
                    for variation in variations:
                        values_by_category[category].append((variation, variable, f"IndepVar_{i}"))
            
            # Special handling for Data_Sources (split multiple sources)
            elif variable == 'Data_Sources':
                for source in value.split(';'):
                    clean_source = source.strip()
                    if clean_source:
                        variations = self.create_value_variations(clean_source, variable)
                        for variation in variations:
                            values_by_category[category].append((variation, variable, 'DataSource'))
            
            # Handle all other variables with smart term extraction
            else:
                # Use smart extraction for better success rate
                searchable_terms = self.extract_searchable_terms(value, variable)
                for term, term_type in searchable_terms:
                    values_by_category[category].append((term, variable, term_type))
                
                # Also try original variations method for backward compatibility
                variations = self.create_value_variations(value, variable)
                for variation in variations:
                    values_by_category[category].append((variation, variable, 'Variation'))
        
        # Log summary
        total_values = sum(len(values) for values in values_by_category.values())
        logging.info(f"\n=== COMPREHENSIVE EXTRACTION SUMMARY ===")
        logging.info(f"Total highlight values extracted: {total_values}")
        for category, values in values_by_category.items():
            if values:
                logging.info(f"{category}: {len(values)} values")
        
        return values_by_category
    
    def create_value_variations(self, value: str, variable: str) -> List[str]:
        """Create format variations for better matching."""
        variations = []
        
        # Always include original
        variations.append(value)
        
        # Number handling
        if variable in ['Number_of_Units', 'Crime_Incidents', 'Number_of_Data_Sources', 'Number_of_Variables']:
            # Handle comma-separated numbers
            if ',' in value:
                clean_number = value.replace(',', '')
                variations.append(clean_number)
        
        # Area handling
        elif variable in ['Total_Study_Area_Size']:
            # Extract just numbers from area values
            numbers = re.findall(r'\d+(?:\.\d+)?', value)
            for number in numbers:
                variations.append(number)
                # Also add without decimals
                if '.' in number:
                    base_number = number.split('.')[0]
                    variations.append(base_number)
        
        # Statistical values handling
        elif variable in ['Model_Fit_Statistics', 'Coefficients', 'Confidence_Intervals', 'Effect_Sizes']:
            # Extract decimal numbers
            numbers = re.findall(r'-?\d+\.\d+', value)
            for number in numbers:
                variations.append(number)
        
        # Clean up variations
        unique_variations = []
        for var in variations:
            if var and var not in unique_variations:
                unique_variations.append(var)
        
        return unique_variations
    
    def find_pdf_file(self, study_data: Dict) -> Path:
        """Find the PDF file for this study."""
        pdf_files = list(self.pdf_dir.glob("*.pdf"))
        
        for pdf_file in pdf_files:
            if pdf_file.name.startswith(f"{self.study_id:02d}_"):
                logging.info(f"Found PDF: {pdf_file.name}")
                return pdf_file
        
        raise FileNotFoundError(f"No PDF found for Study {self.study_id}")
    
    def highlight_values_in_pdf(self, pdf_path: Path, values_by_category: Dict) -> Dict:
        """Highlight values in PDF with category-specific colors."""
        doc = fitz.open(pdf_path)
        highlights_created = 0
        exact_matches = []
        
        logging.info(f"\nProcessing PDF: {pdf_path.name}")
        logging.info(f"Total pages: {len(doc)}")
        
        # Process each category
        for category, values in values_by_category.items():
            if not values:
                continue
                
            color = self.color_scheme[category]
            logging.info(f"\n--- {category.upper()} HIGHLIGHTING ---")
            logging.info(f"Color: {color}, Values: {len(values)}")
            
            for value, variable, sub_type in values:
                found_match = False
                
                for page_num in range(len(doc)):
                    page = doc[page_num]
                    text_instances = page.search_for(value)
                    
                    if text_instances:
                        # Check if we should highlight (once per paper rule)
                        if value not in self.highlighted_globally:
                            self.highlighted_globally.add(value)
                            
                            # Highlight first instance
                            rect = text_instances[0]
                            highlight = page.add_highlight_annot(rect)
                            highlight.set_colors(stroke=color)
                            
                            # Add simple note annotation instead of popup (to avoid image-like appearance)
                            note_text = f"{category}: {variable} = '{value}'"
                            if sub_type != 'Main':
                                note_text += f" ({sub_type})"
                            
                            # Add a small note annotation that's less intrusive
                            note_point = fitz.Point(rect.x0, rect.y0)
                            note = page.add_text_annot(note_point, note_text)
                            note.set_info(content=note_text)
                            note.update()
                            
                            highlight.update()
                            
                            highlights_created += 1
                            found_match = True
                            
                            # Record the match
                            exact_matches.append({
                                'Variable_Name': variable,
                                'Category': category,
                                'Sub_Type': sub_type if sub_type != 'Main' else '',
                                'Value_Highlighted': value,
                                'Color_Used': color,
                                'Page_Number': page_num + 1,
                                'Instances_Found': len(text_instances),
                                'Position_X': text_instances[0].x0,
                                'Position_Y': text_instances[0].y0,
                                'Full_Description': f"{category} variable '{variable}' with value '{value}'" + (f" ({sub_type})" if sub_type != 'Main' else "")
                            })
                            
                            logging.info(f"  ✓ HIGHLIGHTED: '{value}' ({variable}) on page {page_num + 1}")
                            break
                        else:
                            logging.info(f"  - SKIPPED: '{value}' - already highlighted")
                            break
                
                if not found_match and value not in self.highlighted_globally:
                    logging.info(f"  ✗ NOT FOUND: '{value}' ({variable})")
        
        # Save highlighted PDF
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_dir = self.base_dir / "20250820_Comprehensive_Highlighting"
        output_dir.mkdir(exist_ok=True)
        
        original_name = pdf_path.stem
        output_path = output_dir / f"Study_{self.study_id}_{original_name}_COMPREHENSIVE.pdf"
        doc.save(str(output_path))
        doc.close()
        
        logging.info(f"\n✓ Saved comprehensive highlighted PDF: {output_path.name}")
        
        return {
            'success': True,
            'highlights_created': highlights_created,
            'exact_matches': exact_matches,
            'output_path': output_path,
            'total_values_searched': sum(len(values) for values in values_by_category.values())
        }
    
    def save_results(self, result: Dict, study_data: Dict):
        """Save comprehensive highlighting results."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_dir = self.base_dir / "20250820_Comprehensive_Highlighting"
        
        # Save detailed matches with variable information
        if result['exact_matches']:
            matches_df = pd.DataFrame(result['exact_matches'])
            matches_path = output_dir / f"study_{self.study_id}_comprehensive_matches_{timestamp}.csv"
            matches_df.to_csv(matches_path, index=False, encoding='utf-8')
            logging.info(f"✓ Comprehensive match details saved: {matches_path.name}")
            
            # Create a user-friendly variable summary
            variable_summary = []
            for _, match in matches_df.iterrows():
                var_name = match['Variable_Name']
                sub_type = match.get('Sub_Type', '')
                full_var_name = f"{var_name}" + (f" ({sub_type})" if sub_type else "")
                
                variable_summary.append({
                    'Variable_Complete_Name': full_var_name,
                    'Category': match['Category'],
                    'Actual_Value_Found': match['Value_Highlighted'],
                    'Page_Located': match['Page_Number'],
                    'Highlight_Color': match['Color_Used'],
                    'Context_Description': f"Variable '{var_name}' from {match['Category']} category shows value: '{match['Value_Highlighted']}'" + (f" (Type: {sub_type})" if sub_type else "")
                })
            
            summary_df = pd.DataFrame(variable_summary)
            summary_path = output_dir / f"study_{self.study_id}_variable_examples_{timestamp}.csv"
            summary_df.to_csv(summary_path, index=False, encoding='utf-8')
            logging.info(f"✓ Variable examples summary saved: {summary_path.name}")
            print(f"\n📋 Variable Examples Summary: {summary_path.name}")
            
            # Print sample of what was found
            print(f"\n🎯 Sample Variable Highlights:")
            for i, row in summary_df.head(10).iterrows():
                print(f"   • {row['Variable_Complete_Name']}: '{row['Actual_Value_Found']}' (Page {row['Page_Located']})")
            if len(summary_df) > 10:
                print(f"   ... and {len(summary_df)-10} more variables highlighted")
        
        # Save summary by category
        category_summary = {}
        for match in result['exact_matches']:
            category = match['Category']
            if category not in category_summary:
                category_summary[category] = {'count': 0, 'variables': set()}
            category_summary[category]['count'] += 1
            category_summary[category]['variables'].add(match['Variable_Name'])
        
        summary_data = []
        for category, data in category_summary.items():
            summary_data.append({
                'Category': category,
                'Highlights_Created': data['count'],
                'Variables_Covered': len(data['variables']),
                'Variable_List': '; '.join(sorted(data['variables']))
            })
        
        summary_df = pd.DataFrame(summary_data)
        summary_path = output_dir / f"study_{self.study_id}_category_summary_{timestamp}.csv"
        summary_df.to_csv(summary_path, index=False)
        logging.info(f"✓ Category summary saved: {summary_path.name}")
    
    def print_comprehensive_summary(self, result: Dict, study_data: Dict):
        """Print comprehensive highlighting summary."""
        print("\n" + "="*70)
        print("COMPREHENSIVE VARIABLE-SPECIFIC HIGHLIGHTING - RESULTS")
        print("="*70)
        
        print(f"\nSTUDY INFORMATION:")
        print(f"📚 Study ID: {self.study_id}")
        print(f"📄 Title: {study_data.get('Title', '')[:50]}...")
        print(f"✅ Success: {result['success']}")
        
        print(f"\nCOMPREHENSIVE RESULTS:")
        print(f"🎯 Total Values Searched: {result['total_values_searched']}")
        print(f"✨ Total Highlights Created: {result['highlights_created']}")
        
        if result['total_values_searched'] > 0:
            success_rate = (result['highlights_created'] / result['total_values_searched']) * 100
            print(f"📊 Success Rate: {success_rate:.1f}%")
        
        # Show results by category with colors
        print(f"\nHIGHLIGHTS BY VARIABLE CATEGORY:")
        category_counts = {}
        for match in result['exact_matches']:
            category = match['Category']
            category_counts[category] = category_counts.get(category, 0) + 1
        
        for category, color in self.color_scheme.items():
            count = category_counts.get(category, 0)
            if count > 0:
                print(f"  {category}: {count} highlights (Color: {color})")
        
        print(f"\n📁 Output Location: 20250820_Comprehensive_Highlighting")
        print(f"🎨 Color-Coded by Variable Category")
        
        # Show first few highlights by category
        if result['exact_matches']:
            print(f"\nSAMPLE HIGHLIGHTS BY CATEGORY:")
            for category in self.color_scheme.keys():
                category_matches = [m for m in result['exact_matches'] if m['Category'] == category]
                if category_matches:
                    print(f"\n{category}:")
                    for match in category_matches[:3]:  # Show first 3
                        print(f"  Page {match['Page_Number']}: '{match['Value_Highlighted']}' ({match['Variable_Name']})")
                    if len(category_matches) > 3:
                        print(f"  ... and {len(category_matches) - 3} more")
        
        print("="*70)
    
    def process_single_study(self):
        """Process a single study with comprehensive variable-specific highlighting."""
        try:
            # Load study data
            study_data = self.load_study_data()
            if not study_data:
                return False
            
            # Extract all highlight values by category
            values_by_category = self.extract_all_highlight_values(study_data)
            
            # Find PDF file
            pdf_file = self.find_pdf_file(study_data)
            
            # Perform comprehensive highlighting
            result = self.highlight_values_in_pdf(pdf_file, values_by_category)
            
            # Save results
            self.save_results(result, study_data)
            
            # Print summary
            self.print_comprehensive_summary(result, study_data)
            
            return result['success']
            
        except Exception as e:
            logging.error(f"Error processing study {self.study_id}: {e}")
            return False

def main():
    """Main function."""
    study_id = 1
    
    print("="*70)
    print("COMPREHENSIVE VARIABLE-SPECIFIC PDF HIGHLIGHTER")
    print("="*70)
    print("Strategy:")
    print("🎨 7 COLOR CATEGORIES for different variable types")
    print("📊 30+ VARIABLES highlighted with unique colors")
    print("🔍 INDEPENDENT VARIABLES parsed individually") 
    print("🎯 ONCE PER PAPER rule for each value")
    print("="*70)
    
    highlighter = ComprehensiveVariableHighlighter(study_id=study_id)
    success = highlighter.process_single_study()
    
    if success:
        print(f"\n🎉 SUCCESS: Comprehensive highlighting completed for Study {study_id}!")
        return 0
    else:
        print(f"\n❌ FAILED: Could not complete comprehensive highlighting for Study {study_id}")
        return 1

if __name__ == "__main__":
    exit(main())
