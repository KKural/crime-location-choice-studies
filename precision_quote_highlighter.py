#!/usr/bin/env python3
"""
Hybrid Precision Highlighter
============================

This script uses both direct values and supporting quotes intelligently:
- Simple variables (Country, City, etc.) → Use direct values
- Complex variables (Rationales, Methods) → Use supporting quotes
- Clean annotation names
"""

import pandas as pd
import pymupdf
import re
import sys
import logging
from pathlib import Path
from datetime import date
from typing import List, Dict, Tuple, Optional

class HybridPrecisionHighlighter:
    def __init__(self, csv_file: str):
        self.csv_file = csv_file
        self.df = None
        self.expanded_df = None
        self.setup_logging()
        
        # Define which variables use direct values vs supporting quotes
        self.SKIP_SUPPORTING = {
            "Country","City","Data_Collection_Period","Crime_Type","Crime_Type_Group",
            "Crime_Incidents","Spatial_Unit_Name","Number_of_Units","Number_of_Data_Sources",
            "Software_Used","Total_Study_Area_Size","Total_Study_Area_km2",
            "Unit_Size_km2","Number_of_Variables"
        }
        
        self.REQUIRE_SUPPORTING = {
            "Unit_Selection_Rationale","Rationale_Category","Data_Limitations","Computational_Constraints",
            "Alternative_Units","Independent_Variables","Model_Fit_Statistics","Coefficients",
            "Confidence_Intervals","Effect_Sizes","MAUP_Discussion","Sensitivity_Analysis","Future_Suggestions"
        }
        
        # Clean variable names for annotations
        self.CLEAN_NAMES = {
            "Country": "Country",
            "City": "City", 
            "Spatial_Unit_Name": "Spatial Unit",
            "Crime_Type": "Crime Type",
            "Crime_Type_Group": "Crime Category",
            "Crime_Incidents": "Crime Count",
            "Number_of_Units": "Number of Units",
            "Data_Collection_Period": "Data Period",
            "Software_Used": "Software",
            "Unit_Selection_Rationale": "Unit Rationale",
            "Rationale_Category": "Rationale Type",
            "Data_Limitations": "Data Limitations",
            "Computational_Constraints": "Computational Issues",
            "Alternative_Units": "Alternative Units",
            "Independent_Variables": "Variables",
            "Model_Fit_Statistics": "Model Fit",
            "Coefficients": "Coefficients", 
            "Confidence_Intervals": "Confidence Intervals",
            "Effect_Sizes": "Effect Sizes",
            "MAUP_Discussion": "MAUP Discussion",
            "Sensitivity_Analysis": "Sensitivity Analysis",
            "Future_Suggestions": "Future Research"
        }
        
    def setup_logging(self):
        """Setup logging configuration."""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
    
    def load_data(self):
        """Load both original CSV and expanded CSV data."""
        # Load expanded CSV for supporting quotes
        self.logger.info(f"Loading expanded CSV: {self.csv_file}")
        self.expanded_df = pd.read_csv(self.csv_file)
        
        # Filter out empty quotes
        self.expanded_df = self.expanded_df[
            (self.expanded_df['Quote_ID'] > 0) & 
            (self.expanded_df['Supporting_Quote'].notna()) & 
            (self.expanded_df['Supporting_Quote'].str.strip() != '')
        ]
        
        # Load original CSV for direct values
        original_csv = self.csv_file.replace("expanded_quotes_dataset.csv", "combined_dataset.csv")
        if Path(original_csv).exists():
            self.logger.info(f"Loading original CSV: {original_csv}")
            self.df = pd.read_csv(original_csv)
        else:
            self.logger.warning(f"Original CSV not found: {original_csv}")
            
        self.logger.info(f"Loaded {len(self.expanded_df)} quotes and {len(self.df) if self.df is not None else 0} studies")
    
    def get_clean_variable_name(self, variable_name: str) -> str:
        """Convert internal variable name to clean display name."""
        # Remove Supporting_quotes_for__ prefix
        clean = re.sub(r'^Supporting_quotes_for__?', '', variable_name)
        clean = re.sub(r'_$', '', clean)
        
        # Use predefined clean names or create from variable name
        if clean in self.CLEAN_NAMES:
            return self.CLEAN_NAMES[clean]
        else:
            # Convert snake_case to Title Case
            return clean.replace('_', ' ').title()
    
    def should_use_supporting_quotes(self, variable_name: str) -> bool:
        """Determine if we should use supporting quotes for this variable."""
        base_name = re.sub(r'^Supporting_quotes_for__?', '', variable_name).rstrip('_')
        return base_name in self.REQUIRE_SUPPORTING
    
    def get_direct_value_terms(self, study_id: int, variable_name: str) -> List[str]:
        """Get direct values for simple variables from original CSV."""
        if self.df is None:
            return []
            
        study_data = self.df[self.df['Study_ID'] == study_id]
        if study_data.empty:
            return []
        
        value = study_data.iloc[0].get(variable_name, "")
        if pd.isna(value) or str(value).strip() == "":
            return []
        
        value = str(value).strip()
        
        # For simple values, return as-is
        if len(value) < 100:
            return [value]
        
        # For longer values, split into manageable chunks
        return [value[:80] + "..." if len(value) > 80 else value]
    
    def get_study_data(self, study_id: int) -> Dict:
        """Get all highlighting data for a specific study."""
        result = {
            'direct_values': {},  # Variables that use direct values
            'supporting_quotes': []  # Variables that use supporting quotes
        }
        
        # Get supporting quotes from expanded CSV
        study_quotes = self.expanded_df[self.expanded_df['Study_ID'] == study_id].copy()
        if not study_quotes.empty:
            result['supporting_quotes'] = study_quotes.to_dict('records')
        
        # Get direct values for SKIP_SUPPORTING variables
        if self.df is not None:
            study_data = self.df[self.df['Study_ID'] == study_id]
            if not study_data.empty:
                for var in self.SKIP_SUPPORTING:
                    terms = self.get_direct_value_terms(study_id, var)
                    if terms:
                        result['direct_values'][var] = terms
        
        return result
    
    def create_search_phrases(self, text: str, max_phrases: int = 2) -> List[str]:
        """Create search phrases from text."""
        if not text or len(text) < 10:
            return [text] if text else []
        
        phrases = []
        
        # For short text, use as-is
        if len(text) <= 60:
            phrases.append(text)
        else:
            # Use beginning and truncated version
            words = text.split()
            if len(words) >= 4:
                phrases.append(" ".join(words[:6]))  # First 6 words
            phrases.append(text[:50] + "...")  # First 50 chars
        
        return phrases[:max_phrases]
    
    def find_pdf_file(self, study_id: int, pdf_folder: str = "Review_articles") -> Optional[str]:
        """Find the PDF file for a study."""
        pdf_path = Path(pdf_folder)
        if not pdf_path.exists():
            self.logger.warning(f"PDF folder not found: {pdf_folder}")
            return None
        
        patterns = [f"{study_id:02d}_*.pdf"]
        
        for pattern in patterns:
            matches = list(pdf_path.glob(pattern))
            if matches:
                return str(matches[0])
        
        self.logger.warning(f"No PDF found for Study {study_id}")
        return None
    
    def highlight_study(self, study_id: int, pdf_folder: str = "Review_articles", max_highlights: int = 75) -> Dict:
        """Highlight study using hybrid approach."""
        
        # Get study data
        study_data = self.get_study_data(study_id)
        if not study_data['direct_values'] and not study_data['supporting_quotes']:
            return {'success': False, 'error': 'No data found for study'}
        
        # Find PDF file
        pdf_file = self.find_pdf_file(study_id, pdf_folder)
        if not pdf_file:
            return {'success': False, 'error': 'PDF file not found'}
        
        # Create output folder
        output_folder = Path("20250825_ModeBased_Highlighting")
        output_folder.mkdir(exist_ok=True)
        
        self.logger.info(f"Processing Study {study_id}")
        self.logger.info(f"  Direct values: {len(study_data['direct_values'])} variables")
        self.logger.info(f"  Supporting quotes: {len(study_data['supporting_quotes'])} quotes")
        
        try:
            doc = pymupdf.open(pdf_file)
            highlights_created = 0
            
            # Color scheme
            colors = {
                'geographic': (0.0, 0.8, 0.0),      # Green - Country, City  
                'data': (0.0, 0.4, 1.0),            # Blue - Data info
                'methodology': (1.0, 0.6, 0.0),     # Orange - Methods
                'results': (0.8, 0.0, 0.8),         # Purple - Results
                'rationale': (1.0, 1.0, 0.0),       # Yellow - Rationales
                'default': (0.5, 0.5, 1.0)          # Light Blue
            }
            
            def get_variable_color(var_name: str) -> Tuple[float, float, float]:
                var_lower = var_name.lower()
                if any(x in var_lower for x in ['country', 'city']):
                    return colors['geographic']
                elif any(x in var_lower for x in ['data', 'incidents', 'collection', 'units']):
                    return colors['data']
                elif any(x in var_lower for x in ['rationale', 'alternative']):
                    return colors['rationale']
                elif any(x in var_lower for x in ['model', 'coefficient', 'effect', 'confidence']):
                    return colors['results']
                elif any(x in var_lower for x in ['method', 'software', 'variables']):
                    return colors['methodology']
                return colors['default']
            
            # Process direct values first
            for variable, terms in study_data['direct_values'].items():
                if highlights_created >= max_highlights:
                    break
                    
                color = get_variable_color(variable)
                clean_name = self.get_clean_variable_name(variable)
                
                for term in terms:
                    if highlights_created >= max_highlights:
                        break
                        
                    # Search across all pages  
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        matches = page.search_for(term, quads=True)
                        
                        for match in matches:
                            try:
                                # Create highlight
                                highlight = page.add_highlight_annot(match)
                                highlight.set_colors(stroke=color)
                                highlight.set_opacity(0.8)
                                highlight.set_info(
                                    title=clean_name,
                                    content=f"{clean_name}: {term}"
                                )
                                highlight.update()
                                highlights_created += 1
                                
                                if highlights_created >= max_highlights:
                                    break
                            except Exception as e:
                                self.logger.warning(f"Highlight failed: {e}")
                        
                        if highlights_created >= max_highlights:
                            break
                
                if highlights_created > 0:
                    self.logger.info(f"  ✅ {clean_name}: highlighted '{term}'")
            
            # Process supporting quotes
            for quote_data in study_data['supporting_quotes']:
                if highlights_created >= max_highlights:
                    break
                    
                variable_name = quote_data['Variable_Name']
                quote_text = quote_data['Supporting_Quote']
                
                # Skip if not in REQUIRE_SUPPORTING
                if not self.should_use_supporting_quotes(variable_name):
                    continue
                
                color = get_variable_color(variable_name)
                clean_name = self.get_clean_variable_name(variable_name)
                
                # Create search phrases
                phrases = self.create_search_phrases(quote_text)
                
                found_match = False
                for phrase in phrases:
                    if highlights_created >= max_highlights:
                        break
                        
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        matches = page.search_for(phrase, quads=True)
                        
                        if matches:
                            for match in matches:
                                try:
                                    highlight = page.add_highlight_annot(match)
                                    highlight.set_colors(stroke=color)
                                    highlight.set_opacity(0.7)
                                    highlight.set_info(
                                        title=clean_name,
                                        content=f"{clean_name}: {quote_text[:100]}..." if len(quote_text) > 100 else f"{clean_name}: {quote_text}"
                                    )
                                    highlight.update()
                                    highlights_created += 1
                                    found_match = True
                                    
                                    if highlights_created >= max_highlights:
                                        break
                                except Exception as e:
                                    self.logger.warning(f"Quote highlight failed: {e}")
                            
                            if highlights_created >= max_highlights:
                                break
                        
                        if highlights_created >= max_highlights or found_match:
                            break
                    
                    if found_match:
                        break
                
                if found_match:
                    self.logger.info(f"  ✅ {clean_name}: highlighted quote")
            
            # Save PDF
            output_file = output_folder / f"Study_{study_id}_HYBRID_HIGHLIGHTED.pdf"
            doc.save(str(output_file))
            doc.close()
            
            self.logger.info(f"✅ Saved: {output_file}")
            self.logger.info(f"✨ Total highlights: {highlights_created}")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'output_file': str(output_file)
            }
            
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {'success': False, 'error': str(e)}
            
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {'success': False, 'error': str(e)}
        """Highlight all quotes for a specific study."""
        
        # Get quotes for this study
        study_quotes = self.get_study_quotes(study_id)
        if study_quotes.empty:
            return {'success': False, 'error': 'No quotes found for study'}
        
        # Find PDF file
        pdf_file = self.find_pdf_file(study_id, pdf_folder)
        if not pdf_file:
            return {'success': False, 'error': 'PDF file not found'}
        
        self.logger.info(f"Processing Study {study_id}: {len(study_quotes)} quotes")
        
        try:
            # Open PDF
            doc = pymupdf.open(pdf_file)
            highlights_created = 0
            quote_stats = []
            
            # Define colors for different variable types (RGB values)
            colors = {
                'Unit_Size': (1.0, 1.0, 0.0),      # Yellow
                'Rationale': (0.0, 1.0, 0.0),      # Green  
                'Data_Sources': (0.0, 0.8, 1.0),   # Light Blue
                'Model_Type': (1.0, 0.5, 0.0),     # Orange
                'Results': (1.0, 0.0, 1.0),        # Magenta
                'Coefficients': (1.0, 0.2, 0.2),   # Red
                'Effect_Sizes': (0.8, 0.0, 0.8),   # Purple
                'Independent_Variables': (0.0, 0.6, 0.0), # Dark Green
                'Default': (0.7, 0.7, 0.9)         # Light Purple
            }
            
            # Process each quote
            for idx, quote_row in study_quotes.iterrows():
                if highlights_created >= max_highlights:
                    break
                    
                quote_text = quote_row['Supporting_Quote']
                variable_name = quote_row['Variable_Name']
                quote_id = quote_row['Quote_ID']
                
                # Determine color based on variable type
                color = colors['Default']
                variable_lower = variable_name.lower()
                
                if 'unit_size' in variable_lower or 'spatial_unit' in variable_lower:
                    color = colors['Unit_Size']
                elif 'rationale' in variable_lower:
                    color = colors['Rationale']
                elif 'data_sources' in variable_lower or 'data_collection' in variable_lower:
                    color = colors['Data_Sources']
                elif 'model_type' in variable_lower or 'model_fit' in variable_lower:
                    color = colors['Model_Type']
                elif 'effect_size' in variable_lower:
                    color = colors['Effect_Sizes']
                elif 'coefficient' in variable_lower:
                    color = colors['Coefficients']
                elif 'independent_variable' in variable_lower or 'variable' in variable_lower:
                    color = colors['Independent_Variables']
                elif any(keyword in variable_lower for keyword in ['result', 'confidence', 'statistic']):
                    color = colors['Results']
                
                # Create search phrases
                search_phrases = self.create_search_phrases(quote_text)
                
                quote_highlights = 0
                for phrase in search_phrases:
                    if not phrase or highlights_created >= max_highlights:
                        break
                    
                    # Search across all pages
                    for page_num in range(len(doc)):
                        page = doc[page_num]
                        
                        # Search with quads for better text wrapping
                        matches = page.search_for(phrase, quads=True)
                        
                        if matches:
                            for match in matches:
                                try:
                                    # Create highlight annotation with proper color
                                    annot = page.add_highlight_annot(match)
                                    annot.set_colors(stroke=color)
                                    annot.set_opacity(0.5)  # More visible opacity
                                    
                                    # Set annotation info with actual quote text
                                    title = f"Q{quote_id}: {variable_name.replace('Supporting_quotes_for__', '').replace('_', ' ')}"
                                    content = f"Quote: {quote_text[:300]}{'...' if len(quote_text) > 300 else ''}"
                                    
                                    annot.set_info(title=title, content=content)
                                    annot.update()
                                    
                                    highlights_created += 1
                                    quote_highlights += 1
                                    
                                    if highlights_created >= max_highlights:
                                        break
                                
                                except Exception as e:
                                    self.logger.warning(f"Failed to create highlight: {e}")
                            
                            if highlights_created >= max_highlights:
                                break
                    
                    if highlights_created >= max_highlights:
                        break
                
                # Record stats for this quote
                quote_stats.append({
                    'quote_id': quote_id,
                    'variable': variable_name,
                    'highlights': quote_highlights,

            
def main():
    if len(sys.argv) < 2:
        print("Usage: python precision_quote_highlighter.py <study_id> [pdf_folder]")
        print("       python precision_quote_highlighter.py all [pdf_folder]")
        sys.exit(1)
    
    # Get today's CSV file
    today = date.today().strftime("%Y%m%d")
    csv_file = f"{today}_Analysis & Results/{today}_expanded_quotes_dataset.csv"
    
    if not Path(csv_file).exists():
        print(f"❌ CSV file not found: {csv_file}")
        print("Run create_expanded_csv.py first!")
        sys.exit(1)
    
    # Create highlighter
    highlighter = HybridPrecisionHighlighter(csv_file)
    highlighter.load_data()
    
    study_arg = sys.argv[1]
    pdf_folder = sys.argv[2] if len(sys.argv) > 2 else "Review_articles"
    
    print("🎯 Hybrid Precision Highlighter")
    print("=" * 50)
    print("✅ Direct values: Country, City, Crime Type, etc.")
    print("✅ Supporting quotes: Rationales, Methods, Results")
    print("✅ Clean annotation names")
    print("=" * 50)
    
    if study_arg.lower() == 'all':
        # Process multiple studies
        study_ids = highlighter.expanded_df['Study_ID'].unique() if highlighter.expanded_df is not None else []
        successful = 0
        total_highlights = 0
        
        for study_id in sorted(study_ids)[:5]:  # Limit to first 5 for testing
            print(f"\n📖 Processing Study {study_id}...")
            result = highlighter.highlight_study(study_id, pdf_folder)
            
            if result['success']:
                successful += 1
                total_highlights += result['highlights_created']
                print(f"✅ Study {study_id}: {result['highlights_created']} highlights")
            else:
                print(f"❌ Study {study_id}: {result['error']}")
        
        print(f"\n🎉 Completed: {successful} studies, {total_highlights} total highlights")
    else:
        # Single study
        try:
            study_id = int(study_arg)
            result = highlighter.highlight_study(study_id, pdf_folder)
            
            if result['success']:
                print(f"✅ Success: {result['highlights_created']} highlights created")
                print(f"📄 Output: {result['output_file']}")
            else:
                print(f"❌ Error: {result['error']}")
        except ValueError:
            print(f"❌ Invalid study ID: {study_arg}")
            sys.exit(1)

if __name__ == "__main__":
    main()
