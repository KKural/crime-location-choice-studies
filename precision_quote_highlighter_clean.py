#!/usr/bin/env python3
"""
Hybrid Precision Quote Highlighter
Combines direct variable values with supporting quotes for targeted PDF highlighting.
"""

import pandas as pd
import fitz  # PyMuPDF
from pathlib import Path
import re
import sys
import logging
from datetime import date

class HybridPrecisionHighlighter:
    def __init__(self):
        logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
        self.logger = logging.getLogger(__name__)
        
        # Variables that use direct values (simple highlighting)
        self.SKIP_SUPPORTING = {
            'Country', 'City', 'State_or_Region', 'Data_Collection_Period',
            'Crime_Type', 'Model_Type', 'Data_Sources',
            'Number_of_Data_Sources', 'Crime_Incidents', 'Software_Used'
        }
        
        # Clean variable names for annotations
        self.VARIABLE_DISPLAY_NAMES = {
            'Country': 'Country',
            'City': 'City', 
            'State_or_Region': 'State/Region',
            'Data_Collection_Period': 'Data Collection Period',
            'Crime_Type': 'Crime Type',
            'Model_Type': 'Model Type',
            'Data_Sources': 'Data Sources',
            'Number_of_Data_Sources': 'Number of Data Sources',
            'Crime_Incidents': 'Crime Incidents',
            'Software_Used': 'Software Used',
            'Unit_Selection_Rationale': 'Unit Selection Rationale',
            'MAUP_Discussion': 'MAUP Discussion',
            'Sensitivity_Analysis': 'Sensitivity Analysis',
            'Future_Suggestions': 'Future Suggestions'
        }
        
        # Color scheme for different variable types
        self.COLORS = {
            'location': [1, 0.8, 0],      # Orange - Country, City, State
            'temporal': [0, 0.8, 1],      # Light Blue - Data collection period
            'crime': [1, 0.4, 0.4],       # Light Red - Crime type, incidents
            'method': [0.6, 0.8, 1],      # Light Blue - Model type, data sources, software
            'rationale': [1, 1, 0.4],     # Light Yellow - Unit selection rationale
            'analysis': [0.8, 1, 0.6],    # Light Green - MAUP, sensitivity analysis
            'future': [0.9, 0.7, 1]       # Light Purple - Future suggestions
        }
        
    def get_variable_color(self, variable_name):
        """Get color for variable based on type."""
        color_mapping = {
            'Country': 'location', 'City': 'location', 'State_or_Region': 'location',
            'Data_Collection_Period': 'temporal',
            'Crime_Type': 'crime', 'Crime_Incidents': 'crime',
            'Model_Type': 'method', 'Data_Sources': 'method', 'Number_of_Data_Sources': 'method', 'Software_Used': 'method',
            'Unit_Selection_Rationale': 'rationale',
            'MAUP_Discussion': 'analysis', 'Sensitivity_Analysis': 'analysis',
            'Future_Suggestions': 'future'
        }
        return self.COLORS.get(color_mapping.get(variable_name, 'method'), self.COLORS['method'])
    
    def clean_text(self, text):
        """Clean and normalize text for matching."""
        if pd.isna(text) or text == '':
            return ''
        return re.sub(r'\s+', ' ', str(text).strip())
    
    def find_text_instances(self, page, search_text, min_length=3):
        """Find all instances of text on a page with position information."""
        if len(search_text.strip()) < min_length:
            return []
            
        instances = []
        text_instances = page.search_for(search_text, quads=True)
        
        for quad in text_instances:
            instances.append({
                'quad': quad,
                'text': search_text,
                'length': len(search_text)
            })
        
        return instances
    
    def highlight_text_instances(self, page, instances, color, annotation_text):
        """Add highlights to text instances on a page."""
        highlights_added = 0
        
        for instance in instances:
            try:
                # Create highlight annotation
                highlight = page.add_highlight_annot(instance['quad'])
                highlight.set_colors(stroke=color)  # Only stroke color
                # Use set_info instead of set_content for newer PyMuPDF versions
                highlight.set_info(content=annotation_text)
                highlight.update()
                highlights_added += 1
                
            except Exception as e:
                self.logger.warning(f"Failed to add highlight: {e}")
        
        return highlights_added
    
    def process_study(self, study_id, pdf_folder="Review_articles", combined_csv=None, expanded_csv=None, max_highlights=500):
        """Process a single study with hybrid highlighting approach."""
        try:
            self.logger.info(f"🔍 Processing Study {study_id}")
            
            # Load data
            if combined_csv is None:
                combined_csv = "20250820_Analysis & Results/20250820_combined_dataset.csv"
            
            if expanded_csv is None:
                today = date.today().strftime("%Y%m%d")
                expanded_csv = f"{today}_Analysis & Results/{today}_expanded_quotes_dataset.csv"
            
            # Load both datasets
            combined_df = pd.read_csv(combined_csv)
            expanded_df = pd.read_csv(expanded_csv)
            
            # Get study data
            study_data = combined_df[combined_df['Study_ID'] == int(study_id)]
            if study_data.empty:
                raise ValueError(f"Study {study_id} not found")
            
            study_row = study_data.iloc[0]
            study_quotes = expanded_df[expanded_df['Study_ID'] == int(study_id)]
            
            # Open PDF - try different naming patterns
            pdf_patterns = [
                f"01_{study_id}.pdf",  # Original pattern
                f"0{study_id}_*.pdf" if int(study_id) < 10 else f"{study_id}_*.pdf"  # Wildcard pattern
            ]
            
            pdf_file = None
            for pattern in pdf_patterns:
                matches = list(Path(pdf_folder).glob(pattern))
                if matches:
                    pdf_file = matches[0]
                    break
            
            # If no wildcard match, try specific naming for first few studies
            if pdf_file is None:
                if study_id == "1":
                    pdf_file = Path(pdf_folder) / "01_a_discrete_spatial_choice_model_of_burglary_target.pdf"
                else:
                    pdf_file = Path(pdf_folder) / f"0{study_id}_{study_id}.pdf" if int(study_id) < 10 else Path(pdf_folder) / f"{study_id}_{study_id}.pdf"
            
            if not pdf_file.exists():
                raise FileNotFoundError(f"PDF not found: {pdf_file}")
            
            doc = fitz.open(pdf_file)
            self.logger.info(f"📖 Opened PDF: {pdf_file} ({len(doc)} pages)")
            
            highlights_created = 0
            processing_stats = {
                'direct_values': 0,
                'supporting_quotes': 0,
                'skipped_empty': 0
            }
            
            # Process each variable
            for variable_name in self.SKIP_SUPPORTING.union({'Unit_Selection_Rationale', 'MAUP_Discussion', 'Sensitivity_Analysis', 'Future_Suggestions'}):
                if highlights_created >= max_highlights:
                    break
                
                color = self.get_variable_color(variable_name)
                display_name = self.VARIABLE_DISPLAY_NAMES.get(variable_name, variable_name)
                
                if variable_name in self.SKIP_SUPPORTING:
                    # Use direct value from main dataset
                    direct_value = self.clean_text(study_row.get(variable_name, ''))
                    if direct_value and len(direct_value.strip()) >= 3:
                        self.logger.info(f"  🎯 Direct highlighting: {variable_name} = '{direct_value[:60]}...'")
                        
                        # Search all pages
                        variable_highlights = 0
                        for page_num in range(len(doc)):
                            page = doc[page_num]
                            instances = self.find_text_instances(page, direct_value)
                            page_highlights = self.highlight_text_instances(page, instances, color, display_name)
                            variable_highlights += page_highlights
                        
                        highlights_created += variable_highlights
                        processing_stats['direct_values'] += variable_highlights
                        
                        if variable_highlights > 0:
                            self.logger.info(f"    ✅ {variable_highlights} highlights created")
                        else:
                            self.logger.info(f"    ❌ No matches found")
                    else:
                        self.logger.info(f"  ⚠️  Skipping {variable_name}: empty/short value")
                        processing_stats['skipped_empty'] += 1
                
                else:
                    # Use supporting quotes for complex variables
                    variable_quotes = study_quotes[study_quotes['Variable_Name'] == variable_name]
                    if not variable_quotes.empty:
                        self.logger.info(f"  📝 Quote highlighting: {variable_name} ({len(variable_quotes)} quotes)")
                        
                        variable_highlights = 0
                        for _, quote_row in variable_quotes.iterrows():
                            if highlights_created >= max_highlights:
                                break
                            
                            quote_text = self.clean_text(quote_row['Supporting_Quote'])
                            if quote_text and len(quote_text.strip()) >= 10:
                                # Search all pages for this quote
                                quote_highlights = 0
                                for page_num in range(len(doc)):
                                    page = doc[page_num]
                                    instances = self.find_text_instances(page, quote_text, min_length=10)
                                    page_highlights = self.highlight_text_instances(page, instances, color, display_name)
                                    quote_highlights += page_highlights
                                
                                variable_highlights += quote_highlights
                                highlights_created += quote_highlights
                        
                        processing_stats['supporting_quotes'] += variable_highlights
                        
                        if variable_highlights > 0:
                            self.logger.info(f"    ✅ {variable_highlights} highlights from quotes")
                        else:
                            self.logger.info(f"    ❌ No quote matches found")
                    else:
                        self.logger.info(f"  ⚠️  No quotes available for {variable_name}")
                        processing_stats['skipped_empty'] += 1
            
            # Save result
            today = date.today().strftime("%Y%m%d")
            output_folder = Path(f"{today}_ModeBased_Highlighting")
            output_folder.mkdir(exist_ok=True)
            
            output_path = output_folder / f"Study_{study_id}_HYBRID_HIGHLIGHTED.pdf"
            doc.save(str(output_path))
            doc.close()
            
            self.logger.info(f"✅ Saved: {output_path}")
            self.logger.info(f"✨ Total highlights: {highlights_created}")
            self.logger.info(f"📊 Stats - Direct: {processing_stats['direct_values']}, Quotes: {processing_stats['supporting_quotes']}, Skipped: {processing_stats['skipped_empty']}")
            
            return {
                'success': True,
                'highlights_created': highlights_created,
                'output_file': str(output_path),
                'stats': processing_stats
            }
            
        except Exception as e:
            self.logger.error(f"Error processing study {study_id}: {e}")
            return {'success': False, 'error': str(e)}

def main():
    if len(sys.argv) < 2:
        print("Usage: python precision_quote_highlighter_clean.py <study_id> [pdf_folder]")
        print("       python precision_quote_highlighter_clean.py all [pdf_folder]")
        sys.exit(1)
    
    study_id = sys.argv[1]
    pdf_folder = sys.argv[2] if len(sys.argv) > 2 else "Review_articles"
    
    highlighter = HybridPrecisionHighlighter()
    
    if study_id.lower() == 'all':
        # Process all studies
        today = date.today().strftime("%Y%m%d")
        combined_csv = "20250820_Analysis & Results/20250820_combined_dataset.csv"
        
        try:
            df = pd.read_csv(combined_csv)
            study_ids = df['Study_ID'].unique()
            
            print(f"Processing {len(study_ids)} studies...")
            success_count = 0
            
            for sid in sorted(study_ids):
                result = highlighter.process_study(sid, pdf_folder)
                if result['success']:
                    success_count += 1
                    print(f"✅ Study {sid}: {result['highlights_created']} highlights")
                else:
                    print(f"❌ Study {sid}: {result['error']}")
            
            print(f"\n🎉 Completed: {success_count}/{len(study_ids)} studies processed successfully")
            
        except Exception as e:
            print(f"Error processing all studies: {e}")
    
    else:
        # Process single study
        result = highlighter.process_study(study_id, pdf_folder)
        if result['success']:
            print(f"✅ Successfully highlighted Study {study_id}")
            print(f"   Highlights created: {result['highlights_created']}")
            print(f"   Output file: {result['output_file']}")
        else:
            print(f"❌ Failed to process Study {study_id}: {result['error']}")

if __name__ == "__main__":
    main()
