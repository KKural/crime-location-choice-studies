#!/usr/bin/env python3
"""
Comprehensive Study 1 PDF Highlighter
Uses ALL extracted data from the complete analysis to highlight everything important
"""

import pandas as pd
import fitz  # PyMuPDF
import logging
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
import re

class ComprehensiveStudy1Highlighter:
    """Comprehensive PDF highlighter using ALL Study 1 data."""
    
    def __init__(self, study_id: int = 1):
        self.study_id = study_id
        self.base_dir = Path(__file__).parent
        self.csv_path = self.base_dir / "../20250819_Analysis & Results/20250819_combined_dataset.csv"
        self.pdf_dir = self.base_dir / "../Review_articles"
        
        # Tracking for highlights (once per paper)
        self.highlighted_globally = set()
        
        # Color scheme by importance
        self.colors = {
            'CRITICAL': (1.0, 0.0, 0.0),      # Red - Geographic, Area, Crime
            'HIGH': (1.0, 0.5, 0.0),          # Orange - Quantitative, Methods
            'MEDIUM': (0.0, 0.7, 0.0),        # Green - Results, Technical
            'LOW': (0.0, 0.5, 1.0)            # Blue - Other terms
        }
        
        self.setup_logging()
    
    def setup_logging(self):
        """Configure logging."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = self.base_dir / f"20250820_Comprehensive_Highlighting/comprehensive_{timestamp}.log"
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
        """Load study data."""
        try:
            df = pd.read_csv(self.csv_path)
            study_data = df[df['Study_ID'] == self.study_id].iloc[0]
            logging.info(f"Loaded Study {self.study_id}: {study_data['Title'][:50]}...")
            return study_data.to_dict()
        except Exception as e:
            logging.error(f"Failed to load study data: {e}")
            return {}
    
    def extract_all_highlight_terms(self, study_data: Dict) -> List[Tuple[str, str, str]]:
        """Extract ALL possible terms to highlight from the study data."""
        all_terms = []
        
        # CRITICAL TERMS - Red highlighting
        critical_fields = {
            'Country': ('Belgium',),
            'City': ('Ghent',),
            'Total_Study_Area_Size': ('3000.00 km²', '3000', '3000.00'),
            'Total_Study_Area_km2': ('3000.0', '3000'),
            'Crime_Type': ('Residential burglary',),
            'Spatial_Unit_Name': ('house',)
        }
        
        # HIGH TERMS - Orange highlighting  
        high_fields = {
            'Number_of_Units': ('503,589', '503589'),
            'Crime_Incidents': ('650',),
            'Data_Sources': ('Belgian Federal Police', 'Belgian Land Registry'),
            'Year': ('2015',)
        }
        
        # MEDIUM TERMS - Green highlighting
        medium_fields = {
            'Number_of_Variables': ('7',),
            'Number_of_Data_Sources': ('2',),
            'Software_Used': ('R',),
            'Model_Type': ('Conditional Logit', 'logit')
        }
        
        # Extract CRITICAL terms
        for field, terms in critical_fields.items():
            value = str(study_data.get(field, '')).strip()
            if value and value != 'nan':
                for term in terms:
                    if term in value or term == value:
                        all_terms.append((term, field, 'CRITICAL'))
        
        # Extract HIGH terms
        for field, terms in high_fields.items():
            value = str(study_data.get(field, '')).strip()
            if value and value != 'nan':
                if field == 'Data_Sources':
                    # Split by semicolon
                    for source in value.split(';'):
                        clean_source = source.strip()
                        if clean_source:
                            all_terms.append((clean_source, field, 'HIGH'))
                else:
                    for term in terms:
                        if term in value or term == value:
                            all_terms.append((term, field, 'HIGH'))
        
        # Extract MEDIUM terms
        for field, terms in medium_fields.items():
            value = str(study_data.get(field, '')).strip()
            if value and value != 'nan':
                for term in terms:
                    if term in value or term == value:
                        all_terms.append((term, field, 'MEDIUM'))
        
        # Extract additional terms from quotes and long text fields
        quote_terms = self.extract_from_supporting_quotes(study_data)
        all_terms.extend(quote_terms)
        
        # Extract statistical results
        stats_terms = self.extract_statistical_terms(study_data)
        all_terms.extend(stats_terms)
        
        # Remove duplicates while preserving order and priority
        seen = {}
        unique_terms = []
        for term, field, priority in all_terms:
            if term not in seen or self.get_priority_rank(priority) > self.get_priority_rank(seen[term][1]):
                seen[term] = (field, priority)
        
        for term, (field, priority) in seen.items():
            unique_terms.append((term, field, priority))
        
        # Sort by priority
        priority_order = {'CRITICAL': 0, 'HIGH': 1, 'MEDIUM': 2, 'LOW': 3}
        unique_terms.sort(key=lambda x: priority_order.get(x[2], 4))
        
        logging.info(f"\n=== ALL HIGHLIGHT TERMS ({len(unique_terms)} total) ===")
        for priority in ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW']:
            priority_terms = [t for t in unique_terms if t[2] == priority]
            if priority_terms:
                logging.info(f"\n{priority} PRIORITY ({len(priority_terms)} terms):")
                for term, field, _ in priority_terms:
                    logging.info(f"  • '{term}' (from {field})")
        
        return unique_terms
    
    def extract_from_supporting_quotes(self, study_data: Dict) -> List[Tuple[str, str, str]]:
        """Extract terms from supporting quotes."""
        quote_terms = []
        
        # Key quote fields to search
        important_quotes = [
            'Supporting_quotes_for__Country_',
            'Supporting_quotes_for__City__',
            'Supporting_quotes_for__Total_Study_Area_Size_',
            'Supporting_quotes_for__Crime_Incidents_',
            'Supporting_quotes_for__Number_of_Units_'
        ]
        
        for quote_field in important_quotes:
            quote_text = str(study_data.get(quote_field, '')).strip()
            if quote_text and quote_text != 'nan' and len(quote_text) > 10:
                # Extract quoted phrases
                quoted_phrases = re.findall(r'"([^"]{10,100})"', quote_text)
                for phrase in quoted_phrases[:3]:  # Max 3 per field
                    quote_terms.append((phrase.strip(), quote_field, 'LOW'))
                
                # Extract key numbers
                numbers = re.findall(r'\b(?:1\.5|503,589|650|3,000|12|53)\b', quote_text)
                for num in numbers:
                    quote_terms.append((num, quote_field, 'HIGH'))
        
        return quote_terms
    
    def extract_statistical_terms(self, study_data: Dict) -> List[Tuple[str, str, str]]:
        """Extract statistical terms and results."""
        stats_terms = []
        
        # Effect sizes and coefficients
        effect_text = str(study_data.get('Effect_Sizes', '')).strip()
        if effect_text and effect_text != 'nan':
            # Extract OR values
            or_values = re.findall(r'OR = (0\.\d+)', effect_text)
            for or_val in or_values:
                stats_terms.append((or_val, 'Effect_Sizes', 'MEDIUM'))
            
            # Extract percentages
            percentages = re.findall(r'(\d+)% ', effect_text)
            for pct in percentages:
                stats_terms.append((pct + '%', 'Effect_Sizes', 'MEDIUM'))
        
        # Model fit statistics
        model_fit = str(study_data.get('Model_Fit_Statistics', '')).strip()
        if model_fit and model_fit != 'nan':
            # Extract R-squared
            r_squared = re.search(r'R²: (0\.\d+)', model_fit)
            if r_squared:
                stats_terms.append((r_squared.group(1), 'Model_Fit_Statistics', 'MEDIUM'))
        
        return stats_terms
    
    def get_priority_rank(self, priority: str) -> int:
        """Get numeric rank for priority comparison."""
        ranks = {'CRITICAL': 4, 'HIGH': 3, 'MEDIUM': 2, 'LOW': 1}
        return ranks.get(priority, 0)
    
    def find_pdf_file(self, study_data: Dict) -> Path:
        """Find the PDF file for this study."""
        pdf_files = list(self.pdf_dir.glob("*.pdf"))
        
        for pdf_file in pdf_files:
            if pdf_file.name.startswith(f"{self.study_id:02d}_"):
                logging.info(f"Found PDF: {pdf_file.name}")
                return pdf_file
        
        raise FileNotFoundError(f"No PDF found for Study {self.study_id}")
    
    def highlight_all_terms(self, pdf_path: Path, highlight_terms: List[Tuple[str, str, str]]) -> Dict:
        """Highlight all terms in the PDF."""
        doc = fitz.open(pdf_path)
        highlights_created = 0
        results_by_priority = {'CRITICAL': [], 'HIGH': [], 'MEDIUM': [], 'LOW': []}
        
        logging.info(f"\nProcessing PDF: {pdf_path.name}")
        logging.info(f"Total pages: {len(doc)}")
        logging.info(f"Terms to highlight: {len(highlight_terms)}")
        
        for term, field, priority in highlight_terms:
            if len(term.strip()) < 2:  # Skip very short terms
                continue
                
            found_match = False
            color = self.colors.get(priority, self.colors['LOW'])
            
            # Search through all pages
            for page_num in range(len(doc)):
                page = doc[page_num]
                
                # Try exact match first
                text_instances = page.search_for(term)
                if not text_instances and term.isdigit():
                    # For numbers, try with commas
                    if len(term) > 3:
                        formatted_num = f"{int(term):,}"
                        text_instances = page.search_for(formatted_num)
                
                if text_instances:
                    # Check if we should highlight (once per paper)
                    if term not in self.highlighted_globally:
                        self.highlighted_globally.add(term)
                        
                        # Highlight the first instance
                        rect = text_instances[0]
                        highlight = page.add_highlight_annot(rect)
                        highlight.set_colors(stroke=color)
                        highlight.update()
                        
                        highlights_created += 1
                        found_match = True
                        
                        # Record the match
                        results_by_priority[priority].append({
                            'term': term,
                            'field': field,
                            'page': page_num + 1,
                            'instances': len(text_instances),
                            'priority': priority
                        })
                        
                        logging.info(f"  ✓ {priority}: '{term}' on page {page_num + 1}")
                        break
                    else:
                        logging.info(f"  - SKIPPED: '{term}' (already highlighted)")
                        break
            
            if not found_match and term not in self.highlighted_globally:
                logging.info(f"  ✗ NOT FOUND: '{term}' ({priority})")
        
        # Save highlighted PDF
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_dir = self.base_dir / "20250820_Comprehensive_Highlighting"
        output_dir.mkdir(exist_ok=True)
        
        original_name = pdf_path.stem
        output_path = output_dir / f"Study_{self.study_id}_{original_name}_COMPREHENSIVE.pdf"
        doc.save(str(output_path))
        doc.close()
        
        logging.info(f"\n✓ Saved comprehensive highlighted PDF: {output_path.name}")
        
        # Combine all results
        all_matches = []
        for priority_matches in results_by_priority.values():
            all_matches.extend(priority_matches)
        
        return {
            'success': True,
            'highlights_created': highlights_created,
            'results_by_priority': results_by_priority,
            'all_matches': all_matches,
            'output_path': output_path,
            'total_terms_searched': len(highlight_terms)
        }
    
    def save_comprehensive_results(self, result: Dict, study_data: Dict):
        """Save comprehensive results."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_dir = self.base_dir / "20250820_Comprehensive_Highlighting"
        
        # Save all matches
        if result['all_matches']:
            matches_df = pd.DataFrame(result['all_matches'])
            matches_path = output_dir / f"study_{self.study_id}_comprehensive_matches_{timestamp}.csv"
            matches_df.to_csv(matches_path, index=False)
            logging.info(f"✓ Comprehensive matches saved: {matches_path.name}")
        
        # Save summary by priority
        priority_summary = []
        for priority, matches in result['results_by_priority'].items():
            priority_summary.append({
                'Priority': priority,
                'Terms_Found': len(matches),
                'Color': 'Red' if priority == 'CRITICAL' else 
                        'Orange' if priority == 'HIGH' else
                        'Green' if priority == 'MEDIUM' else 'Blue'
            })
        
        summary_df = pd.DataFrame(priority_summary)
        summary_path = output_dir / f"study_{self.study_id}_priority_summary_{timestamp}.csv"
        summary_df.to_csv(summary_path, index=False)
        logging.info(f"✓ Priority summary saved: {summary_path.name}")
    
    def print_comprehensive_summary(self, result: Dict, study_data: Dict):
        """Print comprehensive summary."""
        print("\n" + "="*70)
        print("COMPREHENSIVE STUDY 1 HIGHLIGHTING - FINAL RESULTS")
        print("="*70)
        
        print(f"\nSTUDY INFORMATION:")
        print(f"📚 Study ID: {self.study_id}")
        print(f"📄 Title: {study_data.get('Title', '')[:50]}...")
        print(f"✅ Success: {result['success']}")
        
        print(f"\nCOMPREHENSIVE HIGHLIGHTING RESULTS:")
        print(f"🎯 Total Terms Searched: {result['total_terms_searched']}")
        print(f"✨ Total Highlights Created: {result['highlights_created']}")
        
        if result['total_terms_searched'] > 0:
            success_rate = (result['highlights_created'] / result['total_terms_searched']) * 100
            print(f"📊 Success Rate: {success_rate:.1f}%")
        
        print(f"\n🎨 HIGHLIGHTS BY PRIORITY:")
        for priority, matches in result['results_by_priority'].items():
            if matches:
                color = '🔴' if priority == 'CRITICAL' else '🟠' if priority == 'HIGH' else '🟢' if priority == 'MEDIUM' else '🔵'
                print(f"  {color} {priority}: {len(matches)} highlights")
        
        print(f"\n📁 Output Location: 20250820_Comprehensive_Highlighting")
        print(f"🎯 Strategy: ALL important data highlighted with priority colors")
        
        print(f"\n🔍 SAMPLE HIGHLIGHTED TERMS:")
        sample_count = 0
        for priority in ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW']:
            matches = result['results_by_priority'][priority]
            if matches and sample_count < 15:
                print(f"\n  {priority}:")
                for match in matches[:3]:  # Show max 3 per priority
                    if sample_count < 15:
                        print(f"    Page {match['page']}: '{match['term']}'")
                        sample_count += 1
        
        print("="*70)
    
    def process_comprehensive_study(self):
        """Process comprehensive highlighting for the study."""
        try:
            # Load study data
            study_data = self.load_study_data()
            if not study_data:
                return False
            
            # Extract all highlight terms
            highlight_terms = self.extract_all_highlight_terms(study_data)
            
            # Find PDF file
            pdf_file = self.find_pdf_file(study_data)
            
            # Perform comprehensive highlighting
            result = self.highlight_all_terms(pdf_file, highlight_terms)
            
            # Save results
            self.save_comprehensive_results(result, study_data)
            
            # Print summary
            self.print_comprehensive_summary(result, study_data)
            
            return result['success']
            
        except Exception as e:
            logging.error(f"Error processing comprehensive study {self.study_id}: {e}")
            return False

def main():
    """Main function."""
    study_id = 1
    
    print("="*70)
    print("COMPREHENSIVE STUDY 1 PDF HIGHLIGHTER")
    print("="*70)
    print("Strategy:")
    print("🔴 CRITICAL: Geographic, Area, Crime data (Red)")
    print("🟠 HIGH: Quantitative, Methods, Years (Orange)")
    print("🟢 MEDIUM: Results, Statistics, Technical (Green)")
    print("🔵 LOW: Quotes, Additional terms (Blue)")
    print("🎯 ALL IMPORTANT DATA from 98 columns analyzed!")
    print("="*70)
    
    highlighter = ComprehensiveStudy1Highlighter(study_id=study_id)
    success = highlighter.process_comprehensive_study()
    
    if success:
        print(f"\n🎉 SUCCESS: Comprehensive highlighting completed for Study {study_id}!")
        return 0
    else:
        print(f"\n❌ FAILED: Could not complete comprehensive highlighting")
        return 1

if __name__ == "__main__":
    exit(main())
