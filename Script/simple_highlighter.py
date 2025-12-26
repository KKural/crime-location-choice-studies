#!/usr/bin/env python3
"""
Simple PDF Highlighter
======================

A very simple PDF highlighter that focuses on the most important, 
easily-findable terms from the study data.
"""

import pandas as pd
import fitz  # PyMuPDF
import re
from pathlib import Path
import csv
import argparse

class SimplePDFHighlighter:
    """Simple PDF highlighter focusing on high-value terms."""
    
    def __init__(self, study_id: int = 1):
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_Simple_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        self.match_log = []

    def load_study_data(self) -> pd.Series:
        """Load study data from CSV."""
        df = pd.read_csv(self.csv_path, engine="python", quoting=csv.QUOTE_MINIMAL, keep_default_na=False)
        df = df.apply(lambda col: col.map(lambda x: x.replace('""', '"') if isinstance(x, str) else x))
        
        if 'Study_ID' in df.columns:
            df['Study_ID'] = df['Study_ID'].astype(int)
        
        study_rows = df[df['Study_ID'] == self.study_id]
        if study_rows.empty:
            raise ValueError(f"Study_ID {self.study_id} not found")
        
        return study_rows.iloc[0]

    def find_pdf_file(self) -> Path:
        """Find PDF file."""
        pattern = f"{self.study_id:02d}_*.pdf"
        matches = list(self.pdf_folder.glob(pattern))
        if matches:
            return matches[0]
        
        # Fallback
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if pdf_files:
            return pdf_files[0]
        
        raise FileNotFoundError("No PDF files found")

    def extract_simple_terms(self, study_data: pd.Series) -> dict:
        """Extract simple, high-value terms."""
        terms = {}
        
        # High-value simple terms
        if not pd.isna(study_data.get('Country', '')):
            country = str(study_data['Country']).strip()
            if country and country != 'Not specified':
                terms['Country'] = [country]
        
        if not pd.isna(study_data.get('City', '')):
            city = str(study_data['City']).strip()
            if city and city != 'Not specified':
                terms['City'] = [city]
        
        # Extract years from Data_Collection_Period
        period = str(study_data.get('Data_Collection_Period', ''))
        years = re.findall(r'(19|20)\d{2}', period)
        if years:
            terms['Data_Collection_Period'] = years[:3]  # Max 3 years
        
        # Extract simple numbers
        units = str(study_data.get('Number_of_Units', ''))
        if units and units.isdigit():
            terms['Number_of_Units'] = [units]
        elif '503' in units:  # Common number from study
            terms['Number_of_Units'] = ['503', '503589', '503,589']
        
        # Software - extract simple terms
        software = str(study_data.get('Software_Used', ''))
        if 'R' in software:
            terms['Software_Used'] = ['R']
        
        # Model type - simplified
        model = str(study_data.get('Model_Type', ''))
        if 'logit' in model.lower():
            terms['Model_Type'] = ['logit', 'conditional logit']
        
        # Crime type
        crime = str(study_data.get('Crime_Type', ''))
        if crime and 'burglary' in crime.lower():
            terms['Crime_Type'] = ['burglary', 'burglaries']
        
        return terms

    def highlight_pdf(self, pdf_path: Path, terms_dict: dict) -> bool:
        """Highlight terms in PDF."""
        try:
            doc = fitz.open(str(pdf_path))
            highlights_made = 0
            total_terms = sum(len(terms) for terms in terms_dict.values())
            
            print(f"🔍 Searching {len(doc)} pages for {total_terms} terms")
            
            # Simple color scheme
            colors = {
                'Country': (1.0, 0.8, 0.8),      # Light red
                'City': (1.0, 0.8, 0.8),         # Light red
                'Data_Collection_Period': (0.8, 1.0, 0.8),  # Light green
                'Number_of_Units': (0.8, 0.8, 1.0),         # Light blue
                'Software_Used': (1.0, 1.0, 0.8),           # Light yellow
                'Model_Type': (1.0, 0.8, 1.0),              # Light pink
                'Crime_Type': (0.8, 1.0, 1.0),              # Light cyan
            }
            
            # Search first 8 pages only (before references)
            search_pages = list(range(min(8, len(doc))))
            
            for variable, terms in terms_dict.items():
                color = colors.get(variable, (0.9, 0.9, 0.9))
                variable_hits = 0
                
                print(f"\n--- {variable} ---")
                
                for term in terms:
                    found = False
                    
                    for page_num in search_pages:
                        page = doc[page_num]
                        
                        # Try multiple variations of search
                        search_variants = [term, term.lower(), term.upper(), term.title()]
                        
                        for variant in search_variants:
                            try:
                                hits = page.search_for(variant)
                                if hits:
                                    # Use first hit only
                                    rect = hits[0]
                                    
                                    # Skip if in top 15% of first page (author area)
                                    if page_num == 0 and rect.y0 < page.rect.height * 0.15:
                                        continue
                                    
                                    # Create highlight
                                    highlight = page.add_highlight_annot(rect)
                                    highlight.set_colors(stroke=color)
                                    highlight.set_opacity(0.4)
                                    highlight.update()
                                    
                                    highlights_made += 1
                                    variable_hits += 1
                                    found = True
                                    
                                    print(f"   ✅ '{term}' found on page {page_num + 1}")
                                    
                                    self.match_log.append({
                                        'Variable': variable,
                                        'Term': term,
                                        'Page': page_num + 1,
                                        'Found': 'Yes'
                                    })
                                    
                                    break  # Stop after first hit
                                    
                            except Exception as e:
                                continue
                        
                        if found:
                            break  # Stop searching other pages for this term
                    
                    if not found:
                        print(f"   ❌ '{term}' not found")
                        self.match_log.append({
                            'Variable': variable,
                            'Term': term,
                            'Page': 'Not Found',
                            'Found': 'No'
                        })
                
                print(f"📊 {variable}: {variable_hits}/{len(terms)} terms found")
            
            # Save result
            output_path = self.output_folder / f"Study_{self.study_id}_simple_highlighted.pdf"
            doc.save(str(output_path))
            doc.close()
            
            success_rate = (highlights_made / total_terms * 100) if total_terms > 0 else 0
            print(f"\n✅ Saved: {output_path.name}")
            print(f"📊 Success: {highlights_made}/{total_terms} ({success_rate:.1f}%)")
            
            return True
            
        except Exception as e:
            print(f"❌ Error: {e}")
            return False

    def save_log(self):
        """Save match log."""
        if self.match_log:
            log_path = self.output_folder / f"Study_{self.study_id}_simple_matches.csv"
            df = pd.DataFrame(self.match_log)
            df.to_csv(log_path, index=False)
            print(f"📋 Log saved: {log_path.name}")

    def process(self) -> bool:
        """Main processing."""
        print("=" * 50)
        print("SIMPLE PDF HIGHLIGHTER")
        print("=" * 50)
        print(f"🆔 Study ID: {self.study_id}")
        
        try:
            # Load data
            study_data = self.load_study_data()
            print(f"✅ Loaded: {study_data.get('Title', 'No Title')[:50]}...")
            
            # Find PDF
            pdf_path = self.find_pdf_file()
            print(f"📄 PDF: {pdf_path.name}")
            
            # Extract simple terms
            terms_dict = self.extract_simple_terms(study_data)
            
            if not terms_dict:
                print("❌ No terms found!")
                return False
            
            print(f"\n🔍 TERMS TO HIGHLIGHT:")
            for var, terms in terms_dict.items():
                print(f"   {var}: {terms}")
            
            # Highlight
            success = self.highlight_pdf(pdf_path, terms_dict)
            self.save_log()
            
            return success
            
        except Exception as e:
            print(f"❌ Error: {e}")
            return False


def main():
    parser = argparse.ArgumentParser(description='Simple PDF Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID')
    args = parser.parse_args()
    
    highlighter = SimplePDFHighlighter(study_id=args.study_id)
    return highlighter.process()


if __name__ == "__main__":
    main()
