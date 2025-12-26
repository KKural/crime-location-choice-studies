#!/usr/bin/env python3
"""
Clean PDF Highlighter
=====================

A simplified, focused approach to highlighting study variables in PDFs.
Uses PyMuPDF with clean, surgical highlighting - no complex modes or overlaps.
"""

import pandas as pd
import fitz  # PyMuPDF
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple
import logging
import csv
import argparse

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class CleanStudyHighlighter:
    """Clean, focused PDF highlighter for study variables."""
    
    def __init__(self, study_id: int = 1):
        """Initialize the highlighter."""
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_Clean_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Track results
        self.match_log = []
        
        # Section filtering
        self.SECTION_KEEP = ["abstract", "method", "data", "results", "discussion", "conclusion", "limitations"]
        self.SECTION_STOP = ["references", "acknowledgements", "appendix", "bibliography"]

    def load_study_data(self) -> Optional[pd.Series]:
        """Load study data from CSV."""
        try:
            df = pd.read_csv(self.csv_path, engine="python", quoting=csv.QUOTE_MINIMAL, keep_default_na=False)
            df = df.apply(lambda col: col.map(lambda x: x.replace('""', '"') if isinstance(x, str) else x))
            
            if 'Study_ID' in df.columns:
                df['Study_ID'] = df['Study_ID'].astype(int)
            
            study_rows = df[df['Study_ID'] == int(self.study_id)]
            if study_rows.empty:
                available_ids = sorted(df['Study_ID'].unique())
                raise ValueError(f"Study_ID {self.study_id} not found. Available IDs: {available_ids}")
            
            study_data = study_rows.iloc[0]
            print(f"✅ Loaded Study {self.study_id}: {study_data.get('Title', 'No Title')[:60]}...")
            return study_data
            
        except Exception as e:
            logging.error(f"Error loading study data: {e}")
            return None

    def find_pdf_file(self, study_data: pd.Series) -> Optional[Path]:
        """Find PDF file."""
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if not pdf_files:
            raise FileNotFoundError(f"No PDF files found in {self.pdf_folder}")
        
        # Try numbered patterns first
        for pattern in [f"{self.study_id:02d}_*.pdf", f"{self.study_id}_*.pdf"]:
            matches = list(self.pdf_folder.glob(pattern))
            if matches:
                print(f"📄 Found PDF: {matches[0].name}")
                return matches[0]
        
        print(f"⚠️ Using fallback PDF: {pdf_files[0].name}")
        return pdf_files[0]

    def clean_text(self, text: str) -> str:
        """Clean and normalize text."""
        if not text:
            return ""
        
        text = str(text).strip()
        text = (text
                .replace('\u2018', "'").replace('\u2019', "'")
                .replace('\u201C', '"').replace('\u201D', '"')
                .replace('\u2013', '–').replace('\u2014', '—')
                .replace('\u00AD', '').replace('\u00A0', ' '))
        
        return re.sub(r'\s+', ' ', text)

    def is_meaningful_term(self, term: str) -> bool:
        """Check if a term is worth highlighting."""
        if not term or len(term.strip()) <= 2:
            return False
        
        term = term.strip()
        
        # Skip lone punctuation
        if re.fullmatch(r"[-–—.,;:]+", term):
            return False
            
        # Skip pure numbers (but keep numbers with letters like "km²")
        if re.fullmatch(r"\d+(?:[ ,.\u00A0]\d+)*", term) and not re.search(r"[A-Za-z]", term):
            return False
            
        return True

    def generate_search_terms(self, variable: str, value: str) -> List[str]:
        """Generate clean, focused search terms."""
        if pd.isna(value) or not str(value).strip():
            return []
        
        value_str = str(value).strip()
        
        # Skip empty/not applicable entries
        skip_phrases = ["not specified", "not mentioned", "not applicable", "n/a"]
        if any(phrase in value_str.lower() for phrase in skip_phrases):
            return []
        
        clean_value = self.clean_text(value_str)
        terms = []
        
        # Handle different variable types
        if variable in ["Country", "City", "Crime_Type", "Model_Type", "Software_Used"]:
            # Direct string matching for simple fields
            if self.is_meaningful_term(clean_value):
                terms = [clean_value]
        
        elif variable in ["Total_Study_Area_Size", "Number_of_Units", "Crime_Incidents"]:
            # Numeric fields - extract numbers and units
            numbers = re.findall(r'\d[\d,.\s]*\d|\d+', clean_value)
            for num in numbers:
                base_num = re.sub(r'[,\s]', '', num)
                if len(base_num) > 2:
                    # Add variations
                    if len(base_num) >= 4:
                        comma_num = re.sub(r'(\d)(?=(\d{3})+$)', r'\1,', base_num)
                        terms.extend([base_num, comma_num])
                    else:
                        terms.append(base_num)
            
            # Add units
            units = re.findall(r'\d+\s*(?:km²|km2|ha|hectares?|m²)', clean_value, re.IGNORECASE)
            terms.extend(units)
        
        elif variable.startswith("Supporting_") or "supporting_quotes_for__" in variable.lower():
            # Extract quoted content only
            quotes = re.findall(r'["\'`]([^"\'`]{10,200})["\'`]', clean_value)
            terms = [self.clean_text(q) for q in quotes if self.is_meaningful_term(q)]
        
        elif variable == "Independent_Variables":
            # Split into individual variables
            items = re.split(r'[;\n•|]', clean_value.replace('_', ' '))
            for item in items:
                item = re.sub(r'^\s*\d+[\.\)]\s*', '', item).strip(' -•')
                if self.is_meaningful_term(item):
                    terms.append(item)
        
        elif variable in ["Coefficients", "Effect_Sizes"]:
            # Extract labels and statistical patterns
            labels = re.findall(r'([^:]+):', clean_value)
            terms.extend([label.strip() for label in labels if self.is_meaningful_term(label.strip())])
            
            # OR patterns
            or_patterns = re.findall(r'OR\s*=\s*\d+\.?\d*', clean_value, re.IGNORECASE)
            terms.extend(or_patterns)
        
        elif variable == "Data_Collection_Period":
            # Extract years
            years = re.findall(r'(?:19|20)\d{2}', clean_value)
            terms = years[:]
            if len(years) >= 2:
                terms.append(f"{years[0]}-{years[-1]}")
        
        else:
            # Default: use full cleaned value if meaningful
            if self.is_meaningful_term(clean_value):
                terms = [clean_value]
        
        # Filter and clean final terms
        final_terms = []
        for term in terms:
            clean_term = self.clean_text(term)
            if self.is_meaningful_term(clean_term) and clean_term not in final_terms:
                final_terms.append(clean_term)
        
        return final_terms

    def get_section_pages(self, doc: fitz.Document) -> List[int]:
        """Determine pages to search based on section content."""
        pages_to_scan = []
        
        for page_num in range(len(doc)):
            page_text = doc[page_num].get_text().lower()
            
            # Stop at references
            if any(stop_word in page_text for stop_word in self.SECTION_STOP):
                break
            
            # Include if has relevant section keywords
            if any(keep_word in page_text for keep_word in self.SECTION_KEEP) or page_num < 3:
                pages_to_scan.append(page_num)
        
        return pages_to_scan or list(range(min(8, len(doc))))  # fallback to first 8 pages

    def highlight_pdf(self, pdf_path: Path, terms_dict: Dict[str, List[str]]) -> bool:
        """Highlight terms in PDF with clean, precise highlighting."""
        try:
            doc = fitz.open(str(pdf_path))
            pages_to_scan = self.get_section_pages(doc)
            
            print(f"🔍 Scanning pages: {[p+1 for p in pages_to_scan]}")
            
            highlights_made = 0
            total_terms = sum(len(terms) for terms in terms_dict.values())
            
            # Simple color scheme
            colors = {
                'Country': (1.0, 0.7, 0.7),      # Light red
                'City': (1.0, 0.7, 0.7),         # Light red  
                'Crime_Type': (0.7, 1.0, 0.7),   # Light green
                'Independent_Variables': (0.7, 0.7, 1.0),  # Light blue
                'Effect_Sizes': (1.0, 1.0, 0.7), # Light yellow
                'Coefficients': (1.0, 1.0, 0.7), # Light yellow
            }
            default_color = (0.9, 0.9, 0.9)  # Light gray
            
            # Process each variable
            for variable, terms in terms_dict.items():
                color = colors.get(variable, default_color)
                variable_hits = 0
                
                print(f"\n--- {variable} ---")
                
                for term in terms:
                    if not self.is_meaningful_term(term):
                        continue
                        
                    found_on_page = None
                    
                    # Search pages in order
                    for page_num in pages_to_scan:
                        page = doc[page_num]
                        
                        # Skip top of page 1 (author info)
                        search_areas = None
                        if page_num == 0:
                            page_rect = page.rect
                            search_areas = [fitz.Rect(0, page_rect.height * 0.15, page_rect.width, page_rect.height)]
                        
                        # Simple text search with case-insensitive matching
                        try:
                            # First try basic search without flags
                            hits = page.search_for(term)
                            
                            # If no hits, try with lowercase
                            if not hits:
                                hits = page.search_for(term.lower())
                            
                            # If still no hits, try with title case
                            if not hits:
                                hits = page.search_for(term.title())
                            
                            if hits and search_areas:
                                # Filter hits to allowed areas
                                hits = [hit for hit in hits if any(area.contains(hit) for area in search_areas)]
                            
                            if hits:
                                # Use first hit only (surgical precision)
                                rect = hits[0]
                                
                                # Create clean highlight
                                highlight = page.add_highlight_annot(rect)
                                highlight.set_colors(stroke=color)
                                highlight.set_opacity(0.3)
                                highlight.set_info(title=variable, content="")
                                highlight.update()
                                
                                highlights_made += 1
                                variable_hits += 1
                                found_on_page = page_num + 1
                                
                                print(f"   ✅ '{term}' found on page {found_on_page}")
                                break  # First hit only
                                
                        except Exception as e:
                            logging.debug(f"Search error for '{term}': {e}")
                            continue
                    
                    # Log result
                    self.match_log.append({
                        'Variable': variable,
                        'Term': term,
                        'Page': found_on_page if found_on_page else 'Not Found',
                        'Found': 'Yes' if found_on_page else 'No'
                    })
                    
                    if not found_on_page:
                        print(f"   ❌ '{term}' not found")
                
                print(f"📊 {variable}: {variable_hits}/{len(terms)} terms found")
            
            # Save highlighted PDF
            output_path = self.output_folder / f"Study_{self.study_id}_highlighted.pdf"
            doc.save(str(output_path))
            doc.close()
            
            success_rate = (highlights_made / total_terms * 100) if total_terms > 0 else 0
            print(f"\n✅ Highlighting completed: {output_path.name}")
            print(f"📊 Success rate: {highlights_made}/{total_terms} ({success_rate:.1f}%)")
            
            return True
            
        except Exception as e:
            print(f"❌ Error highlighting PDF: {e}")
            logging.error(f"Error highlighting PDF: {e}")
            return False

    def save_match_log(self) -> None:
        """Save match results to CSV."""
        if not self.match_log:
            return
        
        log_path = self.output_folder / f"Study_{self.study_id}_matches.csv"
        df_log = pd.DataFrame(self.match_log)
        df_log.to_csv(log_path, index=False, encoding='utf-8')
        print(f"📋 Match log saved: {log_path.name}")

    def process_study(self) -> bool:
        """Main processing function."""
        print("=" * 60)
        print("CLEAN PDF HIGHLIGHTER")
        print("=" * 60)
        print(f"🆔 Processing Study ID: {self.study_id}")
        
        # Load data
        study_data = self.load_study_data()
        if study_data is None:
            return False
        
        # Find PDF
        try:
            pdf_path = self.find_pdf_file(study_data)
        except FileNotFoundError as e:
            print(f"❌ {e}")
            return False
        
        # Collect terms
        print(f"\n🔍 COLLECTING TERMS:")
        terms_dict = {}
        
        for column in study_data.index:
            if column in ['Study_ID', 'Title', 'Authors', 'Year']:
                continue
                
            value = study_data[column]
            terms = self.generate_search_terms(column, value)
            
            if terms:
                terms_dict[column] = terms
                term_preview = terms[:2]  # Show first 2 terms
                preview_str = str(term_preview)[1:-1].replace("'", "")
                print(f"📝 {column}: {len(terms)} terms ({preview_str}{'...' if len(terms) > 2 else ''})")
        
        if not terms_dict:
            print("❌ No terms to highlight!")
            return False
        
        total_terms = sum(len(terms) for terms in terms_dict.values())
        print(f"\n📊 SUMMARY: {len(terms_dict)} variables, {total_terms} total terms")
        
        # Highlight PDF
        print(f"\n🎨 HIGHLIGHTING: {pdf_path.name}")
        success = self.highlight_pdf(pdf_path, terms_dict)
        
        # Save log
        self.save_match_log()
        
        if success:
            print(f"\n🎉 SUCCESS! Study {self.study_id} processed.")
            return True
        else:
            print(f"\n❌ FAILED! Study {self.study_id} processing failed.")
            return False


def main():
    """Main function."""
    parser = argparse.ArgumentParser(description='Clean PDF Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID to process')
    
    args = parser.parse_args()
    
    highlighter = CleanStudyHighlighter(study_id=args.study_id)
    return highlighter.process_study()


if __name__ == "__main__":
    main()
