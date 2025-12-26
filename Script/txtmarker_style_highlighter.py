#!/usr/bin/env python3
"""
TxtMarker-Style PDF Highlighter
===============================

A clean, focused PDF highlighter inspired by txtmarker philosophy:
- Simple string matching for high-value terms
- Section restriction (avoid References)
- Clean highlighting with minimal visual noise
- First hit per variable (surgical precision)
- Comprehensive term extraction for different variable types
"""

import pandas as pd
import fitz  # PyMuPDF
import re
from pathlib import Path
from typing import List, Dict, Optional
import csv
import argparse

class TxtMarkerStyleHighlighter:
    """Clean PDF highlighter using txtmarker-inspired approach."""
    
    def __init__(self, study_id: int = 1):
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv" 
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_TxtMarker_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        self.match_log = []
        
        # Section filtering keywords
        self.SECTION_KEEP = ["abstract", "introduction", "method", "data", "results", "discussion", "conclusion", "limitation"]
        self.SECTION_STOP = ["references", "acknowledgement", "appendix", "bibliography"]

    def load_study_data(self) -> pd.Series:
        """Load study data from CSV."""
        df = pd.read_csv(self.csv_path, engine="python", quoting=csv.QUOTE_MINIMAL, keep_default_na=False)
        df = df.apply(lambda col: col.map(lambda x: x.replace('""', '"') if isinstance(x, str) else x))
        
        if 'Study_ID' in df.columns:
            df['Study_ID'] = df['Study_ID'].astype(int)
        
        study_rows = df[df['Study_ID'] == self.study_id]
        if study_rows.empty:
            available = sorted(df['Study_ID'].unique())
            raise ValueError(f"Study_ID {self.study_id} not found. Available: {available}")
        
        return study_rows.iloc[0]

    def find_pdf_file(self) -> Path:
        """Find PDF file using multiple strategies."""
        patterns = [f"{self.study_id:02d}_*.pdf", f"{self.study_id}_*.pdf"]
        
        for pattern in patterns:
            matches = list(self.pdf_folder.glob(pattern))
            if matches:
                return matches[0]
        
        # Fallback to first PDF
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if pdf_files:
            return pdf_files[0]
        
        raise FileNotFoundError(f"No PDF files found in {self.pdf_folder}")

    def clean_text(self, text: str) -> str:
        """Clean and normalize text."""
        if not text:
            return ""
        text = str(text).strip()
        # Normalize quotes and spaces
        text = (text.replace('\u2018', "'").replace('\u2019', "'")
                   .replace('\u201C', '"').replace('\u201D', '"')
                   .replace('\u00A0', ' ').replace('\u00AD', ''))
        return re.sub(r'\s+', ' ', text)

    def is_meaningful_term(self, term: str) -> bool:
        """Check if term is worth highlighting."""
        if not term or len(term.strip()) <= 2:
            return False
        term = term.strip()
        # Skip pure punctuation or lone numbers
        if re.fullmatch(r"[-–—.,;:]+", term) or (term.isdigit() and len(term) <= 2):
            return False
        return True

    def generate_search_terms(self, variable: str, value: str) -> List[str]:
        """Generate search terms based on variable type."""
        if pd.isna(value) or not str(value).strip():
            return []
        
        value_str = str(value).strip()
        clean_value = self.clean_text(value_str)
        
        # Skip "not specified" entries
        skip_phrases = ["not specified", "not mentioned", "not applicable", "n/a", "na"]
        if any(phrase in value_str.lower() for phrase in skip_phrases):
            return []
        
        terms = []
        
        # Direct match variables (simple, high-value terms)
        if variable in ["Country", "City", "Crime_Type", "Model_Type", "Software_Used"]:
            # Extract quoted content first
            quotes = re.findall(r'["\'`]([^"\'`]+)["\'`]', clean_value)
            if quotes:
                terms = [q.strip() for q in quotes if self.is_meaningful_term(q)]
            elif self.is_meaningful_term(clean_value):
                terms = [clean_value]
        
        # Numeric variables with number extraction
        elif variable in ["Total_Study_Area_Size", "Number_of_Units", "Crime_Incidents"]:
            # Find numbers with possible formatting
            numbers = re.findall(r'\d[\d,.\s]*\d|\d+', clean_value)
            for num in numbers:
                base_num = re.sub(r'[,\s]', '', num)
                if len(base_num) >= 3:  # Only meaningful numbers
                    # Add variations: 503589, 503,589, 503 589
                    terms.append(base_num)
                    if len(base_num) >= 4:
                        comma_num = re.sub(r'(\d)(?=(\d{3})+$)', r'\1,', base_num)
                        space_num = re.sub(r'(\d)(?=(\d{3})+$)', r'\1 ', base_num)
                        terms.extend([comma_num, space_num])
            
            # Also include unit patterns
            units = re.findall(r'\d+\s*(?:km²|km2|ha|hectares?|m²|square)', clean_value, re.IGNORECASE)
            terms.extend(units)
        
        # Date/period variables
        elif variable == "Data_Collection_Period":
            years = re.findall(r'(19|20)\d{2}', clean_value)
            terms = years[:]
            if len(years) >= 2:
                terms.append(f"{years[0]}-{years[-1]}")
        
        # Supporting quotes - extract quoted content only
        elif variable.startswith("Supporting_") or "supporting_quotes_for__" in variable.lower():
            quotes = re.findall(r'["\'`]([^"\'`]{15,150})["\'`]', clean_value)
            terms = [self.clean_text(q) for q in quotes if len(q.split()) >= 4]
        
        # Independent variables - split and clean
        elif variable == "Independent_Variables":
            clean_value = clean_value.replace('_', ' ')
            items = re.split(r'[;\n•|]', clean_value)
            for item in items:
                item = re.sub(r'^\s*\d+[\.\)]\s*', '', item).strip(' -•')
                if self.is_meaningful_term(item):
                    terms.append(item)
                    # Add short versions for long terms
                    words = item.split()
                    if len(words) > 3:
                        terms.append(' '.join(words[:2]))
        
        # Coefficients and effect sizes
        elif variable in ["Coefficients", "Effect_Sizes"]:
            # Extract labels before colons
            labels = re.findall(r'([^:]+):', clean_value)
            for label in labels:
                clean_label = label.strip().replace('_', ' ')
                if self.is_meaningful_term(clean_label):
                    terms.append(clean_label)
            
            # Extract statistical patterns
            patterns = re.findall(r'OR\s*=\s*\d+\.?\d*|odds\s+ratio|coefficient', clean_value, re.IGNORECASE)
            terms.extend(patterns)
        
        # Default case - use cleaned full value
        else:
            if self.is_meaningful_term(clean_value) and len(clean_value) <= 50:
                terms.append(clean_value)
        
        # Filter and deduplicate terms
        final_terms = []
        for term in terms:
            clean_term = self.clean_text(term)
            if self.is_meaningful_term(clean_term) and clean_term not in final_terms:
                final_terms.append(clean_term)
        
        return final_terms[:5]  # Limit to 5 terms per variable for focus

    def get_search_pages(self, doc: fitz.Document) -> List[int]:
        """Determine which pages to search (avoid References)."""
        search_pages = []
        
        for page_num in range(len(doc)):
            page_text = doc[page_num].get_text().lower()
            
            # Stop at references/bibliography
            if any(stop_word in page_text for stop_word in self.SECTION_STOP):
                break
            
            # Include pages with relevant content or first few pages
            if any(keep_word in page_text for keep_word in self.SECTION_KEEP) or page_num < 4:
                search_pages.append(page_num)
        
        # Fallback: first 8 pages max
        return search_pages or list(range(min(8, len(doc))))

    def highlight_pdf(self, pdf_path: Path, terms_dict: Dict[str, List[str]]) -> bool:
        """Highlight terms in PDF with clean, precise highlighting."""
        try:
            doc = fitz.open(str(pdf_path))
            search_pages = self.get_search_pages(doc)
            
            print(f"🔍 Searching pages: {[p+1 for p in search_pages]}")
            
            highlights_made = 0
            total_terms = sum(len(terms) for terms in terms_dict.values())
            
            # Color scheme by category
            colors = {
                'Country': (1.0, 0.7, 0.7),              # Light red
                'City': (1.0, 0.7, 0.7),                 # Light red
                'Crime_Type': (0.7, 1.0, 0.7),           # Light green
                'Data_Collection_Period': (0.7, 0.7, 1.0), # Light blue
                'Number_of_Units': (1.0, 1.0, 0.7),      # Light yellow
                'Independent_Variables': (1.0, 0.7, 1.0), # Light pink
                'Coefficients': (0.7, 1.0, 1.0),         # Light cyan
                'Effect_Sizes': (0.7, 1.0, 1.0),         # Light cyan
            }
            default_color = (0.9, 0.9, 0.9)
            
            # Process each variable (first hit only)
            for variable, terms in terms_dict.items():
                color = colors.get(variable, default_color)
                variable_hits = 0
                
                print(f"\n--- {variable} ---")
                
                for term in terms:
                    if not self.is_meaningful_term(term):
                        continue
                    
                    found = False
                    
                    # Search pages in order
                    for page_num in search_pages:
                        page = doc[page_num]
                        
                        # Try different case variations
                        search_variants = [term, term.lower(), term.title(), term.upper()]
                        
                        for variant in search_variants:
                            try:
                                hits = page.search_for(variant)
                                if hits:
                                    rect = hits[0]  # First hit only
                                    
                                    # Skip author affiliations area on first page
                                    if page_num == 0 and rect.y0 < page.rect.height * 0.15:
                                        continue
                                    
                                    # Create clean highlight
                                    highlight = page.add_highlight_annot(rect)
                                    highlight.set_colors(stroke=color)
                                    highlight.set_opacity(0.3)
                                    highlight.set_info(title=variable, content="")
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
                                    
                                    break
                            except Exception:
                                continue
                        
                        if found:
                            break  # Stop after first hit (surgical precision)
                    
                    if not found:
                        print(f"   ❌ '{term}' not found")
                        self.match_log.append({
                            'Variable': variable,
                            'Term': term,
                            'Page': 'Not Found',
                            'Found': 'No'
                        })
                    
                    # Stop after first successful hit per variable
                    if variable_hits >= 1:
                        print(f"   (stopping after first hit for {variable})")
                        break
                
                print(f"📊 {variable}: {variable_hits} highlight{'s' if variable_hits != 1 else ''}")
            
            # Save highlighted PDF
            output_path = self.output_folder / f"Study_{self.study_id}_highlighted.pdf"
            doc.save(str(output_path))
            doc.close()
            
            success_rate = (highlights_made / len(terms_dict) * 100) if terms_dict else 0
            print(f"\n✅ Saved: {output_path.name}")
            print(f"📊 Success: {highlights_made}/{len(terms_dict)} variables ({success_rate:.1f}%)")
            
            return True
            
        except Exception as e:
            print(f"❌ Error highlighting PDF: {e}")
            return False

    def save_match_log(self):
        """Save match results to CSV."""
        if self.match_log:
            log_path = self.output_folder / f"Study_{self.study_id}_matches.csv"
            df_log = pd.DataFrame(self.match_log)
            df_log.to_csv(log_path, index=False, encoding='utf-8')
            print(f"📋 Match log: {log_path.name}")

    def process_study(self) -> bool:
        """Main processing function."""
        print("=" * 70)
        print("TXTMARKER-STYLE PDF HIGHLIGHTER")
        print("=" * 70)
        print("✅ FEATURES:")
        print("🎯 First hit per variable (surgical precision)")
        print("📄 Section restriction (excludes References)")  
        print("🎨 Clean highlighting with category colors")
        print("🔍 Smart term extraction by variable type")
        print("=" * 70)
        print(f"🆔 Processing Study ID: {self.study_id}")
        
        try:
            # Load study data
            study_data = self.load_study_data()
            print(f"✅ Loaded: {study_data.get('Title', 'No Title')[:60]}...")
            
            # Find PDF
            pdf_path = self.find_pdf_file()
            print(f"📄 PDF: {pdf_path.name}")
            
            # Extract terms from all columns
            terms_dict = {}
            processed_columns = 0
            
            print(f"\n🔍 EXTRACTING TERMS:")
            for column in study_data.index:
                if column in ['Study_ID', 'Title', 'Authors', 'Year']:
                    continue
                
                value = study_data[column]
                terms = self.generate_search_terms(column, value)
                
                if terms:
                    terms_dict[column] = terms
                    processed_columns += 1
                    # Show first 2 terms only
                    preview = terms[:2]
                    preview_str = ', '.join(f"'{t}'" for t in preview)
                    more = '...' if len(terms) > 2 else ''
                    print(f"📝 {column}: {len(terms)} terms ({preview_str}{more})")
            
            if not terms_dict:
                print("❌ No terms extracted!")
                return False
            
            print(f"\n📊 SUMMARY:")
            print(f"   Processed columns: {processed_columns}")
            print(f"   Variables with terms: {len(terms_dict)}")
            print(f"   Total terms: {sum(len(terms) for terms in terms_dict.values())}")
            
            # Highlight PDF
            print(f"\n🎨 HIGHLIGHTING PDF:")
            success = self.highlight_pdf(pdf_path, terms_dict)
            
            # Save log
            self.save_match_log()
            
            if success:
                print(f"\n🎉 SUCCESS! Study {self.study_id} processed successfully.")
                return True
            else:
                print(f"\n❌ FAILED! Study {self.study_id} processing failed.")
                return False
                
        except Exception as e:
            print(f"❌ Error: {e}")
            return False


def main():
    """Main function with command line arguments."""
    parser = argparse.ArgumentParser(description='TxtMarker-Style PDF Highlighter')
    parser.add_argument('--study-id', type=int, default=1, help='Study ID to process (default: 1)')
    
    args = parser.parse_args()
    
    highlighter = TxtMarkerStyleHighlighter(study_id=args.study_id)
    return highlighter.process_study()


if __name__ == "__main__":
    main()
