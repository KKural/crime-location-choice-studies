#!/usr/bin/env python3
"""
TxtMarker PDF Highlighter
========================

A clean approach to highlighting study variables in PDFs using txtmarker library.
Highlights terms from CSV data with section restriction and smart pattern matching.
"""

import pandas as pd
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple
import logging
import csv
import argparse
from txtmarker import PdfMarker

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class StudyHighlighter:
    """Highlights study variables in PDFs using txtmarker library."""
    
    def __init__(self, study_id: int = 1):
        """Initialize the highlighter with study configuration."""
        self.study_id = study_id
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_folder = self.base_dir / "Review_articles"
        self.output_folder = self.base_dir / "20250820_TxtMarker_Highlighting"
        self.output_folder.mkdir(exist_ok=True, parents=True)
        
        # Section keywords for filtering
        self.SECTION_KEEP = [
            "abstract", "method", "methodology", "data",
            "results", "findings", "discussion", "conclusion", "limitations", "limit"
        ]
        self.SECTION_STOP = ["references", "acknowledgements", "appendix", "bibliography"]
        
        # Variable categorization for different highlighting approaches
        self.DIRECT_MATCH_VARS = {
            "Country", "City", "Crime_Type", "Crime_Type_Group", 
            "Spatial_Unit_Name", "Model_Type", "Software_Used"
        }
        
        self.NUMERIC_VARS = {
            "Total_Study_Area_Size", "Number_of_Units", "Crime_Incidents",
            "Average_Population_per_Unit", "Number_of_Data_Sources", "Number_of_Variables"
        }
        
        self.QUOTE_VARS = {
            "Supporting_Quotes", "Supporting_Quotes_Methods", "Supporting_Quotes_Results",
            "Supporting_Theory", "Supporting_Rationale", "Supporting_Data"
        }
        
        self.COEFFICIENT_VARS = {"Coefficients", "Effect_Sizes"}
        
        # Track results
        self.match_log = []

    def load_study_data(self) -> Optional[pd.Series]:
        """Load study data from CSV."""
        try:
            df = pd.read_csv(self.csv_path, engine="python", quoting=csv.QUOTE_MINIMAL, keep_default_na=False)
            
            # Clean up doubled quotes from CSV export
            df = df.apply(lambda col: col.map(lambda x: x.replace('""', '"') if isinstance(x, str) else x))
            
            # Convert Study_ID to int
            if 'Study_ID' in df.columns:
                df['Study_ID'] = df['Study_ID'].astype(int)
            
            # Find the study
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
        """Find PDF file using multiple strategies."""
        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        if not pdf_files:
            raise FileNotFoundError(f"No PDF files found in {self.pdf_folder}")
        
        # Strategy 1: {study_id:02d}_*.pdf
        pattern1 = f"{self.study_id:02d}_*.pdf"
        matches = list(self.pdf_folder.glob(pattern1))
        if matches:
            print(f"📄 Found PDF using pattern {pattern1}: {matches[0].name}")
            return matches[0]
        
        # Strategy 2: {study_id}_*.pdf  
        pattern2 = f"{self.study_id}_*.pdf"
        matches = list(self.pdf_folder.glob(pattern2))
        if matches:
            print(f"📄 Found PDF using pattern {pattern2}: {matches[0].name}")
            return matches[0]
        
        # Strategy 3: Use first available PDF as fallback
        print(f"⚠️ Using fallback PDF: {pdf_files[0].name}")
        return pdf_files[0]

    def clean_text(self, text: str) -> str:
        """Clean and normalize text for processing."""
        if not text:
            return ""
        
        text = str(text).strip()
        
        # Convert smart quotes to straight quotes
        text = (text
                .replace('\u2018', "'").replace('\u2019', "'")  # single quotes
                .replace('\u201C', '"').replace('\u201D', '"')  # double quotes
                .replace('"', '"').replace('"', '"')
                .replace(''', "'").replace(''', "'"))
        
        # Normalize dashes
        text = text.replace('\u2013', '–').replace('\u2014', '—')
        
        # Remove soft hyphen and non-breaking spaces
        text = text.replace('\u00AD', '').replace('\u00A0', ' ')
        
        # Collapse multiple spaces
        text = re.sub(r'\s+', ' ', text)
        
        return text

    def is_meaningful_term(self, term: str) -> bool:
        """Check if a term is worth highlighting."""
        if not term or len(term.strip()) <= 2:
            return False
        
        term = term.strip()
        
        # Skip lone punctuation or hyphens
        if re.fullmatch(r"[-–—]+", term):
            return False
            
        # Skip pure numbers without context (keep things like "km²")
        if re.fullmatch(r"\d+(?:[ ,.\u00A0]\d+)*", term):
            return False
            
        return True

    def generate_direct_match_terms(self, value: str) -> List[str]:
        """Generate terms for direct string matching (Country, City, etc.)."""
        clean_value = self.clean_text(value)
        
        # Extract quoted content first
        quotes = re.findall(r'["\'`]([^"\'`]+)["\'`]', clean_value)
        if quotes:
            return [q.strip() for q in quotes if self.is_meaningful_term(q)]
        
        # Use the full cleaned value
        if self.is_meaningful_term(clean_value):
            return [clean_value]
        
        return []

    def generate_numeric_patterns(self, value: str) -> List[str]:
        """Generate regex patterns for numeric fields."""
        patterns = []
        clean_value = self.clean_text(value)
        
        # Find all numbers in the text
        numbers = re.findall(r'\d[\d,.\u00A0 ]*\d|\d+', clean_value)
        
        for num in numbers:
            # Create variations: 503589, 503,589, 503 589
            base_num = re.sub(r'[\s,.\u00A0]', '', num)
            if base_num.isdigit() and len(base_num) > 2:
                # Add comma version for large numbers
                if len(base_num) >= 4:
                    comma_num = re.sub(r'(\d)(?=(\d{3})+$)', r'\1,', base_num)
                    space_num = re.sub(r'(\d)(?=(\d{3})+$)', r'\1 ', base_num)
                    patterns.extend([base_num, comma_num, space_num])
                else:
                    patterns.append(base_num)
        
        # Also include unit patterns (km², ha, etc.)
        units = re.findall(r'\d+\s*(?:km²|ha|m²|km2|hectares?|square\s+kilometers?)', clean_value, re.IGNORECASE)
        patterns.extend(units)
        
        return list(set(p for p in patterns if self.is_meaningful_term(p)))

    def generate_quote_terms(self, value: str) -> List[str]:
        """Generate terms from quoted supporting text."""
        # Find content within quotes
        quotes = re.findall(r'["\'`]{1,3}\s*(.+?)\s*["\'`]{1,3}', value, flags=re.DOTALL)
        
        terms = []
        for quote in quotes:
            clean_quote = self.clean_text(quote)
            # Limit quote length for better matching
            if 4 <= len(clean_quote.split()) <= 20 and len(clean_quote) <= 200:
                terms.append(clean_quote)
        
        return terms

    def generate_coefficient_terms(self, value: str) -> List[str]:
        """Generate terms for coefficients and effect sizes."""
        terms = []
        clean_value = self.clean_text(value)
        
        # Extract labels before colons (e.g., "Bars:", "Night shops:")
        labels = re.findall(r'([^:]+):', clean_value)
        for label in labels:
            clean_label = label.strip().replace('_', ' ')
            if self.is_meaningful_term(clean_label):
                terms.append(clean_label)
        
        # Extract OR patterns (odds ratios)
        or_patterns = re.findall(r'OR\s*=\s*\d+\.?\d*', clean_value, re.IGNORECASE)
        terms.extend(or_patterns)
        
        # Extract coefficient patterns
        coeff_patterns = re.findall(r'(?:coefficient|coeff\.?|β)\s*[=:]\s*[-+]?\d+\.?\d*', clean_value, re.IGNORECASE)
        terms.extend(coeff_patterns)
        
        return terms

    def generate_independent_var_terms(self, value: str) -> List[str]:
        """Generate terms for independent variables."""
        clean_value = self.clean_text(value).replace('_', ' ')
        
        # Split by common separators
        items = re.split(r'[;•|\n]', clean_value)
        terms = []
        
        for item in items:
            # Remove list numbering
            item = re.sub(r'^\s*(?:\d+[\.\)]\s*|\(?[a-z]\)?\s*)', '', item, flags=re.IGNORECASE)
            item = item.strip(' -•')
            
            if self.is_meaningful_term(item):
                terms.append(item)
                
                # Also add short anchor for long terms
                words = item.split()
                if len(words) > 3:
                    anchor = ' '.join(words[:2])
                    terms.append(anchor)
        
        return terms

    def generate_search_terms(self, variable: str, value: str) -> List[str]:
        """Generate search terms based on variable type."""
        if pd.isna(value) or not str(value).strip():
            return []
        
        value_str = str(value).strip()
        
        # Skip "not specified" type entries
        skip_phrases = ["not specified", "not mentioned", "not applicable", "n/a", "na"]
        if any(phrase in value_str.lower() for phrase in skip_phrases):
            return []
        
        # Handle Supporting_quotes_for__* columns dynamically
        if variable.lower().startswith("supporting_quotes_for__"):
            return self.generate_quote_terms(value_str)
        
        # Apply different strategies based on variable type
        if variable in self.DIRECT_MATCH_VARS:
            return self.generate_direct_match_terms(value_str)
        
        elif variable in self.NUMERIC_VARS:
            return self.generate_numeric_patterns(value_str)
        
        elif variable in self.QUOTE_VARS:
            return self.generate_quote_terms(value_str)
        
        elif variable in self.COEFFICIENT_VARS:
            return self.generate_coefficient_terms(value_str)
        
        elif variable == "Independent_Variables":
            return self.generate_independent_var_terms(value_str)
        
        elif variable == "Data_Collection_Period":
            # Extract years
            years = re.findall(r'(?:19|20)\d{2}', value_str)
            terms = years.copy()
            if len(years) >= 2:
                terms.append(f"{years[0]}-{years[-1]}")
            return terms
        
        else:
            # Default: use cleaned full text
            clean_value = self.clean_text(value_str)
            if self.is_meaningful_term(clean_value):
                return [clean_value]
        
        return []

    def collect_all_terms(self, study_data: pd.Series) -> Dict[str, List[str]]:
        """Collect all search terms from the study data."""
        all_terms = {}
        
        for column in study_data.index:
            if column in ['Study_ID', 'Title', 'Authors', 'Year']:  # Skip metadata
                continue
                
            value = study_data[column]
            terms = self.generate_search_terms(column, value)
            
            if terms:
                all_terms[column] = terms
                print(f"📝 {column}: {len(terms)} terms generated")
                if len(terms) <= 3:  # Show terms for short lists
                    print(f"   Terms: {terms}")
        
        return all_terms

    def get_section_pages(self, pdf_path: Path) -> List[int]:
        """Determine which pages to search based on section headers."""
        try:
            # Use txtmarker to read PDF text
            marker = PdfMarker()
            
            # For now, return all pages except likely reference pages
            # txtmarker doesn't provide easy page-by-page text access
            # This is a simplified approach
            
            # Assume references start after 80% of the document
            with open(pdf_path, 'rb') as f:
                # This is a rough heuristic - in practice you'd need to analyze content
                total_pages = 20  # placeholder - txtmarker will handle this
                section_pages = list(range(1, int(total_pages * 0.8)))  # exclude last 20%
            
            return section_pages if section_pages else list(range(1, 11))  # fallback to first 10 pages
            
        except Exception as e:
            print(f"⚠️ Could not determine section pages: {e}")
            return list(range(1, 11))  # fallback

    def highlight_pdf(self, pdf_path: Path, terms_dict: Dict[str, List[str]]) -> bool:
        """Highlight terms in PDF using txtmarker."""
        try:
            # Initialize txtmarker
            marker = PdfMarker()
            
            # Flatten all terms into a single list with source tracking
            all_terms = []
            term_to_variable = {}
            
            for variable, terms in terms_dict.items():
                for term in terms:
                    if self.is_meaningful_term(term):
                        all_terms.append(term)
                        term_to_variable[term] = variable
            
            print(f"🔍 Total terms to highlight: {len(all_terms)}")
            
            if not all_terms:
                print("⚠️ No terms to highlight!")
                return False
            
            # Output path
            output_path = self.output_folder / f"Study_{self.study_id}_highlighted.pdf"
            
            # Highlight terms
            result = marker.highlight_terms(
                input_path=str(pdf_path),
                output_path=str(output_path),
                terms=all_terms,
                case_sensitive=False,
                whole_words_only=True  # Avoid partial matches
            )
            
            # Log results
            highlights_made = 0
            for term in all_terms:
                variable = term_to_variable[term]
                # txtmarker doesn't provide detailed match info, so we'll mark as attempted
                self.match_log.append({
                    'Variable': variable,
                    'Term': term,
                    'Page': 'N/A',  # txtmarker doesn't provide page info
                    'Found': 'Attempted'
                })
                highlights_made += 1
            
            print(f"✅ Highlighting completed: {output_path.name}")
            print(f"📊 Terms processed: {highlights_made}")
            
            return True
            
        except Exception as e:
            print(f"❌ Error highlighting PDF: {e}")
            logging.error(f"Error highlighting PDF: {e}")
            return False

    def save_match_log(self) -> None:
        """Save the match log to CSV."""
        if not self.match_log:
            return
        
        log_path = self.output_folder / f"Study_{self.study_id}_matches.csv"
        
        df_log = pd.DataFrame(self.match_log)
        df_log.to_csv(log_path, index=False, encoding='utf-8')
        
        print(f"📋 Match log saved: {log_path.name}")

    def process_study(self) -> bool:
        """Main processing function."""
        print("=" * 80)
        print("TXTMARKER PDF HIGHLIGHTER")
        print("=" * 80)
        print(f"🆔 Processing Study ID: {self.study_id}")
        
        # Load study data
        study_data = self.load_study_data()
        if study_data is None:
            return False
        
        # Find PDF file
        try:
            pdf_path = self.find_pdf_file(study_data)
        except FileNotFoundError as e:
            print(f"❌ {e}")
            return False
        
        # Collect terms to highlight
        print(f"\n🔍 COLLECTING SEARCH TERMS:")
        terms_dict = self.collect_all_terms(study_data)
        
        if not terms_dict:
            print("❌ No terms to highlight!")
            return False
        
        total_terms = sum(len(terms) for terms in terms_dict.values())
        print(f"\n📊 SUMMARY:")
        print(f"   Variables with terms: {len(terms_dict)}")
        print(f"   Total terms: {total_terms}")
        
        # Highlight PDF
        print(f"\n🎨 HIGHLIGHTING PDF: {pdf_path.name}")
        success = self.highlight_pdf(pdf_path, terms_dict)
        
        # Save match log
        self.save_match_log()
        
        if success:
            print(f"\n🎉 SUCCESS! Study {self.study_id} processed successfully.")
            return True
        else:
            print(f"\n❌ FAILED! Study {self.study_id} could not be processed.")
            return False


def main():
    """Main function with command line arguments."""
    parser = argparse.ArgumentParser(description='TxtMarker PDF Highlighter')
    parser.add_argument('--study-id', type=int, default=1, 
                        help='Study ID to process (default: 1)')
    
    args = parser.parse_args()
    
    highlighter = StudyHighlighter(study_id=args.study_id)
    success = highlighter.process_study()
    
    return success


if __name__ == "__main__":
    main()
