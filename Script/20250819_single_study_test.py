#!/usr/bin/env python3
"""
Targeted PDF Highlighting - Single Study Test
=============================================

This script highlights the exact extracted values from one specific study
in its corresponding PDF file.

Usage:
    python 20250819_single_study_test.py
"""

import pandas as pd
import fitz  # PyMuPDF
import re
import logging
from pathlib import Path
from rapidfuzz import fuzz
from typing import List, Dict, Tuple, Optional
import json
from datetime import datetime

# Configure logging
current_date = datetime.now().strftime("%Y%m%d")
log_file = f"{current_date}_single_study_test.log"

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)


class SingleStudyHighlighter:
    """Test highlighting for a single study."""

    def __init__(self):
        self.current_date = datetime.now().strftime("%Y%m%d")
        self.output_folder = Path(f"../{self.current_date}_Single_Study_Test")
        self.csv_file = "../20250819_Analysis & Results/20250819_combined_dataset.csv"
        self.pdf_folder = Path("../Review_articles")
        self.df = None

        # Color scheme for different variables
        self.variable_colors = {
            # Geographic variables (Yellow shades)
            'Country': (1.0, 1.0, 0.0),                    # Yellow
            'City': (1.0, 1.0, 0.3),                      # Light Yellow
            'Total_Study_Area_Size': (0.8, 0.8, 0.0),     # Dark Yellow

            # Spatial Unit variables (Cyan shades)
            'Spatial_Unit_Name': (0.0, 1.0, 1.0),         # Cyan
            'Unit_Size': (0.3, 1.0, 1.0),                 # Light Cyan
            'Number_of_Units': (0.0, 0.8, 0.8),           # Dark Cyan
            'Average_Population_per_Unit': (0.2, 0.9, 0.9),  # Medium Cyan
            'Spatial_Aggregation': (0.1, 0.7, 0.7),       # Darker Cyan

            # Unit Selection variables (Green shades)
            'Unit_Selection_Rationale': (0.0, 1.0, 0.0),  # Green
            'Rationale_Category': (0.3, 1.0, 0.3),        # Light Green
            'Data_Limitations': (0.0, 0.8, 0.0),          # Dark Green
            'Computational_Constraints': (0.2, 0.9, 0.2),  # Medium Green
            'Alternative_Units': (0.1, 0.7, 0.1),         # Darker Green

            # Data Collection variables (Orange shades)
            'Data_Collection_Period': (1.0, 0.5, 0.0),    # Orange
            'Data_Sources': (1.0, 0.6, 0.2),              # Light Orange
            'Number_of_Data_Sources': (0.8, 0.4, 0.0),    # Dark Orange

            # Crime variables (Magenta shades)
            'Crime_Type': (1.0, 0.0, 1.0),                # Magenta
            'Crime_Type_Group': (1.0, 0.3, 1.0),          # Light Magenta
            'Crime_Incidents': (0.8, 0.0, 0.8),           # Dark Magenta

            # Model & Analysis variables (Red shades)
            'Model_Type': (1.0, 0.0, 0.0),                # Red
            'Independent_Variables': (1.0, 0.2, 0.2),      # Light Red
            'Number_of_Variables': (0.8, 0.0, 0.0),       # Dark Red
            'Model_Fit_Statistics': (1.0, 0.1, 0.1),      # Medium Red
            'Coefficients': (0.9, 0.0, 0.0),              # Darker Red
            'Confidence_Intervals': (1.0, 0.3, 0.3),      # Lighter Red
            'Effect_Sizes': (0.7, 0.0, 0.0),              # Very Dark Red
            'Software_Used': (1.0, 0.4, 0.4),             # Very Light Red

            # Methodology variables (Purple shades)
            'MAUP_Discussion': (0.5, 0.0, 1.0),           # Purple
            'Sensitivity_Analysis': (0.6, 0.2, 1.0),      # Light Purple
            'Future_Suggestions': (0.4, 0.0, 0.8)         # Dark Purple
        }

        # List of all variables to extract
        self.target_variables = [
            'Country', 'City', 'Total_Study_Area_Size',
            'Spatial_Unit_Name', 'Unit_Size', 'Number_of_Units',
            'Average_Population_per_Unit', 'Spatial_Aggregation',
            'Unit_Selection_Rationale', 'Rationale_Category',
            'Data_Limitations', 'Computational_Constraints', 'Alternative_Units',
            'Data_Collection_Period', 'Data_Sources', 'Number_of_Data_Sources',
            'Crime_Type', 'Crime_Type_Group', 'Crime_Incidents',
            'Model_Type', 'Independent_Variables', 'Number_of_Variables',
            'Model_Fit_Statistics', 'Coefficients', 'Confidence_Intervals',
            'Effect_Sizes', 'Software_Used',
            'MAUP_Discussion', 'Sensitivity_Analysis', 'Future_Suggestions'
        ]        # Create output folder
        self.output_folder.mkdir(exist_ok=True)

    def create_text_variants(self, text: str) -> List[str]:
        """Create multiple variants of a text to handle formatting differences."""
        variants = [text]  # Original text

        # Remove punctuation and special characters
        cleaned = re.sub(r'[^\w\s]', '', text)
        if cleaned != text and cleaned.strip():
            variants.append(cleaned)

        # Remove commas from numbers
        no_commas = re.sub(r'(\d+),(\d+)', r'\1\2', text)
        if no_commas != text:
            variants.append(no_commas)

        # Handle area measurements: km² -> km2, m² -> m2
        area_variants = text.replace('²', '2').replace('³', '3')
        if area_variants != text:
            variants.append(area_variants)
            # Also add version without commas
            area_no_commas = re.sub(r'(\d+),(\d+)', r'\1\2', area_variants)
            if area_no_commas != area_variants:
                variants.append(area_no_commas)

        # Handle common symbol replacements
        symbol_replacements = {
            '–': '-', '—': '-', ''': "'", ''': "'", '"': '"', '"': '"',
            '°': 'degrees', '%': 'percent', '&': 'and'
        }
        for old, new in symbol_replacements.items():
            if old in text:
                variants.append(text.replace(old, new))

        # Extract key numbers and words
        numbers = re.findall(r'\d+', text)
        for num in numbers:
            if len(num) > 1:  # Skip single digits
                variants.append(num)

        # Extract quoted strings
        quotes = re.findall(r'"([^"]*)"', text)
        variants.extend(quotes)

        # Split on common delimiters and add meaningful parts
        parts = re.split(r'[,;:|&\-\n\r]', text)
        for part in parts:
            part = part.strip()
            if len(part) > 3:  # Only meaningful parts
                variants.append(part)

        # Handle parenthetical information
        parenthetical = re.findall(r'\(([^)]+)\)', text)
        for paren in parenthetical:
            if len(paren.strip()) > 2:
                variants.append(paren.strip())

        # Remove duplicates while preserving order
        seen = set()
        unique_variants = []
        for variant in variants:
            variant_lower = variant.lower().strip()
            if variant_lower and variant_lower not in seen:
                seen.add(variant_lower)
                unique_variants.append(variant.strip())

        return unique_variants

    def extract_key_terms(self, text: str) -> List[str]:
        """Extract key terms from longer text."""
        # For very long texts, extract key phrases
        if len(text) > 200:
            # Extract sentences or key phrases
            sentences = re.split(r'[.!?]', text)
            key_terms = []
            for sentence in sentences:
                sentence = sentence.strip()
                if len(sentence) > 10:  # Meaningful sentences
                    key_terms.append(sentence)
            return key_terms
        return [text]

    def load_data(self) -> bool:
        """Load the CSV data."""
        try:
            logger.info(f"Loading data from {self.csv_file}")
            self.df = pd.read_csv(self.csv_file)
            logger.info(f"Loaded {len(self.df)} studies from CSV")
            return True
        except Exception as e:
            logger.error(f"Error loading CSV file: {e}")
            return False

    def find_pdf_for_study(self, study_row: pd.Series) -> Optional[Path]:
        """Find the PDF file that matches this study."""
        title = str(study_row.get('Title', '')).lower()
        study_id = str(study_row.get('Study_ID', ''))

        # Look for PDF files
        pdf_files = list(self.pdf_folder.glob("*.pdf"))

        if not pdf_files:
            logger.error(f"No PDF files found in {self.pdf_folder}")
            return None

        best_match = None
        best_score = 0

        for pdf_file in pdf_files:
            filename = pdf_file.stem.lower()

            # Try matching with Study_ID (e.g., "01_", "02_")
            if study_id and filename.startswith(study_id.zfill(2) + "_"):
                logger.info(f"Found exact Study_ID match: {pdf_file.name}")
                return pdf_file

            # Try matching with title
            if title:
                # Clean title for matching
                clean_title = re.sub(r'[^\w\s]', '', title.lower())
                title_words = clean_title.split()[:5]  # First 5 words

                score = 0
                for word in title_words:
                    if len(word) > 3 and word in filename:
                        score += len(word)

                if score > best_score:
                    best_score = score
                    best_match = pdf_file

        if best_match:
            logger.info(
                f"Found title match: {best_match.name} (score: {best_score})")
        else:
            logger.warning(
                f"No PDF match found for Study_ID: {study_id}, Title: {title[:50]}...")

        return best_match

    def normalize_text_for_search(self, text: str) -> str:
        """Normalize text for better matching by removing formatting differences."""
        if not text:
            return ""

        # Remove extra whitespace and normalize
        text = re.sub(r'\s+', ' ', text.strip())

        # Remove common punctuation that might differ
        text = re.sub(r'[,\(\)\[\]{}]', '', text)

        # Handle common number formatting
        # Remove commas in numbers: 3,000 -> 3000
        text = re.sub(r'(\d),(\d)', r'\1\2', text)

        # Normalize quotes and special characters
        text = text.replace('"', '').replace('"', '').replace('"', '')
        text = text.replace(''', "'").replace(''', "'")

        return text.lower()

    def create_search_variants(self, term: str) -> List[str]:
        """Create multiple variants of a term for better matching."""
        variants = [term]  # Original term

        # Add normalized version
        normalized = self.normalize_text_for_search(term)
        if normalized != term.lower():
            variants.append(normalized)

        # For numbers, try with and without commas
        if re.search(r'\d+,\d+', term):
            no_comma = re.sub(r'(\d+),(\d+)', r'\1\2', term)
            variants.append(no_comma)

        # Enhanced text processing for better matching
        clean_term = term.strip()

        # Add case variations
        variants.extend(
            [clean_term.lower(), clean_term.upper(), clean_term.title()])

        # Remove punctuation variants
        # Keep degree symbols
        no_punct = re.sub(r'[^\w\s°²³]', ' ', clean_term)
        no_punct = re.sub(r'\s+', ' ', no_punct).strip()
        if no_punct != clean_term:
            variants.extend([no_punct, no_punct.lower()])

        # Handle special formats
        # Years ranges: 2006-2012 -> also try 2006–2012 and individual years
        if re.search(r'\b\d{4}[-–]\d{4}\b', clean_term):
            variants.append(clean_term.replace('-', '–'))
            variants.append(clean_term.replace('–', '-'))
            years = re.findall(r'\b\d{4}\b', clean_term)
            variants.extend(years)

        # Extract and add individual numbers
        numbers = re.findall(r'\b\d{1,3}(?:,\d{3})*(?:\.\d+)?\b', clean_term)
        for num in numbers:
            variants.append(num)
            # Remove commas from large numbers
            variants.append(num.replace(',', ''))

        # Extract specific statistical values (R², pseudo R², etc.)
        stat_matches = re.findall(
            r'(?:Pseudo\s+)?R[²2]?\s*[=:]\s*[\d.]+', clean_term, re.IGNORECASE)
        variants.extend(stat_matches)

        # Extract values in parentheses
        paren_content = re.findall(r'\(([^)]+)\)', clean_term)
        for content in paren_content:
            if len(content.strip()) > 2:
                variants.extend([content.strip(), content.strip().lower()])

        # For long descriptive terms, create more variants
        if len(clean_term) > 50:
            # Extract key phrases (words in parentheses, before colons, etc.)
            key_parts = re.findall(
                r'([A-Z][a-z]+(?:\s+[A-Z]?[a-z]+)*)', clean_term)
            variants.extend(key_parts)

            # Split into meaningful chunks
            sentences = re.split(r'[.!?]+', clean_term)
            for sentence in sentences:
                sentence = sentence.strip()
                if len(sentence) > 10:
                    variants.extend([sentence, sentence.lower()])

                    # Add first part of long sentences
                    words = sentence.split()
                    if len(words) > 6:
                        first_part = ' '.join(words[:6])
                        variants.extend([first_part, first_part.lower()])

        # For "Not specified" type entries, add common alternatives
        lower_term = clean_term.lower()
        if any(phrase in lower_term for phrase in ['not specified', 'not mentioned', 'not provided']):
            variants.extend(['not specified', 'not mentioned', 'not provided',
                             'not available', 'n/a', 'na', 'not discussed', 'not stated'])

        # Extract software versions (e.g., "R version 3.0.2")
        software_match = re.search(
            r'([A-Za-z]+)\s+version\s+([\d.]+)', clean_term, re.IGNORECASE)
        if software_match:
            software, version = software_match.groups()
            variants.extend([f"{software} {version}", f"{software.lower()} {version}",
                             f"{software} version {version}", version])

        # Handle area measurements (km², m², etc.)
        area_match = re.search(
            r'(\d+(?:,\d+)?)\s*(km²|m²|hectares?)', clean_term, re.IGNORECASE)
        if area_match:
            number, unit = area_match.groups()
            variants.extend([f"{number}{unit}", f"{number.replace(',', '')}{unit}",
                             f"{number} {unit}", number, number.replace(',', '')])

        # Remove duplicates while preserving order
        seen = set()
        unique_variants = []
        for variant in variants:
            variant_clean = variant.strip()
            if variant_clean and variant_clean not in seen and len(variant_clean) > 2:
                seen.add(variant_clean)
                unique_variants.append(variant_clean)

        return unique_variants

    def extract_study_terms(self, study_row: pd.Series) -> Dict[str, List[str]]:
        """Extract the actual values from this study's row, organized by variable."""
        terms_by_variable = {var: [] for var in self.target_variables}

        for variable in self.target_variables:
            if variable in study_row.index:
                value = study_row[variable]

                # Skip empty/null values
                if pd.isna(value) or str(value).strip() in ['-', 'N/A', 'n/a', 'NA', 'nan', '']:
                    continue

                # Clean and process the value
                clean_value = str(value).strip()

                # For short, simple values (like country, city, numbers), use directly
                if len(clean_value) <= 50 and not any(delim in clean_value for delim in [';', '|', '&', '\n', '\r']):
                    # Create search variants for better matching
                    variants = self.create_search_variants(clean_value)
                    terms_by_variable[variable].extend(variants)
                else:
                    # For longer text, split on delimiters first
                    terms = re.split(r'[;,\|&/\n\r]', clean_value)

                    for term in terms:
                        term = term.strip()
                        if len(term) > 2:
                            # Create variants for each term
                            variants = self.create_search_variants(term)
                            terms_by_variable[variable].extend(variants)

        # Remove duplicates from each variable while preserving order
        for variable in terms_by_variable:
            seen = set()
            unique_terms = []
            for term in terms_by_variable[variable]:
                if term not in seen:
                    seen.add(term)
                    unique_terms.append(term)
            terms_by_variable[variable] = unique_terms

        # Log what we found
        total_terms = sum(len(terms) for terms in terms_by_variable.values())
        logger.info(f"Extracted {total_terms} terms from study:")
        for variable, terms in terms_by_variable.items():
            if terms:
                logger.info(f"  {variable}: {len(terms)} variants - {terms[:3]}..." if len(
                    terms) > 3 else f"  {variable}: {terms}")

        # Additional debug: log variables with NO terms
        empty_vars = [var for var,
                      terms in terms_by_variable.items() if not terms]
        if empty_vars:
            logger.info(f"Variables with NO terms: {empty_vars}")

        return terms_by_variable

    def highlight_terms_in_pdf(self, pdf_path: Path, study_row: pd.Series) -> Dict:
        """Highlight the study's specific terms in its PDF."""
        result = {
            'study_id': study_row.get('Study_ID', ''),
            'title': study_row.get('Title', ''),
            'pdf_file': pdf_path.name,
            'output_file': '',
            'terms_found': {},
            'total_highlights': 0,
            'error': None
        }

        try:
            # Extract terms for this specific study
            terms_by_variable = self.extract_study_terms(study_row)

            if not any(terms_by_variable.values()):
                result['error'] = 'No terms to highlight'
                return result

            # Open PDF
            doc = fitz.open(pdf_path)
            output_path = self.output_folder / \
                f"{pdf_path.stem}_highlighted_test.pdf"

            total_highlights = 0
            terms_found = {var: [] for var in self.target_variables}

            logger.info(f"Processing {pdf_path.name} ({len(doc)} pages)")

            # Keep track of highlighted terms to ensure first-instance only
            highlighted_terms = set()

            # Process each page
            for page_num in range(len(doc)):
                page = doc[page_num]

                # Search for each variable's terms
                for variable, terms in terms_by_variable.items():
                    color = self.variable_colors[variable]

                    for term in terms:
                        # Skip if we already highlighted this term
                        term_key = term.lower().strip()
                        if term_key in highlighted_terms:
                            continue

                        # Strategy 1: Direct search (case insensitive)
                        text_instances = page.search_for(term)

                        # Strategy 2: Try different case variations
                        if not text_instances:
                            for variation in [term.lower(), term.upper(), term.title()]:
                                if variation != term:
                                    text_instances = page.search_for(variation)
                                    if text_instances:
                                        break

                        # Strategy 3: For numbers, try without commas
                        if not text_instances and re.search(r'\d+,\d+', term):
                            no_comma = term.replace(',', '')
                            text_instances = page.search_for(no_comma)

                        # Strategy 4: For long terms, try progressive partial matching
                        if not text_instances and len(term) > 15:
                            words = term.split()
                            if len(words) > 3:
                                # Try different word combinations
                                for i in range(len(words) - 2, 0, -1):  # Work backwards
                                    partial_term = ' '.join(words[:i])
                                    if len(partial_term) > 8:  # Only meaningful lengths
                                        text_instances = page.search_for(
                                            partial_term)
                                        if text_instances:
                                            break

                                # Also try last few words for descriptive text
                                if not text_instances and len(words) > 4:
                                    partial_term = ' '.join(words[-3:])
                                    if len(partial_term) > 8:
                                        text_instances = page.search_for(
                                            partial_term)

                        # Strategy 5: Extract key identifiers (numbers, years, etc.)
                        if not text_instances:
                            # Look for specific numbers in the term
                            # 3+ digit numbers
                            numbers = re.findall(r'\b\d{3,}\b', term)
                            for number in numbers:
                                text_instances = page.search_for(number)
                                if text_instances:
                                    break

                            # Look for year ranges
                            if not text_instances:
                                years = re.findall(r'\b\d{4}\b', term)
                                for year in years:
                                    text_instances = page.search_for(year)
                                    if text_instances:
                                        break

                        # Strategy 6: For statistical terms, try different formats
                        if not text_instances and re.search(r'[Rr]²?\s*[=:]\s*[\d.]+', term):
                            # Extract just the R² value
                            r_squared = re.findall(r'[\d.]+', term)
                            if r_squared:
                                text_instances = page.search_for(r_squared[0])

                        # Strategy 7: Try with normalized text
                        if not text_instances:
                            normalized_term = self.normalize_text_for_search(
                                term)
                            if len(normalized_term) > 3 and normalized_term != term.lower():
                                text_instances = page.search_for(
                                    normalized_term)

                        # Strategy 8: For "not specified" type entries, try alternatives
                        if not text_instances and 'not specified' in term.lower():
                            alternatives = ['not mentioned', 'not provided', 'not available',
                                            'not discussed', 'not stated', 'n/a', 'na']
                            for alt in alternatives:
                                text_instances = page.search_for(alt)
                                if text_instances:
                                    break

                        if text_instances:
                            # Highlight only the first instance
                            first_instance = text_instances[0]

                            try:
                                highlight = page.add_highlight_annot(
                                    first_instance)
                                highlight.set_colors(stroke=color)
                                highlight.set_info(
                                    title=f"{variable}",
                                    content=f"Variable: {variable} | Term: {term} | Page: {page_num + 1}"
                                )
                                highlight.update()

                                total_highlights += 1
                                terms_found[variable].append({
                                    'term': term,
                                    'page': page_num + 1,
                                    'instances_found': len(text_instances)
                                })
                                highlighted_terms.add(term_key)

                                logger.info(
                                    f"✓ Highlighted '{term}' ({variable}) on page {page_num + 1}")

                            except Exception as e:
                                logger.debug(
                                    f"Failed to highlight '{term}' on page {page_num + 1}: {e}")
                        else:
                            # Term not found in this page, continue to next page
                            continue

                # After processing all pages, log terms that were never found
                for variable, terms in terms_by_variable.items():
                    for term in terms:
                        term_key = term.lower().strip()
                        if term_key not in highlighted_terms:
                            logger.debug(
                                f"✗ '{term}' ({variable}) not found in any page")

            # Save highlighted PDF
            doc.save(output_path, garbage=1, clean=True)
            doc.close()

            result['output_file'] = output_path.name
            result['terms_found'] = terms_found
            result['total_highlights'] = total_highlights

            logger.info(
                f"✓ Successfully highlighted {total_highlights} terms in {pdf_path.name}")
            logger.info(f"✓ Saved as: {output_path.name}")

        except Exception as e:
            logger.error(f"Error processing {pdf_path.name}: {e}")
            result['error'] = str(e)

        return result

    def test_single_study(self, study_index: int = 0):
        """Test highlighting for a single study (default: first study)."""
        logger.info("=== SINGLE STUDY HIGHLIGHTING TEST ===")

        # Load data
        if not self.load_data():
            return None

        if study_index >= len(self.df):
            logger.error(
                f"Study index {study_index} is out of range (max: {len(self.df)-1})")
            return None

        # Get the study
        study_row = self.df.iloc[study_index]
        study_id = study_row.get('Study_ID', study_index)
        title = study_row.get('Title', 'Unknown')

        logger.info(f"Testing Study {study_id}: {title[:50]}...")

        # Find matching PDF
        pdf_path = self.find_pdf_for_study(study_row)
        if not pdf_path:
            logger.error("No matching PDF found")
            return None

        # Highlight the PDF
        result = self.highlight_terms_in_pdf(pdf_path, study_row)

        # Print summary
        print("\n" + "="*60)
        print("SINGLE STUDY TEST RESULTS")
        print("="*60)
        print(f"Study ID: {result['study_id']}")
        print(f"Title: {result['title'][:50]}...")
        print(f"PDF File: {result['pdf_file']}")
        print(f"Output File: {result['output_file']}")
        print(f"Total Highlights: {result['total_highlights']}")

        if result['error']:
            print(f"Error: {result['error']}")
        else:
            print("\nTerms Found by Variable:")
            for variable, terms in result['terms_found'].items():
                if terms:
                    print(f"  {variable}:")
                    for term_info in terms:
                        print(
                            f"    - '{term_info['term']}' on page {term_info['page']} ({term_info['instances_found']} instances)")

        print("="*60)

        return result


def main():
    """Main function to test single study highlighting."""
    highlighter = SingleStudyHighlighter()

    # Test with the first study (index 0)
    # You can change this to test different studies
    result = highlighter.test_single_study(study_index=0)

    if result:
        print(
            f"\n✓ Test completed! Check the output folder: {highlighter.output_folder}")


if __name__ == "__main__":
    main()
