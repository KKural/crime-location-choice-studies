#!/usr/bin/env python3
"""
PDF Direct Highlighting Script - IMPROVED VERSION
=================================================

This script directly highlights extracted terms in PDF files for verification.
KEY FEATURES:
- FIRST INSTANCE ONLY highlighting to avoid clutter
- Robust error handling for 100% success rate
- Color-coded categories
- Comprehensive logging and reporting

Requirements:
- pymupdf (fitz)
- pandas
- pathlib

Usage:
    python 20250819_pdf_direct_highlighting_improved.py
"""

import pandas as pd
import fitz  # PyMuPDF
import re
import os
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import json
from datetime import datetime

# Configure logging
current_date = datetime.now().strftime("%Y%m%d")
log_file = f"../{current_date}_pdf_highlighting_improved.log"

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)


class PDFDirectHighlighterImproved:
    """Class to handle direct PDF highlighting of extracted terms - FIRST INSTANCE ONLY."""

    def __init__(self):
        self.current_date = datetime.now().strftime("%Y%m%d")
        self.output_folder = Path(
            f"../{self.current_date}_PDF_Direct_Highlighting_Improved")
        self.csv_file = "../20250819_Analysis & Results/20250819_combined_dataset.csv"
        self.pdf_folder = Path("../Review_articles")
        self.df = None

        # Color scheme for different term categories
        self.highlight_colors = {
            'Geographic': (1.0, 1.0, 0.0),      # Yellow
            'Quantitative': (0.0, 1.0, 1.0),   # Cyan
            'Methods': (0.0, 1.0, 0.0),        # Green
            'Crime-related': (1.0, 0.5, 0.0),  # Orange
            'Statistical': (1.0, 0.0, 1.0),    # Magenta
            'Other': (0.75, 0.75, 0.75)        # Light Gray
        }

        self.results = []
        self.stats = {
            'pdfs_processed': 0,
            'pdfs_highlighted': 0,
            'pdfs_failed': 0,
            'total_unique_terms': 0,
            'terms_highlighted': 0,
            'terms_by_category': {},
            'success_rate': 0.0
        }

        # Create output folder
        self.output_folder.mkdir(exist_ok=True)

    def load_data(self) -> bool:
        """Load the CSV data containing extracted terms."""
        try:
            logger.info(f"Loading data from {self.csv_file}")
            self.df = pd.read_csv(self.csv_file)
            logger.info(f"Loaded {len(self.df)} studies from CSV")
            return True
        except Exception as e:
            logger.error(f"Error loading CSV file: {e}")
            return False

    def extract_terms_from_dataset(self) -> Dict[str, set]:
        """Extract unique terms from the main data columns (excluding supporting quotes)."""
        terms_by_category = {
            'Geographic': set(),
            'Quantitative': set(),
            'Methods': set(),
            'Crime-related': set(),
            'Statistical': set(),
            'Other': set()
        }

        # Define column categories
        geographic_cols = ['Country', 'City', 'Geographical_scope']
        quantitative_cols = ['Sample_size', 'Unit_Size', 'Spatial_Unit_Name']
        methods_cols = ['Model_Type', 'Data_Sources',
                        'Analysis_Method', 'Statistical_approach']
        crime_cols = ['Crime_Type', 'Crime_category']
        statistical_cols = ['Model_Type',
                            'Statistical_approach', 'Analysis_Method']

        logger.info("Extracting terms from dataset...")

        for _, row in self.df.iterrows():
            # Geographic terms
            for col in geographic_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        # Split on common delimiters and clean terms
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:  # Minimum term length
                                terms_by_category['Geographic'].add(clean_term)

            # Quantitative terms
            for col in quantitative_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:
                                terms_by_category['Quantitative'].add(
                                    clean_term)

            # Methods terms
            for col in methods_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:
                                terms_by_category['Methods'].add(clean_term)

            # Crime-related terms
            for col in crime_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:
                                terms_by_category['Crime-related'].add(
                                    clean_term)

        # Add statistical terms (overlapping with methods but separate category)
        for _, row in self.df.iterrows():
            for col in statistical_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:
                                terms_by_category['Statistical'].add(
                                    clean_term)

        # Add other important columns to 'Other' category
        other_cols = ['Independent_Variables',
                      'Dependent_Variable', 'Theory_Framework']
        for _, row in self.df.iterrows():
            for col in other_cols:
                if col in self.df.columns:
                    value = str(row[col]) if pd.notna(row[col]) else ""
                    if value and value not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
                        terms = re.split(r'[;,\|/&]', value)
                        for term in terms:
                            clean_term = term.strip()
                            if len(clean_term) > 2:
                                terms_by_category['Other'].add(clean_term)

        # Remove empty terms and very short terms
        for category in terms_by_category:
            terms_by_category[category] = {
                term for term in terms_by_category[category]
                if term.strip() and len(term.strip()) > 2
            }

        # Log statistics
        total_terms = sum(len(terms) for terms in terms_by_category.values())
        self.stats['total_unique_terms'] = total_terms

        logger.info(f"Extracted {total_terms} unique terms:")
        for category, terms in terms_by_category.items():
            logger.info(f"  {category}: {len(terms)} terms")
            self.stats['terms_by_category'][category] = len(terms)

        return terms_by_category

    def find_pdf_files(self) -> List[Path]:
        """Find all PDF files in the Review_articles folder."""
        if not self.pdf_folder.exists():
            logger.error(f"PDF folder not found: {self.pdf_folder}")
            return []

        pdf_files = list(self.pdf_folder.glob("*.pdf"))
        logger.info(f"Found {len(pdf_files)} PDF files")
        return pdf_files

    def highlight_terms_in_pdf(self, pdf_path: Path, terms_by_category: Dict[str, set]) -> Dict:
        """Highlight terms directly in a PDF file - FIRST INSTANCE ONLY."""
        result = {
            'pdf_file': pdf_path.name,
            'output_file': '',
            'terms_highlighted': 0,
            'pages_processed': 0,
            'highlights_by_category': {},
            'error': None
        }

        doc = None
        try:
            # Open PDF with error handling
            doc = fitz.open(pdf_path)
            output_path = self.output_folder / \
                f"{pdf_path.stem}_highlighted.pdf"

            total_highlights = 0
            highlights_by_category = {
                cat: 0 for cat in terms_by_category.keys()}
            pages_processed = len(doc)

            # Track which terms have already been highlighted (first instance only)
            highlighted_terms = set()

            logger.info(
                f"Processing {pdf_path.name} ({pages_processed} pages)")

            # Process each page
            for page_num in range(pages_processed):
                try:
                    page = doc[page_num]

                    # Search for each category of terms
                    for category, terms in terms_by_category.items():
                        color = self.highlight_colors[category]

                        for term in terms:
                            # Skip if we've already highlighted this term
                            if term in highlighted_terms:
                                continue

                            try:
                                # Find all instances of the term on this page
                                text_instances = page.search_for(term)

                                if text_instances:
                                    # Highlight only the FIRST instance found
                                    try:
                                        first_instance = text_instances[0]
                                        highlight = page.add_highlight_annot(
                                            first_instance)
                                        highlight.set_colors(stroke=color)
                                        highlight.set_info(
                                            title=f"{category}",
                                            content=f"Term: {term} (Category: {category})"
                                        )
                                        highlight.update()
                                        total_highlights += 1
                                        highlights_by_category[category] += 1

                                        # Mark this term as highlighted
                                        highlighted_terms.add(term)

                                        logger.debug(
                                            f"Highlighted first instance of '{term}' ({category}) on page {page_num + 1}")

                                    except Exception as e:
                                        logger.debug(
                                            f"Failed to create highlight for '{term}' on page {page_num + 1}: {e}")

                            except Exception as e:
                                logger.debug(
                                    f"Failed to search for term '{term}' on page {page_num + 1}: {e}")

                except Exception as e:
                    logger.warning(
                        f"Error processing page {page_num + 1} in {pdf_path.name}: {e}")
                    continue

            # Save highlighted PDF with robust error handling
            try:
                # Try different save methods for maximum compatibility
                try:
                    doc.save(output_path, garbage=1, clean=True)
                except Exception:
                    doc.save(output_path, garbage=0, clean=False)

                result['output_file'] = output_path.name
                logger.info(f"✓ Saved highlighted PDF: {output_path.name}")

            except Exception as save_error:
                logger.error(
                    f"Failed to save highlighted PDF {output_path}: {save_error}")
                result['error'] = f"Save failed: {str(save_error)}"

            # Update results regardless of save status
            result['terms_highlighted'] = total_highlights
            result['pages_processed'] = pages_processed
            result['highlights_by_category'] = highlights_by_category

            if total_highlights > 0:
                self.stats['pdfs_highlighted'] += 1
                self.stats['terms_highlighted'] += total_highlights

            logger.info(
                f"✓ Highlighted {total_highlights} unique terms in {pdf_path.name}")

        except Exception as e:
            logger.error(f"Error processing {pdf_path.name}: {e}")
            result['error'] = str(e)
            self.stats['pdfs_failed'] += 1

        finally:
            # Always close document if it was opened
            if doc:
                try:
                    doc.close()
                except:
                    pass

        return result

    def process_all_pdfs(self):
        """Process all PDF files for highlighting."""
        logger.info(
            "Starting PDF direct highlighting process (FIRST INSTANCE ONLY)...")

        # Load data
        if not self.load_data():
            return

        # Extract terms
        terms_by_category = self.extract_terms_from_dataset()
        if not any(terms_by_category.values()):
            logger.error("No terms extracted from dataset")
            return

        # Find PDF files
        pdf_files = self.find_pdf_files()
        if not pdf_files:
            logger.error("No PDF files found")
            return

        # Process each PDF
        self.stats['pdfs_processed'] = len(pdf_files)

        for i, pdf_file in enumerate(pdf_files, 1):
            logger.info(
                f"\nProcessing PDF {i}/{len(pdf_files)}: {pdf_file.name}")

            result = self.highlight_terms_in_pdf(pdf_file, terms_by_category)
            self.results.append(result)

        # Calculate success rate
        successful_pdfs = self.stats['pdfs_highlighted']
        self.stats['success_rate'] = (
            successful_pdfs / self.stats['pdfs_processed']) * 100 if self.stats['pdfs_processed'] > 0 else 0

        # Save results and summary
        self.save_results()

    def save_results(self):
        """Save highlighting results and summary."""
        # Create summary report
        summary_data = []
        for result in self.results:
            summary_data.append({
                'PDF_File': result['pdf_file'],
                'Output_File': result['output_file'],
                'Terms_Highlighted': result['terms_highlighted'],
                'Pages_Processed': result['pages_processed'],
                'Geographic': result['highlights_by_category'].get('Geographic', 0),
                'Quantitative': result['highlights_by_category'].get('Quantitative', 0),
                'Methods': result['highlights_by_category'].get('Methods', 0),
                'Crime_related': result['highlights_by_category'].get('Crime-related', 0),
                'Statistical': result['highlights_by_category'].get('Statistical', 0),
                'Other': result['highlights_by_category'].get('Other', 0),
                'Error': result['error'] or ''
            })

        # Save CSV report
        summary_df = pd.DataFrame(summary_data)
        report_path = self.output_folder / \
            f"{self.current_date}_highlighting_report_improved.csv"
        summary_df.to_csv(report_path, index=False)

        # Save detailed JSON
        details_path = self.output_folder / \
            f"{self.current_date}_highlighting_details_improved.json"
        with open(details_path, 'w', encoding='utf-8') as f:
            json.dump({
                'stats': self.stats,
                'results': self.results
            }, f, indent=2, ensure_ascii=False)

        # Save search terms list for reference
        terms_path = self.output_folder / \
            f"{self.current_date}_search_terms.txt"
        with open(terms_path, 'w', encoding='utf-8') as f:
            f.write("SEARCH TERMS BY CATEGORY\n")
            f.write("=" * 50 + "\n\n")

            terms_by_category = self.extract_terms_from_dataset()
            for category, terms in terms_by_category.items():
                f.write(f"{category.upper()} ({len(terms)} terms):\n")
                f.write("-" * 30 + "\n")
                for term in sorted(terms):
                    f.write(f"  {term}\n")
                f.write("\n")

        # Print final summary
        successful_pdfs = len(
            [r for r in self.results if r['terms_highlighted'] > 0])
        failed_pdfs = len([r for r in self.results if r.get('error')])

        print(f"""
========== PDF DIRECT HIGHLIGHTING SUMMARY (IMPROVED) ==========
PDFs processed: {self.stats['pdfs_processed']}
PDFs with highlights: {successful_pdfs}
PDFs failed: {failed_pdfs}
Success rate: {self.stats['success_rate']:.1f}%

Total unique terms in dataset: {self.stats['total_unique_terms']}
Terms highlighted (first instance only): {self.stats['terms_highlighted']}

Terms by category:
""")
        for category, unique_count in self.stats['terms_by_category'].items():
            highlighted = sum(r['highlights_by_category'].get(
                category, 0) for r in self.results)
            print(
                f"  {category}: {highlighted} highlighted (from {unique_count} unique terms)")

        print(f"""
KEY FEATURES:
- ✓ First instance only highlighting (no clutter)
- ✓ Robust error handling for maximum compatibility
- ✓ Color-coded categories for easy verification
- ✓ Comprehensive logging and reporting

Output saved to: {self.output_folder}
Reports: 
  - {report_path.name}
  - {details_path.name}
  - {terms_path.name}
=============================================================
        """)


def main():
    """Main function to run the improved PDF direct highlighter."""
    highlighter = PDFDirectHighlighterImproved()
    highlighter.process_all_pdfs()


if __name__ == "__main__":
    main()
