#!/usr/bin/env python3
"""
IMPROVED PDF Direct Highlighting Script
=======================================

This script directly highlights extracted terms in PDF files with robust error handling
and 100% success rate targeting.

Requirements:
- pymupdf (fitz)
- pandas
- rapidfuzz
- pathlib

Usage:
    python 20250819_improved_pdf_highlighting.py
"""

import pandas as pd
import fitz  # PyMuPDF
import re
import os
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Set
import json
from datetime import datetime
import traceback
import time

# Configure logging
current_date = datetime.now().strftime("%Y%m%d")
log_file = f"../20250819_improved_pdf_highlighting.log"

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)


class ImprovedPDFHighlighter:
    """Improved class for direct PDF highlighting with robust error handling."""

    def __init__(self):
        self.current_date = datetime.now().strftime("%Y%m%d")
        self.output_folder = Path(f"../20250819_Improved_PDF_Highlighting")
        self.csv_file = "../20250819_Analysis & Results/20250819_combined_dataset.csv"
        self.pdf_folder = Path("../Review_articles")
        self.df = None

        # Enhanced color scheme for different term categories
        self.highlight_colors = {
            'Geographic': (1.0, 1.0, 0.0, 0.5),      # Yellow with transparency
            'Quantitative': (0.0, 1.0, 1.0, 0.5),   # Cyan with transparency
            'Methods': (0.0, 1.0, 0.0, 0.5),        # Green with transparency
            'Crime-related': (1.0, 0.5, 0.0, 0.5),  # Orange with transparency
            'Statistical': (1.0, 0.0, 1.0, 0.5),    # Magenta with transparency
            # Light Gray with transparency
            'Other': (0.8, 0.8, 0.8, 0.5)
        }

        self.results = []
        self.stats = {
            'pdfs_processed': 0,
            'pdfs_successful': 0,
            'pdfs_with_errors': 0,
            'total_terms_found': 0,
            'total_highlights_created': 0,
            'terms_by_category': {},
            'highlights_by_category': {},
            'processing_time': 0
        }

        # Create output folder
        self.output_folder.mkdir(exist_ok=True)
        logger.info(f"Output folder created: {self.output_folder}")

    def load_data(self) -> bool:
        """Load the CSV data containing extracted terms."""
        try:
            logger.info(f"Loading data from {self.csv_file}")
            if not Path(self.csv_file).exists():
                logger.error(f"CSV file not found: {self.csv_file}")
                return False

            self.df = pd.read_csv(self.csv_file)
            logger.info(f"✓ Loaded {len(self.df)} studies from CSV")
            return True
        except Exception as e:
            logger.error(f"Error loading CSV file: {e}")
            logger.error(f"Full traceback: {traceback.format_exc()}")
            return False

    def clean_and_split_terms(self, value: str) -> Set[str]:
        """Clean and split terms from a cell value."""
        if pd.isna(value) or str(value).strip() in ['-', 'N/A', 'n/a', 'NA', 'nan', '']:
            return set()

        # Convert to string and clean
        text = str(value).strip()

        # Split on various delimiters
        terms = re.split(r'[;,\|/&\n\r]+', text)

        cleaned_terms = set()
        for term in terms:
            # Clean each term
            clean_term = re.sub(r'[^\w\s\-\.]', ' ', term.strip())
            clean_term = re.sub(r'\s+', ' ', clean_term).strip()

            # Only keep terms that are meaningful
            if len(clean_term) > 2 and clean_term.lower() not in ['the', 'and', 'for', 'with', 'from']:
                cleaned_terms.add(clean_term)

        return cleaned_terms

    def extract_terms_from_dataset(self) -> Dict[str, Set[str]]:
        """Extract unique terms from the main data columns with improved categorization."""
        terms_by_category = {
            'Geographic': set(),
            'Quantitative': set(),
            'Methods': set(),
            'Crime-related': set(),
            'Statistical': set(),
            'Other': set()
        }

        # Define column mappings with more comprehensive coverage
        column_mappings = {
            'Geographic': ['Country', 'City', 'Geographical_scope', 'Location', 'Region'],
            'Quantitative': ['Sample_size', 'Unit_Size', 'Spatial_Unit_Name', 'Number_of_areas', 'Population'],
            'Methods': ['Data_Sources', 'Data_collection', 'Methodology', 'Approach'],
            'Crime-related': ['Crime_Type', 'Crime_category', 'Offense_type', 'Criminal_activity'],
            'Statistical': ['Model_Type', 'Statistical_approach', 'Analysis_Method', 'Statistical_method'],
            'Other': ['Independent_Variables', 'Dependent_Variable', 'Theory_Framework', 'Variables', 'Factors']
        }

        logger.info("Extracting terms from dataset with improved cleaning...")

        # Process all columns in the dataset
        for _, row in self.df.iterrows():
            for category, columns in column_mappings.items():
                for col in columns:
                    if col in self.df.columns:
                        terms = self.clean_and_split_terms(row[col])
                        terms_by_category[category].update(terms)

        # Remove very common/generic terms that would create too much noise
        noise_terms = {
            'study', 'analysis', 'data', 'results', 'method', 'approach', 'using',
            'based', 'total', 'number', 'type', 'level', 'area', 'time', 'used'
        }

        for category in terms_by_category:
            terms_by_category[category] = {
                term for term in terms_by_category[category]
                if term.lower() not in noise_terms and len(term) > 2
            }

        # Log statistics
        total_terms = sum(len(terms) for terms in terms_by_category.values())
        logger.info(f"✓ Extracted {total_terms} unique terms:")
        for category, terms in terms_by_category.items():
            count = len(terms)
            logger.info(f"  {category}: {count} terms")
            self.stats['terms_by_category'][category] = count
            self.stats['highlights_by_category'][category] = 0

        return terms_by_category

    def find_pdf_files(self) -> List[Path]:
        """Find all PDF files with robust error handling."""
        if not self.pdf_folder.exists():
            logger.error(f"PDF folder not found: {self.pdf_folder}")
            return []

        try:
            pdf_files = list(self.pdf_folder.glob("*.pdf"))
            logger.info(f"✓ Found {len(pdf_files)} PDF files")
            return sorted(pdf_files)  # Sort for consistent processing order
        except Exception as e:
            logger.error(f"Error finding PDF files: {e}")
            return []

    def safe_pdf_open(self, pdf_path: Path, max_retries: int = 3) -> Optional[fitz.Document]:
        """Safely open a PDF with multiple retry attempts."""
        for attempt in range(max_retries):
            try:
                # Try different opening methods
                if attempt == 0:
                    doc = fitz.open(pdf_path)
                elif attempt == 1:
                    # Try opening with specific flags
                    doc = fitz.open(pdf_path, filetype="pdf")
                else:
                    # Last resort - try with binary read
                    with open(pdf_path, 'rb') as f:
                        pdf_bytes = f.read()
                    doc = fitz.open(stream=pdf_bytes, filetype="pdf")

                # Verify the document is valid
                if len(doc) > 0:
                    return doc
                else:
                    logger.warning(f"PDF has no pages: {pdf_path.name}")
                    if doc:
                        doc.close()

            except Exception as e:
                logger.warning(
                    f"Attempt {attempt + 1} failed for {pdf_path.name}: {e}")
                if attempt == max_retries - 1:
                    logger.error(f"All attempts failed for {pdf_path.name}")
                else:
                    time.sleep(0.5)  # Brief pause before retry

        return None

    def highlight_terms_in_pdf(self, pdf_path: Path, terms_by_category: Dict[str, Set[str]]) -> Dict:
        """Highlight terms in PDF with improved error handling and success tracking."""
        start_time = time.time()

        result = {
            'pdf_file': pdf_path.name,
            'output_file': '',
            'pages_processed': 0,
            'terms_found': 0,
            'highlights_created': 0,
            'highlights_by_category': {},
            'processing_time': 0,
            'success': False,
            'error': None,
            'warnings': []
        }

        doc = None
        output_path = None

        try:
            # Safely open the PDF
            doc = self.safe_pdf_open(pdf_path)
            if not doc:
                result['error'] = "Failed to open PDF after multiple attempts"
                return result

            num_pages = len(doc)
            output_path = self.output_folder / \
                f"{pdf_path.stem}_highlighted.pdf"

            logger.info(f"Processing {pdf_path.name} ({num_pages} pages)")

            total_highlights = 0
            highlights_by_category = {
                cat: 0 for cat in terms_by_category.keys()}
            terms_found_in_pdf = set()

            # Process each page with individual error handling
            pages_processed = 0
            for page_num in range(num_pages):
                try:
                    page = doc[page_num]
                    page_text = page.get_text().lower()

                    # Search for each category of terms
                    for category, terms in terms_by_category.items():
                        # Use RGB only for compatibility
                        color_rgb = self.highlight_colors[category][:3]

                        for term in terms:
                            term_lower = term.lower()

                            # Only search if term is reasonably likely to be in academic text
                            if len(term_lower) < 3:
                                continue

                            try:
                                # Find all instances of the term on this page
                                text_instances = page.search_for(term_lower)

                                if text_instances:
                                    terms_found_in_pdf.add(term)

                                    for inst in text_instances:
                                        try:
                                            # Create highlight annotation
                                            highlight = page.add_highlight_annot(
                                                inst)
                                            highlight.set_colors(
                                                stroke=color_rgb)
                                            highlight.set_info(
                                                title=f"{category}: {term}",
                                                content=f"Category: {category}\nTerm: {term}\nPage: {page_num + 1}"
                                            )
                                            highlight.update()
                                            total_highlights += 1
                                            highlights_by_category[category] += 1

                                        except Exception as highlight_error:
                                            result['warnings'].append(
                                                f"Failed to create highlight for '{term}' on page {page_num + 1}: {str(highlight_error)}")

                            except Exception as search_error:
                                result['warnings'].append(
                                    f"Search failed for '{term}' on page {page_num + 1}: {str(search_error)}")

                    pages_processed += 1

                except Exception as page_error:
                    logger.warning(
                        f"Error processing page {page_num + 1} of {pdf_path.name}: {page_error}")
                    result['warnings'].append(
                        f"Page {page_num + 1} processing failed: {str(page_error)}")

            # Save the highlighted PDF with multiple save strategies
            save_success = False
            save_attempts = [
                lambda: doc.save(output_path, garbage=1, clean=True),
                lambda: doc.save(output_path, garbage=2),
                lambda: doc.save(output_path),
                lambda: doc.save(output_path, incremental=False,
                                 encryption=fitz.PDF_ENCRYPT_NONE)
            ]

            for i, save_method in enumerate(save_attempts):
                try:
                    save_method()
                    save_success = True
                    logger.info(
                        f"✓ Saved highlighted PDF: {output_path.name} (method {i+1})")
                    break
                except Exception as save_error:
                    if i < len(save_attempts) - 1:
                        logger.debug(
                            f"Save method {i+1} failed, trying next method: {save_error}")
                    else:
                        logger.error(
                            f"All save methods failed for {output_path}: {save_error}")
                        result['error'] = f"Save failed: {str(save_error)}"

            # Update results
            if save_success:
                result['output_file'] = output_path.name
                result['success'] = True

            result['pages_processed'] = pages_processed
            result['terms_found'] = len(terms_found_in_pdf)
            result['highlights_created'] = total_highlights
            result['highlights_by_category'] = highlights_by_category
            result['processing_time'] = time.time() - start_time

            # Update global stats
            if result['success']:
                self.stats['pdfs_successful'] += 1
                self.stats['total_terms_found'] += result['terms_found']
                self.stats['total_highlights_created'] += result['highlights_created']

                for category, count in highlights_by_category.items():
                    self.stats['highlights_by_category'][category] += count
            else:
                self.stats['pdfs_with_errors'] += 1

            logger.info(
                f"✓ Processed {pdf_path.name}: {total_highlights} highlights created from {len(terms_found_in_pdf)} unique terms")

        except Exception as e:
            logger.error(f"Critical error processing {pdf_path.name}: {e}")
            logger.error(f"Full traceback: {traceback.format_exc()}")
            result['error'] = str(e)
            result['processing_time'] = time.time() - start_time
            self.stats['pdfs_with_errors'] += 1

        finally:
            # Ensure document is properly closed
            if doc:
                try:
                    doc.close()
                except:
                    pass

        return result

    def process_all_pdfs(self):
        """Process all PDF files with comprehensive error handling and progress tracking."""
        overall_start_time = time.time()

        logger.info("=" * 60)
        logger.info("STARTING IMPROVED PDF DIRECT HIGHLIGHTING")
        logger.info("=" * 60)

        # Load data
        if not self.load_data():
            logger.error("Failed to load data. Exiting.")
            return

        # Extract terms
        terms_by_category = self.extract_terms_from_dataset()
        if not any(terms_by_category.values()):
            logger.error("No terms extracted from dataset. Exiting.")
            return

        # Find PDF files
        pdf_files = self.find_pdf_files()
        if not pdf_files:
            logger.error("No PDF files found. Exiting.")
            return

        # Initialize processing stats
        self.stats['pdfs_processed'] = len(pdf_files)

        logger.info(f"Starting processing of {len(pdf_files)} PDF files...")

        # Process each PDF with detailed progress tracking
        for i, pdf_file in enumerate(pdf_files, 1):
            logger.info(
                f"\n[{i}/{len(pdf_files)}] Processing: {pdf_file.name}")

            result = self.highlight_terms_in_pdf(pdf_file, terms_by_category)
            self.results.append(result)

            # Progress update
            if result['success']:
                logger.info(
                    f"  ✓ SUCCESS: {result['highlights_created']} highlights, {result['terms_found']} terms found")
            else:
                logger.error(f"  ✗ FAILED: {result['error']}")
                if result['warnings']:
                    logger.info(
                        f"  Warnings: {len(result['warnings'])} issues")

            # Periodic progress summary
            if i % 10 == 0:
                success_rate = (self.stats['pdfs_successful'] / i) * 100
                logger.info(
                    f"\n--- Progress Update: {i}/{len(pdf_files)} processed ({success_rate:.1f}% success rate) ---")

        # Final processing
        self.stats['processing_time'] = time.time() - overall_start_time
        self.save_comprehensive_results()
        self.print_final_summary()

    def save_comprehensive_results(self):
        """Save comprehensive results with detailed reporting."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Create detailed summary report
        summary_data = []
        for result in self.results:
            summary_data.append({
                'PDF_File': result['pdf_file'],
                'Success': result['success'],
                'Output_File': result['output_file'],
                'Pages_Processed': result['pages_processed'],
                'Terms_Found': result['terms_found'],
                'Highlights_Created': result['highlights_created'],
                'Geographic': result['highlights_by_category'].get('Geographic', 0),
                'Quantitative': result['highlights_by_category'].get('Quantitative', 0),
                'Methods': result['highlights_by_category'].get('Methods', 0),
                'Crime_related': result['highlights_by_category'].get('Crime-related', 0),
                'Statistical': result['highlights_by_category'].get('Statistical', 0),
                'Other': result['highlights_by_category'].get('Other', 0),
                'Processing_Time_Seconds': round(result['processing_time'], 2),
                'Warnings_Count': len(result.get('warnings', [])),
                'Error': result['error'] or ''
            })

        # Save CSV report
        summary_df = pd.DataFrame(summary_data)
        report_path = self.output_folder / \
            f"comprehensive_highlighting_report_{timestamp}.csv"
        summary_df.to_csv(report_path, index=False)

        # Save detailed JSON
        details_path = self.output_folder / \
            f"detailed_results_{timestamp}.json"
        with open(details_path, 'w', encoding='utf-8') as f:
            json.dump({
                'summary_stats': self.stats,
                'detailed_results': self.results,
                'timestamp': timestamp
            }, f, indent=2, ensure_ascii=False)

        logger.info(f"\n✓ Reports saved:")
        logger.info(f"  - Summary: {report_path.name}")
        logger.info(f"  - Details: {details_path.name}")

    def print_final_summary(self):
        """Print comprehensive final summary."""
        success_rate = (self.stats['pdfs_successful'] / self.stats['pdfs_processed']
                        * 100) if self.stats['pdfs_processed'] > 0 else 0

        print(f"""
{'=' * 60}
IMPROVED PDF HIGHLIGHTING - FINAL SUMMARY
{'=' * 60}

PROCESSING RESULTS:
📊 PDFs Processed: {self.stats['pdfs_processed']}
✅ Successful: {self.stats['pdfs_successful']} ({success_rate:.1f}%)
❌ With Errors: {self.stats['pdfs_with_errors']}

HIGHLIGHTING STATISTICS:
🎯 Total Terms Found: {self.stats['total_terms_found']:,}
✨ Total Highlights Created: {self.stats['total_highlights_created']:,}

HIGHLIGHTS BY CATEGORY:""")

        for category, count in self.stats['highlights_by_category'].items():
            terms_available = self.stats['terms_by_category'].get(category, 0)
            print(
                f"  {category}: {count:,} highlights (from {terms_available} unique terms)")

        print(f"""
⏱️ Total Processing Time: {self.stats['processing_time']:.1f} seconds
📁 Output Location: {self.output_folder}

SUCCESS RATE TARGET: {'🎉 ACHIEVED!' if success_rate >= 95 else '⚠️  REVIEW NEEDED'}
{'=' * 60}
        """)


def main():
    """Main function to run the improved PDF highlighter."""
    try:
        highlighter = ImprovedPDFHighlighter()
        highlighter.process_all_pdfs()

    except KeyboardInterrupt:
        logger.info("\n⚠️ Process interrupted by user")
    except Exception as e:
        logger.error(f"Critical error in main process: {e}")
        logger.error(f"Full traceback: {traceback.format_exc()}")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
