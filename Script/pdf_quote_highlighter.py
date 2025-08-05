#!/usr/bin/env python3
"""
PDF Quote Highlighter
=====================

This script highlights supporting quotes from Elicit extractions in PDF files.
It reads quotes from a CSV file and highlights them in corresponding PDF documents.

Requirements:
- pymupdf (fitz)
- pandas
- rapidfuzz
- pathlib

Usage:
    python pdf_quote_highlighter.py --csv_file "path/to/combined_dataset.csv" --pdf_folder "Review_articles" --output_folder "Review_articles_highlighted"
"""

import argparse
import pandas as pd
import fitz  # PyMuPDF
import re
import os
import logging
from pathlib import Path
from rapidfuzz import fuzz, process, utils
from typing import List, Dict, Tuple, Optional
import json

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('pdf_highlighting.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)


class PDFQuoteHighlighter:
    """Class to handle PDF quote highlighting operations."""

    def __init__(self, csv_file: str, pdf_folder: str, output_folder: str):
        self.csv_file = Path(csv_file)
        self.pdf_folder = Path(pdf_folder)
        self.output_folder = Path(output_folder)
        self.df = None
        self.highlight_colors = {
            'Country': (1.0, 1.0, 0.0),  # Yellow
            'City': (0.0, 1.0, 1.0),  # Cyan
            'Crime_Type': (0.0, 1.0, 0.0),  # Green
            'Spatial_Unit_Name': (1.0, 0.5, 0.0),  # Orange
            'Unit_Size': (1.0, 0.0, 1.0),  # Magenta
            'Independent_Variables': (0.5, 0.5, 1.0),  # Light Blue
            'Model_Type': (1.0, 0.0, 0.0),  # Red
            'Data_Sources': (0.5, 1.0, 0.5),  # Light Green
            'default': (1.0, 1.0, 0.0)  # Yellow for others
        }

        # Remove all smart highlighting variables - not needed for exact matching
        self.results = []

        # Create output folder if it doesn't exist
        self.output_folder.mkdir(parents=True, exist_ok=True)

    def load_data(self) -> bool:
        """Load the CSV data containing quotes and study information."""
        try:
            logger.info(f"Loading data from {self.csv_file}")
            self.df = pd.read_csv(self.csv_file)
            logger.info(f"Loaded {len(self.df)} studies from CSV")
            return True
        except Exception as e:
            logger.error(f"Error loading CSV file: {e}")
            return False

    def clean_text(self, text: str) -> str:
        """Clean text for better matching."""
        if pd.isna(text) or text == "":
            return ""

        # Remove extra whitespace and normalize
        text = re.sub(r'\s+', ' ', str(text).strip())
        # Remove common artifacts
        text = re.sub(r'^[-"\s]+|[-"\s]+$', '', text)
        # Remove special characters that might cause issues
        # Remove fancy quotes and dashes
        text = re.sub(r'[""''‚„…–—]', '', text)
        # Normalize common ligatures
        text = text.replace('ﬁ', 'fi').replace('ﬂ', 'fl')

        return text

    def extract_quotes_for_study(self, row: pd.Series) -> Dict[str, List[str]]:
        """Extract all supporting quotes for a single study."""
        quotes_by_variable = {}

        # Get all supporting quote columns - handle both formats
        quote_columns = [
            col for col in self.df.columns if 'Supporting_quotes_for' in col]

        for col in quote_columns:
            quote_text = self.clean_text(row[col])
            if quote_text and quote_text not in ['-', 'N/A', 'n/a', 'NA']:
                # Extract variable name from column name
                # Handle the format: Supporting_quotes_for__VariableName_
                variable_name = col.replace('Supporting_quotes_for__', '').replace(
                    'Supporting_quotes_for_', '')
                # Remove leading/trailing underscores
                variable_name = re.sub(r'^_+|_+$', '', variable_name)
                variable_name = variable_name.replace(
                    '_', ' ')  # Convert underscores to spaces

                # Split multiple quotes if they exist
                quotes = self.split_quotes(quote_text)
                if quotes:
                    quotes_by_variable[variable_name] = quotes

        return quotes_by_variable

    def split_quotes(self, quote_text: str) -> List[str]:
        """Split text that may contain multiple quotes."""
        if not quote_text:
            return []

        # Primary split on the delimiter pattern used in the dataset: " - " (inside quotes)
        # Pattern: "text - text" should be split into ["text", "text"]
        quotes = re.split(r'\s*-\s*', quote_text)

        # Clean up quotes
        cleaned_quotes = []
        for quote in quotes:
            # Remove leading/trailing quotes and whitespace
            quote = re.sub(r'^["\s]+|["\s]+$', '', quote.strip())

            # Skip very short fragments
            if len(quote) < 15:
                continue

            # Split on other common delimiters if the quote is very long
            if len(quote) > 500:
                # Try splitting on sentence boundaries for very long quotes
                sub_quotes = re.split(r'[.!?]+\s*(?=[A-Z])', quote)
                for sub_quote in sub_quotes:
                    sub_quote = sub_quote.strip()
                    if len(sub_quote) > 20:  # Minimum quote length
                        cleaned_quotes.append(sub_quote)
            else:
                cleaned_quotes.append(quote)

        # If no good splits found, return the whole text cleaned
        if not cleaned_quotes:
            cleaned_text = re.sub(r'^["\s]+|["\s]+$', '', quote_text.strip())
            if len(cleaned_text) > 15:
                cleaned_quotes.append(cleaned_text)

        return cleaned_quotes

    def find_pdf_file(self, title: str, authors: str) -> Optional[Path]:
        """Find the corresponding PDF file for a study."""
        # Clean title and authors for filename matching
        clean_title = re.sub(r'[^\w\s-]', '', title.lower())
        clean_authors = re.sub(
            r'[^\w\s-]', '', str(authors).lower()) if pd.notna(authors) else ""

        # Look for PDF files in the folder
        pdf_files = list(self.pdf_folder.glob("*.pdf"))

        if not pdf_files:
            logger.warning(f"No PDF files found in {self.pdf_folder}")
            return None

        best_match = None
        best_score = 0

        for pdf_file in pdf_files:
            filename = pdf_file.stem.lower()

            # Try matching with title
            title_score = fuzz.partial_ratio(clean_title, filename)

            # Try matching with authors
            author_score = 0
            if clean_authors:
                author_score = fuzz.partial_ratio(clean_authors, filename)

            # Combined score
            combined_score = max(title_score, author_score)

            if combined_score > best_score and combined_score > 60:  # Minimum threshold
                best_score = combined_score
                best_match = pdf_file

        if best_match:
            logger.info(
                f"Found PDF match: {best_match.name} (score: {best_score})")
        else:
            logger.warning(f"No PDF match found for: {title[:50]}...")

        return best_match

    def extract_pdf_text_with_positions(self, pdf_path: Path) -> List[Dict]:
        """Extract text from PDF with position information."""
        try:
            # Open with no password (handles some PDF/A compliance issues)
            doc = fitz.open(pdf_path)
            text_blocks = []

            for page_num in range(len(doc)):
                page = doc[page_num]

                # Use different text extraction method for better compatibility
                try:
                    blocks = page.get_text("dict")
                except Exception as e:
                    logger.warning(
                        f"Standard text extraction failed for page {page_num}, trying alternative method: {e}")
                    # Fallback to simpler text extraction
                    try:
                        simple_text = page.get_text()
                        if simple_text:
                            # Create a simple block structure
                            rect = page.rect
                            text_blocks.append({
                                'text': simple_text,
                                'bbox': (rect.x0, rect.y0, rect.x1, rect.y1),
                                'page': page_num,
                                'font': 'unknown',
                                'size': 12
                            })
                        continue
                    except Exception as e2:
                        logger.error(
                            f"All text extraction methods failed for page {page_num}: {e2}")
                        continue

                for block in blocks["blocks"]:
                    if "lines" in block:
                        for line in block["lines"]:
                            line_text = ""
                            line_bbox = None
                            line_spans = []

                            for span in line["spans"]:
                                if span["text"].strip():
                                    line_text += span["text"]
                                    line_spans.append(span)

                                    # Calculate combined bounding box for the line
                                    if line_bbox is None:
                                        line_bbox = span["bbox"]
                                    else:
                                        line_bbox = (
                                            min(line_bbox[0], span["bbox"][0]),
                                            min(line_bbox[1], span["bbox"][1]),
                                            max(line_bbox[2], span["bbox"][2]),
                                            max(line_bbox[3], span["bbox"][3])
                                        )

                            # Add the entire line as one block if it has text
                            if line_text.strip():
                                text_blocks.append({
                                    'text': line_text,
                                    'bbox': line_bbox,
                                    'page': page_num,
                                    'font': line_spans[0]["font"] if line_spans else 'unknown',
                                    'size': line_spans[0]["size"] if line_spans else 12
                                })

            doc.close()
            return text_blocks

        except Exception as e:
            logger.error(f"Error extracting text from {pdf_path}: {e}")
            return []

    def find_quote_in_pdf(self, quote: str, text_blocks: List[Dict], variable_name: str = "") -> List[Dict]:
        """Find exact quote locations in PDF text blocks - 100% match only."""
        matches = []

        # Clean the quote for exact matching
        quote_clean = quote.strip().lower()

        # Remove extra whitespace in quote
        quote_clean = re.sub(r'\s+', ' ', quote_clean)

        # Create full text with better spacing handling
        full_text_parts = []
        block_positions = []
        current_pos = 0

        for block in text_blocks:
            block_text = block["text"]
            # Normalize whitespace in block text too
            block_text = re.sub(r'\s+', ' ', block_text)

            block_positions.append({
                'start': current_pos,
                'end': current_pos + len(block_text),
                'block': block
            })

            full_text_parts.append(block_text)
            current_pos += len(block_text) + 1  # +1 for space between blocks

        full_text = " ".join(full_text_parts)
        full_text_lower = full_text.lower()

        # Only exact substring matching (case insensitive)
        if quote_clean in full_text_lower:
            # Found exact match
            start_pos = full_text_lower.find(quote_clean)
            end_pos = start_pos + len(quote_clean)

            # Find ALL blocks that contain any part of the quote
            match_blocks = []

            for pos_info in block_positions:
                # Check if this block overlaps with the quote span
                if (pos_info['start'] <= start_pos < pos_info['end']) or \
                   (pos_info['start'] < end_pos <= pos_info['end']) or \
                   (start_pos <= pos_info['start'] and end_pos >= pos_info['end']):
                    match_blocks.append(pos_info['block'])

            if match_blocks:
                matches.append({
                    'quote': quote,
                    'found_text': full_text[start_pos:end_pos],
                    'similarity': 100.0,
                    'blocks': match_blocks,
                    'page': match_blocks[0]["page"] if match_blocks else 0
                })

        return matches

    def highlight_pdf(self, pdf_path: Path, quotes_by_variable: Dict[str, List[str]], output_path: Path) -> Dict:
        """Highlight quotes in PDF and save the result."""
        try:
            doc = fitz.open(pdf_path)
            text_blocks = self.extract_pdf_text_with_positions(pdf_path)

            total_quotes = sum(len(quotes)
                               for quotes in quotes_by_variable.values())
            highlighted_quotes = 0
            highlight_details = []

            logger.info(
                f"Processing {total_quotes} quotes for {pdf_path.name}")

            # Simple highlighting - just find exact quotes and add variable names in comments
            for variable_name, quotes in quotes_by_variable.items():
                color = self.highlight_colors.get(
                    variable_name, self.highlight_colors['default'])

                for quote in quotes:
                    matches = self.find_quote_in_pdf(
                        quote, text_blocks, variable_name)

                    if matches:  # Found matches for this quote
                        for match in matches:
                            page_num = match['page']
                            page = doc[page_num]

                            # Highlight each block in the match
                            highlighted_any = False
                            for block in match['blocks']:
                                bbox = fitz.Rect(block['bbox'])
                                try:
                                    highlight = page.add_highlight_annot(bbox)
                                    highlight.set_colors(stroke=color)
                                    # Just add variable name in comment
                                    highlight.set_info(
                                        title=f"{variable_name}",
                                        content=f"{variable_name.replace('_', ' ')}"
                                    )
                                    highlight.update()
                                    highlighted_any = True
                                except Exception as e:
                                    logger.warning(
                                        f"Failed to highlight block on page {page_num + 1}: {e}")

                            if highlighted_any:
                                highlighted_quotes += 1
                                highlight_details.append({
                                    'variable': variable_name,
                                    'quote': quote[:100] + '...' if len(quote) > 100 else quote,
                                    'found_text': match['found_text'][:100] + '...' if len(match['found_text']) > 100 else match['found_text'],
                                    'similarity': match['similarity'],
                                    'page': page_num + 1,
                                    'blocks_highlighted': len(match['blocks'])
                                })

                                logger.info(
                                    f"Highlighted quote for {variable_name} on page {page_num + 1} (similarity: {match['similarity']}%, blocks: {len(match['blocks'])})")
                    else:
                        # Log failed matches for debugging
                        logger.debug(
                            f"No match found for {variable_name}: {quote[:50]}...")

            # Save the highlighted PDF (simple approach to avoid PDF/A issues)
            try:
                # First try simple save
                doc.save(output_path)
            except Exception as save_error:
                logger.warning(
                    f"Standard save failed, trying with basic options: {save_error}")
                try:
                    # Fallback: save with minimal options
                    doc.save(output_path, garbage=1, clean=True)
                except Exception as fallback_error:
                    logger.error(f"All save methods failed: {fallback_error}")
                    raise fallback_error

            doc.close()

            result = {
                'pdf_file': pdf_path.name,
                'output_file': output_path.name,
                'total_quotes': total_quotes,
                'highlighted_quotes': highlighted_quotes,
                'success_rate': (highlighted_quotes / total_quotes * 100) if total_quotes > 0 else 0,
                'details': highlight_details
            }

            logger.info(
                f"Successfully highlighted {highlighted_quotes}/{total_quotes} quotes in {pdf_path.name}")
            return result

        except Exception as e:
            logger.error(f"Error highlighting PDF {pdf_path}: {e}")
            return {
                'pdf_file': pdf_path.name,
                'error': str(e),
                'total_quotes': sum(len(quotes) for quotes in quotes_by_variable.values()),
                'highlighted_quotes': 0,
                'success_rate': 0
            }

    def process_single_study(self, row: pd.Series, threshold: int = 80) -> Dict:
        """Process a single study for testing purposes."""
        title = row.get('Title', '')
        authors = row.get('Authors', '')

        logger.info(f"Processing single study: {title[:50]}...")

        # Extract quotes for this study
        quotes_by_variable = self.extract_quotes_for_study(row)

        if not quotes_by_variable:
            logger.warning(f"No quotes found for study: {title[:50]}...")
            return {
                'success': False,
                'error': 'No quotes found',
                'study_title': title
            }

        # Find PDF file
        pdf_file = self.find_pdf_file(title, authors)
        if not pdf_file:
            logger.warning(f"PDF not found for: {title[:50]}...")
            return {
                'success': False,
                'error': 'PDF not found',
                'study_title': title
            }

        # Create output filename
        safe_title = "".join(c for c in title if c.isalnum()
                             or c in (' ', '-', '_')).strip()
        safe_title = safe_title[:50] + "_highlighted.pdf"
        output_path = self.output_folder / safe_title

        # Highlight the PDF
        result = self.highlight_pdf(pdf_file, quotes_by_variable, output_path)
        result['success'] = True
        result['study_title'] = title

        return result

    def process_all_studies(self):
        """Process all studies in the dataset."""
        if self.df is None:
            logger.error("No data loaded")
            return

        logger.info(f"Processing {len(self.df)} studies")

        for idx, row in self.df.iterrows():
            title = row.get('Title', '')
            authors = row.get('Authors', '')

            logger.info(
                f"Processing study {idx + 1}/{len(self.df)}: {title[:50]}...")

            # Extract quotes for this study
            quotes_by_variable = self.extract_quotes_for_study(row)

            if not quotes_by_variable:
                logger.info(f"No quotes found for study: {title[:50]}...")
                continue

            # Find corresponding PDF
            pdf_file = self.find_pdf_file(title, authors)

            if not pdf_file:
                self.results.append({
                    'study_title': title,
                    'pdf_file': 'NOT_FOUND',
                    'total_quotes': sum(len(quotes) for quotes in quotes_by_variable.values()),
                    'highlighted_quotes': 0,
                    'success_rate': 0,
                    'error': 'PDF file not found'
                })
                continue

            # Create output filename
            output_filename = f"{pdf_file.stem}_highlighted.pdf"
            output_path = self.output_folder / output_filename

            # Highlight the PDF
            result = self.highlight_pdf(
                pdf_file, quotes_by_variable, output_path)
            result['study_title'] = title
            self.results.append(result)

    def save_results_report(self):
        """Save a detailed report of the highlighting results."""
        if not self.results:
            logger.warning("No results to save")
            return

        # Create summary report
        report_path = self.output_folder / "highlighting_report.csv"

        summary_data = []
        for result in self.results:
            summary_data.append({
                'Study_Title': result.get('study_title', ''),
                'PDF_File': result.get('pdf_file', ''),
                'Output_File': result.get('output_file', ''),
                'Total_Quotes': result.get('total_quotes', 0),
                'Highlighted_Quotes': result.get('highlighted_quotes', 0),
                'Success_Rate': result.get('success_rate', 0),
                'Error': result.get('error', '')
            })

        summary_df = pd.DataFrame(summary_data)
        summary_df.to_csv(report_path, index=False)

        # Create detailed report
        detailed_path = self.output_folder / "highlighting_details.json"
        with open(detailed_path, 'w', encoding='utf-8') as f:
            json.dump(self.results, f, indent=2, ensure_ascii=False)

        # Print summary statistics
        total_studies = len(self.results)
        successful_studies = len(
            [r for r in self.results if r.get('highlighted_quotes', 0) > 0])
        total_quotes = sum(r.get('total_quotes', 0) for r in self.results)
        total_highlighted = sum(r.get('highlighted_quotes', 0)
                                for r in self.results)

        logger.info(f"""
        ========== HIGHLIGHTING SUMMARY ==========
        Total studies processed: {total_studies}
        Studies with highlights: {successful_studies}
        Total quotes: {total_quotes}
        Successfully highlighted: {total_highlighted}
        Overall success rate: {(total_highlighted/total_quotes*100):.1f}% if total_quotes > 0 else 0
        ==========================================
        """)

        logger.info(f"Reports saved to:")
        logger.info(f"  - Summary: {report_path}")
        logger.info(f"  - Details: {detailed_path}")


def main():
    """Main function to run the PDF quote highlighter."""
    parser = argparse.ArgumentParser(
        description='Highlight supporting quotes in PDF files')
    parser.add_argument('--csv_file', required=True,
                        help='Path to the combined dataset CSV file')
    parser.add_argument('--pdf_folder', required=True,
                        help='Path to folder containing PDF files')
    parser.add_argument('--output_folder', required=True,
                        help='Path to output folder for highlighted PDFs')
    parser.add_argument('--threshold', type=int, default=80,
                        help='Similarity threshold for quote matching (default: 80)')

    args = parser.parse_args()

    # Create highlighter instance
    highlighter = PDFQuoteHighlighter(
        args.csv_file, args.pdf_folder, args.output_folder)

    # Load data
    if not highlighter.load_data():
        return 1

    # Process all studies
    highlighter.process_all_studies()

    # Save results
    highlighter.save_results_report()

    return 0


if __name__ == "__main__":
    exit(main())
