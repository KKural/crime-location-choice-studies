#!/usr/bin/env python3
"""
Debug Quote Matching
====================

This script helps debug the quote matching process to see exactly what's happening.
"""

import pandas as pd
import fitz
import re
from pathlib import Path


def debug_quote_matching():
    """Debug the quote matching process."""

    # Load data
    csv_file = "../20250731_Analysis & Results/20250731_combined_dataset.csv"
    df = pd.read_csv(csv_file)

    # Get first study
    row = df.iloc[0]
    title = row.get('Title', '')
    print(f"Testing with study: {title[:100]}...")

    # Extract quotes
    quote_columns = [
        col for col in df.columns if 'Supporting_quotes_for' in col]
    print(f"Found {len(quote_columns)} quote columns")

    # Find first quote
    test_quote = None
    test_variable = None

    for col in quote_columns:
        quote_text = str(row[col]).strip()
        if quote_text and quote_text not in ['-', 'N/A', 'n/a', 'NA', 'nan']:
            test_quote = quote_text
            test_variable = col.replace('Supporting_quotes_for__', '').replace(
                'Supporting_quotes_for_', '')
            test_variable = re.sub(
                r'^_+|_+$', '', test_variable).replace('_', ' ')
            break

    if not test_quote:
        print("No quotes found in first study")
        return

    print(f"\nTesting quote for variable: {test_variable}")
    print(f"Quote text: {test_quote[:200]}...")
    print(f"Quote length: {len(test_quote)} characters")

    # Clean the quote
    quote_clean = test_quote.strip().lower()
    print(f"Cleaned quote: {quote_clean[:200]}...")

    # Find PDF file
    pdf_folder = Path("../Review_articles")
    pdf_files = list(pdf_folder.glob("*.pdf"))

    if not pdf_files:
        print("No PDF files found")
        return

    # Test with first PDF
    test_pdf = pdf_files[0]
    print(f"\nTesting with PDF: {test_pdf.name}")

    # Extract text
    try:
        doc = fitz.open(test_pdf)
        print(f"PDF has {len(doc)} pages")

        # Extract text from first page
        page = doc[0]
        page_text = page.get_text()
        page_text_lower = page_text.lower()

        print(f"First page text length: {len(page_text)} characters")
        print(f"First 500 chars of page: {page_text[:500]}...")

        # Test if quote is in the page
        if quote_clean in page_text_lower:
            start_pos = page_text_lower.find(quote_clean)
            print(f"\n✓ FOUND EXACT MATCH at position {start_pos}")

            # Show context
            context_start = max(0, start_pos - 100)
            context_end = min(len(page_text), start_pos +
                              len(quote_clean) + 100)
            context = page_text[context_start:context_end]
            print(f"Context: ...{context}...")

        else:
            print("\n✗ No exact match found on first page")

            # Try to find partial matches
            quote_words = quote_clean.split()[:10]  # First 10 words
            for i, word in enumerate(quote_words):
                if len(word) > 3 and word in page_text_lower:
                    word_pos = page_text_lower.find(word)
                    print(f"Found word {i+1} '{word}' at position {word_pos}")

        doc.close()

    except Exception as e:
        print(f"Error processing PDF: {e}")


if __name__ == "__main__":
    debug_quote_matching()
