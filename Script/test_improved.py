#!/usr/bin/env python3
"""
Simple Test of the Improved Highlighting
"""

from pdf_quote_highlighter import PDFQuoteHighlighter


def test_improved_highlighting():
    # Test with one study
    highlighter = PDFQuoteHighlighter(
        '../20250731_Analysis & Results/20250731_combined_dataset.csv',
        '../Review_articles',
        '../Review_articles_highlighted_debug'
    )

    if highlighter.load_data():
        row = highlighter.df.iloc[0]  # First study
        result = highlighter.process_single_study(row)

        print('\nTest Result:')
        print(f'Success: {result.get("success", False)}')
        print(
            f'Highlighted: {result.get("highlighted_quotes", 0)}/{result.get("total_quotes", 0)}')

        if "details" in result:
            print('\nFirst few highlights:')
            for detail in result["details"][:3]:  # Show first 3 details
                print(f'  - Variable: {detail["variable"]}')
                print(f'    Blocks: {detail.get("blocks_highlighted", 1)}')
                print(f'    Quote: {detail["quote"][:80]}...')
                print()


if __name__ == "__main__":
    test_improved_highlighting()
