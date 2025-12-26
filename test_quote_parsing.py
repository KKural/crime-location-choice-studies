#!/usr/bin/env python3
"""Test the quote parsing functionality."""

import pandas as pd
from enhanced_hybrid_highlighter import EnhancedHybridHighlighter

def test_quote_parsing():
    """Test the quote parsing functionality."""
    highlighter = EnhancedHybridHighlighter()
    
    # Test with sample bullet-formatted quotes
    test_cases = [
        '- "First quote." - "Second quote."',
        '- "Quote with, commas and details." - "Another quote here."',
        '"Simple quote without bullets."',
        '- "Multi-line quote that might span\nacross different lines." - "Second quote."'
    ]
    
    print("Testing quote parsing:")
    for i, test_case in enumerate(test_cases):
        print(f"\nTest {i+1}: {test_case[:50]}...")
        parsed = highlighter.split_quotes_cell(test_case)
        print(f"Parsed {len(parsed)} quotes:")
        for j, quote in enumerate(parsed):
            print(f"  {j+1}. {quote}")
    
    # Test with real data if available
    try:
        df = pd.read_csv('20250820_Analysis & Results/20250820_combined_dataset.csv')
        study_data = df[df['Study_ID'] == 1].iloc[0]
        
        # Test quote parsing for a supporting quote field
        quote_col = 'Supporting_quotes_for__Unit_Selection_Rationale_'
        if quote_col in study_data:
            raw_quotes = study_data[quote_col]
            print(f"\n\nReal data test:")
            print(f"Raw quote data: {str(raw_quotes)[:200]}...")
            
            parsed_quotes = highlighter.split_quotes_cell(str(raw_quotes))
            print(f"Parsed {len(parsed_quotes)} quotes from real data:")
            for i, quote in enumerate(parsed_quotes):
                print(f"{i+1}. {quote[:100]}...")
        else:
            print(f"\nColumn {quote_col} not found in data")
            
    except Exception as e:
        print(f"\nError loading real data: {e}")

if __name__ == "__main__":
    test_quote_parsing()
