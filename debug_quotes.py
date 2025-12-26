#!/usr/bin/env python3
"""Debug quote parsing."""

import pandas as pd
from enhanced_hybrid_highlighter import EnhancedHybridHighlighter
import re

def debug_quote_parsing():
    """Debug the quote parsing."""
    highlighter = EnhancedHybridHighlighter()
    
    # Load real data
    df = pd.read_csv('20250820_Analysis & Results/20250820_combined_dataset.csv')
    study_data = df[df['Study_ID'] == 1].iloc[0]
    quote_col = 'Supporting_quotes_for__Unit_Selection_Rationale_'
    
    if quote_col in study_data:
        raw = str(study_data[quote_col])
        print("Raw data (first 200 chars):")
        print(repr(raw[:200]))
        
        # Test normalization
        normalized = highlighter._normalize_quotes(raw)
        print("\nNormalized (first 200 chars):")
        print(repr(normalized[:200]))
        
        # Test regex
        quoted = re.findall(r'"([^"]+)"', normalized)
        print(f"\nFound {len(quoted)} quotes with regex:")
        for i, q in enumerate(quoted[:3]):  # First 3
            print(f"{i+1}. {repr(q[:50])}")
        
        # Test the full method
        parsed = highlighter.split_quotes_cell(raw)
        print(f"\nFinal parsed: {len(parsed)} quotes")
        for i, q in enumerate(parsed[:3]):
            print(f"{i+1}. {q[:100]}")

if __name__ == "__main__":
    debug_quote_parsing()
