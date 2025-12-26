#!/usr/bin/env python3
"""Debug quote parsing with real data."""

import pandas as pd
import re

def debug_real_quotes():
    """Debug the actual quote data format."""
    df = pd.read_csv('20250820_Analysis & Results/20250820_combined_dataset.csv')
    study_data = df[df['Study_ID'] == 1].iloc[0]
    
    # Check all supporting quote columns
    quote_columns = [col for col in df.columns if col.startswith('Supporting_quotes_for__')]
    
    print("Available quote columns:")
    for col in quote_columns:
        print(f"  {col}")
    
    print("\nSample quote data:")
    for col in quote_columns[:3]:  # First 3 columns
        value = study_data.get(col, '')
        if value and not pd.isna(value) and len(str(value).strip()) > 10:
            print(f"\n{col}:")
            print(f"Type: {type(value)}")
            print(f"Value: {repr(str(value)[:200])}")
            
            # Try to find quotes manually
            text = str(value)
            pattern = r'"([^"]{6,})"'
            matches = re.findall(pattern, text)
            print(f"Quotes found with simple regex: {len(matches)}")

if __name__ == "__main__":
    debug_real_quotes()
