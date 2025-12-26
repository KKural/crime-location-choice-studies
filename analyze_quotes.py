#!/usr/bin/env python3
"""Debug quote character positions."""

import pandas as pd
import re

def analyze_quote_chars():
    df = pd.read_csv('20250820_Analysis & Results/20250820_combined_dataset.csv')
    study_data = df[df['Study_ID'] == 1].iloc[0]
    raw = str(study_data['Supporting_quotes_for__Unit_Selection_Rationale_'])
    
    print("Raw data length:", len(raw))
    
    # Find all quote characters
    quotes_found = []
    for i, char in enumerate(raw[:500]):
        if char == '"':
            quotes_found.append((i, repr(raw[max(0,i-10):i+30])))
    
    print(f"\nFound {len(quotes_found)} quote marks in first 500 chars:")
    for pos, context in quotes_found:
        print(f"  Position {pos}: {context}")
    
    # Test regex directly
    pattern = r'"([^"]{6,})"'
    matches = re.findall(pattern, raw)
    print(f"\nRegex matches: {len(matches)}")
    
    # Try a simpler approach - split on quotes
    parts = raw.split('"')
    print(f"\nSplit on quotes gives {len(parts)} parts")
    for i, part in enumerate(parts[:5]):
        if len(part) > 10:
            print(f"  Part {i}: {repr(part[:50])}")

if __name__ == "__main__":
    analyze_quote_chars()
