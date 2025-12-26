#!/usr/bin/env python3
"""
CSV Expansion Script for Better Highlighting
============================================

This script takes the current CSV with multi-line supporting quotes and creates
a new CSV where each individual supporting quote becomes a separate row.

Original format:
Study_ID=1, Supporting_quotes="Quote1\r\n - Quote2\r\n - Quote3"

New format:
Study_ID=1, Quote_ID=1, Variable="Supporting_quotes_for__Unit_Size_", Quote="Quote1"
Study_ID=1, Quote_ID=2, Variable="Supporting_quotes_for__Unit_Size_", Quote="Quote2"
Study_ID=1, Quote_ID=3, Variable="Supporting_quotes_for__Unit_Size_", Quote="Quote3"
"""

import pandas as pd
import re
from pathlib import Path
from datetime import date

def clean_quote_text(text):
    """Clean individual quote text."""
    if not text:
        return ""
    
    # Remove quotes and extra whitespace
    text = re.sub(r'^["\s]+|["\s]+$', '', str(text))
    text = re.sub(r'\s+', ' ', text)
    text = text.strip()
    
    return text

def split_supporting_quotes(text):
    """Split multi-line supporting quotes into individual quotes."""
    if not text or pd.isna(text):
        return []
    
    text = str(text)
    
    # Handle the CSV quote format: - "Quote 1"\r\n  - "Quote 2"
    if '- "' in text or '"\r\n' in text:
        # First, split on the quote pattern
        quotes = []
        
        # Split by the pattern: \r\n  - " or just - "
        parts = re.split(r'(?:\r?\n\s*)?-\s*"([^"]*)"', text)
        
        for part in parts:
            clean_part = clean_quote_text(part)
            if clean_part and len(clean_part) > 10:  # Filter very short fragments
                quotes.append(clean_part)
        
        # If that didn't work well, try alternative splitting
        if len(quotes) < 2:
            # Try splitting on just the line breaks and clean up
            alt_parts = re.split(r'\r?\n\s*-?\s*"?', text)
            quotes = []
            for part in alt_parts:
                clean_part = clean_quote_text(part)
                if clean_part and len(clean_part) > 10:
                    quotes.append(clean_part)
        
        return quotes
    
    # If no special format, just return the whole text as one quote
    clean_text = clean_quote_text(text)
    return [clean_text] if clean_text and len(clean_text) > 10 else []

def expand_csv(input_file, output_file):
    """Expand the CSV to have one row per supporting quote."""
    
    print(f"📖 Loading CSV: {input_file}")
    df = pd.read_csv(input_file)
    
    print(f"📊 Original CSV: {len(df)} rows, {len(df.columns)} columns")
    
    # Find all supporting quote columns
    supporting_cols = [col for col in df.columns if 'Supporting_quotes_for_' in col]
    print(f"📋 Found {len(supporting_cols)} supporting quote columns")
    
    # Create expanded data
    expanded_rows = []
    
    for idx, row in df.iterrows():
        study_id = row['Study_ID']
        
        # First, add basic study info (non-supporting-quote columns)
        base_info = {col: row[col] for col in df.columns if 'Supporting_quotes_for_' not in col}
        
        quote_id = 1
        study_has_quotes = False
        
        # Process each supporting quote column
        for col in supporting_cols:
            quotes = split_supporting_quotes(row[col])
            
            for quote in quotes:
                if quote:  # Only add non-empty quotes
                    expanded_row = base_info.copy()
                    expanded_row['Quote_ID'] = quote_id
                    expanded_row['Variable_Name'] = col
                    expanded_row['Supporting_Quote'] = quote
                    expanded_rows.append(expanded_row)
                    quote_id += 1
                    study_has_quotes = True
        
        # If no supporting quotes found, still add the base row
        if not study_has_quotes:
            base_row = base_info.copy()
            base_row['Quote_ID'] = 0
            base_row['Variable_Name'] = ""
            base_row['Supporting_Quote'] = ""
            expanded_rows.append(base_row)
    
    # Create new DataFrame
    expanded_df = pd.DataFrame(expanded_rows)
    
    # Reorder columns to put the new ones first
    new_col_order = ['Study_ID', 'Quote_ID', 'Variable_Name', 'Supporting_Quote'] + \
                   [col for col in expanded_df.columns if col not in ['Study_ID', 'Quote_ID', 'Variable_Name', 'Supporting_Quote']]
    
    expanded_df = expanded_df[new_col_order]
    
    print(f"✨ Expanded CSV: {len(expanded_df)} rows, {len(expanded_df.columns)} columns")
    
    # Save the expanded CSV
    expanded_df.to_csv(output_file, index=False)
    print(f"💾 Saved expanded CSV: {output_file}")
    
    # Print summary statistics
    studies_with_quotes = len(expanded_df[expanded_df['Quote_ID'] > 0]['Study_ID'].unique())
    total_quotes = len(expanded_df[expanded_df['Quote_ID'] > 0])
    
    print(f"\n📈 SUMMARY:")
    print(f"   📚 Studies with supporting quotes: {studies_with_quotes}")
    print(f"   💬 Total individual quotes extracted: {total_quotes}")
    print(f"   📊 Average quotes per study: {total_quotes/studies_with_quotes:.1f}")
    
    # Show sample of expanded data
    print(f"\n🔍 SAMPLE EXPANDED DATA:")
    sample_data = expanded_df[expanded_df['Quote_ID'] > 0].head(3)
    for _, row in sample_data.iterrows():
        print(f"   Study {row['Study_ID']}, Quote {row['Quote_ID']}: {row['Supporting_Quote'][:80]}...")

def main():
    # File paths
    input_file = "20250820_Analysis & Results/20250820_combined_dataset.csv"
    
    # Create output filename with current date
    today = date.today().strftime("%Y%m%d")
    output_file = f"{today}_Analysis & Results/{today}_expanded_quotes_dataset.csv"
    
    # Create output directory if it doesn't exist
    Path(output_file).parent.mkdir(exist_ok=True)
    
    # Expand the CSV
    expand_csv(input_file, output_file)
    
    print(f"\n🎉 CSV expansion completed!")
    print(f"📄 Original: {input_file}")
    print(f"📄 Expanded: {output_file}")
    print(f"\n💡 Next step: Update the highlighter to use the expanded CSV for more precise highlighting.")

if __name__ == "__main__":
    main()
