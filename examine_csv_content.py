#!/usr/bin/env python3
import pandas as pd
import numpy as np

def examine_csv_content():
    """Comprehensive examination of the CSV content."""
    
    # Load the CSV
    csv_path = '20250820_Analysis & Results/20250820_combined_dataset.csv'
    df = pd.read_csv(csv_path)
    
    print("=" * 80)
    print("CSV CONTENT EXAMINATION")
    print("=" * 80)
    
    print(f"\n📊 BASIC INFO:")
    print(f"   Shape: {df.shape[0]} rows × {df.shape[1]} columns")
    print(f"   Study IDs: {sorted(df['Study_ID'].unique())}")
    
    # Show first few studies with key data
    print(f"\n📋 STUDY OVERVIEW (First 5 studies):")
    key_columns = ['Study_ID', 'Title', 'Year', 'Country', 'City', 'Crime_Type', 'Data_Sources']
    display_df = df[key_columns].head()
    for idx, row in display_df.iterrows():
        print(f"\n  Study {row['Study_ID']}:")
        print(f"    Title: {str(row['Title'])[:80]}...")
        print(f"    Year: {row['Year']}")
        print(f"    Location: {row['Country']}, {row['City']}")
        print(f"    Crime: {row['Crime_Type']}")
        print(f"    Data: {str(row['Data_Sources'])[:60]}...")
    
    # Examine quote columns in detail
    print(f"\n🔍 QUOTE COLUMNS ANALYSIS:")
    quote_cols = [col for col in df.columns if 'Supporting_quotes_for' in col]
    print(f"   Found {len(quote_cols)} quote columns")
    
    # Check which studies have the most quote data
    quote_data_counts = []
    for idx, row in df.iterrows():
        study_id = row['Study_ID']
        non_empty_quotes = 0
        for col in quote_cols:
            if pd.notna(row[col]) and str(row[col]).strip():
                non_empty_quotes += 1
        quote_data_counts.append((study_id, non_empty_quotes))
    
    # Sort by quote richness
    quote_data_counts.sort(key=lambda x: x[1], reverse=True)
    
    print(f"\n📈 STUDIES WITH MOST QUOTE DATA:")
    for study_id, count in quote_data_counts[:10]:
        study_title = df[df['Study_ID'] == study_id]['Title'].iloc[0]
        print(f"   Study {study_id}: {count} quote columns filled - {str(study_title)[:50]}...")
    
    # Detailed examination of top 3 studies
    print(f"\n🔬 DETAILED QUOTE CONTENT (Top 3 Studies):")
    
    for i, (study_id, count) in enumerate(quote_data_counts[:3]):
        print(f"\n  --- STUDY {study_id} ({count} quote columns) ---")
        study_row = df[df['Study_ID'] == study_id].iloc[0]
        
        # Show non-empty quote columns
        quote_samples = 0
        for col in quote_cols:
            if pd.notna(study_row[col]) and str(study_row[col]).strip():
                quote_samples += 1
                if quote_samples <= 5:  # Show first 5 quote columns
                    field_name = col.replace('Supporting_quotes_for_', '').replace('_', ' ').strip('_')
                    raw_content = str(study_row[col])
                    print(f"\n    {field_name}:")
                    print(f"      Length: {len(raw_content)} chars")
                    print(f"      Preview: {raw_content[:200]}...")
                    
                    # Try to parse it
                    if '"' in raw_content:
                        import re
                        quotes = re.findall(r'"([^"]{10,})"', raw_content)
                        print(f"      Parsed {len(quotes)} quoted strings:")
                        for j, q in enumerate(quotes[:2], 1):
                            print(f"        {j}. {q[:80]}...")
                    else:
                        print("      No quoted strings found - treating as single quote")
                        lines = raw_content.split('\n')
                        if len(lines) > 1:
                            print(f"      Contains {len(lines)} lines")
                            for j, line in enumerate(lines[:3], 1):
                                if line.strip():
                                    print(f"        {j}. {line.strip()[:80]}...")
        
        if quote_samples > 5:
            print(f"    ... and {quote_samples - 5} more quote columns")
    
    # Check data quality issues
    print(f"\n⚠️  DATA QUALITY ANALYSIS:")
    
    # Missing data
    missing_data = df.isnull().sum()
    high_missing = missing_data[missing_data > len(df) * 0.5].sort_values(ascending=False)
    print(f"   Columns with >50% missing data ({len(high_missing)}):")
    for col, count in high_missing.head(10).items():
        pct = (count / len(df)) * 100
        print(f"     {col}: {count}/{len(df)} ({pct:.1f}%)")
    
    # Check for encoding issues
    print(f"\n🔤 ENCODING & SPECIAL CHARACTERS:")
    special_chars = {}
    for col in quote_cols[:5]:  # Check first 5 quote columns
        for idx, row in df.iterrows():
            if pd.notna(row[col]):
                content = str(row[col])
                for char in content:
                    if ord(char) > 127:  # Non-ASCII
                        if char not in special_chars:
                            special_chars[char] = 0
                        special_chars[char] += 1
    
    if special_chars:
        print(f"   Found {len(special_chars)} types of non-ASCII characters:")
        sorted_chars = sorted(special_chars.items(), key=lambda x: x[1], reverse=True)
        for char, count in sorted_chars[:10]:
            print(f"     '{char}' (U+{ord(char):04X}): {count} occurrences")
    else:
        print("   No non-ASCII characters found in quote samples")
    
    print(f"\n✅ ANALYSIS COMPLETE")
    print("=" * 80)

if __name__ == '__main__':
    examine_csv_content()
