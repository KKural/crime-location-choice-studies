import pandas as pd
import numpy as np

# Read the CSV file with different encoding
try:
    df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='utf-8')
except UnicodeDecodeError:
    try:
        df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='latin-1')
    except UnicodeDecodeError:
        df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='cp1252')

# Display column names to check for duplicates
print("Column names:")
print(df.columns.tolist())

print("\n" + "="*50)
print("DUPLICATE COLUMNS:")
print("="*50)
duplicated_cols = df.columns[df.columns.duplicated()].tolist()
if duplicated_cols:
    print(duplicated_cols)
else:
    print("No duplicate columns found")

print("\n" + "="*50)
print("STUDY 1 DATA:")
print("="*50)

# Filter for Study 1
study1 = df[df['Study_ID'] == 1]

if not study1.empty:
    # Get all quote columns (those that start with 'Supporting_quotes_for')
    quote_columns = [col for col in df.columns if col.startswith('Supporting_quotes_for')]
    
    print(f"Found {len(quote_columns)} quote columns")
    print("\nQuote columns with content for Study 1:")
    
    for col in quote_columns:
        value = study1[col].iloc[0]
        if pd.notna(value) and str(value).strip() != "":
            print(f"\n{col}:")
            print("-" * len(col))
            # Split by line breaks and show each quote
            quotes = str(value).split('\n')
            for i, quote in enumerate(quotes, 1):
                if quote.strip():
                    print(f"{i}. {quote.strip()}")
                    
    print("\n" + "="*50)
    print("SUMMARY FOR STUDY 1:")
    print("="*50)
    
    # Count total quotes
    total_quotes = 0
    for col in quote_columns:
        value = study1[col].iloc[0]
        if pd.notna(value) and str(value).strip() != "":
            quotes = [q.strip() for q in str(value).split('\n') if q.strip()]
            total_quotes += len(quotes)
            
    print(f"Total number of individual quotes: {total_quotes}")
    
else:
    print("Study 1 not found in the dataset")
