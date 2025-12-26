#!/usr/bin/env python3
import pandas as pd
import re
from collections import Counter
from pathlib import Path

CSV_PATH = "20250820_Analysis & Results/20250820_combined_dataset.csv"

def normalize(s):
    if s is None: 
        return ""
    s = str(s)
    # Replace curly quotes with straight quotes
    s = s.replace(chr(8220), '"').replace(chr(8221), '"')  # " "
    s = s.replace(chr(8216), "'").replace(chr(8217), "'")  # ' '
    s = s.replace("\u00A0", " ")  # NBSP
    s = s.replace("\u00AD", "")   # soft hyphen
    s = s.replace("–", "-").replace("—", "-")
    return re.sub(r"\s+", " ", s)

def looks_like_quote_column(col):
    return col.startswith("Supporting_quotes_for__") or "quote" in col.lower()

def find_bullets(text):
    seps = set()
    if re.search(r"^\s*[-*]\s+", text, re.M): 
        seps.add("dash/star bullets")
    if "•" in text: 
        seps.add("bullet U+2022")
    if "\n" in text: 
        seps.add("newlines")
    if re.search(r'"\s*-\s*"', text): 
        seps.add('quote"-"-quote pattern')
    return seps

def has_curly_quotes(text):
    curly_chars = [chr(8220), chr(8221), chr(8216), chr(8217)]  # " " ' '
    return any(ch in text for ch in curly_chars)

def split_quotes_cell(cell):
    if not cell: 
        return []
    
    # Normalize curly quotes and other characters
    t = str(cell)
    t = t.replace(chr(8220), '"').replace(chr(8221), '"')  # " "
    t = t.replace(chr(8216), "'").replace(chr(8217), "'")  # ' '
    t = t.replace("\u00A0", " ").replace("\u00AD", "")
    
    # Split by lines and bullets
    lines = re.split(r"\r?\n|\u2022", t)
    chunks = []
    for ln in lines:
        ln = re.sub(r"^\s*[-*]\s*", "", ln.strip())
        if ln: 
            chunks.append(ln)
    joined = " ".join(chunks) if chunks else t

    # Extract text inside double quotes
    quoted = re.findall(r'"([^"]+)"', joined)
    if not quoted:
        # Fallback for malformed quotes
        quoted = [q.strip() for q in re.split(r'"\s*-\s*"', joined.strip('"')) if q.strip()]

    # Clean and deduplicate
    out, seen = [], set()
    for q in quoted:
        q = re.sub(r"\s+", " ", q.strip())
        if len(q) >= 6 and q.lower() not in seen:
            seen.add(q.lower())
            out.append(q)
    return out

def main():
    if not Path(CSV_PATH).exists():
        print(f"CSV not found: {CSV_PATH}")
        return
        
    df = pd.read_csv(CSV_PATH)
    print("=== SHAPE ===")
    print(f"Rows: {df.shape[0]}, Columns: {df.shape[1]}")
    
    print("\n=== COLUMNS ===")
    for i, col in enumerate(df.columns):
        print(f"{i+1:3}. {col}")

    # Basic null counts
    print("\n=== NULLS PER COLUMN (top 20) ===")
    nulls = df.isna().sum().sort_values(ascending=False)
    for col, count in nulls.head(20).items():
        print(f"{count:4} - {col}")

    # Identify quote columns
    quote_cols = [c for c in df.columns if looks_like_quote_column(c)]
    print(f"\n=== DETECTED QUOTE COLUMNS ({len(quote_cols)}) ===")
    for c in quote_cols: 
        print(f" - {c}")
    if not quote_cols:
        print("(!) No quote-looking columns found")

    # Collect formatting stats from quote columns
    print("\n=== QUOTE CELL FORMATTING SUMMARY ===")
    fmt_counter = Counter()
    curly_count = 0
    nbsp_count = 0
    shyp_count = 0
    total_cells = 0

    for c in quote_cols:
        for val in df[c].dropna().astype(str).tolist():
            total_cells += 1
            seps = find_bullets(val)
            for s in seps:
                fmt_counter[s] += 1
            if has_curly_quotes(val): 
                curly_count += 1
            if "\u00A0" in val: 
                nbsp_count += 1
            if "\u00AD" in val: 
                shyp_count += 1

    print(f"Cells inspected: {total_cells}")
    for k, v in fmt_counter.most_common():
        print(f" - {k}: {v}")
    print(f" - has curly quotes: {curly_count}")
    print(f" - has NBSP: {nbsp_count}")
    print(f" - has soft hyphen: {shyp_count}")

    # Sample rows to verify parsing
    print("\n=== SAMPLE PARSED QUOTES (first 3 rows with quotes) ===")
    shown = 0
    for idx, row in df.iterrows():
        samples = {}
        for c in quote_cols:
            cell = row.get(c, "")
            if isinstance(cell, str) and cell.strip():
                parsed = split_quotes_cell(cell)
                if parsed:
                    samples[c] = parsed[:3]  # Show up to 3 samples
        if samples:
            print(f"\nRow {idx}: Study_ID={row.get('Study_ID')}")
            for c, items in samples.items():
                print(f"  {c}:")
                for i, q in enumerate(items, 1):
                    print(f"    {i:2}. {q[:100]}{'...' if len(q)>100 else ''}")
            shown += 1
        if shown >= 3:
            break

    # Check expected simple columns
    SIMPLE = {
        'Country', 'City', 'State_or_Region', 'Data_Collection_Period',
        'Crime_Type', 'Model_Type', 'Data_Sources',
        'Number_of_Data_Sources', 'Crime_Incidents', 'Software_Used'
    }
    missing_simple = [c for c in SIMPLE if c not in df.columns]
    if missing_simple:
        print(f"\n(!) Missing expected simple columns: {missing_simple}")

    # Check Study_ID
    if "Study_ID" in df.columns:
        ids = df["Study_ID"]
        print("\n=== STUDY_ID STATS ===")
        print(f"dtype: {ids.dtype}")
        print(f"unique: {ids.nunique()} / total rows: {len(ids)}")
        if ids.isna().any():
            print("(!) Study_ID has NaNs")
        dups = ids[ids.duplicated(keep=False)]
        if not dups.empty:
            print(f"(!) Duplicated Study_IDs: {dups.head().tolist()}")
    else:
        print("\n(!) Study_ID column not found")

if __name__ == "__main__":
    main()
