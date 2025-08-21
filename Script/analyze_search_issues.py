#!/usr/bin/env python3
"""
Analyze why search success rate is only 33% when it should be much higher
"""

import pandas as pd
import fitz
from pathlib import Path

# Load the data
csv_path = Path("../combined_elicit_data.csv")
df = pd.read_csv(csv_path, encoding='utf-8')

print("=== CSV COLUMNS ===")
print(df.columns.tolist())
print()

# Find Study 1
study_1_row = None
for idx, row in df.iterrows():
    if 'Study_ID' in df.columns:
        if row['Study_ID'] == 1:
            study_1_row = row
            break
    else:
        # Try first row as Study 1
        study_1_row = df.iloc[0]
        break

if study_1_row is None:
    print("Could not find Study 1 data")
    exit()

print(f"=== STUDY 1 DATA ANALYSIS ===")

# Check key variables that should definitely be found
key_variables = [
    'Country', 'City', 'Total_Study_Area_Size', 'Crime_Type', 
    'Number_of_Units', 'Crime_Incidents', 'Model_Type'
]

for var in key_variables:
    if var in study_1_row.index:
        value = str(study_1_row[var])
        print(f"{var}: '{value}'" + (f" (Length: {len(value)})" if len(value) > 50 else ""))

# Now test PDF search
print(f"\n=== PDF SEARCH TEST ===")
pdf_path = Path("../Review_articles/01_a_discrete_spatial_choice_model_of_burglary_target.pdf")

if pdf_path.exists():
    doc = fitz.open(str(pdf_path))
    
    # Test some key searches
    test_searches = [
        ("Belgium", "Country"),
        ("Ghent", "City"), 
        ("3000", "Total_Study_Area_Size"),
        ("house", "Spatial_Unit_Name"),
        ("503,589", "Number_of_Units"),
        ("650", "Crime_Incidents"),
        ("Conditional Logit Model", "Model_Type"),
        ("Residential burglary", "Crime_Type")
    ]
    
    for search_term, variable in test_searches:
        total_found = 0
        pages_found = []
        for page_num in range(len(doc)):
            page = doc[page_num]
            results = page.search_for(search_term)
            if results:
                total_found += len(results)
                pages_found.append(page_num + 1)
        
        status = "✓ FOUND" if total_found > 0 else "✗ MISSING"
        print(f"{status}: '{search_term}' ({variable}) - {total_found} instances on pages {pages_found}")
    
    doc.close()
else:
    print(f"PDF not found at: {pdf_path}")

# Check annotation issue
print(f"\n=== ANNOTATION ANALYSIS ===")
print("Annotations might appear as 'image-like' elements because:")
print("1. Text annotations (popup) can render as visual elements")
print("2. Multiple annotation types being added to same location")
print("3. Annotation positioning might overlap with highlights")
