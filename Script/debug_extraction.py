#!/usr/bin/env python3
"""
Debug script to check variable extraction
"""

import pandas as pd
import re
from pathlib import Path

# Load the CSV
csv_path = Path("../20250819_Analysis & Results/20250819_combined_dataset.csv")
df = pd.read_csv(csv_path)

# Get first study
study_row = df.iloc[0]

# All 30 target variables
target_variables = [
    'Country', 'City', 'Total_Study_Area_Size',
    'Spatial_Unit_Name', 'Unit_Size', 'Number_of_Units',
    'Average_Population_per_Unit', 'Spatial_Aggregation',
    'Unit_Selection_Rationale', 'Rationale_Category',
    'Data_Limitations', 'Computational_Constraints', 'Alternative_Units',
    'Data_Collection_Period', 'Data_Sources', 'Number_of_Data_Sources',
    'Crime_Type', 'Crime_Type_Group', 'Crime_Incidents',
    'Model_Type', 'Independent_Variables', 'Number_of_Variables',
    'Model_Fit_Statistics', 'Coefficients', 'Confidence_Intervals',
    'Effect_Sizes', 'Software_Used',
    'MAUP_Discussion', 'Sensitivity_Analysis', 'Future_Suggestions'
]

print("=== DEBUGGING VARIABLE EXTRACTION ===")
print(f"Study: {study_row['Title'][:50]}...")
print(f"Total variables to check: {len(target_variables)}")
print()

terms_by_variable = {var: [] for var in target_variables}

for variable in target_variables:
    print(f"Processing: {variable}")

    if variable in study_row.index:
        value = study_row[variable]
        print(f"  Raw value: {repr(value)}")

        # Skip empty/null values
        if pd.isna(value) or str(value).strip() in ['-', 'N/A', 'n/a', 'NA', 'nan', '']:
            print(f"  -> SKIPPED (empty/null)")
            continue

        # Clean and split the value
        clean_value = str(value).strip()
        print(f"  Clean value: {clean_value[:100]}...")

        # Split on common delimiters
        terms = re.split(r'[;,\|&/]', clean_value)
        print(f"  Split terms: {terms}")

        for term in terms:
            term = term.strip()
            # Only add terms that are meaningful (length > 2)
            if len(term) > 2:
                terms_by_variable[variable].append(term)
                print(f"  -> ADDED: {term[:50]}...")
            else:
                print(f"  -> SKIPPED (too short): {term}")

        print(
            f"  Final terms for {variable}: {len(terms_by_variable[variable])} terms")
    else:
        print(f"  -> COLUMN NOT FOUND IN CSV")

    print()

# Summary
total_terms = sum(len(terms) for terms in terms_by_variable.values())
print(f"=== SUMMARY ===")
print(f"Total terms extracted: {total_terms}")
print(
    f"Variables with terms: {sum(1 for terms in terms_by_variable.values() if terms)}")

print("\nVariables with terms:")
for variable, terms in terms_by_variable.items():
    if terms:
        print(f"  {variable}: {len(terms)} terms")
        for term in terms[:3]:  # Show first 3
            print(f"    - {term[:50]}...")
        if len(terms) > 3:
            print(f"    ... and {len(terms) - 3} more")

print("\nVariables WITHOUT terms:")
for variable, terms in terms_by_variable.items():
    if not terms:
        print(f"  {variable}")
