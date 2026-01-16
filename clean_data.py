"""
Clean SUoA dataset by removing supporting and reasoning columns
Keeps only the main variables for analysis
"""

import pandas as pd
import os

# File paths
input_file = r"c:\Users\kukumar\OneDrive - UGent\My Projects\Grafitti Project\Urban_Foraging_Archive\Articles_drafts\2024 Snatching\Data\2026013_working.xlsx"
output_file = r"c:\Users\kukumar\OneDrive - UGent\My Projects\Grafitti Project\Urban_Foraging_Archive\Articles_drafts\2024 Snatching\Data\20260113_cleaned.xlsx"

print(f"Reading file: {input_file}")
df = pd.read_excel(input_file)

print(f"\nOriginal dataset shape: {df.shape}")
print(f"Total columns: {df.shape[1]}")

# Identify columns to remove (containing 'supporting', 'reasoning', 'Supporting', 'Reasoning')
columns_to_remove = [col for col in df.columns if
                     'supporting' in col.lower() or
                     'reasoning' in col.lower() or
                     'Supporting' in col or
                     'Reasoning' in col]

print(f"\n{'='*80}")
print(f"COLUMNS TO REMOVE ({len(columns_to_remove)}):")
print(f"{'='*80}")
for i, col in enumerate(columns_to_remove, 1):
    print(f"{i:3d}. {col}")

# Remove the columns
df_cleaned = df.drop(columns=columns_to_remove)

print(f"\n{'='*80}")
print(f"REMAINING COLUMNS ({len(df_cleaned.columns)}):")
print(f"{'='*80}")
for i, col in enumerate(df_cleaned.columns, 1):
    print(f"{i:3d}. {col}")

print(f"\n{'='*80}")
print(f"SUMMARY:")
print(f"{'='*80}")
print(f"Original columns: {df.shape[1]}")
print(f"Removed columns:  {len(columns_to_remove)}")
print(f"Remaining columns: {df_cleaned.shape[1]}")
print(f"Rows: {df_cleaned.shape[0]}")

# Save cleaned dataset
print(f"\nSaving cleaned dataset to: {output_file}")
df_cleaned.to_excel(output_file, index=False)

# Also save as CSV for easier inspection
csv_file = output_file.replace('.xlsx', '.csv')
print(f"Also saving as CSV: {csv_file}")
df_cleaned.to_csv(csv_file, index=False)

print(f"\n✅ CLEANING COMPLETE!")
print(f"\nNext steps:")
print(f"1. Review the cleaned file: {output_file}")
print(f"2. Verify all necessary variables are retained")
print(f"3. Begin analysis with the clean dataset")
