import pandas as pd
import numpy as np

# Load the CSV
df = pd.read_csv('20260106_working.csv')

print('=== DATASET OVERVIEW ===')
print(f'Total rows: {len(df)}')
print(f'Total columns: {len(df.columns)}')

print('\n=== KEY VARIABLES AVAILABILITY ===')
key_vars = [
    'Title', 'Year', 'Country', 'City',
    'Spatial_Unit_Name', 'Unit_Size', 'Number_of_Units',
    'Average_Population_per_Unit', 'Spatial_Aggregation',
    'Rationale_Category', 'Data_Limitations',
    'Computational constraints (Yes/No)', 'Alternative_Units',
    'Crime_Type', 'Crime_Type_Group', 'Crime_Incidents',
    'Model_Type', 'Number_of_Variables',
    'MAUP_Discussion', 'Sensitivity_Analysis',
    'Data_Sources', 'Number_of_Data_Sources',
    'Independent_Variables', 'Model_Fit_Statistics',
    'Coefficients', 'Confidence_Intervals', 'Effect_Sizes',
    'Software_Used'
]

for var in key_vars:
    if var in df.columns:
        non_null = df[var].notna().sum()
        pct = (non_null/len(df))*100
        print(f'{var:45s}: {non_null:3d}/{len(df)} ({pct:5.1f}%) non-null')
    else:
        print(f'{var:45s}: NOT FOUND')

print('\n=== SAMPLE VALUES FOR KEY VARIABLES ===')
sample_vars = ['Unit_Size', 'Rationale_Category',
               'MAUP_Discussion', 'Sensitivity_Analysis']
for var in sample_vars:
    if var in df.columns:
        print(f'\n{var}:')
        print(df[var].value_counts().head(5))

print('\n=== UNIQUE VALUES COUNT ===')
count_vars = ['Country', 'Crime_Type_Group',
              'Model_Type', 'Rationale_Category']
for var in count_vars:
    if var in df.columns:
        print(f'{var:30s}: {df[var].nunique()} unique values')
