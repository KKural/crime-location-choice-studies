import pandas as pd

# Read the CSV data
df = pd.read_csv(
    '../20250819_Analysis & Results/20250819_combined_dataset.csv')
study_1 = df[df['Study_ID'] == 1].iloc[0]

print('=== STUDY 1 - KEY VALUES TO HIGHLIGHT ===')
key_fields = [
    'Title', 'Citation', 'Year', 'Country', 'City',
    'Total_Study_Area_Size', 'Spatial_Unit_Name', 'Unit_Size',
    'Number_of_Units', 'Crime_Type', 'Crime_Incidents',
    'Data_Sources', 'Total_Study_Area_km2', 'Unit_Size_km2'
]

for field in key_fields:
    value = str(study_1[field])
    if value != 'nan' and value.strip():
        print(f'{field}: "{value}"')
