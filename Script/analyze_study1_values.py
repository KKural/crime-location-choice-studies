import pandas as pd

# Check what specific values are in Study 1 CSV
df = pd.read_csv('../20250819_Analysis & Results/20250819_combined_dataset.csv')
study_1 = df[df['Study_ID'] == 1].iloc[0]

print('=== STUDY 1 EXACT CSV VALUES ===')
important_fields = [
    'Country', 'City', 'Total_Study_Area_Size', 'Total_Study_Area_km2', 
    'Spatial_Unit_Name', 'Unit_Size', 'Unit_Size_km2', 'Number_of_Units', 
    'Crime_Type', 'Crime_Incidents', 'Data_Sources'
]

for field in important_fields:
    value = str(study_1[field])
    if value != 'nan' and value.strip():
        print(f'{field}: "{value}"')
        
print('\n=== AREA VALUES BREAKDOWN ===')
area_size = str(study_1['Total_Study_Area_Size'])
area_km2 = str(study_1['Total_Study_Area_km2'])
print(f'Total_Study_Area_Size: "{area_size}"')
print(f'Total_Study_Area_km2: "{area_km2}"')

# Check if area values are in different formats
import re
if area_size != 'nan':
    numbers = re.findall(r'\d+(?:\.\d+)?', area_size)
    print(f'Numbers found in area size: {numbers}')
