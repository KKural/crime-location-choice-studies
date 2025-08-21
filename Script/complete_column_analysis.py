import pandas as pd
import re

def analyze_study_columns():
    """Extract and analyze all column information for Study 1."""
    
    # Read the CSV data
    df = pd.read_csv('../20250819_Analysis & Results/20250819_combined_dataset.csv')
    study_1 = df[df['Study_ID'] == 1].iloc[0]
    
    print("="*80)
    print("COMPLETE STUDY 1 COLUMN ANALYSIS")
    print("="*80)
    
    # Define all important columns to analyze
    important_columns = [
        'Study_ID', 'Title', 'Citation', 'Year', 'Country', 'City',
        'Total_Study_Area_Size', 'Total_Study_Area_km2',
        'Spatial_Unit_Name', 'Unit_Size', 'Unit_Size_km2', 
        'Number_of_Units', 'Average_Population_per_Unit',
        'Crime_Type', 'Crime_Incidents',
        'Data_Collection_Period', 'Data_Sources',
        'Model_Type', 'Independent_Variables', 'Software_Used'
    ]
    
    # Analyze each column
    for i, column in enumerate(important_columns, 1):
        value = study_1.get(column, 'NOT_FOUND')
        value_str = str(value).strip()
        
        print(f"\n{i:2d}. {column}")
        print("-" * 50)
        print(f"Raw Value: '{value_str}'")
        
        # Check if it's meaningful data
        if value_str in ['nan', 'NOT_FOUND', '']:
            print("Status: EMPTY/NOT_AVAILABLE")
        else:
            print(f"Status: HAS DATA")
            print(f"Length: {len(value_str)} characters")
            
            # Extract different components for analysis
            if column in ['Total_Study_Area_Size', 'Total_Study_Area_km2', 'Unit_Size_km2']:
                # Analyze area/size values
                numbers = re.findall(r'\d+(?:\.\d+)?', value_str)
                units = re.findall(r'[a-zA-Z²³]+', value_str)
                print(f"Numbers found: {numbers}")
                print(f"Units found: {units}")
                
            elif column in ['Number_of_Units', 'Crime_Incidents', 'Average_Population_per_Unit']:
                # Analyze numeric values
                numbers = re.findall(r'\d+(?:,\d+)*(?:\.\d+)?', value_str)
                print(f"Numbers found: {numbers}")
                
            elif column in ['Data_Sources']:
                # Analyze multiple sources
                sources = [s.strip() for s in value_str.split(';') if s.strip()]
                print(f"Number of sources: {len(sources)}")
                for j, source in enumerate(sources, 1):
                    print(f"  Source {j}: '{source}'")
                    
            elif column in ['Independent_Variables']:
                # Analyze variables (might be long text)
                if len(value_str) > 100:
                    print(f"First 100 chars: '{value_str[:100]}...'")
                else:
                    print(f"Full text: '{value_str}'")
                    
            # Check for potential PDF matching challenges
            if len(value_str) > 50:
                print("⚠️  NOTE: Long text - may be hard to find exact match in PDF")
            elif any(char in value_str for char in ['(', ')', '"', ';', ',']):
                print("⚠️  NOTE: Contains special characters - may need flexible matching")
            elif re.search(r'\d+\.\d+', value_str):
                print("💡 NOTE: Contains decimals - may appear as whole numbers in PDF")
            elif ',' in value_str and re.search(r'\d+,\d+', value_str):
                print("💡 NOTE: Contains comma-separated numbers - PDF may have without commas")
    
    print("\n" + "="*80)
    print("EXTRACTION RECOMMENDATIONS")
    print("="*80)
    
    # Provide recommendations for what to extract
    extract_priority = {
        'HIGH_PRIORITY': [
            'Country', 'City', 'Crime_Type', 'Spatial_Unit_Name',
            'Data_Sources' # (individual sources)
        ],
        'MEDIUM_PRIORITY': [
            'Number_of_Units', 'Crime_Incidents', 
            'Total_Study_Area_km2' # (just the number)
        ],
        'LOW_PRIORITY': [
            'Year', 'Model_Type', 'Software_Used'
        ],
        'SKIP': [
            'Title', 'Citation', 'Independent_Variables', 'Unit_Size'
        ]
    }
    
    for priority, columns in extract_priority.items():
        print(f"\n{priority}:")
        for col in columns:
            if col in important_columns:
                value = str(study_1.get(col, '')).strip()
                if value and value != 'nan':
                    if priority == 'SKIP':
                        reason = "(Too long/complex for exact matching)"
                    elif priority == 'LOW_PRIORITY':
                        reason = "(Common words, may match too many places)"
                    else:
                        reason = "(Good for highlighting)"
                    print(f"  - {col}: '{value[:30]}{'...' if len(value) > 30 else ''}' {reason}")

if __name__ == "__main__":
    analyze_study_columns()
