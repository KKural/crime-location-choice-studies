#!/usr/bin/env python3
"""
Complete Column Analysis for Study 1
Extract ALL information from each cell to understand what we can highlight
"""

import pandas as pd
import re
from pathlib import Path

def analyze_all_study1_data():
    """Extract and analyze ALL data from Study 1."""
    
    # Load the CSV
    csv_path = Path("../20250819_Analysis & Results/20250819_combined_dataset.csv")
    df = pd.read_csv(csv_path)
    study_1 = df[df['Study_ID'] == 1].iloc[0]
    
    print("="*80)
    print("COMPLETE STUDY 1 DATA EXTRACTION")
    print("="*80)
    
    # Get all column names
    all_columns = df.columns.tolist()
    
    print(f"\nTOTAL COLUMNS: {len(all_columns)}")
    print(f"STUDY ID: {study_1['Study_ID']}")
    print(f"STUDY TITLE: {study_1['Title']}")
    
    print("\n" + "="*80)
    print("ALL COLUMN DATA ANALYSIS")
    print("="*80)
    
    # Categories for organization
    categories = {
        'STUDY_INFO': ['Study_ID', 'Title', 'Citation', 'Year'],
        'GEOGRAPHIC': ['Country', 'City'],
        'STUDY_AREA': ['Total_Study_Area_Size', 'Total_Study_Area_km2'],
        'SPATIAL_UNITS': ['Spatial_Unit_Name', 'Unit_Size', 'Unit_Size_km2', 'Number_of_Units'],
        'POPULATION': ['Average_Population_per_Unit'],
        'SPATIAL_ANALYSIS': ['Spatial_Aggregation'],
        'RATIONALE': ['Unit_Selection_Rationale', 'Rationale_Category'],
        'LIMITATIONS': ['Data_Limitations', 'Computational_Constraints', 'Alternative_Units'],
        'DATA_COLLECTION': ['Data_Collection_Period', 'Data_Sources', 'Number_of_Data_Sources'],
        'CRIME_DATA': ['Crime_Type', 'Crime_Type_Group', 'Crime_Incidents'],
        'MODEL_INFO': ['Model_Type', 'Independent_Variables', 'Number_of_Variables'],
        'RESULTS': ['Model_Fit_Statistics', 'Coefficients', 'Confidence_Intervals', 'Effect_Sizes'],
        'TECHNICAL': ['Software_Used'],
        'DISCUSSION': ['MAUP_Discussion', 'Sensitivity_Analysis', 'Future_Suggestions'],
        'VERIFICATION': ['Verified_by_Kural', 'Verified_by_Stephanie']
    }
    
    # Track all potential highlight values
    all_highlight_values = []
    
    for category_name, column_list in categories.items():
        print(f"\n{'='*20} {category_name} {'='*20}")
        
        for column in column_list:
            if column in all_columns:
                value = str(study_1[column])
                
                print(f"\n📋 {column}:")
                print(f"   Raw Value: '{value}'")
                print(f"   Length: {len(value)} characters")
                
                if value != 'nan' and value.strip():
                    # Extract potential highlight terms
                    highlight_terms = extract_highlight_terms(value, column)
                    if highlight_terms:
                        print(f"   🎯 Highlight Terms: {highlight_terms}")
                        for term, term_type in highlight_terms:
                            all_highlight_values.append({
                                'column': column,
                                'category': category_name,
                                'term': term,
                                'term_type': term_type,
                                'original_value': value
                            })
                    else:
                        print(f"   ⚠️  No clear highlight terms identified")
                else:
                    print(f"   ❌ Empty/NaN value")
    
    # Check supporting quotes columns too
    print(f"\n{'='*20} SUPPORTING QUOTES {'='*20}")
    
    quote_columns = [col for col in all_columns if 'Supporting_quotes' in col]
    for column in quote_columns:
        if column in all_columns:
            value = str(study_1[column])
            if value != 'nan' and value.strip() and len(value) > 10:
                print(f"\n📋 {column}:")
                print(f"   Quote Length: {len(value)} characters")
                print(f"   First 100 chars: '{value[:100]}...'")
                
                # Extract key terms from quotes
                quote_terms = extract_terms_from_quotes(value)
                if quote_terms:
                    print(f"   🎯 Key Terms: {quote_terms[:5]}...")  # Show first 5
    
    print(f"\n{'='*80}")
    print("SUMMARY OF ALL HIGHLIGHT VALUES")
    print(f"{'='*80}")
    
    print(f"\nTOTAL POTENTIAL HIGHLIGHT TERMS: {len(all_highlight_values)}")
    
    # Group by category
    by_category = {}
    for item in all_highlight_values:
        cat = item['category']
        if cat not in by_category:
            by_category[cat] = []
        by_category[cat].append(item)
    
    for category, items in by_category.items():
        print(f"\n{category} ({len(items)} terms):")
        for item in items:
            print(f"  • '{item['term']}' ({item['term_type']}) from {item['column']}")
    
    return all_highlight_values

def extract_highlight_terms(value, column_name):
    """Extract specific terms that could be highlighted in PDF."""
    terms = []
    
    # Geographic terms
    if column_name in ['Country', 'City']:
        terms.append((value.strip(), 'geographic'))
    
    # Numbers and quantities
    elif column_name in ['Year', 'Crime_Incidents', 'Number_of_Units', 'Number_of_Variables', 'Number_of_Data_Sources']:
        # Extract numbers
        numbers = re.findall(r'\b\d+(?:,\d{3})*(?:\.\d+)?\b', value)
        for num in numbers:
            terms.append((num, 'number'))
    
    # Area and size terms
    elif 'Area' in column_name or 'Size' in column_name:
        # Extract numbers with units
        area_patterns = re.findall(r'\b\d+(?:[.,]\d+)*\s*(?:km²?|hectare|meter|mile)?\b', value, re.IGNORECASE)
        for pattern in area_patterns:
            terms.append((pattern.strip(), 'area'))
        
        # Also extract just numbers
        numbers = re.findall(r'\b\d+(?:[.,]\d+)?\b', value)
        for num in numbers:
            terms.append((num, 'area_number'))
    
    # Crime types
    elif 'Crime' in column_name:
        terms.append((value.strip(), 'crime_type'))
    
    # Data sources
    elif 'Data_Source' in column_name:
        # Split by semicolons or commas
        sources = re.split(r'[;,]', value)
        for source in sources:
            clean_source = source.strip()
            if clean_source and len(clean_source) > 3:
                terms.append((clean_source, 'data_source'))
    
    # Spatial units
    elif 'Unit' in column_name and 'Name' in column_name:
        terms.append((value.strip(), 'spatial_unit'))
    
    # Model types
    elif 'Model' in column_name:
        # Extract key model terms
        model_terms = re.findall(r'\b(?:logistic|regression|choice|spatial|discrete|multinomial)\b', value, re.IGNORECASE)
        for term in model_terms:
            terms.append((term, 'model_term'))
    
    # Software
    elif 'Software' in column_name:
        # Extract software names
        software_names = re.findall(r'\b(?:R|STATA|SPSS|Python|ArcGIS|MATLAB)\b', value, re.IGNORECASE)
        for software in software_names:
            terms.append((software, 'software'))
    
    # For other text fields, extract meaningful phrases
    else:
        if len(value.strip()) > 0 and value.strip() != 'nan':
            # Extract key phrases (3-50 characters)
            words = value.split()
            if len(words) <= 5:  # Short phrases only
                terms.append((value.strip(), 'text_phrase'))
    
    return terms

def extract_terms_from_quotes(quote_text):
    """Extract key terms from supporting quote text."""
    # Extract quoted text
    quoted_parts = re.findall(r'"([^"]+)"', quote_text)
    
    # Extract numbers
    numbers = re.findall(r'\b\d+(?:,\d{3})*(?:\.\d+)?\b', quote_text)
    
    # Extract place names (capitalized words)
    places = re.findall(r'\b[A-Z][a-z]+\b', quote_text)
    
    # Combine all
    all_terms = quoted_parts[:3] + numbers[:5] + places[:5]  # Limit each type
    
    return all_terms

if __name__ == "__main__":
    all_values = analyze_all_study1_data()
    print(f"\n🎯 TOTAL EXTRACTABLE TERMS: {len(all_values)}")
    print("\n✅ Analysis complete! Use this data to create targeted highlighting.")
