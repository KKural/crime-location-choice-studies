#!/usr/bin/env python3
"""
Direct Variables Analysis Script
Analyze which direct variables are available and have values for Study 1
"""

import pandas as pd
import sys

def analyze_direct_variables():
    """Analyze direct variables in the dataset"""
    
    try:
        # Try different encodings
        df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='utf-8')
    except UnicodeDecodeError:
        try:
            df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='latin-1')
        except UnicodeDecodeError:
            df = pd.read_csv('20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv', encoding='cp1252')
    
    # Get Study 1 data
    study1 = df[df['Study_ID'] == 1]
    if study1.empty:
        print("Study 1 not found!")
        return
    
    study1_row = study1.iloc[0]
    
    # Define direct variables by category
    VARIABLE_CATEGORIES = {
        'location': {'Country', 'City', 'State_or_Region', 'Spatial_Unit_Name'},
        'temporal': {'Data_Collection_Period'},
        'crime': {'Crime_Type', 'Crime_Incidents', 'Crime_Type_Group'},
        'methodology': {'Model_Type', 'Data_Sources', 'Number_of_Data_Sources', 'Software_Used'},
        'spatial': {'Unit_Size', 'Total_Study_Area_Size', 'Number_of_Units', 'Average_Population_per_Unit'},
        'analytical': {'Independent_Variables', 'Model_Fit_Statistics', 'Coefficients'},
        'quality': {'Data_Limitations', 'MAUP_Discussion', 'Sensitivity_Analysis'},
        'future': {'Future_Suggestions'}
    }
    
    print("="*80)
    print("DIRECT VARIABLES ANALYSIS - STUDY 1")
    print("="*80)
    print()
    
    # Analyze by category
    total_variables = 0
    variables_with_values = 0
    
    for category, variables in VARIABLE_CATEGORIES.items():
        print(f"📂 {category.upper()} VARIABLES:")
        print("-" * 40)
        
        category_has_values = False
        for var_name in sorted(variables):
            total_variables += 1
            
            if var_name in study1_row and not pd.isna(study1_row[var_name]):
                value = str(study1_row[var_name]).strip()
                if value and value.lower() not in ['n/a', 'na', '-', '--', '']:
                    print(f"  ✅ {var_name}: {value[:60]}{'...' if len(value) > 60 else ''}")
                    variables_with_values += 1
                    category_has_values = True
                else:
                    print(f"  ❌ {var_name}: (empty/N/A)")
            else:
                print(f"  ❌ {var_name}: (not in dataset)")
        
        if not category_has_values:
            print(f"  🔍 No values found for {category} variables")
        
        print()
    
    print("="*80)
    print("SUMMARY:")
    print("="*80)
    print(f"Total direct variables defined: {total_variables}")
    print(f"Variables with values in Study 1: {variables_with_values}")
    print(f"Coverage: {(variables_with_values/total_variables)*100:.1f}%")
    print()
    
    # Show variables that will be highlighted
    print("🎯 VARIABLES THAT WILL BE HIGHLIGHTED:")
    print("-" * 50)
    highlighted_vars = []
    
    for category, variables in VARIABLE_CATEGORIES.items():
        for var_name in sorted(variables):
            if var_name in study1_row and not pd.isna(study1_row[var_name]):
                value = str(study1_row[var_name]).strip()
                if value and value.lower() not in ['n/a', 'na', '-', '--', '']:
                    highlighted_vars.append((category, var_name, value))
    
    for category, var_name, value in highlighted_vars:
        print(f"  {var_name} ({category}): {value[:50]}{'...' if len(value) > 50 else ''}")
    
    print(f"\nTotal: {len(highlighted_vars)} variables will be highlighted")
    
    return highlighted_vars

if __name__ == "__main__":
    analyze_direct_variables()
