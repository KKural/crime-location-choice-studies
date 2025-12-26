#!/usr/bin/env python3
"""
Direct Variables Highlighter Results Analysis
"""

import json
import pandas as pd
from pathlib import Path

def analyze_results():
    """Analyze the highlighting results"""
    
    results_file = "20250827_DirectVariables_Highlighted/Study_1_results.json"
    
    if not Path(results_file).exists():
        print("❌ Results file not found. Please run the highlighter first.")
        return
    
    with open(results_file, 'r') as f:
        data = json.load(f)
    
    print("="*80)
    print("DIRECT VARIABLES HIGHLIGHTER RESULTS ANALYSIS")
    print("="*80)
    print()
    
    print("📊 OVERALL PERFORMANCE:")
    print("-" * 30)
    print(f"Study ID: {data['study_id']}")
    print(f"Variables processed: {data['total_variables_processed']}")
    print(f"Variables found: {data['variables_found']}")
    print(f"Highlights created: {data['highlights_created']}")
    print(f"Success rate: {data['success_rate']:.1f}%")
    print(f"Processing time: {data['processing_time']:.2f}s")
    print(f"Output file: {data['output_file']}")
    print()
    
    # Analyze by category
    category_stats = {}
    found_vars = []
    not_found_vars = []
    
    for result in data['detailed_results']:
        var_name = result['variable_name']
        found = result['found']
        
        # Determine category
        categories = {
            'Location': ['Country', 'City', 'State_or_Region', 'Spatial_Unit_Name'],
            'Temporal': ['Data_Collection_Period'],
            'Crime': ['Crime_Type', 'Crime_Incidents', 'Crime_Type_Group'],
            'Methodology': ['Model_Type', 'Data_Sources', 'Number_of_Data_Sources', 'Software_Used'],
            'Spatial': ['Unit_Size', 'Total_Study_Area_Size', 'Number_of_Units', 'Average_Population_per_Unit'],
            'Analytical': ['Independent_Variables', 'Model_Fit_Statistics', 'Coefficients'],
            'Quality': ['Data_Limitations', 'MAUP_Discussion', 'Sensitivity_Analysis'],
            'Future': ['Future_Suggestions']
        }
        
        var_category = 'Unknown'
        for category, vars_list in categories.items():
            if var_name in vars_list:
                var_category = category
                break
        
        if var_category not in category_stats:
            category_stats[var_category] = {'total': 0, 'found': 0}
        
        category_stats[var_category]['total'] += 1
        if found:
            category_stats[var_category]['found'] += 1
            found_vars.append(result)
        else:
            not_found_vars.append(result)
    
    print("📈 PERFORMANCE BY CATEGORY:")
    print("-" * 40)
    for category, stats in sorted(category_stats.items()):
        success_rate = (stats['found'] / stats['total']) * 100
        status = "✅" if success_rate >= 70 else "⚠️" if success_rate >= 40 else "❌"
        print(f"{status} {category}: {stats['found']}/{stats['total']} ({success_rate:.1f}%)")
    print()
    
    print("🎯 SUCCESSFULLY HIGHLIGHTED VARIABLES:")
    print("-" * 50)
    for result in found_vars:
        page = result['page_number']
        highlighted = result['highlighted_text'][:60] + "..." if len(result['highlighted_text']) > 60 else result['highlighted_text']
        print(f"  ✅ {result['variable_name']} (Page {page})")
        print(f"      Text: \"{highlighted}\"")
        print(f"      Value: {result['value'][:50]}{'...' if len(result['value']) > 50 else ''}")
        print()
    
    print("❌ VARIABLES NOT FOUND:")
    print("-" * 30)
    for result in not_found_vars:
        print(f"  ❌ {result['variable_name']}")
        print(f"      Value: {result['value'][:80]}{'...' if len(result['value']) > 80 else ''}")
        print()
    
    print("🔍 ANALYSIS & RECOMMENDATIONS:")
    print("=" * 40)
    
    # Analyze patterns in not found variables
    not_found_categories = {}
    for result in not_found_vars:
        var_name = result['variable_name']
        for category, vars_list in {
            'Location': ['Country', 'City', 'State_or_Region', 'Spatial_Unit_Name'],
            'Temporal': ['Data_Collection_Period'],
            'Crime': ['Crime_Type', 'Crime_Incidents', 'Crime_Type_Group'],
            'Methodology': ['Model_Type', 'Data_Sources', 'Number_of_Data_Sources', 'Software_Used'],
            'Spatial': ['Unit_Size', 'Total_Study_Area_Size', 'Number_of_Units', 'Average_Population_per_Unit'],
            'Analytical': ['Independent_Variables', 'Model_Fit_Statistics', 'Coefficients'],
            'Quality': ['Data_Limitations', 'MAUP_Discussion', 'Sensitivity_Analysis'],
            'Future': ['Future_Suggestions']
        }.items():
            if var_name in vars_list:
                if category not in not_found_categories:
                    not_found_categories[category] = []
                not_found_categories[category].append(var_name)
                break
    
    for category, vars_list in not_found_categories.items():
        print(f"🔍 {category} Issues:")
        for var in vars_list:
            # Find the specific result
            var_result = next(r for r in not_found_vars if r['variable_name'] == var)
            value = var_result['value']
            
            if category == 'Analytical':
                print(f"  • {var}: Complex analytical text may need manual highlighting")
            elif category == 'Quality':
                print(f"  • {var}: Discussion sections often use different terminology")
            elif category == 'Future':
                print(f"  • {var}: Recommendations may be scattered across conclusion")
            elif category == 'Temporal':
                print(f"  • {var}: Date formats may need additional variants")
            else:
                print(f"  • {var}: May need additional search terms or be in different location")
        print()
    
    print("💡 SUGGESTED IMPROVEMENTS:")
    print("-" * 30)
    
    location_success = category_stats.get('Location', {}).get('found', 0) / category_stats.get('Location', {}).get('total', 1)
    spatial_success = category_stats.get('Spatial', {}).get('found', 0) / category_stats.get('Spatial', {}).get('total', 1)
    
    if location_success >= 0.7 and spatial_success >= 0.7:
        print("✅ Location and spatial variables work well with current approach")
    
    if category_stats.get('Analytical', {}).get('found', 0) == 0:
        print("🔧 Analytical variables need better search terms (tables, statistical results)")
    
    if category_stats.get('Quality', {}).get('found', 0) == 0:
        print("🔧 Quality discussions may need section-specific searching")
    
    print("\n📋 NEXT STEPS:")
    print("-" * 15)
    print("1. ✅ Current approach works well for basic variables")
    print("2. 🔧 Improve search terms for analytical variables")
    print("3. 📝 Consider manual highlighting for complex discussions")
    print("4. 🎯 Expand to other studies to validate patterns")
    
    print("\n" + "="*80)

if __name__ == "__main__":
    analyze_results()
