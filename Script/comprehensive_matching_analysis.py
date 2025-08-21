#!/usr/bin/env python3
"""
Comprehensive CSV-PDF Matching Analysis
=====================================

This script will:
1. Extract ALL terms from each CSV cell
2. Test manual matching against the PDF  
3. Show exactly what matches vs what doesn't
4. Identify the real issues
"""

import pandas as pd
import fitz
from pathlib import Path
import re
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class ComprehensiveMatchingAnalyzer:
    def __init__(self):
        self.base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
        self.csv_path = self.base_dir / "20250820_Analysis & Results/20250820_combined_dataset.csv"
        self.pdf_path = self.base_dir / "Review_articles/01_a_discrete_spatial_choice_model_of_burglary_target.pdf"
        
        # Key variables to analyze
        self.key_variables = [
            'Country', 'City', 'Total_Study_Area_Size', 'Spatial_Unit_Name', 'Unit_Size',
            'Number_of_Units', 'Crime_Incidents', 'Crime_Type', 'Data_Sources',
            'Model_Type', 'Independent_Variables', 'Software_Used', 'Model_Fit_Statistics',
            'Coefficients'
        ]

    def extract_all_terms_from_value(self, value, variable_name):
        """Extract ALL possible searchable terms from a CSV value."""
        if pd.isna(value) or str(value).strip() == '':
            return []
        
        value_str = str(value).strip()
        terms = []
        
        print(f"\n📋 ANALYZING: {variable_name}")
        print(f"   Raw Value: {value_str[:200]}{'...' if len(value_str) > 200 else ''}")
        
        # 1. Original value
        terms.append(('Original', value_str))
        
        # 2. Extract numbers (all types)
        numbers = re.findall(r'\d+[,.]?\d*\.?\d*%?', value_str)
        for num in numbers:
            if len(num) > 0:
                terms.append(('Number', num))
        
        # 3. Extract words/phrases (3+ characters)
        words = re.findall(r'\b[A-Za-z]{3,}\b', value_str)
        for word in words:
            terms.append(('Word', word))
        
        # 4. Extract quoted text
        quotes = re.findall(r'"([^"]+)"', value_str)
        for quote in quotes:
            terms.append(('Quote', quote))
        
        # 5. Extract parenthetical content
        parens = re.findall(r'\(([^)]+)\)', value_str)
        for paren in parens:
            terms.append(('Parenthetical', paren))
        
        # 6. Split on common delimiters
        for delimiter in [';', ':', '-', ',']:
            if delimiter in value_str:
                parts = [p.strip() for p in value_str.split(delimiter)]
                for part in parts:
                    if len(part) > 2:
                        terms.append(('Split', part))
        
        # 7. Special handling for specific variables
        if 'Model' in variable_name:
            model_patterns = ['logit', 'regression', 'choice', 'conditional', 'nested']
            for pattern in model_patterns:
                if pattern.lower() in value_str.lower():
                    # Extract model name around this pattern
                    words = value_str.split()
                    for i, word in enumerate(words):
                        if pattern.lower() in word.lower():
                            start = max(0, i-1)
                            end = min(len(words), i+3)
                            model_name = ' '.join(words[start:end])
                            terms.append(('ModelName', model_name))
        
        print(f"   → Extracted {len(terms)} terms")
        for term_type, term in terms[:10]:  # Show first 10
            print(f"      {term_type}: '{term}'")
        if len(terms) > 10:
            print(f"      ... and {len(terms) - 10} more")
        
        return terms

    def test_pdf_matching(self, terms, variable_name):
        """Test each term against the PDF and report results."""
        if not self.pdf_path.exists():
            print(f"❌ PDF not found: {self.pdf_path}")
            return {}
        
        doc = fitz.open(str(self.pdf_path))
        results = {}
        
        print(f"\n🔍 TESTING PDF MATCHES for {variable_name}:")
        
        for term_type, term in terms:
            matches_found = []
            total_instances = 0
            
            # Search across all pages
            for page_num in range(len(doc)):
                page = doc[page_num]
                instances = page.search_for(term)
                if instances:
                    total_instances += len(instances)
                    matches_found.append(page_num + 1)
            
            if total_instances > 0:
                status = "✅ FOUND"
                print(f"   {status}: '{term}' ({term_type}) - {total_instances} instances on pages {matches_found}")
            else:
                status = "❌ NOT FOUND"
                print(f"   {status}: '{term}' ({term_type})")
            
            results[term] = {
                'term_type': term_type,
                'instances': total_instances,
                'pages': matches_found,
                'found': total_instances > 0
            }
        
        doc.close()
        return results

    def analyze_study_1(self):
        """Comprehensive analysis of Study 1 data."""
        print("="*80)
        print("COMPREHENSIVE CSV-PDF MATCHING ANALYSIS")
        print("="*80)
        
        # Load CSV data
        if not self.csv_path.exists():
            print(f"❌ CSV not found: {self.csv_path}")
            return
        
        df = pd.read_csv(self.csv_path)
        study_1 = df[df['Study_ID'] == 1].iloc[0]
        
        print(f"📊 Study: {study_1['Title']}")
        print(f"📄 PDF: {self.pdf_path.name}")
        
        all_results = {}
        total_terms = 0
        total_found = 0
        
        # Analyze each key variable
        for variable in self.key_variables:
            if variable in study_1.index:
                value = study_1[variable]
                
                # Extract all terms
                terms = self.extract_all_terms_from_value(value, variable)
                total_terms += len(terms)
                
                # Test PDF matching
                match_results = self.test_pdf_matching(terms, variable)
                
                # Count found terms
                found_count = sum(1 for result in match_results.values() if result['found'])
                total_found += found_count
                
                all_results[variable] = {
                    'raw_value': str(value)[:100] + ('...' if len(str(value)) > 100 else ''),
                    'terms_extracted': len(terms),
                    'terms_found': found_count,
                    'match_rate': (found_count / len(terms) * 100) if len(terms) > 0 else 0,
                    'matches': match_results
                }
                
                print(f"   📈 {variable}: {found_count}/{len(terms)} terms found ({found_count/len(terms)*100:.1f}%)")
        
        # Final summary
        print("\n" + "="*80)
        print("FINAL ANALYSIS SUMMARY")
        print("="*80)
        
        overall_rate = (total_found / total_terms * 100) if total_terms > 0 else 0
        print(f"🎯 OVERALL MATCH RATE: {total_found}/{total_terms} ({overall_rate:.1f}%)")
        
        print(f"\n📊 BY VARIABLE:")
        for variable, result in all_results.items():
            rate = result['match_rate']
            status = "✅" if rate > 50 else "⚠️" if rate > 20 else "❌"
            print(f"   {status} {variable}: {result['terms_found']}/{result['terms_extracted']} ({rate:.1f}%)")
        
        # Identify issues
        print(f"\n🔍 ISSUE ANALYSIS:")
        
        low_match_vars = [v for v, r in all_results.items() if r['match_rate'] < 30]
        if low_match_vars:
            print(f"   📉 LOW MATCH RATE (<30%): {low_match_vars}")
        
        long_value_vars = [v for v, r in all_results.items() if len(r['raw_value']) > 80]
        if long_value_vars:
            print(f"   📝 LONG VALUES: {long_value_vars}")
        
        zero_match_vars = [v for v, r in all_results.items() if r['match_rate'] == 0]
        if zero_match_vars:
            print(f"   🚫 ZERO MATCHES: {zero_match_vars}")
        
        # Save detailed results
        results_path = self.base_dir / "Script/comprehensive_matching_analysis.txt"
        with open(results_path, 'w', encoding='utf-8') as f:
            f.write("COMPREHENSIVE CSV-PDF MATCHING ANALYSIS RESULTS\n")
            f.write("="*50 + "\n\n")
            f.write(f"Overall Match Rate: {overall_rate:.1f}% ({total_found}/{total_terms})\n\n")
            
            for variable, result in all_results.items():
                f.write(f"\nVARIABLE: {variable}\n")
                f.write(f"Raw Value: {result['raw_value']}\n")
                f.write(f"Match Rate: {result['match_rate']:.1f}% ({result['terms_found']}/{result['terms_extracted']})\n")
                f.write(f"Matches:\n")
                for term, match_info in result['matches'].items():
                    status = "FOUND" if match_info['found'] else "NOT FOUND"
                    f.write(f"  {status}: '{term}' ({match_info['term_type']})")
                    if match_info['found']:
                        f.write(f" - {match_info['instances']} instances on pages {match_info['pages']}")
                    f.write("\n")
        
        print(f"\n📝 Detailed results saved to: {results_path}")
        
        return all_results

if __name__ == "__main__":
    analyzer = ComprehensiveMatchingAnalyzer()
    analyzer.analyze_study_1()
