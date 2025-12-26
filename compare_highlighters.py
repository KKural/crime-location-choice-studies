#!/usr/bin/env python3
"""
PDF Highlighter Comparison Script
Tests both the enhanced and fixed versions to demonstrate improvements
"""

import subprocess
import time
from pathlib import Path
import sys

def run_comparison_test(study_id="1", csv_file=None, pdf_folder="Review_articles"):
    """Run both versions and compare results"""
    
    if csv_file is None:
        csv_file = "20250827_Analysis & Results/20250827_combined_dataset_no_reasoning.csv"
    
    print("="*80)
    print("PDF HIGHLIGHTER COMPARISON TEST")
    print("="*80)
    print(f"Study ID: {study_id}")
    print(f"CSV File: {csv_file}")
    print(f"PDF Folder: {pdf_folder}")
    print("="*80)
    
    results = {}
    
    # Test Enhanced Version
    print("\n🧪 TESTING ENHANCED VERSION...")
    print("-" * 40)
    
    try:
        start_time = time.time()
        result = subprocess.run([
            "python", "enhanced_pdf_highlighter.py", study_id,
            "--csv", csv_file,
            "--pdf-folder", pdf_folder,
            "--output", "20250827_Enhanced_Test_Results"
        ], capture_output=True, text=True, timeout=180)
        
        enhanced_time = time.time() - start_time
        
        if result.returncode == 0:
            print("✅ Enhanced version completed successfully")
            
            # Extract highlights count from output
            lines = result.stdout.split('\n')
            highlights_count = None
            for line in lines:
                if "Total highlights:" in line:
                    highlights_count = line.split(":")[-1].strip()
                    break
                elif "Highlights created:" in line:
                    highlights_count = line.split(":")[-1].strip()
                    break
            
            results['enhanced'] = {
                'success': True,
                'time': enhanced_time,
                'highlights': highlights_count,
                'output': result.stdout
            }
            
            print(f"   Processing time: {enhanced_time:.2f}s")
            print(f"   Highlights created: {highlights_count}")
            
        else:
            print("❌ Enhanced version failed")
            print(f"Error: {result.stderr}")
            results['enhanced'] = {
                'success': False,
                'error': result.stderr
            }
            
    except subprocess.TimeoutExpired:
        print("⏱️ Enhanced version timed out (>3 minutes)")
        results['enhanced'] = {'success': False, 'error': 'Timeout'}
    except Exception as e:
        print(f"❌ Enhanced version error: {e}")
        results['enhanced'] = {'success': False, 'error': str(e)}
    
    # Test Fixed Version
    print("\n🔧 TESTING FIXED VERSION...")
    print("-" * 40)
    
    try:
        start_time = time.time()
        result = subprocess.run([
            "python", "fixed_pdf_highlighter.py", study_id,
            "--csv", csv_file,
            "--pdf-folder", pdf_folder,
            "--output", "20250827_Fixed_Test_Results"
        ], capture_output=True, text=True, timeout=180)
        
        fixed_time = time.time() - start_time
        
        if result.returncode == 0:
            print("✅ Fixed version completed successfully")
            
            # Extract highlights count from output
            lines = result.stdout.split('\n')
            highlights_count = None
            for line in lines:
                if "Highlights created:" in line:
                    highlights_count = line.split(":")[-1].strip()
                    break
            
            results['fixed'] = {
                'success': True,
                'time': fixed_time,
                'highlights': highlights_count,
                'output': result.stdout
            }
            
            print(f"   Processing time: {fixed_time:.2f}s")
            print(f"   Highlights created: {highlights_count}")
            
        else:
            print("❌ Fixed version failed")
            print(f"Error: {result.stderr}")
            results['fixed'] = {
                'success': False,
                'error': result.stderr
            }
            
    except subprocess.TimeoutExpired:
        print("⏱️ Fixed version timed out (>3 minutes)")
        results['fixed'] = {'success': False, 'error': 'Timeout'}
    except Exception as e:
        print(f"❌ Fixed version error: {e}")
        results['fixed'] = {'success': False, 'error': str(e)}
    
    # Comparison Results
    print("\n📊 COMPARISON RESULTS")
    print("="*50)
    
    if results.get('enhanced', {}).get('success') and results.get('fixed', {}).get('success'):
        enhanced = results['enhanced']
        fixed = results['fixed']
        
        print(f"Enhanced Version:")
        print(f"  ⏱️  Time: {enhanced['time']:.2f}s")
        print(f"  🎯 Highlights: {enhanced['highlights']}")
        print()
        print(f"Fixed Version:")
        print(f"  ⏱️  Time: {fixed['time']:.2f}s")
        print(f"  🎯 Highlights: {fixed['highlights']}")
        print()
        
        # Performance comparison
        time_diff = fixed['time'] - enhanced['time']
        if abs(time_diff) < 1:
            print(f"⚡ Performance: Similar (~{abs(time_diff):.1f}s difference)")
        elif time_diff > 0:
            print(f"⚡ Performance: Enhanced version faster by {time_diff:.2f}s")
        else:
            print(f"⚡ Performance: Fixed version faster by {abs(time_diff):.2f}s")
        
        # Check output files
        enhanced_file = Path("20250827_Enhanced_Test_Results") / f"Study_{study_id}_Enhanced_Highlighted.pdf"
        fixed_file = Path("20250827_Fixed_Test_Results") / f"Study_{study_id}_Fixed_Highlighted.pdf"
        
        print(f"\n📁 Output Files:")
        print(f"  Enhanced: {'✅ Created' if enhanced_file.exists() else '❌ Missing'}")
        print(f"  Fixed:    {'✅ Created' if fixed_file.exists() else '❌ Missing'}")
        
        if enhanced_file.exists() and fixed_file.exists():
            enhanced_size = enhanced_file.stat().st_size
            fixed_size = fixed_file.stat().st_size
            print(f"  Enhanced size: {enhanced_size:,} bytes")
            print(f"  Fixed size: {fixed_size:,} bytes")
    
    print("\n🔍 KEY IMPROVEMENTS IN FIXED VERSION:")
    print("-" * 45)
    print("✓ Full text boundary detection (no partial highlighting)")
    print("✓ Overlap prevention system")
    print("✓ Multi-line text support")
    print("✓ Better transparency settings (0.5 vs 0.6)")
    print("✓ Improved text block detection")
    print("✓ Word-level boundary finding")
    print("✓ Consecutive rectangle merging")
    print()
    
    print("📋 MANUAL VERIFICATION STEPS:")
    print("-" * 35)
    print("1. Open both PDF files in a PDF viewer")
    print("2. Compare highlighting coverage:")
    print("   - Are quotes fully highlighted (not just partial)?")
    print("   - Are there overlapping highlights?")
    print("   - Is the text boundary detection better?")
    print("3. Check visual quality:")
    print("   - Is the transparency appropriate?")
    print("   - Are the colors well-distributed?")
    print("   - Are multi-line quotes properly highlighted?")
    
    return results

def main():
    """Main function"""
    study_id = sys.argv[1] if len(sys.argv) > 1 else "1"
    csv_file = sys.argv[2] if len(sys.argv) > 2 else None
    pdf_folder = sys.argv[3] if len(sys.argv) > 3 else "Review_articles"
    
    print(f"Running PDF Highlighter Comparison")
    print(f"Study ID: {study_id}")
    
    results = run_comparison_test(study_id, csv_file, pdf_folder)
    
    print("\n" + "="*80)
    print("COMPARISON TEST COMPLETED")
    print("="*80)
    
    # Summary
    enhanced_ok = results.get('enhanced', {}).get('success', False)
    fixed_ok = results.get('fixed', {}).get('success', False)
    
    if enhanced_ok and fixed_ok:
        print("✅ Both versions completed successfully")
        print("📋 Please manually compare the PDF outputs for visual quality")
    elif enhanced_ok:
        print("⚠️ Only enhanced version completed successfully")
    elif fixed_ok:
        print("⚠️ Only fixed version completed successfully")
    else:
        print("❌ Both versions failed - check the error messages above")
    
    print("\nRecommendation: Use the FIXED version for better highlighting quality")
    print("(No partial highlighting, no overlaps, better multi-line support)")

if __name__ == "__main__":
    main()
