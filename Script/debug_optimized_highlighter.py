#!/usr/bin/env python3
"""
Debug Optimized Variable Highlighter
Quick test to see what's happening with the output files
"""

import pandas as pd
from pathlib import Path
import fitz
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)

def debug_optimized_output():
    """Debug the optimized highlighter output."""
    
    base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching/Script")
    
    # Check if folders exist
    print("🔍 Checking folder structure...")
    
    optimized_folder = base_dir / "20250820_Optimized_Highlighting"
    print(f"Optimized folder path: {optimized_folder}")
    print(f"Optimized folder exists: {optimized_folder.exists()}")
    
    if optimized_folder.exists():
        print(f"Contents: {list(optimized_folder.iterdir())}")
    else:
        print("❌ Optimized folder was not created!")
        # Try to create it manually
        try:
            optimized_folder.mkdir(exist_ok=True, parents=True)
            print("✅ Created optimized folder manually")
        except Exception as e:
            print(f"❌ Failed to create folder: {e}")
    
    # Check CSV data
    csv_path = base_dir / "20250820_Analysis & Results" / "20250820_combined_dataset.csv"
    print(f"\\nCSV path: {csv_path}")
    print(f"CSV exists: {csv_path.exists()}")
    
    if csv_path.exists():
        df = pd.read_csv(csv_path, encoding='utf-8')
        study_1_data = df[df['Study_ID'] == 1]
        print(f"Study 1 rows found: {len(study_1_data)}")
        
        if not study_1_data.empty:
            print("Study 1 data preview:")
            study_data = study_1_data.iloc[0]
            print(f"Country: {study_data.get('Country', 'N/A')}")
            print(f"City: {study_data.get('City', 'N/A')}")
            print(f"Crime_Type: {study_data.get('Crime_Type', 'N/A')}")
    
    # Check PDF
    pdf_dir = base_dir / "Review_articles"
    pdf_path = pdf_dir / "01_a_discrete_spatial_choice_model_of_burglary_target.pdf"
    print(f"\\nPDF path: {pdf_path}")
    print(f"PDF exists: {pdf_path.exists()}")
    
    if pdf_path.exists():
        try:
            doc = fitz.open(str(pdf_path))
            print(f"PDF pages: {len(doc)}")
            
            # Test search for a simple term
            page = doc[0]
            belgium_instances = page.search_for("Belgium")
            print(f"'Belgium' found: {len(belgium_instances)} times on page 1")
            
            doc.close()
        except Exception as e:
            print(f"❌ Error opening PDF: {e}")

if __name__ == "__main__":
    debug_optimized_output()
