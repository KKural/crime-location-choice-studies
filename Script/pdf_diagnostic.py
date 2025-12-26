#!/usr/bin/env python3
"""
PDF Text Diagnostic Tool
========================

Diagnostic tool to examine what text is actually extracted from PDFs
and test simple term matching.
"""

import pandas as pd
import fitz  # PyMuPDF
import re
from pathlib import Path

def diagnostic_pdf_analysis(study_id: int = 1):
    """Analyze PDF content and test basic term matching."""
    
    base_dir = Path("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")
    pdf_folder = base_dir / "Review_articles"
    
    # Find PDF file
    pattern = f"{study_id:02d}_*.pdf"
    matches = list(pdf_folder.glob(pattern))
    if not matches:
        print(f"No PDF found for study {study_id}")
        return
    
    pdf_path = matches[0]
    print(f"📄 Analyzing PDF: {pdf_path.name}")
    
    try:
        doc = fitz.open(str(pdf_path))
        print(f"📊 Total pages: {len(doc)}")
        
        # Test simple terms that should definitely be in the paper
        test_terms = [
            "Belgium", "Belgian", "Ghent", "East Flanders", 
            "house", "burglary", "burglars", "residential",
            "crime", "police", "data", "study",
            "2006", "2012", "650", "503"
        ]
        
        # Search first 3 pages for test terms
        for page_num in range(min(3, len(doc))):
            page = doc[page_num]
            page_text = page.get_text()
            
            print(f"\n--- PAGE {page_num + 1} ---")
            print(f"Text length: {len(page_text)} characters")
            
            # Show first 200 characters
            print("First 200 characters:")
            print(repr(page_text[:200]))
            
            # Test term matching
            found_terms = []
            for term in test_terms:
                if term.lower() in page_text.lower():
                    found_terms.append(term)
            
            print(f"Found terms: {found_terms}")
            
            # Test PyMuPDF search
            search_results = {}
            for term in ["Belgium", "house", "burglary", "650"]:
                try:
                    hits = page.search_for(term, flags=fitz.TEXT_IGNORECASE)
                    search_results[term] = len(hits)
                except Exception as e:
                    search_results[term] = f"Error: {e}"
            
            print(f"PyMuPDF search results: {search_results}")
            
        doc.close()
        
    except Exception as e:
        print(f"Error analyzing PDF: {e}")

if __name__ == "__main__":
    diagnostic_pdf_analysis(1)
