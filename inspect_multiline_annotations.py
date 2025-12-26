#!/usr/bin/env python3
"""
Quick inspection script to check if multi-line annotations are working correctly.
Looks for annotations that span multiple quads (indicating multi-line highlights).
"""

import fitz
import sys
from pathlib import Path

def inspect_annotations(pdf_path):
    """Inspect annotations in the PDF to check for multi-line highlights."""
    doc = fitz.open(pdf_path)
    
    total_annots = 0
    multiline_annots = 0
    empty_contents = 0
    titles = []
    
    for page_num in range(len(doc)):
        page = doc[page_num]
        for annot in page.annots():
            if annot.type[1] == "Highlight":
                total_annots += 1
                
                # Check if annotation has multiple quads (multi-line)
                try:
                    vertices = annot.vertices
                    if vertices and len(vertices) > 8:  # More than 4 points = multi-line
                        multiline_annots += 1
                        print(f"Page {page_num + 1}: Multi-line annotation with {len(vertices)//8} quads")
                        print(f"  Title: {annot.info.get('title', 'No title')}")
                        print(f"  Content: '{annot.info.get('content', 'No content')}'")
                        print()
                except Exception as e:
                    pass
                
                # Check content
                try:
                    content = annot.info.get('content', '')
                    if not content or content.strip() == '':
                        empty_contents += 1
                    title = annot.info.get('title', '')
                    if title:
                        titles.append(title)
                except Exception:
                    pass
    
    doc.close()
    
    print(f"\n=== Annotation Summary ===")
    print(f"Total annotations: {total_annots}")
    print(f"Multi-line annotations: {multiline_annots}")
    print(f"Empty contents: {empty_contents}")
    print(f"Unique titles: {len(set(titles))}")
    
    if multiline_annots > 0:
        print(f"\n✅ Multi-line highlighting is working! Found {multiline_annots} multi-line annotations.")
    else:
        print(f"\n⚠️  No multi-line annotations detected. All highlights appear to be single-line.")
    
    return {
        'total_annots': total_annots,
        'multiline_annots': multiline_annots,
        'empty_contents': empty_contents,
        'unique_titles': len(set(titles))
    }

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python inspect_multiline_annotations.py <pdf_file>")
        sys.exit(1)
    
    pdf_file = sys.argv[1]
    if not Path(pdf_file).exists():
        print(f"Error: File {pdf_file} not found")
        sys.exit(1)
    
    inspect_annotations(pdf_file)
