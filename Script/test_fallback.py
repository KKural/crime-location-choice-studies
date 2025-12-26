#!/usr/bin/env python3
"""
Quick test to run the highlighting with the normalized fallback.
"""

from mode_based_highlighter import ModeBasedHighlighter
import sys

def main():
    print("🚀 Testing Enhanced PDF Highlighter with Normalized Fallback")
    print("=" * 60)
    
    study_id = 1
    highlighter = ModeBasedHighlighter(study_id)
    
    print(f"📖 Processing Study {study_id}...")
    
    try:
        result = highlighter.process_study()
        if result:
            print(f"\n🎉 Success!")
            print(f"✅ Highlights created: {result.get('highlights_created', 0)}")
            print(f"📊 Total terms searched: {result.get('total_searched', 0)}")
            print(f"📄 Output file: {result.get('output_file', 'N/A')}")
        else:
            print("❌ Processing failed")
            
    except Exception as e:
        print(f"❌ Error: {e}")
        return False
        
    return True

if __name__ == "__main__":
    main()
