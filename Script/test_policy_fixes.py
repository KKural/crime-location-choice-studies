#!/usr/bin/env python3
"""
Quick test to verify the policy-driven fixes are working correctly.
"""

from mode_based_highlighter import ModeBasedHighlighter

def test_supporting_column_filtering():
    """Test that Supporting_* columns for SKIP_SUPPORTING variables are filtered out."""
    
    print("🧪 Testing Supporting_* column filtering...")
    
    highlighter = ModeBasedHighlighter(study_id=1)
    
    # Test base variable extraction
    test_cases = [
        ("Supporting_quotes_for__Country_", "Country"),
        ("Supporting_quotes_for__City_", "City"),
        ("Supporting_quotes_for__Independent_Variables_", "Independent_Variables"),
        ("Supporting_Rationale", "Rationale")
    ]
    
    for col, expected_base in test_cases:
        actual_base = highlighter.base_var_from_supporting(col)
        print(f"  {col} → {actual_base} ({'✅' if actual_base == expected_base else '❌'})")
    
    # Test skip logic
    print("\n🔍 Testing skip logic for Supporting_* columns:")
    skip_cases = [
        ("Supporting_quotes_for__Country_", True),  # Should skip (Country in SKIP_SUPPORTING)
        ("Supporting_quotes_for__City_", True),    # Should skip (City in SKIP_SUPPORTING) 
        ("Supporting_quotes_for__Independent_Variables_", False),  # Should NOT skip (Independent_Variables in REQUIRE_SUPPORTING)
        ("Supporting_quotes_for__Effect_Sizes_", False),  # Should NOT skip (Effect_Sizes in REQUIRE_SUPPORTING)
    ]
    
    for col, should_skip in skip_cases:
        base = highlighter.base_var_from_supporting(col)
        will_skip = base and base in highlighter.SKIP_SUPPORTING
        result = "✅" if will_skip == should_skip else "❌"
        status = "SKIP" if will_skip else "PROCESS"
        print(f"  {col} (base: {base}) → {status} {result}")

def test_policy_buckets():
    """Test that the policy buckets contain the right variables."""
    
    print("\n📋 Testing policy buckets...")
    
    highlighter = ModeBasedHighlighter(study_id=1)
    
    print(f"SKIP_SUPPORTING ({len(highlighter.SKIP_SUPPORTING)}): {sorted(highlighter.SKIP_SUPPORTING)}")
    print(f"REQUIRE_SUPPORTING ({len(highlighter.REQUIRE_SUPPORTING)}): {sorted(highlighter.REQUIRE_SUPPORTING)}")  
    print(f"SUMMARY_OK ({len(highlighter.SUMMARY_OK)}): {sorted(highlighter.SUMMARY_OK)}")
    
    # Check for overlaps (should be none)
    overlap1 = highlighter.SKIP_SUPPORTING & highlighter.REQUIRE_SUPPORTING
    overlap2 = highlighter.SKIP_SUPPORTING & highlighter.SUMMARY_OK
    overlap3 = highlighter.REQUIRE_SUPPORTING & highlighter.SUMMARY_OK
    
    print(f"\n🔍 Overlap checks:")
    print(f"  SKIP ∩ REQUIRE: {overlap1 if overlap1 else '✅ None'}")
    print(f"  SKIP ∩ SUMMARY: {overlap2 if overlap2 else '✅ None'}")
    print(f"  REQUIRE ∩ SUMMARY: {overlap3 if overlap3 else '✅ None'}")

if __name__ == "__main__":
    print("🚀 Testing Policy-Driven Fixes")
    print("=" * 50)
    
    test_supporting_column_filtering()
    test_policy_buckets()
    
    print("\n🎉 Tests completed!")
