#!/usr/bin/env python3
"""
Test Quote Matching
===================

This script allows you to test the quote matching functionality interactively.
You can see exactly how the text cleaning and matching works.
"""

import pandas as pd
import re
from pathlib import Path


class QuoteMatchTester:
    """Test quote matching functionality."""

    def __init__(self, csv_file: str):
        self.csv_file = Path(csv_file)
        self.df = None
        self.load_data()

    def load_data(self):
        """Load the CSV data."""
        try:
            self.df = pd.read_csv(self.csv_file)
            print(f"✓ Loaded {len(self.df)} studies from CSV")
        except Exception as e:
            print(f"✗ Error loading CSV: {e}")

    def clean_text(self, text: str) -> str:
        """Clean text for better matching (same as in main script)."""
        if pd.isna(text) or text == "":
            return ""

        # Remove extra whitespace and normalize
        text = re.sub(r'\s+', ' ', str(text).strip())
        # Remove common artifacts
        text = re.sub(r'^[-"\s]+|[-"\s]+$', '', text)
        return text

    def get_supporting_quote_columns(self) -> list:
        """Get all supporting quote columns."""
        if self.df is None:
            return []

        quote_columns = [
            col for col in self.df.columns if 'Supporting_quotes_for' in col]
        return quote_columns

    def show_study_info(self, study_index: int):
        """Show information about a specific study."""
        if self.df is None or study_index >= len(self.df):
            print(f"✗ Invalid study index. Available: 0-{len(self.df)-1}")
            return

        row = self.df.iloc[study_index]
        print(
            f"\n📖 Study {study_index + 1}: {row.get('Title', 'No title')[:80]}...")
        print(f"   Authors: {row.get('Authors', 'No authors')[:60]}...")
        print(f"   Country: {row.get('Country', 'No country')}")
        print(f"   Year: {row.get('Year', 'No year')}")

    def show_quotes_for_study(self, study_index: int, variable_filter: str = ""):
        """Show all quotes for a specific study."""
        if self.df is None or study_index >= len(self.df):
            print(f"✗ Invalid study index. Available: 0-{len(self.df)-1}")
            return

        row = self.df.iloc[study_index]
        self.show_study_info(study_index)

        quote_columns = self.get_supporting_quote_columns()

        if variable_filter:
            quote_columns = [
                col for col in quote_columns if variable_filter.lower() in col.lower()]

        total_quotes = 0

        print(f"\n📝 Supporting Quotes:")
        print("=" * 80)

        for col in quote_columns:
            quote_text = self.clean_text(row[col])
            if quote_text and quote_text not in ['-', 'N/A', 'n/a', 'NA']:
                # Extract variable name
                variable_name = col.replace('Supporting_quotes_for__', '').replace(
                    'Supporting_quotes_for_', '')
                variable_name = re.sub(
                    r'^_+|_+$', '', variable_name).replace('_', ' ')

                # Split multiple quotes
                quotes = self.split_quotes(quote_text)
                total_quotes += len(quotes)

                print(f"\n🏷️  Variable: {variable_name}")
                print(f"   Raw text length: {len(quote_text)} characters")
                print(f"   Number of quotes: {len(quotes)}")

                for i, quote in enumerate(quotes, 1):
                    print(
                        f"   Quote {i}: {quote[:100]}{'...' if len(quote) > 100 else ''}")

        print(f"\n📊 Total quotes found: {total_quotes}")

    def split_quotes(self, quote_text: str) -> list:
        """Split text that may contain multiple quotes (same as main script)."""
        if not quote_text:
            return []

        # Primary split on the delimiter pattern used in the dataset: " - " (inside quotes)
        quotes = re.split(r'\s*-\s*', quote_text)

        # Clean up quotes
        cleaned_quotes = []
        for quote in quotes:
            # Remove leading/trailing quotes and whitespace
            quote = re.sub(r'^["\s]+|["\s]+$', '', quote.strip())

            # Skip very short fragments
            if len(quote) < 15:
                continue

            # Split on other common delimiters if the quote is very long
            if len(quote) > 500:
                sub_quotes = re.split(r'[.!?]+\s*(?=[A-Z])', quote)
                for sub_quote in sub_quotes:
                    sub_quote = sub_quote.strip()
                    if len(sub_quote) > 20:
                        cleaned_quotes.append(sub_quote)
            else:
                cleaned_quotes.append(quote)

        # If no good splits found, return the whole text cleaned
        if not cleaned_quotes:
            cleaned_text = re.sub(r'^["\s]+|["\s]+$', '', quote_text.strip())
            if len(cleaned_text) > 15:
                cleaned_quotes.append(cleaned_text)

        return cleaned_quotes

    def test_quote_matching(self, quote: str, test_text: str):
        """Test how a quote would match against text."""
        print(f"\n🔍 Testing Quote Matching")
        print("=" * 50)
        print(f"Quote: {quote[:100]}{'...' if len(quote) > 100 else ''}")
        print(f"Test text length: {len(test_text)} characters")

        # Clean the quote for matching
        quote_clean = quote.strip().lower()
        test_text_lower = test_text.lower()

        print(
            f"\nCleaned quote: {quote_clean[:100]}{'...' if len(quote_clean) > 100 else ''}")

        # Test exact matching
        if quote_clean in test_text_lower:
            start_pos = test_text_lower.find(quote_clean)
            print(f"✓ EXACT MATCH found at position {start_pos}")

            # Show context
            context_start = max(0, start_pos - 50)
            context_end = min(len(test_text), start_pos +
                              len(quote_clean) + 50)
            context = test_text[context_start:context_end]

            print(f"Context: ...{context}...")
        else:
            print("✗ No exact match found")

    def interactive_test(self):
        """Run interactive testing."""
        print("\n🚀 Interactive Quote Matching Test")
        print("=" * 50)

        while True:
            print(f"\nOptions:")
            print(f"1. Show study info (0-{len(self.df)-1})")
            print(f"2. Show quotes for study")
            print(f"3. Test quote matching")
            print(f"4. Show supporting quote columns")
            print(f"5. Exit")

            choice = input("\nEnter choice (1-5): ").strip()

            if choice == "1":
                try:
                    study_idx = int(
                        input(f"Enter study index (0-{len(self.df)-1}): "))
                    self.show_study_info(study_idx)
                except ValueError:
                    print("Please enter a valid number")

            elif choice == "2":
                try:
                    study_idx = int(
                        input(f"Enter study index (0-{len(self.df)-1}): "))
                    variable_filter = input(
                        "Filter by variable name (optional): ").strip()
                    self.show_quotes_for_study(study_idx, variable_filter)
                except ValueError:
                    print("Please enter a valid number")

            elif choice == "3":
                quote = input("Enter quote to test: ").strip()
                test_text = input("Enter text to search in: ").strip()
                if quote and test_text:
                    self.test_quote_matching(quote, test_text)
                else:
                    print("Please enter both quote and test text")

            elif choice == "4":
                columns = self.get_supporting_quote_columns()
                print(f"\n📋 Supporting Quote Columns ({len(columns)} total):")
                for i, col in enumerate(columns, 1):
                    variable_name = col.replace('Supporting_quotes_for__', '').replace(
                        'Supporting_quotes_for_', '')
                    variable_name = re.sub(
                        r'^_+|_+$', '', variable_name).replace('_', ' ')
                    print(f"   {i}. {variable_name}")

            elif choice == "5":
                print("👋 Goodbye!")
                break

            else:
                print("Invalid choice. Please enter 1-5.")


def main():
    """Main function."""
    print("🔬 Quote Matching Test Tool")
    print("=" * 50)

    # Use the correct CSV file
    csv_file = "../20250731_Analysis & Results/20250731_combined_dataset.csv"

    tester = QuoteMatchTester(csv_file)
    if tester.df is not None:
        tester.interactive_test()


if __name__ == "__main__":
    main()
