import pandas as pd
import re
from typing import Dict, List, Tuple


def verify_value_against_quotes(value: str, quotes: str, reasoning: str, column_name: str) -> Dict:
    """
    Verify if a value is supported by its quotes and reasoning.
    Returns a dictionary with verification results.
    """
    # Convert to string and handle NaN values properly
    value_display = str(value) if not pd.isna(value) else ''
    quotes_display = str(quotes)[:200] if not pd.isna(
        quotes) and str(quotes).strip() else ''
    reasoning_display = str(reasoning)[:200] if not pd.isna(
        reasoning) and str(reasoning).strip() else ''

    result = {
        'column': column_name,
        'value': value_display,
        'quotes': quotes_display,
        'reasoning': reasoning_display,
        'issues': []
    }

    # Skip if value is empty or NaN
    if pd.isna(value) or str(value).strip() == '':
        if not pd.isna(quotes) and str(quotes).strip():
            result['issues'].append('Value is empty but quotes are provided')
        return result

    value_str = str(value).strip().lower()

    # Check if quotes exist when value exists
    if pd.isna(quotes) or str(quotes).strip() == '':
        result['issues'].append('Value provided but no supporting quotes')
        return result

    quotes_str = str(quotes).lower()

    # For numeric values, check if the number appears in quotes
    if re.match(r'^[\d,\.]+\s*[a-z]*\s*\d*$', value_str.replace('²', '').replace('â', '')):
        # Extract numbers from value
        numbers_in_value = re.findall(r'\d+', value_str)
        numbers_in_quotes = re.findall(r'\d+', quotes_str)

        # Check if at least one number from value appears in quotes
        if numbers_in_value:
            found_match = False
            for num in numbers_in_value:
                if num in numbers_in_quotes:
                    found_match = True
                    break
            if not found_match:
                result['issues'].append(f'Numeric value not found in quotes')

    # For text values, check if key words appear in quotes
    else:
        # Extract key words from value (ignore common words)
        common_words = {'the', 'a', 'an', 'and', 'or', 'but',
                        'in', 'on', 'at', 'to', 'for', 'of', 'with', 'by'}
        value_words = [w for w in re.findall(
            r'\w+', value_str) if w not in common_words and len(w) > 2]

        if value_words:
            # Check if at least some key words appear in quotes
            found_words = [w for w in value_words if w in quotes_str]
            if len(found_words) / len(value_words) < 0.3:  # Less than 30% match
                result['issues'].append(
                    f'Key terms from value not found in quotes (only {len(found_words)}/{len(value_words)} matched)')

    return result


def analyze_csv(file_path: str) -> Tuple[List[Dict], pd.DataFrame]:
    """
    Analyze the CSV file and verify all values against their supporting quotes.
    Returns a list of issues found and the dataframe.
    """
    # Read the CSV
    df = pd.read_csv(file_path, encoding='utf-8')

    print(f"Total rows: {len(df)}")
    print(f"Total columns: {len(df.columns)}")

    # Get all main columns (those without Supporting_quotes or Reasoning suffixes)
    main_columns = [col for col in df.columns
                    if not col.startswith('Supporting_quotes_for_')
                    and not col.startswith('Reasoning_for_')]

    print(f"Main data columns: {len(main_columns)}")

    all_issues = []

    # Analyze each row
    for idx, row in df.iterrows():
        row_issues = []

        # Check each main column
        for col in main_columns:
            # Skip metadata columns
            if col in ['Title', 'Authors', 'Verified_by_Kural', 'Verified_by_Stephanie']:
                continue

            # Get corresponding quote and reasoning columns
            quote_col = f'Supporting_quotes_for__{col}_'
            reasoning_col = f'Reasoning_for__{col}_'

            if quote_col in df.columns and reasoning_col in df.columns:
                value = row[col]
                quotes = row[quote_col]
                reasoning = row[reasoning_col]

                verification = verify_value_against_quotes(
                    value, quotes, reasoning, col)

                if verification['issues']:
                    verification['row_index'] = idx
                    verification['study_title'] = row['Title'][:
                                                               100] if 'Title' in row else 'Unknown'
                    row_issues.append(verification)

        if row_issues:
            all_issues.extend(row_issues)

        # Progress update every 10 rows
        if (idx + 1) % 10 == 0:
            print(
                f"Processed {idx + 1} rows, found {len(all_issues)} issues so far...")

    return all_issues, df


def generate_report(issues: List[Dict], output_file: str):
    """Generate a detailed report of all issues found."""

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("# CSV Value Verification Report\n\n")
        f.write(f"Total issues found: {len(issues)}\n\n")

        if not issues:
            f.write(
                "No issues found! All values appear to be supported by their quotes.\n")
            return

        # Group issues by column
        issues_by_column = {}
        for issue in issues:
            col = issue['column']
            if col not in issues_by_column:
                issues_by_column[col] = []
            issues_by_column[col].append(issue)

        f.write("## Issues by Column\n\n")
        for col, col_issues in sorted(issues_by_column.items(), key=lambda x: len(x[1]), reverse=True):
            f.write(f"### {col} ({len(col_issues)} issues)\n\n")

        f.write("\n## Detailed Issues\n\n")

        for i, issue in enumerate(issues, 1):
            f.write(f"### Issue {i}\n")
            f.write(f"- **Row**: {issue['row_index']}\n")
            f.write(f"- **Study**: {issue['study_title']}\n")
            f.write(f"- **Column**: {issue['column']}\n")
            f.write(f"- **Value**: {issue['value']}\n")
            f.write(f"- **Issues**: {', '.join(issue['issues'])}\n")
            f.write(f"- **Quotes (excerpt)**: {issue['quotes']}\n")
            f.write(f"- **Reasoning (excerpt)**: {issue['reasoning']}\n")
            f.write("\n---\n\n")


def main():
    csv_file = r"c:\Users\kukumar\OneDrive - UGent\My Projects\Grafitti Project\Urban_Foraging_Archive\Articles_drafts\2024 Snatching\Data\20251226_reviwed_step.csv"
    output_file = r"c:\Users\kukumar\OneDrive - UGent\My Projects\Grafitti Project\Urban_Foraging_Archive\Articles_drafts\2024 Snatching\Data\verification_report.md"

    print("Starting CSV verification...")
    print(f"Input file: {csv_file}")
    print(f"Output file: {output_file}")
    print("\n")

    issues, df = analyze_csv(csv_file)

    print(f"\n\nAnalysis complete!")
    print(f"Total issues found: {len(issues)}")
    print(f"\nGenerating detailed report...")

    generate_report(issues, output_file)

    print(f"Report saved to: {output_file}")

    # Print summary
    if issues:
        print("\n\nTop 5 columns with most issues:")
        issues_by_column = {}
        for issue in issues:
            col = issue['column']
            issues_by_column[col] = issues_by_column.get(col, 0) + 1

        for col, count in sorted(issues_by_column.items(), key=lambda x: x[1], reverse=True)[:5]:
            print(f"  {col}: {count} issues")


if __name__ == "__main__":
    main()
