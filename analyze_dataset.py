#!/usr/bin/env python3
"""
Analysis script for the extracted crime location choice dataset.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter
import re
import warnings
warnings.filterwarnings('ignore')


def load_and_explore_data():
    """Load the dataset and perform initial exploration."""

    # Load the data
    df = pd.read_csv('Output/chatgpt_extracted_dataset.csv')

    print("=== DATASET OVERVIEW ===")
    print(f"Shape: {df.shape}")
    print(f"Columns: {len(df.columns)}")
    print(f"Rows: {len(df)}")

    # Show column names
    print("\n=== COLUMN NAMES ===")
    for i, col in enumerate(df.columns):
        print(f"{i+1:3d}. {col}")

    return df


def analyze_basic_info(df):
    """Analyze basic study information."""

    print("\n=== BASIC STUDY INFORMATION ===")

    # Extract key columns for analysis
    key_cols = [col for col in df.columns if any(keyword in col.lower() for keyword in
                ['year', 'author', 'country', 'city', 'crime_type', 'title'])]

    print(f"Key columns found: {key_cols}")

    # Try to find year information
    year_cols = [col for col in df.columns if 'year' in col.lower()]
    if year_cols:
        print(f"\nYear columns: {year_cols}")
        year_data = df[year_cols[0]].dropna().head(10)
        print(f"Sample years: {year_data.tolist()}")

    # Try to find author information
    author_cols = [col for col in df.columns if 'author' in col.lower()]
    if author_cols:
        print(f"\nAuthor columns: {author_cols}")
        author_data = df[author_cols[0]].dropna().head(5)
        print(f"Sample authors: {author_data.tolist()}")

    # Try to find country/location information
    location_cols = [col for col in df.columns if any(loc in col.lower() for loc in
                                                      ['country', 'city', 'region', 'location'])]
    if location_cols:
        print(f"\nLocation columns: {location_cols}")
        for col in location_cols[:3]:  # Show first 3 location columns
            loc_data = df[col].dropna().head(5)
            print(f"{col}: {loc_data.tolist()}")


def analyze_methodological_aspects(df):
    """Analyze methodological aspects of the studies."""

    print("\n=== METHODOLOGICAL ANALYSIS ===")

    # Look for method-related columns
    method_keywords = ['model', 'method', 'approach',
                       'analysis', 'software', 'estimation']
    method_cols = [col for col in df.columns if any(
        keyword in col.lower() for keyword in method_keywords)]

    print(f"Method-related columns: {len(method_cols)}")
    for col in method_cols[:5]:  # Show first 5
        print(f"- {col}")

    # Look for sample size information
    sample_keywords = ['sample', 'size', 'number', 'crimes', 'offenders']
    sample_cols = [col for col in df.columns if any(
        keyword in col.lower() for keyword in sample_keywords)]

    print(f"\nSample-related columns: {len(sample_cols)}")
    for col in sample_cols[:5]:  # Show first 5
        print(f"- {col}")


def analyze_spatial_aspects(df):
    """Analyze spatial scale and geographic aspects."""

    print("\n=== SPATIAL ANALYSIS ===")

    # Look for spatial-related columns
    spatial_keywords = ['spatial', 'scale',
                        'unit', 'area', 'geographic', 'boundary']
    spatial_cols = [col for col in df.columns if any(
        keyword in col.lower() for keyword in spatial_keywords)]

    print(f"Spatial-related columns: {len(spatial_cols)}")
    for col in spatial_cols[:5]:  # Show first 5
        print(f"- {col}")

    # Try to extract spatial unit information
    unit_cols = [col for col in df.columns if 'unit' in col.lower()
                 or 'sua' in col.lower()]
    if unit_cols:
        print(f"\nSpatial unit columns: {unit_cols}")
        for col in unit_cols[:3]:
            unit_data = df[col].dropna().head(5)
            print(f"{col}: {unit_data.tolist()}")


def create_summary_statistics(df):
    """Create summary statistics and visualizations."""

    print("\n=== CREATING SUMMARY VISUALIZATIONS ===")

    # Set up the plotting style
    plt.style.use('seaborn-v0_8')
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Crime Location Choice Studies Dataset Overview',
                 fontsize=16, fontweight='bold')

    # 1. Data completeness heatmap
    ax1 = axes[0, 0]
    # Calculate missing data percentage for each column
    missing_pct = (df.isnull().sum() / len(df) *
                   100).sort_values(ascending=False)

    # Show top 20 columns with most missing data
    top_missing = missing_pct.head(20)
    ax1.barh(range(len(top_missing)), top_missing.values)
    ax1.set_yticks(range(len(top_missing)))
    ax1.set_yticklabels([col[:30] + '...' if len(col) >
                        30 else col for col in top_missing.index], fontsize=8)
    ax1.set_xlabel('Missing Data (%)')
    ax1.set_title('Top 20 Columns with Missing Data')
    ax1.grid(True, alpha=0.3)

    # 2. Column categories
    ax2 = axes[0, 1]
    categories = categorize_columns(df.columns)
    category_counts = Counter(categories.values())

    ax2.pie(category_counts.values(), labels=category_counts.keys(),
            autopct='%1.1f%%', startangle=90)
    ax2.set_title('Distribution of Column Categories')

    # 3. Study characteristics (if we can identify them)
    ax3 = axes[1, 0]
    # Try to plot something about the studies
    text_info = f"""
    Dataset Characteristics:
    
    • Total Studies: {len(df)}
    • Total Columns: {len(df.columns)}
    • Data Completeness: {((df.notna().sum().sum()) / (df.shape[0] * df.shape[1]) * 100):.1f}%
    
    Key Information Extracted:
    • Study metadata
    • Methodological details  
    • Spatial characteristics
    • Sample information
    • Geographic coverage
    """

    ax3.text(0.1, 0.5, text_info, transform=ax3.transAxes, fontsize=11,
             verticalalignment='center', bbox=dict(boxstyle="round,pad=0.3", facecolor="lightblue", alpha=0.5))
    ax3.axis('off')
    ax3.set_title('Dataset Summary')

    # 4. Column length distribution
    ax4 = axes[1, 1]
    col_lengths = [len(col) for col in df.columns]
    ax4.hist(col_lengths, bins=20, alpha=0.7,
             color='skyblue', edgecolor='black')
    ax4.set_xlabel('Column Name Length (characters)')
    ax4.set_ylabel('Frequency')
    ax4.set_title('Distribution of Column Name Lengths')
    ax4.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig('Output/dataset_overview.png', dpi=300, bbox_inches='tight')
    plt.show()

    print("Visualization saved as 'Output/dataset_overview.png'")


def categorize_columns(columns):
    """Categorize columns based on their names."""

    categories = {}

    category_keywords = {
        'Study_Info': ['title', 'year', 'author', 'journal', 'doi', 'filename'],
        'Geographic': ['country', 'city', 'region', 'area', 'location', 'geographic'],
        'Methodological': ['model', 'method', 'approach', 'software', 'estimation', 'analysis'],
        'Spatial': ['spatial', 'scale', 'unit', 'sua', 'boundary'],
        'Sample': ['sample', 'size', 'number', 'crimes', 'offenders', 'observations'],
        'Temporal': ['year', 'period', 'time', 'temporal', 'data_collection'],
        'Crime': ['crime', 'offense', 'burglary', 'robbery', 'theft'],
        'Results': ['result', 'finding', 'effect', 'significant', 'odds', 'coefficient'],
        'Limitations': ['limitation', 'constraint', 'bias', 'issue', 'problem'],
        'Theory': ['theory', 'theoretical', 'framework', 'rational_choice', 'routine_activity']
    }

    for col in columns:
        col_lower = col.lower()
        categorized = False

        for category, keywords in category_keywords.items():
            if any(keyword in col_lower for keyword in keywords):
                categories[col] = category
                categorized = True
                break

        if not categorized:
            categories[col] = 'Other'

    return categories


def create_detailed_analysis(df):
    """Create more detailed analysis of the dataset."""

    print("\n=== DETAILED ANALYSIS ===")

    # Analyze data density by column
    print("\nData Density Analysis:")
    non_null_counts = df.notna().sum().sort_values(ascending=False)
    print(f"Most complete columns (top 10):")
    for i, (col, count) in enumerate(non_null_counts.head(10).items()):
        pct = (count / len(df)) * 100
        print(f"{i+1:2d}. {col[:50]:<50} {count:3d}/{len(df)} ({pct:5.1f}%)")

    # Analyze unique values in key columns
    print(f"\nUnique Values Analysis:")
    key_columns = non_null_counts.head(20).index

    for col in key_columns[:5]:  # Analyze top 5 most complete columns
        unique_count = df[col].nunique()
        total_non_null = df[col].notna().sum()
        if total_non_null > 0:
            print(
                f"{col[:40]:<40} Unique: {unique_count:3d}/{total_non_null} ({unique_count/total_non_null*100:5.1f}%)")


def export_processed_data(df):
    """Export processed and cleaned versions of the data."""

    print("\n=== EXPORTING PROCESSED DATA ===")

    # Create a summary dataset with key information
    key_columns = []

    # Find the most important columns (those with most data)
    completeness = df.notna().sum() / len(df)
    important_cols = completeness[completeness > 0.5].index.tolist()

    if important_cols:
        summary_df = df[important_cols].copy()
        summary_df.to_csv('Output/dataset_summary.csv', index=False)
        print(f"Summary dataset saved with {len(important_cols)} columns")

    # Create a metadata file about the dataset
    metadata = {
        'total_studies': len(df),
        'total_columns': len(df.columns),
        'data_completeness': f"{((df.notna().sum().sum()) / (df.shape[0] * df.shape[1]) * 100):.1f}%",
        'most_complete_columns': completeness.sort_values(ascending=False).head(10).to_dict(),
        'column_categories': categorize_columns(df.columns)
    }

    # Save metadata
    import json
    with open('Output/dataset_metadata.json', 'w') as f:
        json.dump(metadata, f, indent=2)

    print("Metadata saved as 'Output/dataset_metadata.json'")
    print("Analysis complete!")


def main():
    """Main analysis function."""

    print("CRIME LOCATION CHOICE STUDIES - DATASET ANALYSIS")
    print("=" * 60)

    try:
        # Load and explore the data
        df = load_and_explore_data()

        # Perform different types of analysis
        analyze_basic_info(df)
        analyze_methodological_aspects(df)
        analyze_spatial_aspects(df)

        # Create visualizations
        create_summary_statistics(df)

        # Detailed analysis
        create_detailed_analysis(df)

        # Export processed data
        export_processed_data(df)

    except FileNotFoundError:
        print("Error: Could not find 'Output/chatgpt_extracted_dataset.csv'")
        print("Please make sure the file exists in the Output directory.")
    except Exception as e:
        print(f"Error during analysis: {str(e)}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
