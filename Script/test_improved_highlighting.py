import logging
import pandas as pd
from pdf_quote_highlighter import PDFQuoteHighlighter
import sys
import os
from datetime import datetime
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

logging.basicConfig(level=logging.INFO)

# Dynamic date handling like R scripts
today_date = datetime.now().strftime("%Y%m%d")

# Test on one study with dynamic paths
csv_file = rf"..//{today_date}_Analysis & Results//{today_date}_combined_dataset.csv"
pdf_folder = r"../Review_articles"
output_folder = rf"..//{today_date}_Analysis & Results//Test_Improved_Highlighting"

print(f"Using today's date: {today_date}")
print(f"CSV file: {csv_file}")
print(f"Output folder: {output_folder}")

# Create output directory
os.makedirs(output_folder, exist_ok=True)

print("Testing improved highlighting algorithm with smart highlighting...")

# Load data
df = pd.read_csv(csv_file)
print(f"Loaded {len(df)} studies")

# Test just the first study
study = df.iloc[0]
print(f"Testing study: {study['Title']}")

# Initialize highlighter
highlighter = PDFQuoteHighlighter(csv_file, pdf_folder, output_folder)

# Load the data
if not highlighter.load_data():
    print("Failed to load data!")
    exit(1)

# Process single study with higher threshold
result = highlighter.process_single_study(study, threshold=85)
print(f"Result: {result}")
