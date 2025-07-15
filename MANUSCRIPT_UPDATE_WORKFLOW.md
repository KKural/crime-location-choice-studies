# Manuscript Update Workflow

This README explains how to update your manuscript with the latest analysis results.

## Files

- `20250714_new_csv_analysis_clean.R`: Main analysis script that processes data and generates tables
- `Systematic_Review_Manuscript.Rmd`: R Markdown file for the manuscript that reads tables from the analysis
- `update_manuscript_with_analysis.R`: Helper script that runs analysis and renders the manuscript

## How to Update the Manuscript

1. Make any changes to the analysis script (`20250714_new_csv_analysis_clean.R`) as needed
2. Run the `update_manuscript_with_analysis.R` script with the following command:

```r
source("update_manuscript_with_analysis.R")
```

This will:
- Run the analysis
- Generate an Excel file with tables in a date-stamped folder
- Render the R Markdown manuscript using the latest data
- Output a date-stamped Word document

## Important Notes

- The Excel file will be created in a folder named `YYYYMMDD_Analysis & Results`
- The manuscript automatically looks for the file with the current date
- No values are hard-coded in the manuscript - all values come from the analysis

## Troubleshooting

If you get an error saying the Excel file couldn't be found, make sure:
1. The analysis script ran successfully
2. The Excel file was created in the expected location
3. The file paths in the manuscript match where the Excel file is located

## Manual Update

If you need to manually update without using the helper script:

1. Run the analysis: `source("20250714_new_csv_analysis_clean.R")`
2. Render the manuscript: `rmarkdown::render("Systematic_Review_Manuscript.Rmd")`
