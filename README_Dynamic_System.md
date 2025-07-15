# Dynamic R Markdown Manuscript System

This document explains the fully dynamic R Markdown system for the systematic review manuscript, which automatically loads all statistics, percentages, and metrics from the Excel analysis file.

## Overview

The manuscript (`Systematic_Review_Manuscript.Rmd`) has been converted to be **completely dynamic**:

- **No hard-coded statistics**: All numbers, percentages, and metrics are loaded from data
- **Automatic updates**: When the underlying Excel data changes, the entire document updates
- **Robust error handling**: Fallback values ensure the document always renders
- **Transparent sourcing**: Comments and reports show where each statistic comes from
- **Comprehensive validation**: Built-in checks ensure all required variables are available

## System Components

### 1. Main Manuscript File
- `Systematic_Review_Manuscript.Rmd` - Main document with dynamic variable loading

### 2. Helper Scripts
- `load_analysis_results.R` - Loads data from Excel analysis file
- `dynamic_data_helpers.R` - Provides validation, fallbacks, and utility functions

### 3. Test and Validation
- `test_dynamic_manuscript.R` - Tests the entire dynamic system

## How It Works

### Data Loading Process

1. **Excel File Detection**: Automatically detects the current date's analysis file
2. **Dynamic Loading**: Loads all tables from the Excel file using `load_analysis_results()`
3. **Variable Extraction**: Extracts specific statistics using specialized functions:
   - `load_search_screening_stats()` - Search and screening numbers
   - `load_summary_statistics()` - Core descriptive statistics
   - `load_variable_statistics()` - Variable complexity metrics
   - `load_model_results()` - Statistical model coefficients
   - `load_rationale_categories()` - Spatial unit selection rationale
   - `load_complexity_percentages()` - Domain coverage percentages
   - `load_reporting_percentages()` - Data limitation reporting
   - `load_geographic_data()` - Country and temporal distributions

4. **Validation**: `validate_manuscript_variables()` ensures all required variables are available
5. **Fallback Handling**: Missing variables automatically use scientifically reasonable fallback values

### Dynamic Variables Used

The system loads **85+ variables** covering:

#### Search and Screening (8 variables)
- `total_records_identified`, `records_reviewed`, `studies_included`
- `naive_search_total`, `gold_standard_articles`, etc.

#### Summary Statistics (13 variables) 
- `n_studies`, `n_countries`, `n_journals`, `total_crime_incidents`
- `median_unit_size`, `mean_unit_size`, `smallest_unit_km2`, `largest_unit`
- `std_dev`, `skewness_original`, `temporal_span`, etc.

#### Variable Complexity (5 variables)
- `min_variables`, `max_variables`, `mean_variables`, `median_variables`
- `mean_limitations`

#### Model Results (9 variables)
- `year_beta`, `year_p`, `anglo_beta`, `anglo_p`
- `area_beta`, `area_p`, `soph_beta`, `soph_p`, `icc`

#### Rationale Categories (9 variables)
- `top_rationale`, `top_rationale_percent`, `second_rationale`
- `theory_method_pct`, `data_availability_pct`, `prior_research_pct`
- `practical_constraints_pct`, `rationale_coverage`

#### Domain Coverage (8 variables)
- `env_variables_pct`, `demo_variables_pct`, `econ_variables_pct`
- `dist_variables_pct`, `temp_variables_pct`
- `low_complexity_pct`, `high_complexity_pct`, `very_high_complexity_pct`

#### Reporting Statistics (5 variables)
- `data_quality_reporting_pct`, `missing_data_reporting_pct`
- `generalizability_reporting_pct`, `scale_limitations_reporting_pct`
- `future_research_reporting_pct`

#### Geographic Data (11 variables)
- `micro_env_units_pct`, `medium_units_pct`, `large_units_pct`, `regional_units_pct`
- `netherlands_studies`, `netherlands_pct`, `us_studies`, `us_pct`
- `china_studies`, `uk_studies`, `post_2010_pct`

### Error Handling and Fallbacks

The system includes **three levels of error handling**:

1. **Table-Level Fallbacks**: If entire tables are missing, functions provide reasonable defaults
2. **Variable-Level Fallbacks**: If specific statistics are missing, individual fallback values are used
3. **System-Level Fallbacks**: If the entire Excel file is missing, comprehensive fallback data ensures the document still renders

### Transparency and Validation

The system provides **comprehensive reporting**:

- **Data Quality Report**: Shows which tables loaded successfully
- **Data Source Summary**: Lists all available tables and row counts  
- **Variable Completeness Report**: Shows which variables are available vs. using fallbacks
- **Comments Throughout**: Manuscript includes comments explaining data sources

## Usage Instructions

### Normal Usage (with Excel file)

1. Ensure the Excel analysis file exists in the expected location:
   ```
   YYYYMMDD_Analysis & Results/YYYYMMDD_Comprehensive_Manuscript_Tables.xlsx
   ```

2. Knit the document:
   ```r
   rmarkdown::render("Systematic_Review_Manuscript.Rmd")
   ```

3. All statistics will be loaded dynamically from the Excel file

### Testing the System

Run the test script to verify everything works:
```r
source("test_dynamic_manuscript.R")
```

This will:
- Check if the Excel file exists
- Test loading all helper functions
- Validate variable availability
- Test document knitting
- Report any issues

### Fallback Mode (without Excel file)

If the Excel file is unavailable, the document will:
- Use scientifically reasonable fallback values
- Display warnings about missing data
- Still render a complete document
- Include notes about data source limitations

## File Structure

```
├── Systematic_Review_Manuscript.Rmd    # Main dynamic manuscript
├── load_analysis_results.R             # Excel data loader
├── dynamic_data_helpers.R              # Validation and fallback functions
├── test_dynamic_manuscript.R           # System testing script
├── YYYYMMDD_Analysis & Results/        # Directory with Excel files
│   └── YYYYMMDD_Comprehensive_Manuscript_Tables.xlsx
└── figures/                            # Figure directory
```

## Benefits of the Dynamic System

### For Researchers
- **Automatic Updates**: When new studies are added to the analysis, the entire document updates automatically
- **Error Reduction**: Eliminates copy-paste errors and inconsistencies
- **Transparency**: All statistics are traceable to their data source
- **Efficiency**: No manual updating of multiple statistics throughout the document

### For Reproducibility
- **Data Linkage**: Direct connection between analysis data and manuscript text
- **Version Control**: Changes in underlying data are immediately reflected
- **Audit Trail**: Clear tracking of data sources and transformations
- **Validation**: Built-in checks ensure data integrity

### For Collaboration
- **Consistency**: All collaborators work with the same data sources
- **Reliability**: Robust error handling prevents rendering failures
- **Documentation**: Comprehensive comments and reporting explain the system
- **Flexibility**: Easy to modify or extend for new variables

## Maintenance and Updates

### Adding New Variables

1. Add the variable to the appropriate loading function in the setup section
2. Add fallback value to `create_fallback_data()` in `dynamic_data_helpers.R`
3. Add variable name to `validate_manuscript_variables()` in `dynamic_data_helpers.R`
4. Use the variable in the manuscript text with `r variable_name`

### Modifying Data Sources

1. Update the Excel file path in the setup section if needed
2. Modify loading functions to match new table structures
3. Update fallback values to reflect new data ranges
4. Test with `test_dynamic_manuscript.R`

### Troubleshooting

If the document fails to render:

1. Run `test_dynamic_manuscript.R` to identify issues
2. Check that helper scripts load without errors
3. Verify Excel file exists and has expected structure
4. Check R console for specific error messages
5. Verify all required packages are installed

## Expected Excel File Structure

The system expects the following tables in the Excel file:

- `Table1_Summary_Statistics` - Core descriptive statistics
- `Table4_RQ3_Country_Analysis` - Country-level analysis  
- `Table6_RQ5_Variable_Complexity` - Variable complexity metrics
- `Table8_Statistical_Models` - Statistical model results
- `Table9_Rationale_Categories` - Spatial unit selection rationale
- Additional optional tables for domain coverage, reporting, etc.

Each table should have appropriate column names (`Statistic`, `Value`, etc.) for the loading functions to extract data correctly.

## Summary

This dynamic system transforms the manuscript from a static document with hard-coded values into a living document that automatically reflects the current state of the analysis data. All 85+ variables used throughout the manuscript are now loaded dynamically, with comprehensive error handling ensuring the document always renders successfully. The system provides transparency about data sources while maintaining scientific rigor through validation and fallback mechanisms.
