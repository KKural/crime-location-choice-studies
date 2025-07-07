# Crime Location Choice Studies - Data Extraction Summary

## Overview
Successfully processed **49 studies** and extracted **716 variables** across 10 different categories.

## Main Output File
**`comprehensive_studies_dataset.csv`** - This is your main file containing everything in one place:

### What's included in the comprehensive dataset:

#### Study Metadata (12 columns):
- `study_id` - Unique identifier for each study
- `original_title` - Title from your original CSV
- `extracted_title` - Title extracted from the detailed text
- `authors` - Study authors
- `year` - Publication year
- `country` - Country where study was conducted
- `city` - City/region of the study
- `crime_type` - Type of crime studied
- `suoa_type` - Spatial unit of analysis type
- `suoa_size` - Size of spatial units
- `num_units` - Number of units in the study
- `sample_size` - Sample size

#### Variable Counts by Category (10 columns):
- `crime_opportunity` - Count of crime opportunity variables
- `demographic` - Count of demographic variables
- `distance_access` - Count of distance/accessibility variables
- `economic` - Count of economic variables
- `infrastructure` - Count of infrastructure variables
- `land_use` - Count of land use variables
- `social_behavioral` - Count of social/behavioral variables
- `temporal_control` - Count of temporal/control variables
- `environmental` - Count of environmental variables
- `other` - Count of other variables

#### Summary Statistics (2 columns):
- `total_variables_count` - Total number of variables per study
- `all_variables` - Complete list of all variables (with categories)

#### Variable Lists by Category (10 columns):
- `vars_crime_opportunity` - List of all crime opportunity variables
- `vars_demographic` - List of all demographic variables
- `vars_distance_access` - List of all distance/accessibility variables
- `vars_economic` - List of all economic variables
- `vars_infrastructure` - List of all infrastructure variables
- `vars_land_use` - List of all land use variables
- `vars_social_behavioral` - List of all social/behavioral variables
- `vars_temporal_control` - List of all temporal/control variables
- `vars_environmental` - List of all environmental variables
- `vars_other` - List of all other variables

## Key Statistics:
- **Total Studies:** 49
- **Total Variables Extracted:** 716
- **Most Common Variable Category:** Other (212 variables)
- **Second Most Common:** Land Use (67 variables)
- **Third Most Common:** Social/Behavioral (67 variables)

## Variable Categories Usage:
1. **Crime Opportunity:** 74 variables across 21 studies
2. **Demographic:** 65 variables across 24 studies
3. **Distance/Access:** 39 variables across 21 studies
4. **Economic:** 39 variables across 19 studies
5. **Environmental:** 19 variables across 14 studies
6. **Infrastructure:** 45 variables across 19 studies
7. **Land Use:** 67 variables across 23 studies
8. **Other:** 212 variables across 13 studies
9. **Social/Behavioral:** 67 variables across 19 studies
10. **Temporal/Control:** 89 variables across 16 studies

## How to Use the Data:

### For Analysis:
- Use the count columns to analyze how many variables of each type studies use
- Use the variable list columns to see exactly what variables were used
- Filter by country, crime type, or spatial unit type for specific analyses

### For Literature Review:
- The comprehensive dataset gives you everything in one place
- Each row is one study with all its metadata and variables
- Variables are separated by " | " in the list columns

## Files Created:
1. **`comprehensive_studies_dataset.csv`** - Main combined dataset (RECOMMENDED)
2. `studies_metadata.csv` - Just the basic study information
3. `variables_extracted.csv` - All variables in long format (for detailed analysis)
4. `variable_counts_by_study.csv` - Just the variable counts
5. `variable_frequency_summary.csv` - Summary statistics
6. `common_variables.csv` - Most frequently used variables

## Scripts Used:
- `improved_variable_extraction.R` - Main extraction script
- `create_comprehensive_dataset.R` - Creates the combined dataset
- `variable_analysis_visualization.R` - Creates visualizations

Your data is now ready for analysis and all files are backed up in your Git repository!
