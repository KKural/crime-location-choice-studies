# Filename Issue Solution Summary

## Problem Identified
Your CSV extractions show filename fields as "Not mentioned" or blank, even though the files clearly have filenames. This makes it difficult to track which extraction corresponds to which source document.

## Root Cause
The extraction prompt wasn't specific enough about filename capture, and Elicit may not always display the filename prominently during extraction.

## Complete Solution Package

### 1. Updated Extraction Prompt (`specialized_extraction_prompts.md`)
- Modified PROMPT 1 to make filename extraction **REQUIRED**
- Added clear instructions on where to find filenames
- Included fallback naming convention

### 2. Filename Extraction Guide (`filename_extraction_guide.md`)
- Detailed instructions for consistent filename capture in Elicit
- Best practices for PDF naming before upload
- Quality control checklist
- Troubleshooting for common issues

### 3. Automated Fix Script (`fix_missing_filenames.R`)
- Automatically generates filenames from author/year/title
- Processes all Elicit CSV files at once
- Handles duplicate filename resolution
- Creates backup with "_with_filenames" suffix

## Immediate Actions You Can Take

### Option 1: Quick Automated Fix
1. Open R in your project directory
2. Run the fix script:
   ```r
   source("fix_missing_filenames.R")
   fix_all_elicit_csvs()
   ```
3. This will generate new CSV files with filenames added

### Option 2: Manual Mapping (More Control)
1. Create a manual mapping file:
   ```r
   source("fix_missing_filenames.R")
   create_manual_filename_mapping()
   ```
2. Edit the mapping CSV to specify exact filenames
3. Apply the mapping:
   ```r
   apply_manual_mapping("your_elicit_file.csv")
   ```

### Option 3: Re-extract with Updated Prompts
1. Use the updated PROMPT 1 for new extractions
2. Pay special attention to filename field
3. Follow the filename extraction guide

## For Future Extractions

### Before Uploading to Elicit:
1. **Rename PDFs** with descriptive names: `Author_Year_Topic.pdf`
2. **Avoid special characters** in filenames
3. **Keep a file list** for reference

### During Extraction in Elicit:
1. **Always fill filename field** - never leave blank
2. **Check document interface** for visible filename
3. **Use naming convention** if filename not visible:
   - Format: `FirstAuthor_Year_KeyWords.pdf`
   - Example: `Baudains_2013_London_Riots.pdf`

### Quality Control:
1. **Verify every row** has a filename
2. **Check uniqueness** of filenames
3. **Ensure filename matches content**

## Expected Results

After applying the solution:
- ✅ Every row will have a descriptive filename
- ✅ Filenames will be unique and identifiable
- ✅ Easy to match extractions to source documents
- ✅ Better organization for systematic review
- ✅ Improved traceability and replication

## Files Created/Updated:

1. **`specialized_extraction_prompts.md`** - Updated PROMPT 1 with filename requirements
2. **`filename_extraction_guide.md`** - Comprehensive guide for filename handling
3. **`fix_missing_filenames.R`** - Automated solution for existing CSV files

## Quick Start Command

To fix your current CSV files immediately:
```r
source("fix_missing_filenames.R")
fix_all_elicit_csvs()
```

This will process all Elicit CSV files in your directory and add appropriate filenames based on the paper content.

The filename issue should be completely resolved with this solution package!
