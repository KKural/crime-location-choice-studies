# Filename Extraction Guide for Elicit

## Problem
Filename information is not being consistently captured in CSV extractions, showing as "Not mentioned" or blank fields despite files having clear filenames.

## Solution Strategies

### 1. Update Prompt Instructions
The BASIC STUDY IDENTIFICATION prompt has been updated to:
- Make filename extraction **REQUIRED**
- Provide clear instructions on where to find filename
- Include fallback naming convention if filename not visible

### 2. Elicit-Specific Instructions

When using Elicit for extraction:

**Step 1: Before Starting Extraction**
- Note the filename of each PDF you're uploading
- Keep a list of: `filename → paper title` mapping
- This will help ensure consistent filename capture

**Step 2: During Extraction**
- Always check if the filename is visible in Elicit's interface
- Look for filename in document tabs or file properties
- If filename not visible, use the naming convention below

**Step 3: Filename Naming Convention (if needed)**
Format: `FirstAuthor_Year_KeyWords.pdf`
Examples:
- `Baudains_2013_London_Riots.pdf`
- `Clare_2009_Barriers_Burglars.pdf`
- `Bernasco_2013_Burglary_Target.pdf`

### 3. CSV Post-Processing Fix

If filenames are missing from existing CSV files:

**Option A: Manual Addition**
1. Create a mapping file with:
   ```
   Title,Filename
   "TARGET CHOICE DURING EXTREME EVENTS...", "Baudains_2013_London_Riots.pdf"
   "Formal Evaluation of the Impact...", "Clare_2009_Barriers_Burglars.pdf"
   ```

2. Use R to merge filename information:
   ```r
   # Read the extraction CSV
   extraction_data <- read_csv("your_extraction_file.csv")
   
   # Read the filename mapping
   filename_mapping <- read_csv("filename_mapping.csv")
   
   # Merge by title
   extraction_with_filenames <- extraction_data %>%
     left_join(filename_mapping, by = "Title") %>%
     mutate(Filename = coalesce(Filename.y, Filename.x)) %>%
     select(-Filename.x, -Filename.y)
   ```

**Option B: Extract from File List**
If you have the original PDF files:
```r
# Get filenames from directory
pdf_files <- list.files("path/to/pdfs", pattern = "\\.pdf$", full.names = FALSE)

# Create mapping based on title similarity
# (This requires manual verification)
```

### 4. Quality Control Checklist

Before finalizing extraction data:
- [ ] Every row has a filename value
- [ ] Filenames are consistent and descriptive
- [ ] No "Not mentioned" or blank filename fields
- [ ] Filename format follows convention (if standardized)

### 5. Elicit Best Practices

**Upload Phase:**
1. Rename PDFs with descriptive names before uploading
2. Use consistent naming: `Author_Year_Topic.pdf`
3. Avoid special characters in filenames

**Extraction Phase:**
1. Always complete the filename field
2. Double-check filename matches the document
3. Use fallback naming if filename not visible

**Verification Phase:**
1. Check that all files have unique, identifiable filenames
2. Verify filename-to-content match
3. Update missing filenames before final export

### 6. Updated Prompt Language

For future extractions, use this specific instruction:

```
**FILENAME EXTRACTION PRIORITY:**
1. Use the actual PDF filename if visible in Elicit interface
2. If not visible, construct as: FirstAuthor_Year_KeyWords.pdf
3. Never leave filename field blank or as "Not mentioned"
4. Ensure filename uniquely identifies the paper
```

### 7. Troubleshooting Common Issues

**Issue:** Filename shows as "document.pdf" or generic name
**Solution:** Use the naming convention based on paper content

**Issue:** Multiple papers by same author/year
**Solution:** Add distinguishing keywords: `Smith_2020_Burglary.pdf`, `Smith_2020_Robbery.pdf`

**Issue:** Long complex titles
**Solution:** Use 2-3 key words: `Johnson_2019_Location_Choice.pdf`

**Issue:** Special characters in original filename
**Solution:** Replace with underscores: `Smith_2020_Crime_Analysis.pdf`

## Implementation

1. **Immediate:** Use the updated PROMPT 1 for all new extractions
2. **Existing Data:** Apply post-processing fix to current CSV files
3. **Future:** Follow Elicit best practices for consistent filename capture

This approach will ensure all papers have identifiable, consistent filenames in your systematic review database.
