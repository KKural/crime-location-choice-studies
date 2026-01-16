# ⚠️ FILE LOCKED - ACTION REQUIRED

The Excel file `2026013_working.xlsx` appears to be:
- Currently open in Excel, OR
- Locked by another process, OR  
- Has sharing restrictions from OneDrive

## SOLUTION:

**Option 1: Close the file**
1. Close Excel if you have `2026013_working.xlsx` open
2. Re-run: `Rscript clean_data.R`

**Option 2: Use a different filename**
1. Save a copy as `2026013_working_copy.xlsx`
2. Update line 7 in clean_data.R to use the copy

**Option 3: Work with CSV instead**
If you have a CSV version, I can create a simpler script.

---

Once the file is accessible, the script will:
- Remove all columns containing "supporting" or "reasoning"
- Keep only the core variables (Title, Authors, Year, Country, Unit_Size, etc.)
- Save cleaned version as: `20260113_cleaned.xlsx` and `.csv`

Let me know when you've closed the file or if you'd prefer Option 2 or 3!
