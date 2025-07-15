# Improved Elicit Prompt for Study Area Size Extraction

## Enhanced Study Area Size Extraction Prompt

**Task:** Extract comprehensive study area size information from academic papers on crime location choice, including both direct mentions and embedded references.

**CRITICAL ISSUE TO ADDRESS:**
Previous extraction attempts have FAILED to identify study area size information even when it is clearly mentioned in the papers. For example:
- Papers contain explicit statements like "the study area, which covered 203km²" but are being marked as "not mentioned"
- Area information is present but embedded within other sentences about data collection or results
- Size information appears in various sections (methodology, results, discussion) but is being overlooked
- Unit information (km², square kilometers, hectares) is present but not being recognized

**YOU MUST BE MORE THOROUGH** - if area size information exists anywhere in the paper, you must find it and extract it.

**Instructions:**
Please extract the following information about the study area size. Look carefully throughout the entire paper, as study area size may be mentioned in various contexts (methodology, results, discussion, etc.).

**Fields to Extract:**

1. **Total Study Area Size (km²):**
   - Look for direct statements like "study area of X km²", "covering X square kilometers"
   - Look for embedded mentions like "the study area, which covered X km²", "spanning X km²", "over X km² of urban area"
   - Include mentions in results/discussion: "93% of time was spent in the study area, which covered 203km²"
   - Convert units if needed (square miles to km², hectares to km², etc.)
   - If multiple values given (e.g., total vs land area), note both: "203km² total (182km² land)"

2. **Study Area Description:**
   - Geographic boundaries or administrative units covered
   - Examples: "Greater Manchester area", "downtown core and surrounding neighborhoods", "entire city limits"

3. **Area Calculation Method:**
   - How was the area determined? 
   - Examples: "GIS calculation", "official administrative boundaries", "GPS tracking coverage", "stated in municipal records"

4. **Area Source/Reference:**
   - Where did this information come from?
   - Examples: "calculated by authors", "municipal data", "previous study citation", "GIS analysis"

5. **Alternative Area Information:**
   - If exact km² not available, extract related size indicators:
   - Number of spatial units × average unit size
   - Population density × population = approximate area
   - Linear distance measurements (e.g., "15km radius from city center")

6. **Area Context Notes:**
   - Any important qualifications about the area
   - Examples: "excluding water bodies", "urban area only", "accessible by road", "residential areas only"

**Search Strategy:**
- **READ THE ENTIRE PAPER THOROUGHLY** - area information can appear anywhere
- Check methodology/study design sections for area definitions
- Look in results for area coverage statistics  
- Check data description for geographic scope
- **Scan every paragraph** for embedded area mentions
- Look for phrases like: "covered", "spanning", "encompassing", "within", "across", "throughout"
- Search for units: km², km2, square km, square kilometers, hectares, square miles
- Look for percentage coverage statements that might reference total area
- **Common locations where area size is mentioned but missed:**
  - In results when discussing data coverage: "93% of activity occurred within the 203km² study area"
  - In methodology when describing spatial scope: "the analysis encompassed a 50km² urban region"
  - In discussion when comparing to other studies: "our 180km² study area was larger than..."
  - In data description: "GPS tracks covered approximately 25 square kilometers"

**Output Format:**
```
Total Study Area Size: [X km² or "Not specified"]
Study Area Description: [Description or "Not provided"]
Area Calculation Method: [Method or "Not mentioned"]
Area Source: [Source or "Not stated"]
Alternative Size Info: [Any related measurements or "None found"]
Context Notes: [Important qualifications or "None"]
```

**Important Notes:**
- **DO NOT mark as "Not specified" if area information exists anywhere in the paper**
- Previous extractions have incorrectly marked papers as having no area information when it was clearly present
- Examples of MISSED information that should have been found:
  - "Over 93% of the time was spent inside the study area, which covered 203km²" - this WAS NOT extracted previously but clearly states 203km²
  - "The analysis covered the downtown area spanning approximately 15 square kilometers" - this type of statement is being missed
  - "Data collection occurred across 247 census blocks with an average area of 0.8km² each" - this allows calculation of ~198km² total
- Include approximate or partial information rather than marking as missing
- If you find conflicting area measurements, include both with context
- Pay special attention to results sections where area coverage might be discussed
- Look for area information in figure captions, tables, and supplementary materials references
- **DOUBLE-CHECK your work** - if you're about to mark something as "Not specified", re-read the entire paper one more time

**Example Extractions:**

Paper mentioning: "Over 93% of the time was spent inside the study area, which covered 203km2 (of which 182km2 are on land)"
```
Total Study Area Size: 203km² total (182km² land area)
Study Area Description: Mixed land and water area with 93% activity coverage
Area Calculation Method: Not specified
Area Source: Results section analysis
Alternative Size Info: 182km² land area specifically
Context Notes: Includes water bodies; 93% of tracked activity occurred within boundaries
```

Paper mentioning: "The study encompassed 145 census tracts across the metropolitan area, with an average tract size of 2.3km²"
```
Total Study Area Size: Approximately 334km² (145 tracts × 2.3km²)
Study Area Description: 145 census tracts across metropolitan area
Area Calculation Method: Calculated from tract count and average size
Area Source: Census data and author calculation
Alternative Size Info: 145 spatial units, 2.3km² average unit size
Context Notes: Based on census tract boundaries
```
