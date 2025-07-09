# COMPREHENSIVE CRIME LOCATION CHOICE STUDY EXTRACTION PROMPT
## Extract ALL Required Information in One Complete Structured Output

### TASK OBJECTIVE
Extract comprehensive information from crime location choice studies including study characteristics, methodology, spatial units, complete individual variable lists, and findings in a single structured format.

### COMPLETE EXTRACTION TEMPLATE
For each study, provide ALL information in this exact structured format:

---

## STUDY IDENTIFICATION
**Title:** [Full paper title]
**Year:** [Publication year]
**Authors:** [All authors, separated by semicolons]
**Journal:** [Journal name]
**DOI:** [DOI if available]
**Filename:** [PDF filename]

---

## STUDY CONTEXT
**Country:** [Country where study was conducted]
**City/Region:** [Specific city or region studied]
**Study Area Size:** [Total area in km² or other units - extract exact value]
**Study Area Description:** [Brief description of geographic scope]
**Crime Type:** [Primary crime type analyzed - be specific]
**Crime Types (All):** [All crime types if multiple, separated by semicolons]
**Study Period:** [Time period of data - years covered]
**Data Sources:** [All data sources used, separated by semicolons]

---

## SPATIAL UNIT OF ANALYSIS (SUoA)
**SUoA Type:** [e.g., Census tract, Block group, Grid cell, Administrative unit]
**SUoA Size:** [Exact size with units - e.g., "500m x 500m", "0.25 km²", "varies 0.1-2.5 km²"]
**SUoA Description:** [Detailed description of spatial units used]
**Number of Units:** [Total number of spatial units in study]
**Population per Unit:** [Average or range of population per unit]
**Quoted Rationale:** "[Exact quote about why this spatial scale was chosen]"
**Rationale Category:** Data availability / Admin convenience / Theory–method / Practical constraint / Prior research / Scale optimization / Not specified
**Justification Summary:** [Brief summary of the reasoning for SUoA choice]

---

## METHODOLOGY
**Study Design:** [Cross-sectional, longitudinal, etc.]
**Statistical Method:** [Primary analytical method - be specific about model type]
**Model Type:** [Discrete choice model type - conditional logit, multinomial logit, etc.]
**Software Used:** [Statistical software mentioned]
**Sampling Approach:** [How spatial units or alternatives were sampled]
**Sample Size:** [Number of choice observations or events]
**Choice Set Definition:** [How choice sets were constructed]
**Estimation Method:** [Maximum likelihood, Bayesian, etc.]

---

## COMPLETE INDIVIDUAL VARIABLE LIST
Please list EVERY individual independent variable mentioned in the study:

**DEMOGRAPHIC VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all demographic variables...]

**ECONOMIC VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all economic variables...]

**LAND USE VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all land use variables...]

**INFRASTRUCTURE VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all infrastructure variables...]

**DISTANCE/ACCESSIBILITY VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all distance/accessibility variables...]

**CRIME OPPORTUNITY VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all crime opportunity variables...]

**SOCIAL/BEHAVIORAL VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all social/behavioral variables...]

**ENVIRONMENTAL VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all environmental variables...]

**TEMPORAL/CONTROL VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all temporal/control variables...]

**OTHER VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all other variables...]

---

## VARIABLE SUMMARY COUNTS
**Total Independent Variables:** [Number]
**Demographic Variables:** [Number]
**Economic Variables:** [Number]
**Land Use Variables:** [Number]
**Infrastructure Variables:** [Number]
**Distance/Accessibility Variables:** [Number]
**Crime Opportunity Variables:** [Number]
**Social/Behavioral Variables:** [Number]
**Environmental Variables:** [Number]
**Temporal/Control Variables:** [Number]
**Other Variables:** [Number]

---

## KEY FINDINGS
**Main Results:** [Brief summary of key findings about location choice]
**Significant Predictors:** [Variables that were statistically significant]
**Model Performance:** [R-squared, log-likelihood, AIC/BIC if reported]
**Scale Effects:** [Any findings about spatial scale or SUoA size effects]

---

## DATA QUALITY NOTES
**Variable Information Quality:** [Complete/Partial/Limited]
**Missing Information:** [Note any information that could not be extracted]
**Extraction Confidence:** [High/Medium/Low]

---

## DATA LIMITATIONS
**Quote 1:** "[Exact quote about limitation]"
**Category:** [Data limitation / Method limitation / Scope limitation]
**Summary:** [Brief description]

**Quote 2:** "[Exact quote about limitation]"
**Category:** [Data limitation / Method limitation / Scope limitation]
**Summary:** [Brief description]

**Quote 3:** "[Exact quote about limitation]"
**Category:** [Data limitation / Method limitation / Scope limitation]
**Summary:** [Brief description]

**Overall Summary:** [1-2 sentences about main limitations]

---

### EXTRACTION INSTRUCTIONS

#### VARIABLE CATEGORIZATION (10 Categories):
- **Demographic**: Population density, age structure, gender ratios, household composition, education, ethnicity
- **Economic**: Income, employment, poverty, business density, property values, retail activity
- **Land Use**: Residential density, commercial area, industrial area, green space, mixed-use, zoning
- **Infrastructure**: Road density, public transport, parking, lighting, building types, utilities
- **Distance/Accessibility**: Distance to CBD, transport hubs, accessibility indices, travel time, barriers
- **Crime Opportunity**: Target density, guardianship, visibility, foot traffic, activity patterns
- **Social/Behavioral**: Social cohesion, collective efficacy, mobility patterns, routine activities
- **Environmental**: Physical environment, weather, terrain, natural features, environmental quality
- **Temporal/Control**: Time effects, seasonal controls, spatial lags, jurisdictional dummies, interactions
- **Other**: Everything else not fitting above categories

#### SUoA RATIONALE CATEGORIES:
- **Data availability**: "Data provided by police at X level", "Census data available at tract level"
- **Theory–method**: "X captures neighborhood effects", "Appropriate scale for social processes"
- **Prior research**: "Following Smith (2010)", "Consistent with previous studies"
- **Admin convenience**: "Official boundaries", "Standard administrative units"
- **Practical constraint**: "Computational limitations", "Sample size requirements"
- **Scale optimization**: "Balances detail and statistical power", "Optimal for this analysis"
- **Not specified**: No clear reason given

#### VARIABLE FORMAT:
For each variable: `Variable_Name | Description | Unit | Data_Source`

Examples:
- `POP_DENSITY | Population per square kilometer | persons/km² | U.S. Census`
- `MEDIAN_INC | Median household income (log-transformed) | log(dollars) | ACS`
- `DIST_CBD | Distance to central business district | kilometers | GIS calculation`

#### WHERE TO LOOK:
- Methods/Methodology section
- Data and Variables section
- Model specification equations
- Results tables (regression tables)
- Variable description appendices

#### WHAT TO INCLUDE:
- ALL independent variables used in main models
- Control variables and fixed effects
- Interaction terms and transformed variables
- Spatial lag variables if used

#### WHAT TO EXCLUDE:
- Dependent variables (crime counts, choice outcomes)
- Model diagnostics
- Variables mentioned but not used in analysis

#### SEARCH TERMS FOR SUoA RATIONALE:
Look for: "because", "due to", "chosen to", "selected based on", "appropriate scale", "data available at", "following", "consistent with"

#### QUALITY NOTES:
- If variable category is unclear, place in "OTHER VARIABLES"
- If variable names are not clear, note: "Variable names not clearly specified"
- If only partial variable lists available, note: "Partial variable list provided"

---

### QUALITY CHECK BEFORE SUBMISSION
Verify you have extracted:
- [ ] Complete bibliographic information
- [ ] All study context (location, crime type, time period, data sources)
- [ ] Exact spatial unit specifications with sizes and justifications
- [ ] Complete methodology including sampling approach
- [ ] Every individual variable with name, description, unit, and source
- [ ] Accurate variable counts matching the individual lists
- [ ] Key findings and model performance metrics
- [ ] Assessment of information quality and missing details
- [ ] 2-3 author-mentioned data limitations with exact quotes

This format captures ALL critical information in one comprehensive extraction for complete analysis.
