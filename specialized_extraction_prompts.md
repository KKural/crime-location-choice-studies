# SPECIALIZED EXTRACTION PROMPTS FOR CRIME LOCATION CHOICE STUDIES
## 19 Focused Prompts for Comprehensive Data Extraction - LOGICAL FLOW

**CRITICAL:** Before extraction, consult the Variable Categorization Guidelines to ensure proper classification and prevent variable duplication across categories.

**LOGICAL FLOW:** These prompts are organized in your preferred order:
1. **Basic Study Information** (Prompts 1-2)
2. **Spatial Scale & Crime Context** (Prompts 3-5)  
3. **Methodology & Design** (Prompts 6-8)
4. **Variables by Category** (Prompts 9-13)
5. **Results & Performance** (Prompts 14-16)
6. **Limitations & Issues** (Prompts 17-18)
7. **Implications & Future Directions** (Prompt 19)

---

## PROMPT 1: BASIC STUDY IDENTIFICATION
**Focus:** Bibliographic information and basic study details

Extract ONLY the following information:
- **Title:** [Full paper title]
- **Year:** [Publication year]
- **Authors:** [All authors, separated by semicolons]
- **Journal:** [Journal name]
- **DOI:** [DOI if available]
- **Filename:** [PDF filename - REQUIRED: Always include the filename of the PDF document you are reading]

**Instructions:** 
1. Look at the first page/header of the paper for bibliographic details
2. **CRITICAL:** Always include the PDF filename - this should be visible in your document interface or provided in the task
3. If filename shows as "untitled" or similar, use the first author's last name + year + key words from title
4. Extract exact bibliographic details as they appear in the document

---

## PROMPT 2: TEMPORAL SCOPE & DATA SOURCES
**Focus:** Time period and data collection details

Extract ONLY the following information:
- **Study Period:** [Time period of data - years covered]
- **Data Collection Period:** [When data was collected]
- **Data Sources:** [All data sources used, separated by semicolons]
- **Data Availability:** [Any mentions of data limitations or availability]

**Instructions:** Look in methodology and data sections. Note exact time periods and all data sources mentioned.

---

## PROMPT 3: SPATIAL UNITS - DESCRIPTION & JUSTIFICATION
**Focus:** Technical details and rationale for spatial units used

Extract ONLY the following information:
- **SUoA Type:** [e.g., Census tract, Block group, Grid cell, Administrative unit]
- **SUoA Size:** [Exact size with units - e.g., "500m x 500m", "0.25 km²", "varies 0.1-2.5 km²"]
- **SUoA Description:** [Detailed description of spatial units used]
- **Number of Units:** [Total number of spatial units in study]
- **Population per Unit:** [Average or range of population per unit]
- **Quoted Rationale:** "[Exact quote about why this spatial scale was chosen]"
- **Rationale Category:** Data availability / Admin convenience / Theory–method / Practical constraint / Prior research / Scale optimization / Not specified
- **Justification Summary:** [Brief summary of the reasoning for SUoA choice]

**Instructions:** Look in methodology sections. Extract exact technical specifications and search for keywords: "because", "due to", "chosen to", "selected based on", "appropriate scale", "data available at", "following", "consistent with". Quote exact justifications.

---

## PROMPT 4: STUDY CONTEXT & GEOGRAPHY
**Focus:** Geographic scope and crime context

Extract ONLY the following information:
- **Country:** [Country where study was conducted]
- **City/Region:** [Specific city or region studied]
- **Study Area Size:** [Total area in km² or other units - extract exact value]
- **Study Area Description:** [Brief description of geographic scope]
- **Crime Type:** [Primary crime type analyzed - be specific]
- **Crime Types (All):** [All crime types if multiple, separated by semicolons]

**Instructions:** Look in the introduction, study area, or data sections. Be specific about geographic boundaries and crime definitions.

---

## PROMPT 5: SAMPLING & CHOICE SETS
**Focus:** How alternatives and samples were constructed

Extract ONLY the following information:
- **Sampling Approach:** [How spatial units or alternatives were sampled]
- **Sample Size:** [Number of choice observations or events]
- **Number of Crimes Analyzed:** [Total number of crime incidents/events in the study]
- **Number of Offenders:** [Total number of unique offenders if mentioned]
- **Choice Set Definition:** [How choice sets were constructed]
- **Alternative Selection:** [How alternatives were chosen]
- **Sample Restrictions:** [Any restrictions on the sample]

**Instructions:** Look for details about how choice sets were built and sampling procedures. Pay special attention to the total number of crime events analyzed and number of offenders involved.

---

## PROMPT 6: THEORETICAL FRAMEWORK & OBJECTIVES
**Focus:** Theoretical grounding and research objectives

Extract ONLY the following information:
- **Theoretical Framework:** [What criminological or geographic theory the study builds on]
- **Research Objectives:** [Main research questions or hypotheses]
- **Literature Gap:** [What gap in knowledge the study addresses]
- **Study Motivation:** [Why this research was needed]

**Instructions:** Look in introduction and literature review sections for theoretical foundations and research objectives.

---

## PROMPT 7: STUDY DESIGN & METHODOLOGY
**Focus:** Discrete choice methodology and analytical approach

Extract ONLY the following information:
- **Study Design:** [Cross-sectional, longitudinal, panel, etc.]
- **Discrete Choice Model:** [Conditional logit, multinomial logit, nested logit, mixed logit, probit, etc.]
- **Model Specification:** [Random utility model, location choice model, destination choice model, etc.]
- **Software Used:** [Statistical software mentioned - R, Stata, Python, BIOGEME, etc.]
- **Estimation Method:** [Maximum likelihood, simulated maximum likelihood, Bayesian, etc.]
- **Model Extensions:** [Random parameters, spatial effects, temporal effects, interactions, etc.]

**Instructions:** Look in methodology sections for discrete choice model specifications. Focus on identifying the specific type of discrete choice model used and any model extensions or specifications.

---

## PROMPT 8: DATA PREPARATION & PROCESSING
**Focus:** How data was prepared and processed for analysis

Extract ONLY the following information:
- **Data Cleaning:** [How data was cleaned or processed]
- **Variable Construction:** [How key variables were created or calculated]
- **Missing Data Handling:** [How missing data was addressed]
- **Data Integration:** [How different data sources were combined]
- **Quality Control:** [Data validation or quality assurance measures]

**Instructions:** Look in methodology and data sections for information about data preparation procedures.

---

## PROMPT 9: DEMOGRAPHIC & SOCIAL VARIABLES
**Focus:** Population characteristics and social structure

**CRITICAL:** Do NOT include economic status variables (income, deprivation indices) - these belong in ECONOMIC VARIABLES.

Extract ONLY demographic and social variables in this format:
**DEMOGRAPHIC & SOCIAL VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all demographic and social variables...]

**INCLUDE in this category:** 
- **Population Characteristics:** Population density, age structure, gender ratios, household size/composition
- **Social Structure:** Ethnicity/race, ethnic diversity, migration/churn, family structure, marital status
- **Social Processes:** Social cohesion, collective efficacy, social disorganization, social capital, community organization
- **Educational Characteristics:** Education levels (when not economic indicators)

**EXCLUDE from this category:**
- Income, wealth, employment (→ ECONOMIC)
- Deprivation indices (→ ECONOMIC)  
- Business/commercial characteristics (→ ECONOMIC)
- Physical infrastructure (→ ENVIRONMENTAL)

**Instructions:** Look in variable tables, methodology, and results sections. Extract ALL demographic and social characteristics variables used.

---

## PROMPT 10: ECONOMIC VARIABLES
**Focus:** Economic and financial characteristics

**CRITICAL:** Include ALL economic status and prosperity measures here, including deprivation indices.

Extract ONLY economic variables in this format:
**ECONOMIC VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all economic variables...]

**INCLUDE in this category:** 
- **Economic Status:** Income levels, wealth indicators, poverty measures, economic deprivation indices (IMD, etc.)
- **Employment:** Employment/unemployment rates, job characteristics, occupation types
- **Property/Housing Economics:** Property values, housing costs, rent levels, housing affordability
- **Business/Commercial:** Business density, commercial activity, retail sales volume, economic development
- **Economic Inequality:** Income inequality measures, economic disparities

**EXCLUDE from this category:**
- Physical retail infrastructure (→ ENVIRONMENTAL)
- Population using services (→ DEMOGRAPHIC)
- Distance to economic centers (→ DISTANCE & ACCESSIBILITY)

**Instructions:** Look in variable tables, methodology, and results sections. Extract ALL economic variables used.

---

## PROMPT 11: ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES
**Focus:** Physical environment, built environment, land use, infrastructure, crime opportunities, and behavioral patterns

**CRITICAL:** Do NOT include distance/accessibility, demographic, economic, or temporal variables here. These belong in separate categories.

Extract ONLY environmental and crime attractor variables in this format:
**ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all environmental and crime attractor variables...]

**INCLUDE in this category:** 
- **Land Use:** Residential density, commercial area, industrial area, green space, mixed-use, zoning
- **Crime Attractors:** Retail establishments, bars/nightlife, schools, entertainment venues, shopping centers, restaurants, ATMs
- **Physical Environment:** Parks, terrain, natural features, environmental quality, lighting, visibility
- **Built Environment:** Building types, building density, housing characteristics, urban form
- **Crime Opportunity:** Target density, guardianship, foot traffic, activity patterns, suitable targets, surveillance, security measures
- **Behavioral Patterns:** Mobility patterns, routine activities, movement patterns, activity schedules, behavioral indicators, offender behavior

**EXCLUDE from this category:**
- Transportation networks/stations (→ DISTANCE & ACCESSIBILITY)
- Population characteristics (→ DEMOGRAPHIC & SOCIAL)  
- Economic indicators (→ ECONOMIC)
- Distance measures (→ DISTANCE & ACCESSIBILITY)
- Time-related variables (→ TEMPORAL & CONTROL)

**Instructions:** Extract ALL variables related to land use, physical environment, built infrastructure, places/features that attract crime, crime opportunities, and behavioral activity patterns.

---

## PROMPT 12: DISTANCE & ACCESSIBILITY VARIABLES
**Focus:** Spatial relationships and accessibility measures

**CRITICAL:** Include ALL distance and accessibility measures here, regardless of how they're labeled elsewhere.

Extract ONLY distance/accessibility variables in this format:
**DISTANCE & ACCESSIBILITY VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all distance/accessibility variables...]

**INCLUDE in this category:** 
- **Distance Measures:** Home-to-crime distance, journey-to-crime, distance to CBD, distance to city center, distance between any locations
- **Transportation Networks:** Road networks, public transit systems, train/subway stations, bus stops, airports
- **Accessibility Indices:** Public transport accessibility, connectivity measures, network analysis results
- **Spatial Barriers:** Rivers when acting as barriers, highways as barriers, administrative boundaries
- **Travel Measures:** Travel time, travel cost, network distance vs. Euclidean distance

**EXCLUDE from this category:**
- Crime attractors at transport locations (→ ENVIRONMENTAL)
- Economic aspects of transportation (→ ECONOMIC)
- Population using transportation (→ DEMOGRAPHIC)

**Instructions:** Extract ALL variables measuring distance, travel time, accessibility, or spatial relationships between locations.

---

## PROMPT 13: TEMPORAL & CONTROL VARIABLES
**Focus:** Time effects and statistical controls

Extract ONLY temporal and control variables in this format:
**TEMPORAL & CONTROL VARIABLES:**
1. [Variable_Name | Description | Unit | Data_Source]
2. [Variable_Name | Description | Unit | Data_Source]
[Continue for all temporal and control variables...]

**Category includes:** Time effects, seasonal controls, spatial lags, jurisdictional dummies, interactions, fixed effects, year dummies, month effects, day-of-week effects, holiday effects, weather controls

**Instructions:** Extract ALL variables used for temporal controls, spatial controls, and statistical control purposes.

---

## PROMPT 14: MODEL FIT & PERFORMANCE METRICS
**Focus:** Statistical model performance and goodness-of-fit measures

Extract ONLY the following information:
- **Model Performance:** [R-squared, pseudo R-squared, log-likelihood values]
- **Information Criteria:** [AIC, BIC, other information criteria if reported]
- **Goodness-of-Fit Tests:** [Chi-square tests, likelihood ratio tests, other fit tests]
- **Model Comparison:** [Comparisons between different model specifications]
- **Sample Size Effects:** [How sample size affected model performance]
- **Convergence Issues:** [Any estimation or convergence problems mentioned]

**Instructions:** Look in results sections for statistical measures of model fit and performance. Focus on quantitative measures of how well the model fits the data.

---

## PROMPT 15: MAJOR FINDINGS & RESULTS
**Focus:** Key research findings and their implications

Extract ONLY the following information:
- **Main Results:** [Brief summary of the most important findings about location choice]
- **Significant Predictors:** [Variables that were statistically significant and their effects]
- **Effect Directions:** [Which variables increased/decreased choice probability]
- **Effect Magnitudes:** [Specific odds ratios, coefficients, or effect sizes if reported]
- **Surprising Findings:** [Any unexpected or counterintuitive results]
- **Robustness Checks:** [Any sensitivity analyses or robustness tests performed]

**Instructions:** Look in results sections. Focus on the substantive findings and their statistical significance.

---

## PROMPT 16: SCALE EFFECTS & SPATIAL FINDINGS
**Focus:** Spatial scale effects and scale-related findings

Extract ONLY the following information:
- **Scale Effects:** [Any findings about spatial scale or SUoA size effects on results]
- **Scale Sensitivity:** [How results varied with different spatial scales]
- **Spatial Autocorrelation:** [Tests for or findings about spatial dependence]
- **Scale Recommendations:** [Author recommendations about optimal spatial scales]
- **Scale Limitations:** [Limitations related to the chosen spatial scale]
- **Cross-Scale Comparisons:** [Comparisons with studies using different scales]

**Instructions:** Look for any discussion of how spatial scale affected the results, spatial dependence issues, or recommendations about spatial unit choices.

---

## PROMPT 17: DATA LIMITATIONS & METHODOLOGICAL ISSUES
**Focus:** Study limitations related to data quality and methodology

Extract ONLY the following information:
- **Data Quality Issues:** [Problems with data accuracy, completeness, or reliability]
- **Missing Data:** [What data was unavailable or missing]
- **Data Source Limitations:** [Limitations of specific data sources used]
- **Measurement Issues:** [Problems with how variables were measured or defined]
- **Temporal Limitations:** [Issues related to time period coverage or temporal resolution]
- **Geographic Limitations:** [Spatial coverage or boundary issues]
- **Model Limitations:** [Constraints or assumptions of the discrete choice model used]
- **Analytical Constraints:** [Statistical or methodological limitations]

**Instructions:** Look for author-mentioned limitations specifically related to data quality, availability, measurement issues, and methodological constraints.

---

## PROMPT 18: GENERALIZABILITY & COMPARATIVE LIMITATIONS
**Focus:** External validity and comparative constraints

Extract ONLY the following information:
- **Sample Limitations:** [Issues with sample size, representativeness, or selection]
- **Causal Inference:** [Limitations in establishing causality]
- **Generalizability:** [Issues with external validity or generalization]
- **Comparative Limitations:** [Inability to compare across contexts or time periods]
- **Context Specificity:** [How findings may be specific to the study context]
- **Population Constraints:** [Limitations related to the study population]

**Instructions:** Focus on limitations that affect the broader applicability and interpretation of findings.

---

## PROMPT 19: IMPLICATIONS & FUTURE DIRECTIONS
**Focus:** Broader significance and research recommendations

Extract ONLY the following information:
- **Theoretical Contributions:** [How the study advances crime location choice theory]
- **Policy Implications:** [What the findings suggest for policy or practice]
- **Crime Prevention Implications:** [What the findings suggest for crime prevention]
- **Urban Planning Implications:** [Relevance for urban design and planning]
- **Policy Recommendations:** [Specific policy suggestions based on findings]
- **Future Research Directions:** [Author suggestions for future research]
- **Spatial Scale Recommendations:** [Author suggestions for optimal spatial units or scales in future work]
- **Data Collection Suggestions:** [Recommendations for future data collection]
- **Methodological Improvements:** [Suggestions for improving analytical approaches]
- **Broader Societal Implications:** [Wider significance for society or criminal justice]
- **Interdisciplinary Connections:** [Links to other fields or disciplines]

**Instructions:** Look in discussion, conclusion, and future research sections for theoretical contributions, practical implications, and recommendations for future work.

---

## USAGE INSTRUCTIONS

### Logical Flow Explanation:
The prompts are now organized in 7 logical stages:

**Stage 1: Study Identification (Prompts 1-2)**
- Basic bibliographic information
- Temporal scope and data sources

**Stage 2: Spatial Scale & Crime Context (Prompts 3-5)**  
- Spatial units description and justification
- Study context and geography
- Sampling and choice sets

**Stage 3: Methodology & Design (Prompts 6-8)**
- Theoretical framework and objectives
- Study design and methodology
- Data preparation and processing

**Stage 4: Variables by Category (Prompts 9-13)**
- Demographic & social variables
- Economic variables
- Environmental & crime attractor variables
- Distance & accessibility variables
- Temporal & control variables

**Stage 5: Results & Performance (Prompts 14-16)**
- Model fit and performance metrics
- Major findings and results
- Scale effects and spatial findings

**Stage 6: Limitations & Issues (Prompts 17-18)**
- Data limitations and methodological issues
- Generalizability and comparative limitations

**Stage 7: Implications & Future Directions (Prompt 19)**
- Theoretical contributions and broader implications

### How to Use These Prompts:
1. **Follow the logical sequence** - each stage builds on the previous
2. **Use ONE prompt at a time** for each paper
3. **Complete all 19 prompts** for comprehensive extraction
4. **Focus only on the specified information** in each prompt
5. **Follow the exact format** provided in each prompt
6. **Move to next prompt** only after completing the current one

### Quality Control:
- Each prompt focuses on specific aspects with minimal overlap
- Logical flow reduces confusion and improves extraction accuracy
- Specialized focus ensures comprehensive coverage
- Can be used by different researchers for parallel extraction
- Results can be combined into comprehensive database

### Efficiency Benefits:
- **Stage-based approach** allows for strategic extraction planning
- **Related information grouped together** reduces back-and-forth reading
- **Clear progression** from basic identification to complex implications
- **Variable categorization stage** ensures systematic coverage
- **Results stage** captures all analytical outcomes

### Final Assembly:
After completing all 19 prompts following the logical flow, combine results into the comprehensive template for complete study profile. The logical sequence ensures no important information is missed and provides a natural narrative structure for systematic review synthesis.
