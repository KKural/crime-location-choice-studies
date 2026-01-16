# Data Extraction: Yue et al. 2023 - Guangzhou, China

## Study Identification

**Citation**: Yue, H., Liu, L., & Xiao, L. (2023). Investigating the effect of people on the street and streetscape physical environment on the location choice of street theft crime offenders using street view images and a discrete spatial choice model. *Applied Geography*, 157, 103025.

**DOI**: https://doi.org/10.1016/j.apgeog.2023.103025

## Geographic Information

### Country
**Value**: China
**Supporting Quotes**: "a large Chinese city" (Abstract)
**Reasoning**: Study conducted in China.

### City
**Value**: Guangzhou
**Supporting Quotes**: "Guangzhou University, Guangzhou, 510006, China" (Affiliation); "ZG city" referenced in methodology
**Reasoning**: Authors affiliated with Guangzhou University; "ZG city" is abbreviation for Guangzhou (confirmed in previous CSV data showing 2,643 communities and 7,434 km² total area matching Guangzhou).

### Geographic_Scope
**Value**: City-level
**Supporting Quotes**: "a large Chinese city" (Abstract)
**Reasoning**: Study focuses on a single city (Guangzhou).

## Spatial Unit Characteristics

### Spatial_Unit_Type
**Value**: Community (administrative neighborhood units)
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Based on CSV data indicating 2,643 communities in Guangzhou; communities are administrative units in Chinese urban areas.

### Spatial_Unit_Name
**Value**: Community (社区, shequ)
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Standard administrative unit in Chinese cities below district level.

### Number_of_Units
**Value**: 1,636 (from 2,643 total available communities)
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Based on CSV data showing 1,636 communities used in analysis from 2,643 total communities in Guangzhou.

### Unit_Selection_Rationale
**Value**: **[NEED FULL PAPER - likely communities with sufficient crime data or street view image coverage]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract; 1,636 of 2,643 communities used suggests data availability constraints.

**Rationale_Category**: **[NEED FULL PAPER]**

### Spatial_Aggregation
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Need to verify whether crime data were geocoded directly to communities or aggregated from finer spatial units.

## Crime Data

### Crime_Type
**Value**: Street theft
**Supporting Quotes**: "the location choice of street theft crime offenders" (Title and Abstract)
**Reasoning**: Study explicitly focuses on street theft crimes.

### Number_of_Crimes
**Value**: 1,540
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Based on CSV data indicating 1,540 street thefts analyzed.

### Temporal_Scope
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Time period not mentioned in abstract.

### Data_Source_Crime
**Value**: **[NEED FULL PAPER - likely local police department]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract.

## Offender Information

### Offender_Data_Available
**Value**: Yes
**Supporting Quotes**: "discrete spatial choice model to investigate the influence of people on the street and the streetscape's physical environments on the location choice of street theft crime offenders" (Abstract)
**Reasoning**: Discrete spatial choice models require individual offender-level data.

### Offender_Characteristics_Used
**Value**: **[NEED FULL PAPER - likely residential location, possibly age, gender, ethnicity based on literature cited]**
**Supporting Quotes**: "Controlling the influence of residence-crime proximity" (Abstract); "Individual characteristics like age...gender...ethnicity" (Introduction, referencing prior studies)
**Reasoning**: Model controls for residence-crime proximity; introduction cites research on age, gender, ethnicity effects.

## Model Specification

### Model_Type
**Value**: Discrete spatial choice model (conditional logit)
**Supporting Quotes**: "we constructed a discrete spatial choice model" (Abstract)
**Reasoning**: Standard discrete choice modeling approach for crime location choice.

### Dependent_Variable
**Value**: Binary indicator of whether a community was chosen as the crime location by an offender
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Standard dependent variable structure for discrete spatial choice models.

### Unit_of_Analysis
**Value**: Offender-community dyad
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Discrete spatial choice models analyze each offender's choice among available spatial units.

## Independent Variables

### Number_of_Variables
**Value**: **[NEED FULL PAPER - at least 13 based on abstract]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Abstract mentions 6 physical environment elements + people on street + residence-crime proximity + crime attractors + generators + detractors + socioeconomic features = at least 13 categories of variables.

### Independent_Variables
**Value**: 

**Streetscape variables (extracted from street view images):**
1. Number of people on the street
2. Fences (presence/area)
3. Walls (presence/area)
4. Windows (presence/area)
5. Grass (presence/area)
6. Sidewalk (presence/area)
7. Plants (presence/area)

**Control variables:**
8. Residence-crime proximity (distance from offender's home)
9. Crime attractors **[NEED FULL PAPER for specific measures]**
10. Crime generators **[NEED FULL PAPER for specific measures]**
11. Crime detractors **[NEED FULL PAPER for specific measures]**
12. Socioeconomic features **[NEED FULL PAPER for specific measures]**

**Supporting Quotes**: 
- "The extracted elements include fences, walls, windows, grass, sidewalk, and plants" (Abstract)
- "the number of people on the street" (Abstract)
- "Controlling the influence of residence-crime proximity, crime attractors, generators, detractors, and socioeconomic features" (Abstract)

**Reasoning**: Variables explicitly mentioned in abstract; specific operationalizations require full paper.

### Variable_Categories
**Value**: 
- Streetscape physical environment (7 variables from street view images)
- Social environment (1 variable: people on street)
- Spatial distance (1 variable: residence-crime proximity)
- Crime opportunities (3 categories: attractors, generators, detractors)
- Neighborhood characteristics (1 category: socioeconomic features)

**Supporting Quotes**: See Independent_Variables above
**Reasoning**: Grouped based on conceptual categories mentioned in abstract.

## Data Sources (Beyond Crime Data)

### Data_Source_Environment
**Value**: Street view images; **[NEED FULL PAPER for other sources]**
**Supporting Quotes**: "extract on-street people and physical environment elements from fine-grained street view images (SVIs)" (Abstract)
**Reasoning**: Primary innovation is use of street view images with deep learning.

### Data_Source_Sociodemographic
**Value**: **[NEED FULL PAPER - likely census or administrative data]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Socioeconomic variables mentioned but source not specified in abstract.

### Data_Processing_Methods
**Value**: Integrative deep learning algorithm (object detection network + semantic segmentation network) applied to street view images
**Supporting Quotes**: "applied an integrative deep learning algorithm combining an object detection network and a semantic segmentation network to extract on-street people and physical environment elements from fine-grained street view images" (Abstract)
**Reasoning**: Novel methodological contribution of the study.

## Alternative Set Construction

### Alternative_Set
**Value**: **[NEED FULL PAPER - likely all 1,636 communities for each offender, or sampled subset]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract; critical methodological detail.

### Sampling_Strategy
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not mentioned in abstract.

## Model Results

### Key_Findings
**Value**: 

**Streetscape variables:**
- People on the street: Significant positive effect (more people attract offenders)
- Fences: Significant positive effect (attract offenders)
- Plants: Significant positive effect (attract offenders)
- Grass: Significant negative effect (deter offenders)
- Sidewalk: Significant negative effect (deter offenders)
- Walls: No significant effect
- Windows: No significant effect

**Model performance:**
- Model performance improved after including streetscape variables
- Streetscape context essential for understanding crime location preferences

**Control variables:**
- Most control variables conform to previous research findings

**Supporting Quotes**:
- "the number of people on the street presents a significantly positive relationship with the offenders' preferences" (Abstract)
- "Fences and plants have significant and positive effects on attracting criminals" (Abstract)
- "Grasses and sidewalks negatively affect offenders' location choices" (Abstract)
- "Walls and windows do not significantly affect criminals' crime location choices" (Abstract)
- "Results reveal an improvement in model performance after the streetscape variables are considered" (Abstract)
- "the associations between most control variables and offenders' preferences for crime locations conform to previous research findings" (Abstract)

**Reasoning**: Results explicitly stated in abstract.

### Model_Fit_Statistics
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not reported in abstract.

### Sensitivity_Analysis
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not mentioned in abstract.

## Methodological Innovations

### Novel_Contributions
**Value**: 
1. First study to use street view images with deep learning to measure streetscape conditions for crime location choice analysis
2. First to distinguish on-street people from total ambient population (vs. social media/phone data)
3. Integration of object detection and semantic segmentation networks for environmental feature extraction
4. Application to street theft crime in Chinese urban context

**Supporting Quotes**:
- "fine-grained streetscape data are hard to obtain. Traditional data-gathering methods like questionnaires, field surveys, and manual audits are inefficient and only applied to small areas" (Abstract)
- "Social media and mobile phone data have recently been used to measure the ambient population. However, they are unable to distinguish between indoor and on-street people" (Abstract)
- "As the first attempt in combining SVIs, deep learning algorithms, and discrete spatial choice model, this study makes a contribution to the extant crime location choice literature" (Abstract)

**Reasoning**: Methodological innovations explicitly highlighted in abstract.

### Limitations
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not discussed in abstract.

## Theoretical Framework

### Theoretical_Foundation
**Value**: Crime pattern theory (implied); Routine activity theory (implied)
**Supporting Quotes**: 
- "characteristics of target areas such as the presence of crime generators and crime attractors" (Introduction)
- "the guardianship of ambient population" (Introduction)
**Reasoning**: References to generators, attractors, and guardianship indicate routine activity/crime pattern theory framework.

### Key_Hypotheses
**Value**: 
1. People on the street affect street crime location choice
2. Physical streetscape elements affect street crime location choice
3. Effects persist when controlling for traditional factors (distance, attractors, generators, detractors, socioeconomics)

**Supporting Quotes**: "it is reasonable to assume that the location choice of street crime offenders is affected by streetscape conditions and people on street. However, this issue has not been investigated by previous research" (Abstract)
**Reasoning**: Study designed to test whether streetscape factors matter beyond traditional variables.

## Additional Notes

### Study_Context
**Value**: Guangzhou is a major Chinese megacity with population over 15 million; uses community (shequ) administrative units as spatial framework
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Context important for interpreting findings.

### Data_Availability
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not mentioned in abstract.

### Replication_Information
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not mentioned in abstract.

---

## NOTES FOR COMPLETION

**Information available from abstract only. Full paper needed for:**
1. Exact number of variables and their operationalizations
2. Temporal scope of crime data
3. Data sources for socioeconomic and crime opportunity variables
4. Alternative set construction and sampling strategy (if any)
5. Model fit statistics and detailed results
6. Sensitivity analyses
7. Discussion of limitations
8. Complete methodological details
9. Verification of community counts and geographic coverage
10. Specific measures of attractors, generators, detractors

**Key details confirmed from CSV file:**
- 1,636 communities used (from 2,643 total)
- 1,540 street theft crimes
- Location: Guangzhou (ZG city)
- Average community size: ~2.81 km²

**Next steps: Obtain full paper to complete extraction.**
