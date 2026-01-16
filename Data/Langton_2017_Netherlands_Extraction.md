# Data Extraction: Langton & Steenbeek 2017 - The Netherlands

## Study Identification

**Citation**: Langton, S. H., & Steenbeek, W. (2017). Residential burglary target selection: An analysis at the property-level using Google Street View. *Applied Geography*, 84, 1-8.

**DOI**: http://dx.doi.org/10.1016/j.apgeog.2017.06.014

## Geographic Information

### Country
**Value**: The Netherlands
**Supporting Quotes**: "The Netherlands Institute for the Study of Crime and Law Enforcement, The Netherlands" (Affiliation)
**Reasoning**: Second author's affiliation indicates Dutch study location.

### City
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Specific city or cities not mentioned in abstract.

### Geographic_Scope
**Value**: **[NEED FULL PAPER - likely city-level or neighborhood-level within specific urban area(s)]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Property-level analysis requires bounded geographic area; scope not specified in abstract.

## Spatial Unit Characteristics

### Spatial_Unit_Type
**Value**: Individual residential properties
**Supporting Quotes**: "an analysis at the property-level" (Title); "characteristics of individual properties within the same neighbourhood" (Abstract)
**Reasoning**: Study explicitly focuses on property-level target selection.

### Spatial_Unit_Name
**Value**: Residential dwelling/property
**Supporting Quotes**: "the physical attributes of residential homes and their immediate surrounding area" (Abstract)
**Reasoning**: Individual homes/dwellings as units of analysis.

### Number_of_Units
**Value**: **[NEED FULL PAPER - case-control design suggests matched pairs/sets of burglarized and non-burglarized properties]**
**Supporting Quotes**: "we utilise a case-control design to isolate property-level effects" (Abstract)
**Reasoning**: Case-control design requires sample of burglarized properties (cases) and non-burglarized properties (controls).

### Unit_Selection_Rationale
**Value**: Case-control matching to isolate property-level effects while controlling for neighborhood-level factors
**Supporting Quotes**: "we utilise a case-control design to isolate property-level effects" (Abstract)
**Reasoning**: Case-control design allows comparison of burglarized vs. non-burglarized properties within same neighborhoods.

**Rationale_Category**: Methodological appropriateness; Theoretical appropriateness

### Spatial_Aggregation
**Value**: No aggregation - individual properties analyzed
**Supporting Quotes**: "an analysis at the property-level" (Title); "characteristics of individual properties" (Abstract)
**Reasoning**: Finest possible spatial resolution for residential burglary analysis.

## Crime Data

### Crime_Type
**Value**: Residential burglary
**Supporting Quotes**: "Residential burglary target selection" (Title); "residential burglary" (Abstract, Keywords)
**Reasoning**: Study exclusively focuses on residential burglary.

### Number_of_Crimes
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract.

### Temporal_Scope
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract.

### Data_Source_Crime
**Value**: **[NEED FULL PAPER - likely police records]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not specified in abstract.

## Offender Information

### Offender_Data_Available
**Value**: No (property-level analysis, not offender-based)
**Supporting Quotes**: "case-control design to isolate property-level effects" (Abstract)
**Reasoning**: Case-control design compares properties, not offenders; focus on target characteristics rather than offender characteristics.

### Offender_Characteristics_Used
**Value**: N/A
**Supporting Quotes**: N/A
**Reasoning**: Not an offender-based discrete choice model; property-level case-control design.

## Model Specification

### Model_Type
**Value**: Conditional logistic regression (case-control design)
**Supporting Quotes**: "Analysis is carried out using conditional logistic regression" (Abstract)
**Reasoning**: Conditional logistic regression is standard for matched case-control designs.

### Dependent_Variable
**Value**: Binary indicator of whether a property was burglarized (case) or not (control)
**Supporting Quotes**: "we utilise a case-control design" (Abstract)
**Reasoning**: Standard case-control outcome comparing victimized vs. non-victimized properties.

### Unit_of_Analysis
**Value**: Individual residential property
**Supporting Quotes**: "an analysis at the property-level" (Title)
**Reasoning**: Each property is a unit of observation.

## Independent Variables

### Number_of_Variables
**Value**: **[NEED FULL PAPER - at least 5 categories mentioned in abstract]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Abstract mentions ease of escape, accessibility, surveillability, wealth indicators, collective efficacy as key variable categories.

### Independent_Variables
**Value**: 

**Property-level physical attributes (from Google Street View):**
1. Ease of escape from property **[NEED FULL PAPER for operationalization]**
2. Accessibility of dwelling **[NEED FULL PAPER for operationalization]**
3. Surveillability (visibility from neighbors/passers-by) **[NEED FULL PAPER for operationalization]**
4. Indications of resident wealth **[NEED FULL PAPER for operationalization]**

**Neighborhood-level control (interaction):**
5. Collective efficacy **[NEED FULL PAPER for operationalization]**

**Additional variables likely included:** **[NEED FULL PAPER]**
- Immediate surrounding area characteristics
- Other property attributes coded from Street View

**Supporting Quotes**: 
- "The ease of escape from a property, the extent to which the dwelling is accessible, and the extent to which it is closed to surveillance from neighbours and passers-by are all positively related to burglary risk" (Abstract)
- "There is no evidence that indications of resident wealth are related to the likelihood of victimisation, or that the effect of surveillability varies depending on the extent of collective efficacy in a neighbourhood" (Abstract)
- "the physical attributes of residential homes and their immediate surrounding area" (Abstract)

**Reasoning**: Variables explicitly tested in results; operationalizations require full paper.

### Variable_Categories
**Value**: 
- Target accessibility/permeability (escape, access)
- Target surveillability (visibility, guardianship)
- Target attractiveness (wealth indicators)
- Neighborhood context (collective efficacy)

**Supporting Quotes**: See Independent_Variables above
**Reasoning**: Conceptually aligned with rational choice/routine activity perspectives on burglary target selection.

## Data Sources (Beyond Crime Data)

### Data_Source_Environment
**Value**: Google Street View (GSV) images coded via Systematic Social Observation (SSO)
**Supporting Quotes**: 
- "Collecting fresh, micro-level data using Google Street View (GSV) as a tool of Systematic Social Observation (SSO)" (Abstract)
- "using Google Street View" (Title)
**Reasoning**: Primary innovation is use of GSV for property-level data collection.

### Data_Source_Sociodemographic
**Value**: **[NEED FULL PAPER - likely census or survey data for collective efficacy measure]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Collective efficacy mentioned but source not specified in abstract.

### Data_Processing_Methods
**Value**: Systematic Social Observation (SSO) coding protocol applied to Google Street View imagery
**Supporting Quotes**: "Google Street View (GSV) as a tool of Systematic Social Observation (SSO)" (Abstract)
**Reasoning**: Manual or semi-automated coding of property attributes from street-level imagery.

## Alternative Set Construction

### Alternative_Set
**Value**: Control properties matched to burglarized properties (likely within same neighborhood)
**Supporting Quotes**: "we utilise a case-control design to isolate property-level effects" (Abstract)
**Reasoning**: Case-control design requires selection of comparable non-burglarized properties; matching strategy details need full paper.

### Sampling_Strategy
**Value**: Matched case-control sampling **[NEED FULL PAPER for matching criteria and ratio]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Case-control design requires explicit matching strategy; details not in abstract.

## Model Results

### Key_Findings
**Value**: 

**Significant positive effects (increased burglary risk):**
- Ease of escape from property
- Accessibility of dwelling
- Closed to surveillance (low visibility from neighbors/passers-by)

**No significant effects:**
- Indications of resident wealth
- Interaction between surveillability and collective efficacy

**Supporting Quotes**:
- "The ease of escape from a property, the extent to which the dwelling is accessible, and the extent to which it is closed to surveillance from neighbours and passers-by are all positively related to burglary risk" (Abstract)
- "There is no evidence that indications of resident wealth are related to the likelihood of victimisation, or that the effect of surveillability varies depending on the extent of collective efficacy in a neighbourhood" (Abstract)

**Reasoning**: Main findings explicitly stated in abstract; align with rational choice perspective emphasizing risk minimization over reward maximization.

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
1. First quantitative property-level burglary study using Google Street View
2. Application of Systematic Social Observation to digital street imagery
3. Fresh micro-level data collection overcoming limitations of proxy measures from building registrar data
4. Quantitative test of offender-based interview/experimental findings
5. Case-control design isolating property effects from neighborhood effects

**Supporting Quotes**:
- "Property-level target selection research has been largely neglected in the quantitative literature, not because it lacks scientific or societal relevance, but because appropriate data is such a rarity" (Introduction)
- "Using variables not collected for the purposes of such research calls into question the statistical validity of measurements" (Introduction, critiquing proxy measures)
- "Collecting fresh, micro-level data using Google Street View (GSV) as a tool of Systematic Social Observation (SSO)" (Abstract)
- "To quantitatively test findings from offender-based literature" (Abstract)
- "We encourage future research to consider using GSV as a method of collecting fresh data, with the broader aim of explaining criminal behaviour at micro-spatial scales" (Abstract)

**Reasoning**: Methodological innovations address data limitations in property-level burglary research.

### Limitations
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not discussed in abstract.

## Theoretical Framework

### Theoretical_Foundation
**Value**: Rational choice theory; Routine activity theory (implied); Offender decision-making models
**Supporting Quotes**: 
- "To quantitatively test findings from offender-based literature—primarily consisting of small-sample interviews or experimental scenarios with convicted burglars" (Abstract)
- References to escape, accessibility, surveillance align with rational choice framework
**Reasoning**: Testing offender-reported decision factors suggests rational choice/routine activity framework.

### Key_Hypotheses
**Value**: 
1. Property-level physical attributes affect burglary risk beyond neighborhood-level factors
2. Escape routes, accessibility, and low surveillability increase burglary risk (from offender literature)
3. Wealth indicators increase burglary risk (from offender literature)
4. Surveillability effects may vary by neighborhood collective efficacy

**Supporting Quotes**: 
- "Burglary target selection does not stop at the selection of a target neighbourhood, but certain characteristics of individual properties within the same neighbourhood are in turn indicative of burglary risk" (Abstract)
- Testing of wealth and collective efficacy interactions mentioned in results
**Reasoning**: Hypotheses derived from prior offender-based research and neighborhood-level findings.

## Additional Notes

### Study_Context
**Value**: 
- Netherlands context with available Google Street View coverage
- Addresses gap between macro-level quantitative research and offender-based qualitative research
- Part of broader trend to use digital street imagery for crime research

**Supporting Quotes**: 
- "Researchers have recently come to acknowledge the role Google Maps can play in re-imagining the geospatial analysis of crime" (Introduction)
**Reasoning**: Methodological bridge between two research traditions.

### Comparison_to_Offender_Literature
**Value**: Quantitative findings partly support offender-based research
**Supporting Quotes**: "Quantitative analyses partly support findings from the offender-based research on residential burglary" (Abstract)
**Reasoning**: Some offender-reported factors (escape, access, surveillance) confirmed; others (wealth) not supported.

### Data_Availability
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not mentioned in abstract.

### Replication_Information
**Value**: Google Street View is publicly accessible; SSO coding protocol details needed for replication
**Supporting Quotes**: **[NEED FULL PAPER for coding protocol]**
**Reasoning**: GSV publicly available but coding scheme requires documentation.

---

## NOTES FOR COMPLETION

**Information available from abstract and partial introduction only. Full paper needed for:**
1. Specific city/cities in the Netherlands
2. Number of burglarized properties (cases) and controls
3. Temporal scope of burglary data
4. Exact variable operationalizations and measurement from GSV
5. Complete variable list and count
6. Matching strategy and case-control ratio
7. Model fit statistics and detailed results
8. Sensitivity analyses
9. Data sources for collective efficacy and other neighborhood controls
10. Discussion of limitations
11. Coding protocol details for replication

**Key methodological innovation:**
- Google Street View + Systematic Social Observation for property-level data
- Addresses data scarcity in property-level burglary research
- Quantitatively tests offender interview findings

**Key substantive findings:**
- Property characteristics matter within neighborhoods (escape, access, surveillance)
- Wealth indicators do not predict victimization (contradicts some offender literature)
- No collective efficacy × surveillability interaction

**Next steps: Obtain full paper to complete extraction.**
