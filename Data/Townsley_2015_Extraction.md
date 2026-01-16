# Data Extraction: Burglar Target Selection - A Cross-national Comparison

## Study Identification
- **Title**: Burglar Target Selection: A Cross-national Comparison
- **Authors**: Michael Townsley, Daniel Birks, Wim Bernasco, Stijn Ruiter, Shane D. Johnson, Gentry White, Scott Baum
- **Year**: 2015
- **Journal**: Journal of Research in Crime and Delinquency, Vol. 52(1) 3-31

---

## Study Location (Multi-site)

### Countries & Cities
- **Netherlands**: The Hague
- **United Kingdom**: Birmingham  
- **Australia**: Brisbane

### Total Study Area Size
**Not explicitly reported** for individual cities

**Note**: Can be approximated from Table 2:
- The Hague: ~58 km² (89 areas × 0.65 km² mean)
- Birmingham: ~267 km² (131 areas × 2.04 km² mean)  
- Brisbane: ~1,340 km² (158 areas × 8.48 km² mean)

### Spatial Units

#### The Hague (NL)
- **Spatial Unit Name**: Neighborhoods (statistical districts)
- **Number of Units**: 89
- **Mean Unit Size**: 0.65 km² (range: 0.13-3.18 km²)
- **Mean Households per Unit**: 2,380 (range: 212-7,476)
- **Average Population per Unit**: Not reported (only household data available)
- **Target Density**: 3,652 households/km²

#### Birmingham (UK)
- **Spatial Unit Name**: Super Output Areas (medium layer)
- **Number of Units**: 131
- **Mean Unit Size**: 2.04 km² (range: 0.53-13.11 km²)
- **Mean Households per Unit**: 3,086 (range: 2,092-5,067)
- **Average Population per Unit**: Not reported (only household data available)
- **Target Density**: 1,513 households/km²

#### Brisbane (AU)
- **Spatial Unit Name**: Statistical Local Areas
- **Number of Units**: 158
- **Mean Unit Size**: 8.48 km² (range: 0.7-184.85 km²)
- **Mean Households per Unit**: 2,537 (range: 94-7,344)
- **Average Population per Unit**: Not reported (only household data available)
- **Target Density**: 299 households/km²

---

## Spatial Unit Selection

### Spatial Aggregation
**No** - Used predefined census units (neighborhoods, Super Output Areas, Statistical Local Areas)

### Unit Selection Rationale
**Supporting Quote**: "Study regions and their associated choice set geographies (NL—neighborhoods, UK—Super Output Areas [medium layer], AU—Statistical Local Areas) were selected based on equivalence in size of the known burglar population, number of targets (households) within each area, and total number of areas in a study region. Study regions were purposefully selected based on differences in target density"

**Reasoning**: 
- Selected for equivalence in: (1) offender population size, (2) number of targets per area, (3) total number of areas
- Purposefully selected to vary in target density (high, medium, low)
- Used predefined administrative/statistical boundaries

### Rationale Category
**Previous research and data availability**
- Following Bernasco & Nieuwbeerta (2005) framework
- Census units readily available in all three countries
- Practical constraints of cross-national comparison
- Selected for comparability (equivalence in offender population, targets per area, total areas)
- Purposefully varied target density (high/medium/low) for comparison

---

## Data Sources & Collection

### Data Collection Period
- **Netherlands (The Hague)**: 1996-2001
- **United Kingdom (Birmingham)**: 2009
- **Australia (Brisbane)**: 2006

### Data Sources
1. **Police recorded crime data** (cleared burglaries)
2. **Census/Statistical agency data**:
   - NL: Municipal Agency for Urban Development
   - UK: Office of National Statistics (ONS)
   - AU: Australian Bureau of Statistics

**Number of Data Sources**: 2

---

## Crime Data

### Crime Type
**Residential burglary**

### Crime Type Group
**Property crime**

### Crime Incidents (Cleared Offenses)
- **The Hague**: 548 cleared burglaries
- **Birmingham**: 398 cleared burglaries
- **Brisbane**: 889 cleared burglaries

### Offender Characteristics
- **The Hague**: 290 unique offenders (5.3% juvenile)
- **Birmingham**: 291 unique offenders (13.6% juvenile)
- **Brisbane**: 273 unique offenders (8.0% juvenile)

**Note**: Co-offending burglaries removed (276 NL, 94 UK, 247 AU cases)

---

## Independent Variables

### Number of Variables
**6 main variables** (plus interaction terms)

### Variable List
1. **Residential real estate value** (affluence - measured differently: NL & UK = property value; AU = housing repayment; aggregated into deciles)
2. **Residential mobility** (turnover - incoming + outgoing proportion × 10)
3. **Proportion of single-family dwellings** (accessibility - % × 10)
4. **Proximity to offender home** (distance in km × -1)
5. **Proximity to city center** (distance in km × -1)
6. **Number of households** (target availability - in 1000s)

**Additional**: Age interaction (juvenile vs. adult, based on legal driving age)

---

## Statistical Model

### Model Type
**Conditional logit model** (discrete spatial choice approach)

**Supporting Quote**: "The conditional logit model was used to estimate parameters of the behavioral rule using maximum likelihood methods"

### Model Specification
- Utility function: Uij = βxij + εij
- Single consolidated model with study region interactions
- Robust standard errors (clustered by offender)

### Model Fit Statistics
- **AIC and BIC** reported
- **Odds Ratios (OR)** presented
- **Standard errors** (robust Huber-White estimates)

---

## Results Summary

### Consistent Findings (All 3 Regions)
1. **Proximity to home**: Strong positive effect (OR: NL=1.67, UK=1.90, AU=1.21)
2. **Single-family dwellings**: Positive effect (OR: NL=1.19, UK=1.12, AU=1.13)
3. **Number of households**: Positive effect (OR: NL=1.34, UK=1.76, AU=1.47)
4. **Juvenile-adult differential**: Juveniles more influenced by proximity

### Inconsistent Findings
- **Real estate value**: Negative in NL (OR=0.92), non-significant in UK & AU
- **Residential mobility**: Non-significant in all regions
- **Proximity to city center**: Positive in AU only (OR=1.06)

### Statistical Comparisons
**Supporting Quote**: "Drawing on initial research undertaken by Bernasco and Nieuwbeerta (2005), this article tests the applicability of their theoretical account of crime placement in multiple environments by comparing burglar target selection across different cities"

- Wald tests conducted for between-region differences
- Proximity effects significantly different between Brisbane vs. The Hague/Birmingham
- No difference in single-family dwelling effects across regions

---

## Coefficients & Effect Sizes

### Coefficients
**Yes** - Reported as **odds ratios** with standard errors

**Example from Table 3**:
- NL Proximity: OR = 1.67 (SE = 0.14)**
- UK Proximity: OR = 1.90 (SE = 0.12)**
- AU Proximity: OR = 1.21 (SE = 0.03)**

### Confidence Intervals
**Standard errors reported**, not explicit confidence intervals

### Effect Sizes
**Yes** - Odds ratios interpreted as effect sizes
- OR > 1 indicates increased odds
- Percentage changes calculated (e.g., "67% increase in odds per km")

---

## Software Used
**Not explicitly stated** in the paper

---

## MAUP Discussion

### MAUP Mentioned
**No explicit MAUP discussion** - but discusses scale/aggregation effects

**Supporting Quote**: "differences in the magnitude of impact that target proximity has on location choices of offenders are of particular interest because it may suggest that the commonly observed distance decay curve is at least in part reflective of the number of potential targets available to offenders within a given distance"

**Discussion Points**:
- Differences in spatial unit size affect results (acknowledged)
- Target density varies by unit size (3,652 vs. 1,513 vs. 299 households/km²)
- "This observation is consistent with the notion of intervening opportunities"
- Acknowledges "environment morphology affects crime volumes"
- Notes that "offender cohort effects" and "environmental differences" create area effects
- Does NOT use term "MAUP" or "modifiable areal unit problem"

**Note**: The entire study design (comparing 3 different sized units) implicitly addresses MAUP by examining how results vary with different spatial aggregations, but they frame this as "environmental context" rather than MAUP.

---

## Sensitivity Analysis

### Conducted
**Yes** - Extensive cross-region comparisons

**Supporting Quote**: "consolidating multiple study regions into a single model enabled statistical rather than descriptive comparisons of the hypothesized behavioral rule across study regions; thus, formally quantifying study setting effects on parameter estimates between environments"

**Methods**:
1. Consolidated model with region interactions
2. Bivariate Wald tests between regions
3. Separate models for juveniles vs. adults
4. Post-hoc tests of effect magnitude differences

---

## Data Limitations

### Acknowledged Limitations

1. **Cleared offense data only**
   - "findings rely solely on cleared offense data"
   - Non-probabilistic sample of all burglaries

2. **Crime trip origin assumption**
   - "analyses presented assume crime trips originate from an offender's residence"

3. **Temporal mismatch**
   - Census data (2011 for some) vs. crime data (1996-2001, 2006, 2009)

4. **Measurement inconsistencies**
   - Real estate value measured differently across regions
   - Could not measure ethnic heterogeneity consistently

5. **Missing variables**
   - "alternative methodologies such as offender interviews and ethnographic studies suffer from the same nonprobabilistic sampling issue"

6. **Co-offending removed**
   - Multiple offender cases excluded

---

## Computational Constraints

### Mentioned
**No explicit computational constraints** discussed

---

## Alternative Units Discussed

### Alternative Units
**Not discussed**

**Reasoning**: Study followed existing framework from Bernasco & Nieuwbeerta (2005), using administratively defined census units available in each country.

---

## Future Research Suggestions

### Suggested Directions

1. **More study regions**
   - "criminological knowledge can be made broader by applying the specified behavioral rule in more study regions"

2. **Expanded behavioral rule**
   - "expanding the behavioral rule to include more nuanced factors"
   - Include street network characteristics (permeability, connectedness)
   - Incorporate public transport networks

3. **Mixed logit models**
   - "Relaxing this assumption involves the use of a more complex statistical model, the mixed logit"
   - Allow individual variation in preferences (not constant effects)

4. **Intervening opportunities**
   - Further explore the role of target density in distance decay

---

## Theoretical Framework

### Key Theories
1. **Discrete spatial choice approach** (Bernasco & Nieuwbeerta 2005)
2. **Rational choice perspective**
3. **Routine activity approach**
4. **Crime pattern theory**
5. **Social disorganization theory**

### Behavioral Rule
Offenders select neighborhoods that:
1. Appear to contain valuable goods (REWARD)
2. Require little effort (EFFORT)
3. Have low risk of detection (RISK)

---

## Key Contributions

1. **Systematic cross-national replication**
2. **Statistical comparison** of effects across regions (not just descriptive)
3. **Quantification of environmental effects** on offender behavior
4. **Consolidated modeling approach** for replications

---

## Notes for CSV Entry

- **Multiple spatial units**: This is a multi-site study with different units in each location
- **Spatial unit**: Could be coded as "Multiple: Neighborhoods (NL), Super Output Areas (UK), Statistical Local Areas (AU)"
- **Unit size**: Highly variable (0.65-8.48 km² mean)
- **Rationale category**: "Data availability, previous research"
- **Crime type**: Single (residential burglary only)
- **Model type**: Conditional logit
- **MAUP discussion**: Yes (implicit through cross-region analysis)
- **Sensitivity analysis**: Yes (extensive)

---

## Missing/Uncertain Information

### Not Explicitly Reported in Paper:
1. **Total study area size** - Only calculable from mean × number of units
2. **Population per unit** - Only household counts provided
3. **Software used** - Not mentioned
4. **Total burglaries** (cleared + uncleared) - Only cleared burglaries reported
5. **Explicit MAUP term** - Concept discussed but term not used
6. **Confidence intervals** - Only standard errors provided

### Different Measurement Across Sites:
1. **Real estate value** - NL & UK used property prices; AU used housing repayments (then converted to deciles for comparability)
2. **Legal driving age** - NL: 18 years; UK & AU: 17 years (affects juvenile definition)

---

## Verification Notes

**Extraction verified against**:
- Tables 1, 2, 3, 4, 5, 6, A1
- Methods section (pages 9-15)
- Results section (pages 15-19)
- Discussion and limitations (pages 19-23)
- Appendix (equations A1, A2)

**All direct quotes verified** from original paper text.

**Data quality**: High - all numbers cross-checked with multiple tables in paper.
