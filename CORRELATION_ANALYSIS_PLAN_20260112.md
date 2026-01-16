# Fresh Correlation & Comparative Analysis Plan
## Systematic Review of Spatial Unit Selection in Crime Analysis

**Created**: January 12, 2026  
**Updated**: January 13, 2026  
**Purpose**: Identify meaningful variable relationships to answer research questions  
**Dataset**: 57 studies with comprehensive SUoA variables

---

## TEMPORAL VARIABLE DECISION FRAMEWORK

### **Primary Temporal Variable: Publication Year**
**Rationale**: Unit size *choice* reflects methodological decision-making at time of publication, not data collection
- Researcher decisions about unit selection happen during analysis/writing phase
- Publication year captures contemporary methodological trends, theoretical understanding, and MAUP awareness
- Testing "Are researchers choosing smaller units over time?" requires publication year

### **Secondary Analysis: Data_Recency Variable**
**Definition**: Data_Recency = Publication_Year - Data_Collection_Year
**Purpose**: Test if studies using older data choose different unit sizes
- Hypothesis: Older data may force larger units due to data availability constraints
- Example: 2020 publication using 2010 data has Data_Recency = 10 years

### **Handling Data Collection Year Variations**

**For ranges** (e.g., "2005-2010"):
- Use **midpoint**: (2005 + 2010) / 2 = 2007.5
- Represents temporal center of data collection period

**For multiple discrete years** (e.g., "2008, 2010, 2012"):
- Use **median**: 2010
- More robust than mean if years are unevenly spaced

**For multiple data sources** with different years:
- Use the **crime data year** (primary outcome variable)
- If crime data has range, use midpoint

---

## CORE RESEARCH QUESTIONS

### **PRIMARY RQ**: Are studies using smaller spatial units over time?
- **Hypothesis**: Unit sizes decreasing over time (technology + data availability improving)
- **Key Analysis**: Publication Year → Unit_Size (negative correlation expected)

### **SECONDARY RQs**: What influences unit selection decisions?

1. **RQ1**: What factors influence spatial unit selection?
   - Data availability/limitations
   - Computational constraints
   - Theoretical rationale
   - Geographic context (country, city)

2. **RQ2**: Do researchers consider MAUP when selecting units?
   - MAUP discussion presence over time
   - MAUP discussion → unit selection rationale
   - Sensitivity analysis practices

3. **RQ3**: What other methodological factors affect unit choice?
   - Distance variables → unit size
   - Model complexity → unit size
   - Crime type → unit size
   - Number of units → unit size (trade-offs)

---

## ANALYSIS STRATEGY: 6 CORRELATION CLUSTERS

### 📊 **CLUSTER 1: TEMPORAL TRENDS** 
**Question**: *How has the field evolved over time?*

#### A1. Methodological Sophistication Over Time
```r
# Year vs Number_of_Variables (Are studies becoming more complex?)
# Year vs Model_Type (Evolution of statistical methods)
# Year vs Software_Used (Technology adoption patterns)
```

**Expected Insights**:
- Increasing variable counts = more sophisticated modeling
- Shift from basic logit to mixed/hierarchical models
- Software trends (SPSS → R → Python)

#### A2. MAUP Awareness Evolution
```r
# Year vs MAUP (Binary: discussed/not discussed)
# Year vs Sensitivity (Binary: performed/not performed)  
# Year vs Alternative_Units (Binary: mentioned/not mentioned)
```

**Expected Insights**:
- MAUP awareness increasing post-2010?
- Sensitivity analysis adoption rate
- Growing discussion of alternative units

#### A3. Unit Type Preferences Over Time
```r
# Year vs Spatial_Unit_Name (Categorical)
# Year vs Unit_Size (Continuous, if parsed)
# Year vs Number_of_Units (Continuous)
```

**Expected Insights**:
- Shift from large (census tracts) to small (street segments)?
- Increasing number of units in studies (finer resolution)?
- Standardization of unit types in recent years?

**Visualizations**:
- Line chart: Average Number_of_Variables by Year
- Stacked bar: MAUP discussion (Yes/No) by Year
- Box plot: Unit_Size distribution by Year (grouped: pre-2010, 2010-2015, 2016-2020, 2021+)

---

### 🗺️ **CLUSTER 2: SPATIAL UNIT CHARACTERISTICS**
**Question**: *What determines unit selection?*

#### B1. Unit Size Relationships
```r
# Unit_Size vs Number_of_Units (Inverse relationship?)
# Unit_Size vs Total_Study_Area (Proportion analysis)
# Unit_Size vs Average_Population_per_Unit (Density proxy)
# Unit_Size vs Crime_Incidents (Scale effects)
```

**Expected Insights**:
- Smaller units = more units (mechanical relationship)
- Unit size determined by population density?
- Crime incident counts vary by unit size (aggregation effects)

#### B2. Unit Selection Rationales
```r
# Rationale_Category vs Unit_Size (Which rationales lead to small/large units?)
# Rationale_Category vs Spatial_Unit_Name (Certain rationales favor certain unit types?)
# Spatial_Aggregation vs Unit_Size (Aggregated data = larger units?)
```

**Expected Insights**:
- "Data availability" rationale → larger, administrative units
- "Theory-driven" rationale → smaller, micro-place units
- Aggregation occurs when original units too small

#### B3. Geographic Patterns
```r
# Country vs Spatial_Unit_Name (Cultural/administrative differences?)
# Country vs Unit_Size (US census tracts vs Dutch postal codes)
# Country vs Number_of_Units (Study scale variations)
```

**Expected Insights**:
- Country-specific administrative units (e.g., UK: Output Areas, NL: Postal Codes)
- Anglo countries favor smaller units (street segments)?
- Asian/European studies use larger administrative units?

**Visualizations**:
- Scatter plot: Unit_Size vs Number_of_Units (log-log scale)
- Heat map: Country × Spatial_Unit_Name
- Bar chart: Average Unit_Size by Rationale_Category

---

### 🚧 **CLUSTER 3: DATA LIMITATIONS & CONSTRAINTS**
**Question**: *How do constraints shape unit selection?*

#### C1. Data Availability Impacts
```r
# Data_Limitations (Binary) vs Unit_Size (Larger units when limited?)
# "Smallest unit due to data availability" vs Unit_Size
# "Spatial-unit data limitation" vs Alternative_Units mentioned
```

**Expected Insights**:
- Data limitations → larger units (forced aggregation)
- Studies acknowledging limitations more likely to discuss alternatives
- Smallest available unit ≠ optimal unit

#### C2. Computational Constraints
```r
# Computational_Constraints (Yes/No) vs Number_of_Units (Too many units?)
# Computational_Constraints vs Number_of_Variables (Model complexity?)
# Computational_Constraints vs Model_Type (Simpler models when constrained?)
```

**Expected Insights**:
- Computational limits appear with >5,000 units OR >50 variables
- Conditional logit less demanding than mixed logit
- Recent studies (better computers) → fewer computational constraints

#### C3. Limitation Types
```r
# Limitation_type (Categorical) vs Data_Sources
# Number_of_Data_Sources vs Data_Limitations (Binary)
# Data_Collection_Period vs Crime_data_period (Temporal mismatch?)
```

**Expected Insights**:
- Single data source → more limitations
- Temporal mismatches between crime data and predictors
- Administrative boundaries as limitations

**Visualizations**:
- Mosaic plot: Data_Limitations × Computational_Constraints
- Box plot: Number_of_Units by Limitation presence
- Bar chart: Limitation_type frequency with Unit_Size overlay

---

### 📏 **CLUSTER 4: DISTANCE VARIABLES ANALYSIS** ⭐
**Question**: *How do distance variables relate to other study characteristics?*

#### D1. Distance Variables & Model Complexity
```r
# Distance_variables (Binary or Count) vs Number_of_Variables
# Distance_variables vs Model_Type (Which models use distance?)
# Distance_variables vs Independent_Variables (Categorize types)
```

**Expected Insights**:
- Studies with distance variables have more total variables (>15?)
- Distance common in conditional logit (journey-to-crime models)
- Distance variables = environmental criminology theory

#### D2. Distance Variables & Unit Selection
```r
# Distance_variables vs Unit_Size (Smaller units when using distance?)
# Distance_variables vs Spatial_Unit_Name (Micro-places favor distance?)
# Distance_variables vs Number_of_Units (More units = more distance variation?)
```

**Expected Insights**:
- Distance analysis requires fine-grained units (street segments, parcels)
- Point-pattern analysis uses distance; area-based analysis doesn't
- More units → better distance granularity

#### D3. Distance Variables & Methodological Rigor
```r
# Distance_variables vs MAUP (Distance studies more aware of MAUP?)
# Distance_variables vs Sensitivity (Distance requires sensitivity checks?)
# Distance_variables vs Software_Used (GIS software presence?)
```

**Expected Insights**:
- Distance-based studies MORE likely to discuss MAUP
- Sensitivity analysis more common with distance (scale sensitivity)
- GIS software (ArcGIS, QGIS) required for distance calculations

**Visualizations**:
- Bar chart: Average Number_of_Variables by Distance_variables (Yes/No)
- Mosaic plot: Distance_variables × MAUP discussion
- Box plot: Unit_Size distribution by Distance_variables presence

---

### 🔬 **CLUSTER 5: METHODOLOGICAL RIGOR**
**Question**: *What predicts methodological quality?*

#### E1. MAUP Engagement
```r
# MAUP (Binary) vs Year (Trend)
# MAUP vs Sensitivity_Analysis (Co-occurrence)
# MAUP vs Alternative_Units (Discussing alternatives = MAUP aware?)
# MAUP vs Rationale_Category (Which rationales consider MAUP?)
```

**Expected Insights**:
- MAUP discussion increasing over time
- MAUP discussion → sensitivity analysis (70%+ overlap?)
- Theory-driven rationales more likely to address MAUP

#### E2. Model Sophistication
```r
# Model_Type vs Number_of_Variables (Complex models = more variables?)
# Model_Type vs Year (Model evolution)
# Model_Type vs Software_Used (Software capabilities)
```

**Expected Insights**:
- Mixed logit models have more variables than conditional logit
- Hierarchical models emerging post-2015
- R/Python enable complex models; SPSS → simpler models

#### E3. Transparency & Reproducibility
```r
# Software_Used (Reported/Not) vs Year (Increasing transparency?)
# Alternative_Units mentioned vs MAUP (Transparency proxy)
# Data_Sources (Count) vs Number_of_Variables (Data richness)
```

**Expected Insights**:
- Software reporting increasing (reproducibility movement)
- Discussing alternatives = transparency
- More data sources = more comprehensive studies

**Visualizations**:
- Venn diagram: MAUP discussion ∩ Sensitivity analysis ∩ Alternative units
- Stacked bar: Model_Type by Year (evolution)
- Heat map: Rationale_Category × MAUP discussion

---

### 🌍 **CLUSTER 6: CRIME TYPE & CONTEXT**
**Question**: *Does crime type influence unit selection?*

#### F1. Crime Type Effects
```r
# Crime_Type_Group vs Unit_Size (Single vs. Multiple crimes)
# Crime_Type_Group vs Number_of_Variables (Crime-specific predictors?)
# Crime_Type vs Spatial_Unit_Name (Crime-specific units?)
```

**Expected Insights**:
- Specific crimes (burglary) → smaller units (address-level)
- General crime (all crime) → larger units (neighborhoods)
- Multiple crime types → more variables (diverse predictors)

#### F2. Crime Context
```r
# Crime_Incidents (Count) vs Unit_Size (Rare crimes need larger units?)
# Crime_Incidents vs Number_of_Units (Sparse data problem?)
# Crime_Type vs Distance_variables (Journey-to-crime studies?)
```

**Expected Insights**:
- Low-frequency crimes → larger units (statistical power)
- Journey-to-crime (robbery) uses distance variables
- Property crimes more common than violent crimes in studies

#### F3. Geographic Crime Patterns
```r
# Country vs Crime_Type (Regional research focuses?)
# City vs Crime_Type (Urban crime types vary?)
# Total_Study_Area vs Crime_Incidents (Density proxy)
```

**Expected Insights**:
- US: focus on violent crime (guns, robbery)
- Europe: property crime (burglary, theft)
- Larger study areas have more incidents (mechanical)

**Visualizations**:
- Box plot: Unit_Size by Crime_Type_Group
- Scatter: Crime_Incidents vs Number_of_Units (colored by Unit_Size)
- Bar chart: Crime_Type frequency by Country

---

## STATISTICAL METHODS

### For Continuous × Continuous
- **Pearson correlation** (if normally distributed)
- **Spearman correlation** (non-parametric, robust)
- **Scatter plots** with regression lines
- **Example**: Year vs Number_of_Variables

### For Categorical × Continuous
- **ANOVA** / Kruskal-Wallis (comparing groups)
- **Box plots** / Violin plots
- **Example**: Rationale_Category vs Unit_Size

### For Categorical × Categorical
- **Chi-square test** (independence)
- **Cramér's V** (effect size)
- **Mosaic plots** / Heat maps
- **Example**: MAUP × Sensitivity_Analysis

### For Binary × Continuous
- **T-test** / Mann-Whitney U
- **Cohen's d** (effect size)
- **Example**: Distance_variables (Yes/No) vs Number_of_Variables

### For Time Series
- **Trend analysis** (linear/polynomial regression)
- **Mann-Kendall trend test**
- **Example**: Year vs MAUP discussion rate

---

## PRIORITY ANALYSES (Top 10 Most Impactful)

**🎯 = Directly answers core research question**

| Rank | Analysis | Variables | Research Question | Expected Finding |
|------|----------|-----------|-------------------|------------------|
| **1** 🎯 | **UNIT SIZE TEMPORAL TREND** | **Publication_Year × Unit_Size** | **PRIMARY RQ** | **Decreasing unit size over time** |
| **2** 🎯 | **Data limitations → unit choice** | Data_Limitations × Unit_Size | RQ1 | Limitations force larger units |
| **3** 🎯 | **MAUP consideration trend** | Publication_Year × MAUP | RQ2 | Increasing MAUP awareness |
| **4** 🎯 | **Rationale → unit size** | Rationale_Category × Unit_Size | RQ1 | Theory-driven vs. data-driven patterns |
| **5** 🎯 | **MAUP → unit selection rationale** | MAUP × Rationale_Category | RQ2 | MAUP-aware studies cite theory more |
| 6 | **Distance variables → unit size** | Distance_variables × Unit_Size | RQ3 | Distance → smaller units |
| 7 | **Computational constraints impact** | Computational_Constraints × Unit_Size | RQ1 | Constraints → larger units |
| 8 | **Geographic patterns** | Country × Unit_Size | RQ1 | Country-specific preferences |
| 9 | **Unit count trade-off** | Unit_Size × Number_of_Units | RQ3 | Strong inverse correlation |
| 10 | **MAUP-Sensitivity link** | MAUP × Sensitivity_Analysis | RQ2 | MAUP discussion → sensitivity |

---

## IMPLEMENTATION STEPS

### Step 1: Data Preparation (1 day)

#### A. CRITICAL DECISION: Which Year Variable for Which Analysis?

**Available Variables**:
- `Year` = Publication year (when paper was published & unit choice was made)
- `Data_Collection_Period` = When data was collected (may be text: "2005-2010")
- `Crime_data_period` = Specific crime data timeframe (may differ from other data)

**DECISION FRAMEWORK**: **Different questions need different year variables**

| Research Question | Year Variable to Use | Rationale |
|-------------------|---------------------|-----------|
| **Are units getting smaller over time?** | **Publication Year** | Unit choice reflects researcher decisions at time of publication; shows methodological trends |
| **Is MAUP awareness increasing?** | **Publication Year** | MAUP discussion reflects knowledge/norms when paper written |
| **Are models becoming more complex?** | **Publication Year** | Software/methods available at time of analysis |
| **Data availability improvements?** | **Data Collection Year** | Reflects what data existed at that time (e.g., 2000s census vs. 2020s open data) |
| **Data recency impact on choices?** | **Both** (Publication - Data Collection) | Studies using old data may face more limitations |

**PRIMARY APPROACH FOR CORE RQ**:
✅ **Use Publication Year (`Year`) to test if units are getting smaller**

**Why Publication Year for Unit Size Trend?**
1. ✅ Unit selection is a **researcher decision** made during analysis (at publication time)
2. ✅ Reflects **technology available** at time of study (GIS software, computing power)
3. ✅ Reflects **methodological norms** of that era (micro-place criminology emerged ~2008-2012)
4. ✅ Captures when researchers had **access to finer data** (even if data is older)
5. ⚠️ Example: A 2020 paper using 2015 data chose unit size in 2020 with 2020 technology/knowledge

**SECONDARY APPROACH**: 
✅ **Use Data Collection Year to understand data availability constraints**
- Did data quality improve over time? (Finer units available in 2020s vs. 1990s?)
- Control variable: "Even accounting for when data was collected, are recent publications using smaller units?"

**DUAL ANALYSIS STRATEGY**:
```r
# Primary: Publication year trend
Model 1: Unit_Size ~ Publication_Year + controls
  # Interpretation: Are researchers CHOOSING smaller units over time?

# Secondary: Data availability trend  
Model 2: Unit_Size ~ Data_Collection_Year + controls
  # Interpretation: Did available data enable smaller units over time?

# Combined: Distinguishing effects
Model 3: Unit_Size ~ Publication_Year + Data_Recency + controls
  # Data_Recency = Publication_Year - Data_Collection_Year
  # Interpretation: Is it publication era or data era that matters?
  # Example: Do 2020 papers with 2015 data use smaller units than 2015 papers with 2015 data?
```

#### B. Handling Multiple Datasets & Year Ranges

**Problem Cases**:
- "2005-2010" (5-year range)
- "2008, 2010, 2012" (multiple discrete years)
- "Various" or "Not specified"
- Different years for crime vs. predictor data

**DECISION RULES**:

| Scenario | Strategy | Example | New Variable |
|----------|----------|---------|--------------|
| **Single year** | Use as-is | "2010" → 2010 | `Data_Year_Clean = 2010` |
| **Range (X-Y)** | Use **midpoint** | "2005-2010" → 2007.5 | `Data_Year_Clean = 2007.5` |
| **Multiple discrete years** | Use **median** year | "2008, 2010, 2012" → 2010 | `Data_Year_Clean = 2010` |
| **Most recent year specified** | Use most recent | "2005-2010, primarily 2010" → 2010 | `Data_Year_Clean = 2010` |
| **Not specified** | Code as missing (NA) | "Various" → NA | `Data_Year_Clean = NA` |
| **Different crime/predictor years** | Use **crime data year** (primary outcome) | Crime: 2010, Census: 2011 → 2010 | `Data_Year_Clean = 2010` |

**Create Additional Variables**:
```r
# Range width (uncertainty measure)
Data_Year_Range = Max_Year - Min_Year
  # 0 = single year (precise)
  # 5 = 5-year range (less precise)
  
# Data recency (at time of publication)
Data_Recency = Publication_Year - Data_Collection_Year
  # 0-2 years = current data
  # 3-5 years = moderate lag
  # >5 years = old data

# Multiple datasets flag
Multiple_Datasets = Yes/No
  # Based on "2008, 2010, 2012" pattern or explicit statement
```

**Why Midpoint for Ranges?**:
- Represents central tendency of the data period
- Standard practice in meta-analysis
- Avoids bias toward start/end of period
- Example: "2005-2010" crime data reflects mid-2000s context, not 2005 specifically

**Why Median for Multiple Years?**:
- Robust to outliers
- Represents typical year in dataset
- Example: "2001, 2008, 2009, 2010" → median = 2008.5 (reflects post-2008 crime spike)

#### C. Data Preparation Steps
```r
# 1. Parse Unit_Size (extract numeric values)
# 2. Create binary variables (MAUP: Yes/No, Distance_variables: Yes/No)
# 3. Process Year variables:
#    - Clean Data_Collection_Period → Data_Year_Clean (apply decision rules above)
#    - Calculate Data_Year_Range
#    - Calculate Data_Recency
# 4. Categorize Publication Year (Pre-2010, 2010-2015, 2016-2020, 2021+)
# 5. Standardize Country names
# 6. Handle missing data (listwise deletion or imputation)
```

### Step 2: Descriptive Statistics (1 day)
```r
# Summary statistics for all continuous variables
# Frequency tables for all categorical variables
# Identify outliers (Unit_Size, Crime_Incidents)
# Check distributions (normality tests)
```

### Step 3: Correlation Matrix (2 days)
```r
# Cluster 1: Temporal (6 correlations)
# Cluster 2: Spatial Units (9 correlations)
# Cluster 3: Limitations (6 correlations)
# Cluster 4: Distance (9 correlations)
# Cluster 5: Rigor (6 correlations)
# Cluster 6: Crime Type (6 correlations)
# Total: ~40 correlation tests
```

### Step 4: Visualization (2 days)
```r
# Create 15-20 publication-quality figures
# Use consistent color scheme
# Annotate with sample sizes and p-values
# Export as high-res PNG/PDF
```

### Step 5: Interpretation & Writing (3 days)
```r
# Link findings to research questions
# Identify unexpected patterns
# Acknowledge limitations
# Suggest future research directions
```

**Total Timeline**: ~9 working days

---

## EXPECTED NARRATIVE ARC

### 🎯 **MAIN FINDING**: Unit Sizes ARE Getting Smaller (or NOT - both are publishable!)

**Scenario A: Decreasing Unit Size Over Time**
- Publication Year → Unit_Size: **negative correlation** (r = -0.4 to -0.6, p < 0.01)
- Visualization: Scatter plot with regression line showing downward trend
- Interpretation: "Studies increasingly adopt finer spatial resolution (from census tracts → street segments)"
- Mechanisms:
  - Technology: Better GIS software, computing power
  - Data: Open data movement (2010+) enables finer-grained data
  - Theory: Micro-place criminology paradigm shift (~2008-2015)

**Scenario B: No Change in Unit Size**
- Publication Year → Unit_Size: **no correlation** (r = -0.1, p > 0.05)
- Still publishable! "Despite technological advances, studies remain constrained by..."
- Interpretation: Data availability still dictates unit size (not researcher preference)
- Mechanisms:
  - Administrative boundaries unchanged (still using census tracts/postal codes)
  - Theoretical inertia (following previous research)
  - Computational constraints persist for large-scale studies

**Scenario C: Mixed Pattern (Most Likely)**
- Overall trend: slight decrease (r = -0.3, p < 0.05)
- BUT heterogeneity by:
  - Country (US/UK decreasing; China/Europe stable)
  - Crime type (property crime → smaller units; violent crime → stable)
  - Data limitations moderate the trend (limited data → no change; rich data → decrease)

### Finding 1: What Influences Unit Selection?

**Constraints Matter Most**:
- Data limitations → larger units (r = 0.4-0.5, strong effect)
- "Smallest available unit" cited in 60%+ of studies
- Computational constraints → larger units (but declining over time)

**Theory Plays a Role**:
- Theory-driven rationales → more variable unit sizes (intentional choice)
- Data-driven rationales → larger units (forced choice)
- Distance variable studies → smaller units (methodological requirement)

**Geographic Context**:
- Country-specific administrative structures dominate choices
- US: shift toward street segments post-2012
- Europe: postal codes remain standard
- Asia: larger administrative units (data limitations)

### Finding 2: MAUP Consideration Increasing (But Still Limited)

**Temporal Trend**:
- MAUP discussion increasing over time (Year → MAUP: r = 0.3-0.5, p < 0.01)
- Pre-2010: ~20% discuss MAUP
- 2010-2015: ~40% discuss MAUP
- 2016+: ~50% discuss MAUP
- **BUT**: Still only ~50% in recent papers (room for improvement!)

**MAUP Awareness Patterns**:
- MAUP discussion → sensitivity analysis (70%+ overlap)
- MAUP discussion → theory-driven rationale (positive association)
- Distance variable studies MORE likely to discuss MAUP
- Data-limited studies LESS likely to discuss MAUP (when no choice exists)

**Quality Gap**:
- Many MAUP "discussions" are superficial (1 sentence acknowledgment)
- Few studies conduct actual sensitivity analyses across spatial scales
- Alternative units mentioned but not tested

### Finding 3: Distance Variables as Methodological Marker

**Distance Studies are Different**:
- Distance variables → smaller units (strong negative correlation)
- Distance variables → more MAUP awareness (positive correlation)
- Distance variables → more variables overall (complexity)
- Distance variables → GIS software use (ArcGIS, QGIS)

**Interpretation**:
- Distance-based analysis (journey-to-crime, accessibility) requires fine resolution
- Methodological sophistication marker
- Environmental criminology tradition (more theoretically driven)
- Theory rationales → MAUP discussion (Rationale → MAUP: positive)
- Theory rationales → sensitivity analysis (Rationale → Sensitivity: positive)
- Data-availability rationales → less rigor (Rationale → MAUP: negative)

---

## POTENTIAL SURPRISES (Hypotheses to Test)

1. **No improvement in MAUP awareness?** (Null finding = important!)
2. **Computational constraints disappeared after 2015?** (Better computers/software)
3. **Distance variables NOT related to MAUP?** (Missed opportunity)
4. **Unit_Size has NO relationship to Crime_Incidents?** (Scale-invariance)
5. **Country is THE strongest predictor of Unit type?** (Administrative structures dominate)

---

## DELIVERABLES

### Tables
1. **Correlation matrix** (6 clusters, color-coded by strength)
2. **Summary table**: Top 20 significant correlations (r, p-value, interpretation)
3. **Trends table**: Year × Key variables (slopes, R², significance)

### Figures
1. 🎯 **PRIMARY FIGURE**: Unit_Size over Publication_Year (scatter + regression line + confidence interval)
2. 🎯 **MAUP trend**: MAUP discussion rate over time (line chart with smoothing)
3. 🎯 **Rationale patterns**: Unit_Size by Rationale_Category (box plot, ordered by median)
4. 🎯 **Constraints impact**: Data_Limitations vs Unit_Size (grouped comparison)
5. Distance_variables impact (multi-panel: Unit_Size, MAUP, Number_of_Variables)
6. Country × Spatial_Unit_Name heat map
7. Unit_Size vs Number_of_Units (scatter plot, log-log scale)
8. MAUP × Sensitivity analysis Venn diagram
9. Temporal trend panel: 4 subplots (Unit_Size, MAUP, Model_Type, Software)
10. Correlation network diagram (top 20 correlations as edges)

### Text
- **Methods section**: Statistical approach, justification for tests
- **Results section**: Organized by 6 clusters, narrative flow
- **Discussion section**: Link to RQs, theoretical implications
- **Limitations section**: Missing data, small sample, collinearity issues

---

## NEXT STEPS

1. ✅ **Review this plan** with team
2. **Implement Year variable decision rules**:
   - Use `Year` (publication year) for primary analyses
   - Parse `Data_Collection_Period` → create `Data_Year_Clean` (midpoint/median)
   - Calculate `Data_Recency` and `Data_Year_Range`
3. **Parse `Unit_Size` variable** (extract numeric values)
4. **Create binary MAUP/Sensitivity variables**
5. **Run Priority Analyses 1-5** (most impactful)
6. **Generate preliminary visualizations**
7. **Test sensitivity**: Compare Publication Year vs Data Collection Year trends
8. **Iterate based on findings**

---

## SUPPLEMENTARY ANALYSIS: Data Recency Impact

**New Research Sub-Question**: *Does data recency affect methodological choices?*

```r
# Hypothesis: Older data at time of publication → more limitations?
Data_Recency vs Data_Limitations (Yes/No)
Data_Recency vs Unit_Size (Older data → larger units?)
Data_Recency vs Number_of_Variables (Fresher data → more variables available?)

# Control for temporal effects:
# Is it publication year that matters, or data recency?
# Example: 2015 paper with 2005 data vs. 2015 paper with 2013 data
```

**Expected Insights**:
- Longer publication lag (>5 years) → more data limitations mentioned
- Recent data enables finer spatial resolution (better data quality)
- Administrative changes over time create limitations for older data

---

## QUESTIONS FOR TEAM DISCUSSION

1. Should we control for confounders (e.g., Country when analyzing Year trends)?
2. How to handle studies with missing Unit_Size (n=7)?
3. Should we weight studies by sample size/quality?
4. Do we report ALL correlations or only significant ones?
5. Should we use Bonferroni correction for multiple testing (40+ tests)?
6. ✅ **RESOLVED**: Use Publication Year as primary; create Data_Recency variable for lag analysis
7. ✅ **RESOLVED**: Handle year ranges using midpoint; multiple years using median

---

**This plan balances**:
- ✅ Theoretical relevance (RQs 1-5)
- ✅ Statistical rigor (appropriate tests)
- ✅ Practical feasibility (9-day timeline)
- ✅ Interpretability (clear narratives)
- ✅ Novelty (distance variables, MAUP evolution)
