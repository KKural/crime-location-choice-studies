# Additional Analyses Beyond Published Paper
## New Insights from Extended Dataset Variables

**Created**: January 12, 2026  
**Purpose**: Identify untapped analytical opportunities in your SUoA dataset  
**Current Paper**: 51 observations, 7 RQs answered  
**Extended Dataset**: 57 studies with 40+ additional variables

---

## WHAT YOU'VE ALREADY DONE (Published Paper) ✅

### Completed Analyses:
1. ✅ SUoA size distribution (RQ1)
2. ✅ Temporal trends in unit size (RQ2) - **NO trend found**
3. ✅ Cross-national variation (RQ3) - **Strong country clustering (ICC=0.386)**
4. ✅ Crime-type specificity (RQ4) - **Systematic alignment found**
5. ✅ Rationale analysis (RQ5) - **7 categories identified**
6. ✅ Variable complexity (RQ6) - **6-39 variables, mean 21.9**
7. ✅ Basic correlations (RQ7) - **Minimal correlations found**

### Key Findings Already Published:
- Unit sizes stable over time (no technology-driven trend)
- Strong country effects (data infrastructure matters)
- Crime-type specificity (burglary: 0.88 km², theft: 2.18 km²)
- Data Availability (45.1%) most common rationale
- No correlation between year and unit size (r=0.048, p=0.738)

---

## UNTAPPED VARIABLES IN YOUR DATASET 🆕

Based on the variable list you provided, you have **data NOT analyzed** in the paper:

### 🔴 **HIGH PRIORITY - Ready for Analysis**

| Variable | Analysis Potential | Why Important |
|----------|-------------------|---------------|
| **MAUP** | MAUP discussion trend over time | **Critical gap**: Paper mentions MAUP theoretically but doesn't analyze who discusses it |
| **Sensitivity** | Sensitivity analysis practices | **Methodological rigor**: Do studies test robustness? |
| **Distance_variables** | Distance var usage → unit size | **Theory**: Journey-to-crime requires small units? |
| **Data_Limitations** | Limitations → forced unit choices | **Mentioned but not tested quantitatively** |
| **Computational_Constraints** | Constraints → unit size | **Mentioned but not quantified** |
| **Alternative_Units** | Alternative units mentioned | **Transparency indicator** |
| **Limitation_type** | Types of limitations (categorical) | **Deeper understanding of constraints** |
| **Data_Collection_Period** | Data recency analysis | **New**: Publication lag effects |
| **Crime_data_period** | Temporal mismatch analysis | **Data quality proxy** |
| **Software_Used** | Software → methodological choices | **Technology adoption patterns** |
| **Model_Type** | Model sophistication over time | **Brief mention, not fully analyzed** |
| **Number_of_Data_Sources** | Data richness → unit choices | **Resource availability** |
| **Future_Suggestions** | What do authors recommend? | **Field development roadmap** |

### 🟡 **MEDIUM PRIORITY - Interaction Effects**

| Analysis | Variables | New Insight |
|----------|-----------|-------------|
| **Moderation effects** | Year × Country → Unit_Size | Is temporal trend country-specific? |
| **Data limitations moderation** | Data_Limitations × Year → Unit_Size | Are recent studies still constrained? |
| **MAUP awareness patterns** | MAUP × Rationale_Category | Theory-driven → more MAUP aware? |
| **Methodological clusters** | MAUP + Sensitivity + Alternative_Units | Identify "gold standard" studies |

---

## PROPOSED ADDITIONAL ANALYSES (Beyond Paper)

### 📌 **ANALYSIS SET 1: MAUP & METHODOLOGICAL RIGOR** ⭐
**Gap in Paper**: MAUP mentioned theoretically but not empirically analyzed

#### 1A. MAUP Awareness Evolution
```r
# Create binary: MAUP discussed (Yes/No) from text field
# Temporal trend: Year → MAUP (logistic regression)
# Expected: Increasing MAUP awareness post-2010

Visualization: Line chart showing % studies discussing MAUP by year
Expected finding: "MAUP discussion increased from 20% (2000s) to 50% (2020s)"
```

#### 1B. Who Discusses MAUP?
```r
# MAUP × Rationale_Category (Chi-square test)
# MAUP × Unit_Size (t-test: do MAUP-aware studies use smaller units?)
# MAUP × Country (which countries more MAUP-aware?)
# MAUP × Crime_Type (property crime → more MAUP discussion?)

Expected findings:
- Theory-driven rationales → higher MAUP awareness
- Distance-based studies → higher MAUP awareness
- Anglo-Saxon countries → higher MAUP awareness (methodological culture)
```

#### 1C. MAUP Discussion → Action
```r
# MAUP × Sensitivity_Analysis (do they act on MAUP awareness?)
# MAUP × Alternative_Units (do they mention alternatives?)
# Create "Rigor Score" = MAUP + Sensitivity + Alternative_Units

Visualization: Venn diagram (MAUP ∩ Sensitivity ∩ Alternatives)
Expected: 70% overlap between MAUP discussion and sensitivity analysis
```

**Publication Potential**: 
- **New finding**: "Despite increasing MAUP awareness, only X% conduct sensitivity analyses"
- **Policy implication**: "Awareness ≠ Action in methodological rigor"

---

### 📌 **ANALYSIS SET 2: DISTANCE VARIABLES** ⭐
**Gap in Paper**: Distance variables collected but not analyzed

#### 2A. Distance Variables → Unit Size
```r
# Distance_variables (Yes/No) → Unit_Size (t-test)
# Hypothesis: Distance studies use smaller units (need fine resolution)

Expected: Cohen's d = 0.6-0.8 (medium-large effect)
Mean unit size: With distance = 0.5 km², Without distance = 2.0 km²
```

#### 2B. Distance Variables → Methodological Profile
```r
# Distance_variables → Number_of_Variables (more complex models?)
# Distance_variables → MAUP (more aware?)
# Distance_variables → Software_Used (require GIS: ArcGIS, QGIS?)
# Distance_variables → Model_Type (conditional logit models?)

Visualization: Multi-panel comparison (4 box plots)
Expected: Distance studies are methodologically distinct subfield
```

#### 2C. Distance Variables Over Time
```r
# Year → Distance_variables (increasing usage?)
# Hypothesis: GIS adoption enables more distance-based analysis

Visualization: Stacked bar chart by time period
Expected: Distance variables in 20% (2000s) → 60% (2020s)
```

**Publication Potential**:
- **New finding**: "Distance-based studies represent methodologically sophisticated subfield"
- **Theory**: "Journey-to-crime framework drives small unit selection"

---

### 📌 **ANALYSIS SET 3: DATA CONSTRAINTS QUANTIFIED** ⭐
**Gap in Paper**: Mentioned narratively but not tested statistically

#### 3A. Impact of Data Limitations
```r
# Data_Limitations (Yes/No) → Unit_Size
# H0: Limitations force larger units
# Expected: Students with limitations use 2x larger units

# Data_Limitations × Year (are recent studies still limited?)
# Limitation_type (categorical) → Unit_Size (ANOVA)
# Which limitation types most constraining?
```

#### 3B. Computational Constraints
```r
# Computational_Constraints (Yes/No) → Number_of_Units
# Computational_Constraints × Year (decreasing over time?)
# Expected: Constraints rare after 2015 (better computers)

# Constraints → Unit_Size (forced aggregation?)
# Constraints → Model_Type (simpler models when constrained?)
```

#### 3C. "Smallest Available Unit" Problem
```r
# "Smallest unit due to data availability" (Yes/No) → % of studies
# Expected: 60%+ cite this (forced choice, not preference)

# Cross-tabulate with Rationale_Category
# Students citing "smallest available" also cite "data availability" rationale?
```

**Publication Potential**:
- **New finding**: "Data availability, not theory, drives 60% of unit selections"
- **Policy**: "Improving data infrastructure could enable theoretically optimal units"

---

### 📌 **ANALYSIS SET 4: DATA RECENCY & PUBLICATION LAG**
**Gap in Paper**: Not analyzed at all (NEW variable creation)

#### 4A. Create Data Recency Variable
```r
# Data_Recency = Publication_Year - Data_Collection_Year
# Categorize: Fresh (0-2 years), Moderate (3-5 years), Old (>5 years)

# Distribution: What % of studies use old data?
# Expected: 40% use data >5 years old at publication
```

#### 4B. Data Recency Effects
```r
# Data_Recency → Data_Limitations (old data → more limitations?)
# Data_Recency → Unit_Size (old data → larger units?)
# Data_Recency → Number_of_Variables (fresh data → more vars available?)

Hypothesis: Older data at publication → more constraints
```

#### 4C. Temporal Mismatch
```r
# Crime_data_period vs Data_Collection_Period (same or different?)
# Flag studies with temporal mismatch (crime: 2010, census: 2011)
# Mismatch → Data_Limitations (acknowledged?)
```

**Publication Potential**:
- **New finding**: "Studies using 5+ year old data face 2x more limitations"
- **Methods**: "Data recency underappreciated methodological consideration"

---

### 📌 **ANALYSIS SET 5: MODEL & SOFTWARE EVOLUTION**
**Gap in Paper**: Mentioned but not systematically analyzed

#### 5A. Model Type Evolution
```r
# Year → Model_Type (simple conditional logit → mixed logit?)
# Visualize: Stacked area chart showing model type distribution over time

Expected: Shift from conditional logit (2000s) to mixed logit (2010s+)
```

#### 5B. Software Adoption Patterns
```r
# Software_Used (categorical) over time
# SPSS → R → Python transition?
# Software_Used → Model_Type (R enables complex models?)
# Software_Used → Number_of_Variables (R/Python → more variables?)

Visualization: Sankey diagram (Year → Software → Model_Type)
```

#### 5C. Technology ≠ Smaller Units (Paradox)
```r
# Despite better software/computers, no trend toward smaller units
# Reconcile with: Software_Used evolution + Model_Type evolution
# Interpretation: Technology enables COMPLEXITY (more variables)
#                 but not RESOLUTION (smaller units)
#                 → Data availability is the constraint, not technology
```

**Publication Potential**:
- **Paradox**: "Technology advanced, but data infrastructure didn't keep pace"
- **Finding**: "Computational barriers removed, but data barriers persist"

---

### 📌 **ANALYSIS SET 6: ADVANCED INTERACTIONS** 
**Gap in Paper**: No interaction effects tested

#### 6A. Year × Country Interaction
```r
# Is temporal trend country-specific?
# US: decreasing unit size? (micro-place movement)
# Europe: stable unit size? (administrative boundaries fixed)

Model: Unit_Size ~ Year * Country
Expected: Negative slope for US/UK, flat for Netherlands/China
```

#### 6B. Data_Limitations × Year Interaction
```r
# Are recent studies still constrained by data?
# Or has open data movement (2010+) reduced limitations?

Model: Unit_Size ~ Year * Data_Limitations
Expected: Limitation effect weakening over time
```

#### 6C. Crime_Type × Country Interaction
```r
# Do countries specialize in certain crime types?
# US: violent crime? Europe: property crime?
# Does this explain country unit size differences?
```

**Publication Potential**:
- **Nuanced finding**: "Temporal trend exists for US/UK (decreasing), not globally"
- **Context matters**: "Open data movement benefited some countries more than others"

---

## PRIORITY RANKING: WHAT TO ANALYZE NEXT?

### 🥇 **TIER 1: High Impact, Low Effort** (Do First!)

| Rank | Analysis | Why Priority | Effort | Impact |
|------|----------|-------------|--------|--------|
| 1 | **MAUP awareness trend** | Critical gap, simple analysis | Low (binary coding) | **HIGH** (new RQ) |
| 2 | **Distance variables → unit size** | Clear hypothesis, t-test | Low | **HIGH** (theory-driven) |
| 3 | **Data limitations → unit size** | Quantify what paper only mentions | Low (binary test) | **HIGH** (policy relevant) |
| 4 | **MAUP × Sensitivity** | Methodological rigor story | Low (cross-tab) | **MEDIUM-HIGH** |
| 5 | **Data recency analysis** | Novel angle, create new variable | Medium (parsing dates) | **MEDIUM-HIGH** |

### 🥈 **TIER 2: Medium Impact, Medium Effort**

| Rank | Analysis | Why | Effort | Impact |
|------|----------|-----|--------|--------|
| 6 | **Software evolution** | Technology story | Medium (categorizing) | **MEDIUM** |
| 7 | **Model type trends** | Methodological maturation | Medium | **MEDIUM** |
| 8 | **Computational constraints over time** | Decreasing trend? | Low | **MEDIUM** |
| 9 | **Alternative units mentioned** | Transparency metric | Low | **LOW-MEDIUM** |

### 🥉 **TIER 3: Advanced, Higher Effort**

| Rank | Analysis | Why | Effort | Impact |
|------|----------|-----|--------|--------|
| 10 | **Year × Country interaction** | Nuanced temporal story | High (mixed models) | **HIGH** (if significant) |
| 11 | **Rigor score composite** | Holistic quality measure | Medium | **MEDIUM** |
| 12 | **Limitation type ANOVA** | Detailed constraint types | Medium | **MEDIUM** |

---

## RECOMMENDED ANALYSIS SEQUENCE (3 Papers/Outputs)

### 📄 **PAPER 1: Current Published Paper** ✅
- Focus: Descriptive landscape of SUoA selection
- Main findings: Stability over time, country clustering, crime-type alignment
- **Status**: Done!

### 📄 **PAPER 2: "Methodological Rigor in SUoA Selection"** (Next!)
**Focus**: MAUP awareness & best practices

**New Analyses**:
1. MAUP awareness evolution (Tier 1, Rank 1)
2. MAUP × Sensitivity × Alternative_Units (Tier 1, Rank 4)
3. Distance variables as rigor marker (Tier 1, Rank 2)
4. Rationale_Category × MAUP (theory-driven → more rigorous?)
5. Create "Rigor Score" and identify exemplar studies

**Key Research Questions**:
- RQ1: Has MAUP awareness increased over time?
- RQ2: Do studies that discuss MAUP conduct sensitivity analyses?
- RQ3: Which study characteristics predict methodological rigor?
- RQ4: What separates "gold standard" studies from others?

**Expected Contributions**:
- Quantify MAUP awareness gap (talk vs. action)
- Identify best practices (what rigorous studies do)
- Provide methodological guidelines for field

**Timeline**: 2-3 weeks (analyses straightforward)

---

### 📄 **PAPER 3: "Data Constraints in Spatial Criminology"** 
**Focus**: How data availability shapes methodological possibilities

**New Analyses**:
1. Data limitations → unit size (Tier 1, Rank 3)
2. Computational constraints evolution (Tier 2, Rank 8)
3. Data recency effects (Tier 1, Rank 5)
4. "Smallest available unit" prevalence
5. Limitation_type detailed analysis (Tier 3, Rank 12)
6. Year × Data_Limitations interaction (Tier 3)

**Key Research Questions**:
- RQ1: What proportion of studies are constrained by data availability?
- RQ2: How do data limitations affect unit size selection?
- RQ3: Has the open data movement reduced constraints?
- RQ4: Do computational constraints still matter in 2020s?

**Expected Contributions**:
- Quantify extent of constraint-driven decisions (vs. theory-driven)
- Show technology paradox: better software ≠ smaller units
- Policy implications: invest in data infrastructure

**Timeline**: 3-4 weeks (requires date parsing, more complex)

---

### 📊 **ANALYSIS BRIEF: "Technology Adoption in Crime Location Choice Studies"**
**Focus**: Software, models, and analytical evolution

**New Analyses**:
1. Software evolution (Tier 2, Rank 6)
2. Model type trends (Tier 2, Rank 7)
3. Software → Model_Type → Variable_Complexity pathway
4. Year × Country × Technology adoption

**Format**: Methods-focused brief or journal commentary
**Timeline**: 1-2 weeks (descriptive, visualization-heavy)

---

## PRACTICAL NEXT STEPS

### Week 1: Quick Wins (Tier 1 Analyses)
```r
# Day 1: Create binary variables
MAUP_binary <- ifelse(MAUP != "No MAUP discussion", 1, 0)
Distance_binary <- ifelse(Distance_variables != "None", 1, 0)
Data_Lim_binary <- ifelse(Data_Limitations != "None", 1, 0)

# Day 2: MAUP trend
ggplot(aes(x=Year, y=MAUP_binary)) + stat_smooth(method="loess")
glm(MAUP_binary ~ Year, family=binomial)

# Day 3: Distance effects
t.test(Unit_Size ~ Distance_binary)
ggplot(aes(x=Distance_binary, y=log10(Unit_Size))) + geom_boxplot()

# Day 4: Data limitations impact
t.test(Unit_Size ~ Data_Lim_binary)
table(Data_Lim_binary, Rationale_Category)

# Day 5: MAUP × Sensitivity cross-tab
table(MAUP_binary, Sensitivity_binary)
chisq.test(MAUP_binary, Sensitivity_binary)
vennDiagram(MAUP, Sensitivity, Alternative_Units)
```

### Week 2: Data Recency (Tier 1, more complex)
```r
# Parse Data_Collection_Period
# Apply midpoint/median rules from plan
# Create Data_Recency = Year - Data_Year_Clean
# Analyze: Data_Recency → Data_Limitations, Unit_Size, Num_Variables
```

### Week 3: Write Paper 2 Draft
- Introduction: MAUP awareness gap in criminology
- Methods: Binary coding, trend analysis
- Results: 5 Tier 1 analyses
- Discussion: Best practices, guidelines

---

## EXPECTED NEW FINDINGS (Predictions)

### Finding 1: **MAUP Awareness Paradox**
- "MAUP discussion increased from 20% (2000-2010) to 50% (2016-2025)"
- "BUT: Only 30% of studies discussing MAUP conduct sensitivity analyses"
- **Implication**: Awareness ≠ methodological rigor

### Finding 2: **Distance Variables as Methodological Marker**
- "Studies with distance variables use units 75% smaller (Cohen's d=0.7)"
- "Distance studies 2x more likely to discuss MAUP (60% vs. 30%)"
- **Implication**: Journey-to-crime framework drives methodological sophistication

### Finding 3: **Data Constraints Dominate**
- "60% of studies cite 'smallest available unit' as rationale"
- "Studies with data limitations use units 2x larger (1.8 km² vs. 0.9 km²)"
- **Implication**: Theory constrained by data infrastructure

### Finding 4: **Technology Paradox**
- "Software sophistication increased (SPSS→R→Python)"
- "Model complexity increased (mean variables: 18→25 over time)"
- "BUT: Unit size unchanged (no trend toward finer resolution)"
- **Implication**: Computational barriers removed, but data barriers persist

### Finding 5: **Rigor Tiers**
- "Gold Standard (20%): MAUP + Sensitivity + Alternatives + Theory rationale"
- "Average (50%): Some MAUP discussion, no sensitivity"
- "Constraint-Driven (30%): No MAUP, data availability rationale"
- **Implication**: Provide clear methodological guidelines

---

## VARIABLES TO CREATE (Data Preparation)

```r
# Binary transformations
MAUP_binary <- code from text field ("No discussion" = 0, else 1)
Sensitivity_binary <- code from text field
Distance_binary <- code from text field
Data_Lim_binary <- code from yes/no field
Comp_Constraint_binary <- code from yes/no field
Alternative_Units_binary <- mentioned or not

# Data recency
Data_Year_Clean <- parse Data_Collection_Period (midpoint/median)
Data_Recency <- Year - Data_Year_Clean
Data_Recency_Cat <- cut(Data_Recency, breaks=c(0,2,5,Inf), 
                         labels=c("Fresh","Moderate","Old"))

# Rigor score (composite)
Rigor_Score <- MAUP_binary + Sensitivity_binary + Alternative_Units_binary
  # Range: 0-3
  # 3 = "Gold Standard"
  # 2 = "Good"
  # 1 = "Basic"
  # 0 = "No rigor indicators"

# Time periods (for visualization)
Time_Period <- cut(Year, breaks=c(2000,2010,2015,2020,2026),
                   labels=c("2000-2010","2011-2015","2016-2020","2021-2025"))
```

---

## SUMMARY: YOUR PATH FORWARD

### ✅ **What You've Published**:
Comprehensive descriptive landscape of SUoA selection practices (7 RQs)

### 🎯 **What You Can Still Extract** (NEW):

1. **MAUP Awareness Study** (Paper 2) - 2-3 weeks
   - High impact, fills critical gap
   - Clear policy implications
   - Builds on existing data with minimal new coding

2. **Data Constraints Study** (Paper 3) - 3-4 weeks
   - Novel "data infrastructure" angle
   - Quantifies constraint vs. choice
   - Policy-relevant findings

3. **Technology Brief** (Commentary) - 1-2 weeks
   - Lighter lift, methods-focused
   - Explains paradox: better tech ≠ finer units
   - Journal methods section or commentary

### 💡 **Immediate Action** (This Week):
Run **Tier 1 analyses (Ranks 1-5)** to see if patterns emerge:
- 1 day each = 1 week total
- If promising → full Paper 2
- If not → adjust focus

**Total Additional Publication Potential**: 2-3 more papers from same dataset! 🎉
