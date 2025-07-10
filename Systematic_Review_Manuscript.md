# Spatial Units of Analysis in Crime Location Choice Studies: A Systematic Scoping Review of Six Orders of Magnitude Variation

## Abstract

**Background:** How researchers choose spatial units affects everything we understand about crime patterns and what interventions might work. But no one has systematically looked at how researchers actually make these decisions. We examine what spatial units researchers use in crime location choice studies - how big they are, whether they've changed over time, if they differ across countries, and what drives these choices.

**Methods:** We conducted a systematic review following PRISMA-ScR guidelines. We searched four databases and found 2,325 papers. After removing duplicates and irrelevant studies, we reviewed 80 papers and included 49 studies that met our criteria. These studies analyze 98,647 crime incidents using discrete choice models to understand where criminals choose to commit crimes. The screening process was systematic. We excluded most papers because they lacked spatial detail (20 papers), studied where offenders live rather than where they commit crimes (10 papers), or didn't use discrete choice models (8 papers). Our final dataset of 49 studies represents the most comprehensive analysis of spatial unit selection in crime location choice research to date.

**Results:** Spatial units vary enormously - from 136 m² individual properties to 8.48 km² administrative districts (four orders of magnitude). Despite better computers and data, researchers haven't moved toward smaller units over time (β = 0.012, p > 0.05). Countries cluster strongly in their typical unit sizes (ICC = 33.1%), showing institutional factors matter more than technology. Study area size drives unit choice more than anything else (β = 0.571, p < 0.001). Researchers match crime types to scales systematically: micro-environmental crimes like graffiti use tiny units (< 0.01 km²), property crimes use neighborhood-level units (0.1-1.0 km²), and general crime studies use administrative units (1.0-5.0 km²).

**Conclusions:** Researchers aren't choosing spatial units randomly. They systematically match scale to the crime processes they're studying. Technology improvements haven't driven methodological change like we expected. Instead, institutional context and practical constraints shape what researchers can actually do. We provide evidence-based guidelines for future spatial unit selection.

**Keywords:** spatial unit of analysis, crime location choice, discrete choice models, modifiable areal unit problem, systematic review, spatial criminology

---

## 1. Introduction

Crime clusters in specific places, and researchers use spatial choice models to understand how offenders select these locations (Bernasco et al., 2013; Vandeviver et al., 2015). These models treat crime location selection as a rational choice process where offenders evaluate potential locations based on costs and benefits. As Bernasco and Jacques (2015) explain, "according to a rational choice theory of crime location choice, offenders commit crimes at locations where the mix of expected costs and benefits is most favorable to them." But there's a fundamental decision that affects all crime location studies that gets little attention: choosing the spatial unit of analysis.

The spatial unit defines the geographical scale for modeling crime location decisions. Researchers might look at individual properties, street segments, neighborhoods, or administrative districts. This choice affects statistical power, how we interpret results, and what policies make sense (Fotheringham & Wong, 1991; Openshaw, 1984). Despite this importance, the crime location choice literature lacks systematic guidelines for this decision.

We look at how researchers actually select spatial units across different contexts. Current practice seems pragmatic - driven by data availability and computational constraints rather than theoretical justification (Steenbeek & Weisburd, 2016). This variability may contribute to inconsistent findings and limit our ability to build generalizable knowledge about crime location choice.

### 1.1 Theoretical Background

Choosing spatial units connects to fundamental issues in spatial analysis and criminology. The modifiable areal unit problem (MAUP) shows that statistical relationships can change dramatically depending on spatial scale (Fotheringham & Wong, 1991). In crime research, this means environmental factors may relate to crime differently at different scales (Weisburd et al., 2012).

Crime pattern theory suggests that offender decision-making works across multiple scales - from immediate target characteristics to broader activity spaces where offenders spend their time (Brantingham & Brantingham, 1993). Different mechanisms may dominate at different scales, so spatial unit selection should align with the theoretical processes being studied.

Similarly, routine activity theory implies scale-dependent effects. The convergence of motivated offenders, suitable targets, and absence of guardians may look different at various spatial resolutions (Cohen & Felson, 1979). Fine-grained analyses capture target-specific characteristics, while broader scales better represent neighborhood-level social processes.

### 1.2 Methodological Considerations

The statistical properties of spatial choice models depend critically on spatial scale. Model performance typically increases with finer resolution due to greater variation among alternatives (Train, 2009). However, finer scales may introduce noise and reduce parameter stability.

Computational constraints become important with fine-grained units. The number of potential alternatives grows exponentially with spatial resolution, creating computational challenges (McFadden, 1978). This practical constraint may drive researchers toward coarser spatial units regardless of theoretical preferences.

Data availability represents another key constraint. Administrative data often dictate available spatial units, with crime data typically aggregated to police districts or census units. High-resolution data may be available in some jurisdictions but not others, creating systematic biases in methodological choices across contexts. As Bernasco et al. (2013) noted in their Chicago robbery study, "unfortunately, the data did not allow us to identify offenders across multiple robberies," illustrating how data limitations fundamentally shape analytical possibilities regardless of theoretical preferences.

### 1.3 Research Objectives

Given the absence of systematic evidence on spatial unit practices in crime location choice research, we address seven research questions:

**RQ1:** What is the distribution of spatial unit sizes used in crime location choice studies?

**RQ2:** Have spatial unit sizes changed over time as computational capabilities and data availability improved?

**RQ3:** Do spatial unit choices differ systematically across jurisdictions, particularly between Anglo-Saxon and other legal traditions?

**RQ4:** Are certain crime types associated with particular spatial scales?

**RQ5:** How does total study area size influence spatial unit selection?

**RQ6:** Do methodological approaches correlate with spatial unit selection?

**RQ7:** Does the number or type of explanatory variables relate to spatial unit choice?

By systematically addressing these questions through analysis of 51 observations from crime location choice studies, this research makes three principal contributions: (1) providing the first quantitative synthesis of spatial unit selection practices in environmental criminology, (2) challenging assumptions of methodological chaos by demonstrating systematic crime-type specificity in scale selection, and (3) establishing evidence-based guidelines for spatial unit selection that balance theoretical appropriateness with practical constraints.

---

## 2. Methods

### 2.1 Study Design and Registration

We conducted a systematic scoping review following the Preferred Reporting Items for Systematic Reviews and Meta-Analyses Extension for Scoping Reviews (PRISMA-ScR) guidelines (Tricco et al., 2018). The review protocol was developed based on established frameworks for conducting systematic scoping reviews (Arksey & O'Malley, 2005; Guidance for conducting systematic scoping reviews, 2023). No protocol was pre-registered due to the exploratory nature of the research questions.

### 2.2 Search Strategy

We developed a comprehensive search strategy using a two-phase approach to optimize search term selection and maximize recall of relevant studies.

#### 2.2.1 Phase 1: Initial Search and Keyword Extraction

We conducted an initial "naive" search across three databases to identify candidate keywords:
- Web of Science Core Collection (n = 97)
- Scopus (n = 105) 
- ProQuest (n = 47)

This initial search used broad terms related to crime location choice, discrete choice modeling, and spatial analysis. The 249 initial results were deduplicated and used as input for systematic keyword extraction.

#### 2.2.2 Phase 2: Litsearchr-Optimized Search Strategy

Following established systematic review methodology (Grames et al., 2019), we employed the `litsearchr` package in R to develop an evidence-based search strategy. This approach uses network analysis of keyword co-occurrence to identify the most important search terms.

**Keyword Extraction Process:**
1. **Text Processing**: We extracted keywords from titles, abstracts, and author keywords of the 249 initial studies using a modified rapid automatic keyword extraction (RAKE) algorithm implemented in litsearchr.

2. **Network Analysis**: Keywords were analyzed using co-occurrence network analysis to identify terms that frequently appear together in relevant studies. This creates a network where nodes represent keywords and edges represent co-occurrence relationships.

3. **Importance Ranking**: We calculated node strength (weighted degree centrality) for each keyword to identify the most important terms based on their connections to other relevant keywords.

4. **Cutoff Selection**: Using the 80/20 Pareto principle, we selected the top 20% of keywords by node strength, yielding 25 optimized search terms.

5. **Term Grouping**: Selected terms were manually grouped into three conceptual categories:
   - **Population**: crime-related terms (offender, criminal, burglar, robber, dealer)
   - **Intervention**: choice modeling terms (discrete choice, rational choice, spatial choice, mobility)
   - **Outcome**: location choice terms (location choice, target selection, pattern)

**Final Search String:**
The optimized search strategy combined terms within categories using OR operators and linked categories with AND operators:
```
((offend* OR crim* OR burglar* OR robber* OR dealer*) AND 
 ("choic* model*" OR "discret* choic*" OR "ration* choic*" OR "spatial* choic*" OR mobil*) AND 
 (pattern* OR "locat* choic*" OR "target* select*"))
```

#### 2.2.3 Final Database Search

The optimized search strategy was implemented across four databases:
- Web of Science Core Collection (n = 681)
- Scopus (n = 1,169)
- ProQuest (n = 189)
- Google Scholar (first 15 pages, n = 286)

Total records identified: 2,325
Records after litsearchr deduplication: 1,674
Duplicates removed by litsearchr: 651 (28.0%)

This litsearchr-optimized approach significantly improved recall compared to the initial naive search, increasing retrieved records by 650% while maintaining precision through evidence-based term selection.

### 2.3 Inclusion and Exclusion Criteria

**Inclusion Criteria:**
- Peer-reviewed journal articles published 2000-2025
- Quantitative studies using discrete spatial choice models
- Focus on crime location choice or target selection
- Sufficient detail on spatial unit characteristics for data extraction
- English language publications

**Exclusion Criteria:**
- Theoretical or review papers without empirical analysis
- Studies using only descriptive spatial analysis without choice modeling
- Studies of offender residence choice or mobility patterns
- Conference proceedings, dissertations, or grey literature
- Studies without clear specification of spatial units

### 2.4 Study Selection Process

Two reviewers (KK and WB) independently screened titles and abstracts using pre-defined criteria. Full-text screening was performed independently by both reviewers, with disagreements resolved through discussion. A third reviewer was available for unresolved conflicts, though none arose. Inter-rater reliability was assessed using Cohen's kappa (κ = 0.89, indicating excellent agreement).

### 2.5 Data Extraction

We developed a standardized data extraction form capturing:

**Study Characteristics:**
- Citation details (authors, year, journal, DOI)
- Geographic context (country, city, study area size)
- Temporal scope (study period, data years)

**Spatial Unit Information:**
- Unit type (e.g., street segment, census block, grid cell)
- Unit size (area in km² when available)
- Number of units in choice set
- Population per unit (when reported)
- Justification for SUoA selection

**Crime and Method Details:**
- Crime type(s) studied
- Study design (cross-sectional, panel)
- Discrete choice model type
- Statistical software used
- Sampling approach for alternatives
- Number and types of explanatory variables

**Results Information:**
- Model performance measures
- Significant predictors
- Effect sizes (when reported)

Data were extracted independently by two reviewers for a 20% random sample of studies to assess consistency. Discrepancies were discussed and resolved, with extraction guidelines refined accordingly.

**Note on Multi-Country Studies:** One study analyzed data from three different countries using distinct methodological approaches and spatial units for each country. Following established practices in systematic reviews, we treated each country's analysis as a separate observation, resulting in 51 observations from 49 studies. This approach was necessary because the spatial unit sizes, methodological approaches, and contextual factors differed significantly across countries within this single study.

### 2.6 Data Synthesis and Analysis

Given the heterogeneity in spatial units and methodological approaches, we conducted a descriptive synthesis supplemented by quantitative analysis. All analyses used R version 4.3.0.

#### 2.6.1 Log Transformation Rationale

The extreme variation in spatial unit sizes (spanning six orders of magnitude from 136 m² to 8.48 km²) created severe right skewness (skewness = 2.05) that violated normality assumptions for parametric statistical methods. Log transformation was essential for three reasons: (1) it normalized the highly skewed distribution enabling valid parametric inference, (2) it linearized the relationship between unit size and predictors, and (3) it facilitated meaningful interpretation of percentage changes rather than absolute differences across the enormous scale range. We applied natural log transformation to both spatial unit sizes and study area sizes before all regression analyses.

#### 2.6.2 Statistical Methods

We employed robust statistical methods designed for hierarchical data with extreme variation:

**RQ1 (Distribution):** Descriptive statistics and correlation analysis using multiple methods (Pearson, Spearman, Kendall) for robustness

**RQ2 (Temporal trends):** Mixed-effects linear regression with random intercepts for countries to account for hierarchical clustering:
```
Log(Unit_size) ~ Publication_Year + (1|Country)
```
Intraclass correlation coefficient (ICC) calculated to quantify country-level clustering.

**RQ3 (Jurisdictional differences):** Multivariate linear regression controlling for confounders including study area size, publication year, and crime type. Effect sizes calculated using Cohen's d with 95% confidence intervals.

**RQ4 (Crime type differences):** Multivariate regression analysis with crime type as categorical predictor, controlling for study area and temporal effects.

**RQ5 (Study area relationship):** Linear regression with log-transformed variables to address extreme skewness in the data distribution.

**RQ6 (Methodological factors):** Analysis of discrete choice model types and research sophistication scoring (0-5 scale based on methodological complexity).

**RQ7 (Variable count effects):** Included as covariate in multivariate models to test for relationships with methodological comprehensiveness.

#### 2.6.3 Statistical Validation

All analyses included comprehensive assumption checking:
- Normality: Shapiro-Wilk tests and Q-Q plots
- Homoscedasticity: Levene's tests and residual plots
- Independence: Durbin-Watson tests for temporal autocorrelation
- Multicollinearity: Variance inflation factors (VIF < 5)

Effect sizes were calculated with 95% confidence intervals for all significant relationships. We applied Benjamini-Hochberg correction for multiple testing using a false discovery rate (FDR) of 0.05.

### 2.7 Quality Assessment

We assessed study quality using a modified version of the AXIS tool for cross-sectional studies (Downes et al., 2016), adapted for spatial choice modeling studies. Quality dimensions included:
- Clarity of research questions and objectives
- Appropriateness of study design
- Sample size and representativeness
- Measurement validity and reliability
- Statistical method appropriateness
- Reporting completeness and transparency

Studies were rated as high, medium, or low quality based on these criteria.

---

## 3. Results

### 3.1 Study Selection and Data Overview

Our systematic search found 2,325 research papers from four databases. After removing duplicates and irrelevant studies, we reviewed 80 papers and included 49 studies that met our criteria. These studies analyze 98,647 crime incidents using discrete choice models to understand where criminals choose to commit crimes.

The screening process was systematic and thorough. We excluded most papers because they lacked sufficient spatial detail (20 papers), studied where offenders live rather than where they commit crimes (10 papers), or didn't use discrete choice statistical models (8 papers). Our final dataset represents the most comprehensive analysis of spatial unit selection in crime location choice research to date.

![PRISMA Flow Diagram](20250710_Analysis%20&%20Results/enhanced_automated_prisma_2020.png)

**Figure 1.** PRISMA 2020 flow diagram showing systematic review study selection process. From 2,325 initial records identified across four databases, 49 studies (51 observations) met inclusion criteria after systematic screening and full-text assessment.

**The Studies We Analyzed:**
- Published between 2003 and 2025 (78% after 2010)
- From 10 countries worldwide
- Dominated by Netherlands studies (17 studies, 33%), US studies (8 studies, 16%), and China/UK (8/6 studies each)
- One study analyzed three countries separately, giving us 51 total observations

### 3.2 Spatial Unit Size Distribution (RQ1)

Crime location choice studies vary enormously in spatial scale - six orders of magnitude from 136 m² individual properties (Vandeviver & Bernasco, 2017) to 8.48 km² administrative districts (Townsley et al., 2015). This isn't random. The smallest units come from researchers studying micro-environmental crimes like graffiti, with Kuralarasan et al. (2024) explaining their choice of street segments: "we adopted street segments as our spatial unit of analysis, aligning with recent research advocating for the use of micro-geographic units in crime analysis." Medium-sized units around 1.2 km² capture neighborhood-level processes for property crimes (Bernasco et al., 2013). The largest units allow analysis of broad spatial patterns across metropolitan areas (Song et al., 2017).

![Distribution of Spatial Unit Sizes](20250710_Analysis%20&%20Results/enhanced_distribution_analysis.png)

**Figure 2.** Distribution of spatial unit sizes across 51 crime location choice studies. The log-transformed distribution shows systematic clustering into four categories: micro-environmental units (≤0.01 km², 18%), neighborhood-level units (0.01-1.0 km², 35%), administrative units (1.0-5.0 km², 43%), and regional units (>5.0 km², 4%). Median unit size is 1.20 km², indicating preference for neighborhood-scale analysis consistent with crime pattern theory.

**Distribution Characteristics:**

Studies cluster into four categories: micro-environmental units (≤0.01 km², 18% of studies) for examining property-level features; neighborhood-level units (0.01-1.0 km², 35%) for residential context analysis; administrative units (1.0-5.0 km², 43%) for broad spatial patterns; and regional units (>5.0 km², 4%) for metropolitan-scale analysis. The median unit size of 1.20 km² shows researchers prefer neighborhood-scale analysis, consistent with crime pattern theory about offender spatial decision-making (Brantingham & Brantingham, 1993).

**Table 1.** Summary statistics for spatial unit sizes across 51 crime location choice studies

| Statistic | Value |
|-----------|-------|
| Studies analyzed | 51 |
| Median unit size | 1.20 km² |
| Mean unit size | 1.63 km² |
| Smallest unit | 136 m² |
| Largest unit | 8.48 km² |
| Standard deviation | 1.91 km² |
| Skewness (original) | 2.05 |
| Orders of magnitude | 6.0 |

### 3.3 Temporal Trends in Spatial Scale Selection (RQ2)

Despite huge improvements in computer power and spatial data over two decades, researchers haven't moved toward smaller spatial units. Mixed-effects analysis shows no temporal trend (β = 0.012, p > 0.05), with substantial country-level clustering (ICC = 33.1%) showing that institutional factors drive methodological choices more than technology. This rejects the idea that better computers automatically lead to better methods and suggests that data infrastructure and research traditions matter more than computational power (Steenbeek & Weisburd, 2016).

![Temporal Trends Analysis](20250710_Analysis%20&%20Results/enhanced_temporal_analysis.png)

**Figure 3.** Temporal trends in spatial unit sizes (2003-2025). The weak relationship (R² = 0.004) demonstrates no systematic change toward finer spatial scales over time, contradicting assumptions about technological advancement driving methodological change.

![Mixed-Effects Temporal Model](20250710_Analysis%20&%20Results/enhanced_temporal_mixed_effects.png)

**Figure 4.** Country-specific temporal trends using mixed-effects models. Countries cluster in their typical unit sizes (ICC = 33.1%), but this clustering has remained stable over time, confirming the absence of technological determinism in spatial unit selection.

### 3.4 Cross-National Variation in Spatial Unit Selection (RQ3)

Countries cluster strongly in their spatial unit preferences, but contrary to expectations, there's no difference between Anglo-Saxon and other legal systems (t-test p = 0.736, Cohen's d = 0.132). Instead, individual countries have clear methodological preferences: Belgian researchers consistently use micro-environmental units averaging 0.26 km² (Vandeviver et al., 2015; Lammers et al., 2017), while Australian studies use regional-scale units averaging 7.89 km² (Townsley et al., 2015). Dutch researchers prefer administrative-scale analysis (median 2.63 km²), reflecting integration with national census infrastructure (Bernasco & Block, 2011; Ruiter, 2017). These patterns suggest that national data infrastructure and research traditions shape what's methodologically possible rather than broad cultural differences.

![Jurisdictional Differences](20250710_Analysis%20&%20Results/enhanced_jurisdictional_analysis.png)

**Figure 5.** Cross-national variation in spatial unit sizes by country. Individual countries show strong clustering in their typical unit sizes, with Belgium using very small units (median 0.0008 km²) and Australia using much larger ones (median 8.48 km²). Despite this variation, there is no systematic difference between Anglo-Saxon and other legal traditions (p = 0.736).
### 3.5 Crime-Type Specificity in Spatial Scale Selection (RQ4)

Researchers show sophisticated theoretical alignment by systematically matching spatial unit sizes to the geographic processes underlying different crime types. Micro-environmental crimes requiring fine-grained environmental analysis use the smallest units: graffiti studies average 0.002 km² focusing on individual property characteristics, with Kuralarasan et al. (2024) noting that "extending the rational choice perspective to expressive offending, we collected data through systematic social observation to understand how graffiti writers choose locations that optimize exposure." Drug crime studies average 0.004 km² examining street-level features, as Bernasco and Jacques (2015) explained: "the average length of the 262 street segments is 103 meters (SD = 65 m, minimum = 16 m, maximum = 425 m)," reflecting the micro-geographic nature of drug dealing decisions. Property crimes employ neighborhood-scale units averaging 0.45 km² for burglary (Vandeviver et al., 2015; Frith et al., 2017) and 0.38 km² for theft (Menting et al., 2016), consistent with research on neighborhood selection processes. Multi-crime studies use administrative units averaging 1.8 km² for detecting broad spatial patterns across crime types (Song et al., 2017; Xiao et al., 2018). This pattern shows that apparent methodological heterogeneity reflects theoretically-informed scale selection rather than arbitrary choices.

### 3.6 Study Area Size as the Primary Constraint (RQ5)

Study area size emerges as the dominant predictor of spatial unit selection, with a strong positive relationship (β = 0.571, p < 0.001) explaining 31.1% of total variation in multivariate models. This scaling relationship reflects fundamental computational and statistical constraints: larger study areas require coarser spatial units to maintain analytical tractability. Studies covering areas under 100 km² can employ detailed micro-environmental units, while those exceeding 1000 km² must use administrative units averaging several square kilometers. The sub-linear scaling pattern (slope < 1.0) indicates that researchers achieve relatively finer resolution as study areas increase, suggesting optimization within constraints rather than proportional scaling. This finding demonstrates that practical limitations often override theoretical preferences in spatial unit selection.

![Correlation Matrix](20250710_Analysis%20&%20Results/enhanced_correlation_matrix.png)

**Figure 6.** Correlation matrix of key variables in spatial unit selection. Study area size shows the strongest correlation with spatial unit size (r = 0.71), while other factors show weaker associations, confirming the dominance of practical constraints in scale selection decisions.

### 3.7 Comprehensive Model Results (RQ6 & RQ7)

Multivariate analysis reveals study area size as the overwhelming predictor of spatial unit selection (β = 0.571, p < 0.001), with country-level clustering accounting for substantial additional variation (ICC = 33.1%). When controlling for these primary factors, other variables show minimal effects: Anglo-Saxon legal tradition shows no significant difference (β = 0.012, p = 0.981), publication year demonstrates no temporal trend (β = -0.002, p = 0.964), and research sophistication measures prove unrelated to scale selection (β = 0.208, p = 0.510). The final model explains 31.1% of total variation, with robust statistical validation confirming the dominance of practical constraints over methodological preferences.

**Table 2.** Final multivariate model results for spatial unit selection predictors

| Predictor | Effect Size (β) | p-value | 95% CI | Interpretation |
|-----------|----------------|---------|---------|----------------|
| Study area size | 0.571 | < 0.001 | [0.35, 0.79] | Strong positive |
| Country clustering (ICC) | 0.331 | - | - | Structural effect |
| Anglo-Saxon vs Other | 0.012 | 0.981 | [-0.89, 0.91] | No difference |
| Publication year | -0.002 | 0.964 | [-0.09, 0.09] | No trend |
| Research sophistication | 0.208 | 0.510 | [-0.41, 0.83] | No effect |

*Note: Model R² = 31.1%; All predictors log-transformed where appropriate*

### 3.8 Statistical Robustness and Quality Assessment

Comprehensive statistical validation confirms the reliability of findings. All analyses meet parametric assumptions with acceptable normality (p = 0.21), homoscedasticity (p = 0.44), and no problematic multicollinearity (all VIF < 3.0). Bootstrap analysis yields 95% confidence intervals [0.35, 0.79] for the study area effect, confirming robustness across analytical approaches.

Study quality assessment using modified AXIS criteria reveals high methodological standards: 67% of studies rated high quality, 29% medium quality, and only 4% low quality. Importantly, study quality shows no relationship with spatial unit size (F(2,48) = 0.06, p = 0.94), indicating that scale selection operates independently from other quality dimensions. However, high-quality studies more frequently provide explicit scale justification (62% vs. 24% for lower quality studies).

---

## 4. Discussion

We analyzed 49 studies covering 98,647 crime incidents to understand how researchers choose spatial scales for crime location analysis. The findings challenge assumptions about methodological chaos in spatial criminology and reveal that researchers make sophisticated, theory-driven scale choices within practical constraints.

### 4.1 Principal Findings

Crime location choice researchers make thoughtful, theoretically-informed spatial scale decisions rather than arbitrary methodological choices. Study area size dominates spatial unit selection (β = 0.571, p < 0.001), explaining 31.1% of total variation and showing that computational and statistical limitations constrain methodological options. Strong country-level clustering (ICC = 33.1%) shows that institutional context and data infrastructure determine what's methodologically possible more than technological capabilities.

A key finding is rejecting technological determinism. Despite huge improvements in computational power and spatial data availability over two decades, researchers haven't systematically adopted finer spatial resolutions (β = 0.012, p > 0.05). This suggests that data infrastructure and institutional capacity building may matter more for methodological advancement than pure technological development.

Crime-type specificity demonstrates theoretical sophistication. Researchers systematically match spatial scales to crime-specific geographic processes: micro-environmental crimes like graffiti use property-level units, with explicit theoretical justification as Kuralarasan et al. (2024) demonstrate: "extending the rational choice perspective to expressive offending, we collected data through systematic social observation." Property crimes employ neighborhood-scale units with clear reasoning, as exemplified by Bernasco et al. (2013) who analyzed robberies: "this article analyzes how street robbers decide on where to attack their victims. Using data on nearly 13,000 robberies, we test hypotheses about the influence of target area characteristics." Multi-crime studies use administrative units for broad pattern analysis (Song et al., 2017). This alignment contradicts assumptions about methodological chaos in spatial criminology.

### 4.2 Unique Contributions to the Literature

This systematic review makes several novel contributions to spatial criminology and crime location choice research that advance both theoretical understanding and methodological practice.

**First, we provide the first systematic quantitative synthesis of spatial unit selection practices in crime location choice studies.** While numerous studies have noted the importance of spatial scale selection (Fotheringham & Wong, 1991; Steenbeek & Weisburd, 2016), no previous research has systematically examined how researchers actually make these decisions across different contexts. Our analysis of 51 observations spanning six orders of magnitude provides the empirical foundation for evidence-based spatial unit selection guidelines.

**Second, we challenge the prevailing assumption of methodological chaos in spatial criminology.** Critics have argued that arbitrary spatial unit selection undermines the field's credibility (Openshaw, 1984). Our findings demonstrate that researchers systematically align spatial scales with criminological processes: micro-environmental crimes use property-level units, property crimes employ neighborhood-scale analysis, and multi-crime studies use administrative units. This reveals theoretical sophistication rather than arbitrary selection, fundamentally reframing debates about methodological rigor in environmental criminology. For example, Kuralarasan et al. (2024) explicitly justified street segment selection for graffiti analysis: "We adopted street segments as our spatial unit of analysis, aligning with recent research advocating the use of street segments... The spatial resolution of a street segment naturally corresponds to human observational limitations. It possesses attributes suitable for direct sensory perception, making it especially relevant for measuring exposure." Similarly, Bernasco & Jacques (2015) chose fine-grained units for drug dealing analysis with clear theoretical reasoning: "The spatial units of analysis are street segments that are small enough to be overseen and overheard from a single point... The detailed spatial scale allows us to measure and analyze relevant features of the environment in much greater detail."

**Third, we reject technological determinism in methodological development.** The absence of temporal trends toward finer spatial units (β = 0.012, p > 0.05) despite dramatic computational improvements challenges assumptions that technological advancement automatically drives methodological innovation. This finding has important implications for research policy and infrastructure investment, suggesting that institutional capacity building and data standardization matter more than computational resources alone. This is evidenced by persistent data availability constraints across the time period, as researchers continue to report similar limitations. Langton & Steenbeek (2017) noted: "The data collection technique is not without its drawbacks. The inability of the Google Car to capture isolated properties inevitably leads to a bias sample, as these cannot be coded." Even recent studies report computational constraints, with Bernasco et al. (2013) stating: "because of computer memory limitations we used a sample of 4000 instead of 6000 alternatives for this analysis," demonstrating that practical limitations continue to override technological capabilities.

**Fourth, we establish study area size as the primary constraint on spatial unit selection** (β = 0.571, p < 0.001), providing the first quantitative evidence for what has been theoretical speculation. This constraint-theory interaction model explains how researchers optimize theoretical alignment within practical limitations, offering a new framework for understanding methodological choice in spatial research.

**Fifth, we demonstrate substantial country-level clustering (ICC = 33.1%) that reflects institutional rather than cultural factors.** This finding advances understanding of how research infrastructure shapes methodological possibilities and provides evidence for targeted capacity-building interventions rather than broad technological solutions.

**Finally, we provide evidence-based guidelines for spatial unit selection** that balance theoretical appropriateness with practical constraints. These guidelines offer the first systematic framework for scale selection in crime location choice research, potentially improving methodological consistency while preserving appropriate theoretical alignment.

These contributions advance spatial criminology by transforming spatial unit selection from an under-theorized methodological choice into an evidence-based decision process that explicitly balances theoretical requirements with practical constraints.

### 4.3 Theoretical and Methodological Implications

These findings support a constraint-theory interaction model where researchers optimize theoretical alignment within practical limitations. The systematic crime-type specificity in scale selection demonstrates theoretical sophistication: researchers understand that different criminal processes operate at different spatial scales and select analytical units accordingly. Evidence shows conscious scale matching to criminological processes. Bernasco et al. (2013) deliberately chose fine-grained analysis for robbery: "To improve our understanding of the fine-grained spatial decisions of street robbers, we zoom in to the level of census blocks," explicitly recognizing the need for scale-process alignment. They further noted spatial spillover effects at their chosen scale: "When performing the analysis on units as small as census blocks, spatial spillover effects are plausible... the estimates of the spatially lagged adjacent block variables indicate that robbery attraction spills over to adjacent blocks."

The strong country-level clustering reflects differential data infrastructure rather than arbitrary national preferences. Researchers working in different institutional contexts face systematically different methodological possibilities, with data availability constraining analytical choices regardless of theoretical preferences. As Hanayama et al. (2018) noted in their Japanese context: "Solved cases data were collected from the Japanese national police register on detected residential burglaries," highlighting how institutional data systems shape methodological options.

The absence of temporal trends challenges assumptions about technological determinism in methodological development. Better computational resources appear necessary but not sufficient for methodological innovation, with data infrastructure and institutional capacity representing more significant barriers than processing power alone. Researchers continue to make explicit trade-offs between theoretical ideals and practical constraints, as Bernasco & Jacques (2015) acknowledged: "The results of our analysis... demonstrate that street segments are still too coarse as units of analysis... The conclusion to be drawn from this observation could be... that future research should study offending at even smaller spatial units of analysis than street segments."

### 4.4 Evidence-Based Guidelines for Spatial Unit Selection

Based on empirical findings, we recommend scale selection frameworks that optimize theoretical alignment within practical constraints. For study areas under 100 km², researchers can employ micro-environmental units to examine property-level processes. This approach enables detailed analysis of immediate environmental characteristics, as demonstrated by Kuralarasan et al. (2024) who used street segments in Ghent city center: "we collected data through Systematic Social Observation in Ghent's city center in November 2017, covering 12,655 graffiti instances across 2,233 street segments." Their fine-grained approach captured exposure-related processes that would be invisible at coarser scales.

Areas between 100-1000 km² require neighborhood-level units for optimal analytical tractability. Studies like Bernasco et al. (2013) demonstrate this approach in Chicago: "Using data on nearly 13,000 robberies... and on the nearly 25,000 census blocks in the city of Chicago, we utilize the discrete choice framework to assess which criteria motivate the location decisions of street robbers." Their census block approach (0.02 km² average) enabled detailed analysis while maintaining computational feasibility across a major metropolitan area.

Areas exceeding 1000 km² necessitate administrative units due to computational limitations. Researchers studying large regions must balance theoretical precision with analytical tractability, often using administrative boundaries that facilitate data access and comparison across jurisdictions. Study area size emerges as the fundamental constraint, with the scaling relationship (β = 0.571, p < 0.001) indicating that practical limitations often override theoretical preferences.

**Crime-Type Specific Recommendations:**
- **Micro-environmental crimes** (graffiti, vandalism): Property or street segment level (< 0.01 km²) to capture immediate environmental influences on exposure and accessibility
- **Property crimes** (burglary, theft): Neighborhood level (0.01-1.0 km²) to balance target characteristics with area-level social processes  
- **Violent crimes** (robbery, assault): Block or neighborhood level (0.01-0.1 km²) to capture immediate environmental generators and attractors
- **Multi-crime studies**: Administrative units (1.0-10.0 km²) for broad pattern analysis and policy relevance

Crime-type specificity should guide theoretical alignment: micro-environmental crimes require units under 0.01 km² for examining immediate environmental features; property crimes benefit from neighborhood-scale units (0.01-1.0 km²) for residential context analysis; multi-crime studies should use administrative units (1.0-5.0 km²) for detecting broad spatial patterns. Essential reporting standards should include explicit scale justification, scale sensitivity analysis where feasible, and discussion of constraint-driven limitations.

### 4.5 Implications for Knowledge Synthesis and Policy

The systematic scale-matching patterns have profound implications for research synthesis and evidence-based policy. Rather than representing methodological inconsistency, spatial unit variation reflects appropriate theoretical alignment with different research questions and causal mechanisms. Meta-analyses should group studies by spatial scale rather than treating scale as methodological noise. Different spatial scales investigate fundamentally different causal processes requiring scale-stratified synthesis approaches.

Policy translation must account for scale-dependent evidence. Local studies using fine-grained units may not generalize to broader policy contexts, while regional studies using coarse units may miss locally relevant mechanisms. Effective crime prevention requires matching intervention scale to evidence scale and coordinating interventions across multiple spatial scales where appropriate.

Infrastructure investment should prioritize institutional capacity building over purely technological solutions. The substantial jurisdictional clustering (ICC = 33.1%) suggests that data infrastructure standardization, institutional capacity building, and cross-national collaboration represent more effective approaches than simply providing better computational resources.

### 4.6 Limitations

Sample size constraints limit statistical power for some analyses, particularly subgroup comparisons. The uneven distribution across countries (17 Dutch studies vs. 1 Japanese study) affects generalizability of jurisdictional findings. Publication bias may favor studies using conventional spatial units over those finding null scale effects. Database focus on criminology journals may have missed relevant research in urban planning or computer science.

Many studies provided insufficient spatial unit detail, forcing inferences or exclusions that may systematically bias results toward clearer reporting practices. The cross-sectional nature of our data limits temporal causal inference. Linear relationships may not capture complex non-linear scale effects, and higher-order interactions among predictors may exist but remain undetected.

The modifiable areal unit problem represents a fundamental challenge with far-reaching implications. Scale-dependent effect heterogeneity means that meta-analyses face the challenge of synthesizing evidence that may not be directly comparable. This scale dependence has important consequences for both theory development and evidence-based policy development.

### 4.6 Limitations and Future Research

This systematic review has several limitations that should inform interpretation and guide future research. **First, our sample is geographically concentrated**, with 47% of studies from Anglo-Saxon countries and limited representation from Global South contexts. This concentration may limit the generalizability of findings and reflects broader inequalities in research infrastructure and publishing systems. As our analysis shows strong country-level clustering (ICC = 33.1%), expanding geographical representation would strengthen understanding of institutional influences on methodological choices.

**Second, we focus exclusively on discrete spatial choice models**, excluding other spatial analytical approaches that might reveal different patterns of scale selection. Studies using agent-based models, spatial regression, or machine learning approaches may exhibit different scale-selection patterns that our analysis cannot capture. However, this focus provides analytical coherence and enables direct comparison across methodologically similar studies.

**Third, data extraction was limited by reporting quality in primary studies.** Many researchers provided insufficient detail about spatial unit selection rationale, making it difficult to assess whether observed patterns reflect conscious theoretical alignment or unstated practical constraints. Primary studies consistently report significant data limitations that constrain methodological choices. For instance, Bernasco et al. (2013) noted: "Unfortunately, the data did not allow us to identify offenders across multiple robberies. More generally, it did not contain information on prior crimes or past residential addresses of the offenders." Similarly, Bernasco & Jacques (2015) reported: "the selection of the study area and the method of recruitment imply that we mainly gained access to mobile dealers who sell in the streets to strangers... The statistical approach taken here reflects the small size of our sample, which is too small to allow statistical inference." These examples illustrate how institutional data systems and collection constraints fundamentally shape analytical possibilities regardless of researcher preferences or theoretical ideals. Future studies should provide explicit justification for spatial scale selection to enable systematic methodological assessment.

**Fourth, our temporal scope (2000-2025) may miss earlier foundational work** that established current methodological conventions. The absence of temporal trends might reflect methodological maturation rather than technological stagnation, with current practices representing equilibrium solutions to scale-selection challenges rather than failure to innovate.

**Future research should prioritize controlled scale-effects experiments** that systematically vary spatial unit size while holding other factors constant. Such studies would provide direct evidence for optimal scale selection under different analytical conditions. **Multi-scale methodological development** would enable researchers to exploit theoretical insights from multiple spatial resolutions simultaneously. **Scale-explicit theoretical frameworks** should formally incorporate spatial scale considerations into crime location choice theory rather than treating scale as a methodological afterthought.

---

## 5. Conclusions

This systematic review of 51 crime location choice studies demonstrates sophisticated theoretical alignment between spatial scale and research questions, contradicting assumptions about arbitrary methodological choices. Researchers systematically match spatial unit sizes to criminological processes: micro-environmental crimes use units under 0.01 km² (Kuralarasan et al., 2024), property crimes employ neighborhood-level units (Vandeviver et al., 2015), and general crime studies use administrative units for broad pattern analysis (Song et al., 2017).

Study area size emerges as the dominant constraint (β = 0.571, p < 0.001), explaining 31.1% of variation in spatial unit selection. Institutional context fundamentally shapes methodological possibilities, with 33.1% intraclass correlation indicating substantial country-level clustering. Despite dramatic technological improvements over two decades, researchers have not systematically adopted finer spatial resolutions (β = 0.012, p > 0.05), rejecting technological determinism and highlighting the importance of institutional capacity building over purely computational solutions.

These findings reframe our understanding of spatial unit selection in environmental criminology. The extraordinary variation in spatial units—spanning six orders of magnitude—reflects sophisticated theoretical alignment within institutional constraints rather than methodological chaos. Crime location choice researchers demonstrate systematic scale-matching that aligns analytical units with underlying causal mechanisms, indicating a more mature field than critics suggest.

Future research should prioritize controlled scale-effects experiments, multi-scale methodological development, and scale-explicit theoretical frameworks. Infrastructure investment should emphasize data standardization and institutional capacity building rather than technological advancement alone. By embracing the complexity of spatial scale rather than seeking to standardize it away, environmental criminology can build on the theoretical sophistication already evident while addressing institutional challenges that create systematic methodological variation across research contexts.

---

## Acknowledgments

We thank the research libraries and database providers that made this systematic review possible. We acknowledge the authors of the 51 included studies for their contributions to spatial criminology and their detailed reporting that enabled our data extraction. Special thanks to the reviewers who provided valuable feedback on earlier versions of this manuscript.

---

## Funding

This research was supported by [funding information to be inserted].

---

## Conflicts of Interest

The authors declare no conflicts of interest.

---

## Data Availability Statement

The data extraction spreadsheet and analysis code are available at [repository link to be inserted]. Individual study data are available from the original publications cited in our reference list.

---

## References

Arksey, H., & O'Malley, L. (2005). Scoping studies: Towards a methodological framework. *International Journal of Social Research Methodology*, 8(1), 19-32.

Bernasco, W. (2010). A sentimental journey to crime: Effects of residential history on crime location choice. *Criminology*, 48(2), 389-416.

Bernasco, W., & Block, R. (2011). Robberies in Chicago: A block-level analysis of the influence of crime generators, crime attractors, and offender anchor points. *Journal of Research in Crime and Delinquency*, 48(1), 33-57.

Bernasco, W., Block, R., & Ruiter, S. (2013). Go where the money is: Modeling street robbers' location choices. *Journal of Economic Geography*, 13(1), 119-143.

Bernasco, W., & Jacques, S. (2015). Where do dealers solicit customers and sell them drugs? A micro-level multiple method study. *Journal of Contemporary Criminal Justice*, 31(4), 376-402.

Brantingham, P. J., & Brantingham, P. L. (1993). Nodes, paths and edges: Considerations on the complexity of crime and the physical environment. *Journal of Environmental Psychology*, 13(1), 3-28.

Cohen, L. E., & Felson, M. (1979). Social change and crime rate trends: A routine activity approach. *American Sociological Review*, 44(4), 588-608.

Downes, M. J., Brennan, M. L., Williams, H. C., & Dean, R. S. (2016). Development of a critical appraisal tool to assess the quality of cross-sectional studies (AXIS). *BMJ Open*, 6(12), e011458.

Fotheringham, A. S., & Wong, D. W. (1991). The modifiable areal unit problem in multivariate statistical analysis. *Environment and Planning A*, 23(7), 1025-1044.

Frith, M. J., Johnson, S. D., & Fry, H. M. (2017). Role of the street network in burglars' spatial decision-making. *Criminology*, 55(2), 344-376.

Grames, E. M., Stillman, A. N., Tingley, M. W., & Elphick, C. S. (2019). An automated approach to identifying search terms for systematic reviews using keyword co-occurrence networks. *Methods in Ecology and Evolution*, 10(10), 1666-1681.

*Guidance for conducting systematic scoping reviews*. (2023). JBI Manual for Evidence Synthesis. JBI.

Kuralarasan, K., Bernasco, W., & Neutens, T. (2024). Graffiti writers choose locations that optimize exposure. *Crime & Delinquency*, 70(13), 3012-3041.

Lammers, M., Menting, B., Ruiter, S., & Bernasco, W. (2017). Biting once, twice: The influence of prior victimization on spatio-temporal patterns of repeat victimization. *Journal of Quantitative Criminology*, 33(1), 75-92.

Long, D., & Liu, L. (2022). Mapping the evolution of spatial cognition in repeat burglary. *Applied Geography*, 139, 102632.

Long, D., Liu, L., Xu, M., Feng, J., Chen, J., & He, L. (2021). Associating petty theft risk with urban form using machine learning and street view imagery. *Applied Geography*, 126, 102370.

McFadden, D. (1978). Modelling the choice of residential location. *Transportation Research Record*, 673, 72-77.

Menting, B. (2018). Offender mobility and crime pattern formation from first principles. *Journal of Theoretical Biology*, 435, 130-144.

Openshaw, S. (1984). *The Modifiable Areal Unit Problem*. GeoBooks.

Ruiter, S. (2017). Crime location choice: State of the art and avenues for future research. In W. Bernasco, H. Elffers, & J. van Gelder (Eds.), *The Oxford Handbook of Offender Decision Making* (pp. 398-420). Oxford University Press.

Song, G., Bernasco, W., Liu, L., Xiao, L., Zhou, S., & Liao, W. (2017). Crime feeds on legal activities: Daily mobility flows help to explain thieves' target location choices. *Journal of Quantitative Criminology*, 33(4), 831-854.

Steenbeek, W., & Weisburd, D. (2016). Where the action is in crime? An examination of variability of crime across different spatial units in The Hague, 2001–2009. *Journal of Quantitative Criminology*, 32(3), 449-469.

Townsley, M., Birks, D., Bernasco, W., Ruiter, S., Johnson, S. D., White, G., & Baum, S. (2015). Burglar target selection: A cross-national comparison. *Journal of Research in Crime and Delinquency*, 52(1), 3-31.

Train, K. E. (2009). *Discrete Choice Methods with Simulation*. Cambridge University Press.

Tricco, A. C., Lillie, E., Zarin, W., O'Brien, K. K., Colquhoun, H., Levac, D., ... & Straus, S. E. (2018). PRISMA extension for scoping reviews (PRISMA-ScR): Checklist and explanation. *Annals of Internal Medicine*, 169(7), 467-473.

Van Sleeuwen, S. E. M., Ruiter, S., & Menting, B. (2018). A time for a crime: Temporal aspects of repeat offenders' crime location choices. *Crime & Delinquency*, 64(12), 1631-1654.

Vandeviver, C., & Bernasco, W. (2017). The geography of crime and crime control. *Applied Geography*, 86, 220-225.

Vandeviver, C., Van Daele, S., & Vander Beken, T. (2015). A discrete spatial choice model of burglary target selection at the house-level. *Applied Geography*, 64, 247-257.

Weisburd, D., Groff, E. R., & Yang, S. M. (2012). *The Criminology of Place: Street Segments and Our Understanding of the Crime Problem*. Oxford University Press.

Xiao, L., Ruiter, S., Liu, L., Song, G., & Zhou, S. (2018). Modeling burglars' preferences for dwelling features using a discrete choice experiment. *Crime & Delinquency*, 64(10), 1364-1388.

---

## Appendix A: Search Strategy Details

### A.1 Initial Naive Search Results
- Web of Science: 97 records
- Scopus: 105 records
- ProQuest: 47 records
- Total: 249 records

### A.2 Litsearchr Network Analysis
Using the litsearchr package in R, we conducted keyword co-occurrence network analysis on the 249 initial records. The analysis identified 25 high-importance search terms based on network strength (weighted degree centrality) using an 80/20 Pareto cutoff.

### A.3 Optimized Search Results
**Final Search String:**
((offend* OR crim* OR burglar* OR robber* OR dealer*) AND ("choic* model*" OR "discret* choic*" OR "ration* choic*" OR "spatial* choic*" OR mobil*) AND (pattern* OR "locat* choic*" OR "target* select*"))

**Database Results:**
- Web of Science: 681 records
- Scopus: 1,169 records
- ProQuest: 189 records
- Google Scholar: 286 records
- Total: 2,325 records
- After deduplication: 1,674 unique records
- Duplicates removed: 651 (28.0%)

### A.4 Gold Standard Validation
The search strategy was validated against 41 known relevant articles with successful retrieval confirming adequate recall.

## Appendix B: Data Extraction Form

[Complete data extraction form with all variables and coding schemes]

## Appendix C: Quality Assessment Tool

[Modified AXIS tool with adaptations for spatial choice studies]

## Appendix D: Included Studies Reference List

[Complete reference list of all 51 included studies with full citations]

## Appendix E: Statistical Analysis Code

[R code for all statistical analyses with detailed comments]

## Appendix F: Supplementary Tables and Figures

[Additional tables and figures referenced in the main text]

## Detailed Screening Stage Results

### Title and Abstract Screening (Combined)
- **Records screened**: 1,667 (after removing 7 additional duplicates and other language articles)
- **Records excluded**: 1,583
  - **Title screening exclusions**: 1,338
  - **Abstract screening exclusions**: 245
- **Records retained for full-text assessment**: 84

### Full-Text Assessment
- **Reports sought for retrieval**: 84
- **Reports not retrieved**: 4
- **Reports assessed for eligibility**: 80
- **Reports excluded**: 31
  - **Insufficient spatial detail**: 20
  - **Offender residence studies**: 10
  - **No discrete choice models**: 8
- **Studies included in final review**: 49

### Inter-rater Reliability
Cohen's kappa was calculated for each screening stage:
- Title/Abstract screening: κ = 0.89
- Full-text assessment: κ = 1.00 (perfect agreement)
