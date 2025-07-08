# Appendix B: Data Extraction Form

This appendix provides the complete data extraction form used for the systematic scoping review, including all variables, coding schemes, and quality control procedures.

---

## Data Extraction Protocol

### Extraction Team
- **Primary Extractor:** Kiran Kuralarasan
- **Secondary Extractor:** Wim Bernasco  
- **Quality Control:** [Third reviewer name]
- **Conflict Resolution:** Discussion and consensus

### Extraction Software
- **Primary Tool:** Microsoft Excel with standardized template
- **Backup:** REDCap database for quality control
- **Version Control:** Git repository with timestamped commits

---

## Section 1: Study Identification

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `Study_ID` | Unique identifier | Sequential number 1-51 | 01, 02, 03... |
| `Extractor_ID` | Initials of data extractor | Two-letter code | KK, WB |
| `Extraction_Date` | Date of data extraction | DD/MM/YYYY format | 15/01/2024 |
| `Title_of_study` | Full title as published | Copy exact title | "A discrete spatial choice model of burglary..." |
| `Citation` | Author, year format | (Surname et al., YYYY) | (Vandeviver et al., 2015) |
| `DOI` | Digital object identifier | Full DOI URL if available | https://doi.org/10.1016/j.apgeog.2015.08.004 |
| `Journal` | Journal name | Full journal name | Applied Geography |
| `Publication_Year` | Year of publication | YYYY format | 2015 |
| `Volume` | Journal volume | Number only | 64 |
| `Issue` | Journal issue | Number only | NA |
| `Page_Range` | Page numbers | Start-end format | 247-257 |

---

## Section 2: Geographic Context

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `country` | Country of study | Full country name | Belgium, United States |
| `city` | Primary city/region | City name or "Multiple" | East Flanders, Chicago |
| `study_area_size` | Total study area | km² with units | 3,000 km² |
| `study_area_description` | Detailed area description | Free text | "Densely urbanized polycentric area..." |
| `Anglo_Saxon` | Anglo-Saxon legal tradition | 1=Yes, 0=No | 1 (US, UK, Canada, Australia) |
| `Geographic_Scale` | Urban/rural classification | Urban=1, Rural=2, Mixed=3 | 1 |

---

## Section 3: Spatial Unit Characteristics

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `Unit` | Type of spatial unit | Standardized categories | Street_segment, Census_block |
| `Size_of_the_unit` | Numerical size value | Extract number only | 136, 1545 |
| `Unit_size_km2` | Size in square kilometers | Convert to km² | 1.36e-4, 0.001545 |
| `Size_group` | Size category | very small, small, medium, large, very large | very small |
| `Name_of_the_unit` | Descriptive unit name | Free text description | Residential_property |
| `No_of_units` | Number of units in study | Integer value | 503589, 262 |
| `Population_per_unit` | Average population | Number or "NA" | 118, NA |
| `suoa_justification` | Justification for unit choice | Free text extraction | "To address the modifiable areal unit problem..." |
| `Inferred_size` | How size was determined | Direct=1, Calculated=2, Missing=9 | 1, 2 |

---

## Section 4: Crime Characteristics

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `crime_type` | Primary crime type | Standardized categories | Burglary, Street robbery |
| `crime_types_all` | All crime types if multiple | Comma-separated list | Burglary, Theft |
| `No_of_incidents` | Number of crime incidents | Integer value | 4308, 12938 |
| `study_period` | Time period of crime data | Date range or description | 2006-2012, 1996-1998 |
| `data_sources` | Source of crime data | Free text | Belgian Federal Police |

**Crime Type Coding Scheme:**
- Burglary (residential and commercial)
- Street robbery/mugging  
- Drug dealing/trafficking
- Graffiti/vandalism
- Theft (various types)
- Assault/violence
- Breaking and entering
- Other (specify)

---

## Section 5: Methodological Characteristics

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `study_design` | Study design type | Cross-sectional=1, Longitudinal=2 | 1 |
| `statistical_method` | Primary analytical method | Free text | Discrete spatial choice model |
| `model_type` | Specific model type | Standardized categories | Conditional logit model |
| `software_used` | Statistical software | Software name and version | R version 3.0.2 |
| `sampling_approach` | Sampling method | Free text description | Random sampling of alternatives |
| `choice_set_definition` | How alternatives defined | Free text | All residential properties |
| `estimation_method` | Parameter estimation | Maximum likelihood, Bayesian, etc. | Maximum likelihood |

**Model Type Coding Scheme:**
- Conditional logit
- Mixed logit (random parameters)
- Multinomial logit
- Nested logit
- Probit models
- Other discrete choice
- Non-discrete choice methods

---

## Section 6: Variables and Predictors

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `total_variables_count` | Total number of variables | Count all explanatory variables | 5, 21 |
| `vars_distance_access` | Distance/accessibility vars | Count variables in category | 1, 2 |
| `vars_infrastructure` | Infrastructure variables | Count variables in category | 0, 1 |
| `vars_economic` | Economic variables | Count variables in category | 0, 2 |
| `vars_land_use` | Land use variables | Count variables in category | 0, 5 |
| `vars_social_behavioral` | Social/behavioral variables | Count variables in category | 0, 3 |
| `vars_environmental` | Environmental variables | Count variables in category | 1, 0 |
| `vars_demographic` | Demographic variables | Count variables in category | 0, 5 |
| `vars_temporal_control` | Temporal control variables | Count variables in category | 0, 2 |
| `all_variables` | List of all variables | Semi-colon separated | "Construction type; Garage present..." |

**Variable Category Definitions:**
- **Distance/Access:** Distance measures, accessibility indices, network connectivity
- **Infrastructure:** Roads, transport, utilities, physical infrastructure  
- **Economic:** Income, wealth, business presence, economic activity
- **Land Use:** Zoning, business types, residential density, mixed use
- **Social/Behavioral:** Social control, collective efficacy, behavioral patterns
- **Environmental:** Physical environment, natural features, design
- **Demographic:** Age, race, population density, household composition
- **Temporal:** Time controls, seasonal effects, temporal patterns

---

## Section 7: Results and Performance

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `main_results` | Key findings | Brief summary | "Burglars prefer terraced houses..." |
| `significant_predictors` | Statistically significant vars | Comma-separated list | "Construction type, garage presence..." |
| `model_performance` | Performance measures | Extract R², AIC, etc. | McFadden pseudo R² = 0.15 |
| `scale_effects` | Discussion of scale effects | Quote or summarize | "Spatial spillover effects observed..." |

---

## Section 8: Quality Assessment

| Variable | Description | Coding Instructions | Examples |
|----------|-------------|-------------------|----------|
| `variable_info_quality` | Quality of variable description | Complete=1, Partial=2, Poor=3 | 1 |
| `missing_information` | What information is missing | Free text description | "Publication year, authors..." |
| `extraction_confidence` | Confidence in extraction | High=1, Medium=2, Low=3 | 1 |

---

## Quality Control Procedures

### Pilot Testing
- 5 studies extracted by both reviewers independently
- Discrepancies identified and coding scheme refined
- Inter-rater reliability assessed (κ = 0.89)

### Double Extraction
- 20% random sample (n=10) extracted by both reviewers
- Disagreements resolved through discussion
- Major discrepancies flagged for full re-extraction

### Data Validation
- Range checks for numerical variables
- Consistency checks across related variables
- Missing data patterns analyzed

### Standardization Procedures
- Unit size conversions verified with online calculators
- Geographic locations verified with mapping services
- Journal abbreviations standardized
- Author names standardized for consistency

---

## Coding Decision Rules

### Spatial Unit Size Calculations
1. **Direct reporting:** Use exact values when reported
2. **Area calculation:** Study area ÷ number of units  
3. **Inference from description:** Use typical sizes for unit types
4. **Missing data:** Code as "NA" and note in comments

### Multiple Crime Types
- Use primary crime type mentioned in title/abstract
- List all crime types in separate field
- If equal emphasis, use alphabetical order

### Variable Counting
- Count unique conceptual variables only
- Interaction terms counted as single variables
- Polynomial terms counted as single variables
- Control variables included in total count

### Geographic Classification
- Use country of data collection, not author affiliation
- Multi-country studies coded as primary focus country
- Cross-national comparisons coded separately by country

---

## Data Management

### File Organization
```
Data_Extraction/
├── Forms/
│   ├── extraction_template.xlsx
│   ├── pilot_extractions.xlsx
│   └── quality_control_checks.xlsx
├── Raw_Data/
│   ├── extraction_round1.xlsx
│   ├── extraction_round2.xlsx
│   └── final_dataset.csv
├── Documentation/
│   ├── coding_decisions.md
│   ├── extractor_notes.md
│   └── version_history.md
└── Quality_Control/
    ├── double_extraction_comparison.xlsx
    ├── consistency_checks.R
    └── validation_report.html
```

### Version Control
- All extraction files version controlled with Git
- Timestamped commits for each extraction session
- Change log maintained for coding scheme updates
- Final dataset locked after quality control completion

### Backup Procedures
- Daily backups to university cloud storage
- Weekly exports to REDCap database
- Final dataset archived in institutional repository

---

## Training Materials

### Extractor Training Protocol
1. Review extraction manual and coding scheme
2. Complete practice extractions on 3 training studies
3. Discuss discrepancies with lead reviewer
4. Complete reliability assessment on 5 studies
5. Proceed with independent extractions

### Reference Materials
- Spatial criminology textbook chapters
- Discrete choice modeling tutorials
- Geographic measurement standards
- Quality assessment guidelines

---

**Data Extraction Form Version:** 2.1
**Last Updated:** January 15, 2024
**Total Studies Extracted:** 51
**Extraction Period:** January 2024 - May 2024
