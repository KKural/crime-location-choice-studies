# Variable Categorization Guidelines for Crime Location Choice Studies

## Overview
This document provides clear guidelines for categorizing variables to eliminate repetition and ensure consistent classification across extraction prompts.

## Variable Categories and Definitions

### 1. ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES
**Definition:** Physical environment, land use, infrastructure, crime opportunities, and behavioral variables that attract or deter crime.

**Includes:**
- Land use types (residential, commercial, industrial)
- Building characteristics (density, height, type)
- Natural features (parks, rivers, topography)
- Crime generators/attractors (bars, entertainment venues)
- Opportunity structures
- Lighting, visibility, surveillance
- Social disorder indicators

**Excludes:**
- Transportation accessibility (→ DISTANCE & ACCESSIBILITY)
- Population characteristics (→ DEMOGRAPHIC & SOCIAL)
- Economic indicators (→ ECONOMIC)

### 2. DISTANCE & ACCESSIBILITY VARIABLES
**Definition:** Spatial relationships, transportation networks, and accessibility measures.

**Includes:**
- Distance measures (home-to-crime, city center, borders)
- Transportation infrastructure (roads, public transit, stations)
- Journey-to-crime patterns
- Spatial barriers (rivers, highways when acting as barriers)
- Accessibility indices
- Network connectivity measures

**Excludes:**
- Crime attractors at transport hubs (→ ENVIRONMENTAL)
- Economic aspects of transportation (→ ECONOMIC)

### 3. DEMOGRAPHIC & SOCIAL VARIABLES
**Definition:** Population characteristics and social structure indicators.

**Includes:**
- Population size, density, composition
- Age structure, gender distribution
- Ethnic composition, diversity measures
- Household characteristics
- Social cohesion/disorganization indicators
- Collective efficacy measures
- Population mobility/churn

**Excludes:**
- Economic status indicators (→ ECONOMIC)
- Built environment features (→ ENVIRONMENTAL)

### 4. ECONOMIC VARIABLES
**Definition:** Economic conditions, prosperity measures, and financial indicators.

**Includes:**
- Income levels, wealth indicators
- Employment/unemployment rates
- Poverty measures
- Property values, rent levels
- Economic deprivation indices
- Business/commercial activity measures
- Economic inequality indicators

**Excludes:**
- Physical retail infrastructure (→ ENVIRONMENTAL)
- Social aspects of deprivation (→ DEMOGRAPHIC)

### 5. TEMPORAL & CONTROL VARIABLES
**Definition:** Time-related factors and methodological controls.

**Includes:**
- Time of day, day of week, seasonal patterns
- Temporal trends, lag variables
- Historical crime patterns
- Methodological controls (fixed effects, etc.)
- Data collection periods
- Temporal aggregation measures

**Excludes:**
- Substantive variables with temporal dimensions (categorize by content)

## Classification Rules

### Rule 1: No Duplication
Each variable should appear in only ONE category. If a variable could fit multiple categories, use this priority order:
1. Primary theoretical purpose
2. How it's measured/operationalized
3. How it's discussed in the paper

### Rule 2: Specific Over General
Place variables in the most specific applicable category rather than a general one.

### Rule 3: Content Over Format
Categorize based on what the variable measures, not how it's labeled or formatted in the paper.

## Common Misclassifications to Avoid

### Schools/Education Variables
- **Correct:** Number of schools → ENVIRONMENTAL (crime attractors)
- **Incorrect:** → DEMOGRAPHIC (just because it relates to population)

### Deprivation Indices
- **Correct:** Economic deprivation → ECONOMIC
- **Incorrect:** → DEMOGRAPHIC or CONTROL

### Distance Variables
- **Correct:** All distance measures → DISTANCE & ACCESSIBILITY
- **Incorrect:** Home-to-crime distance → ENVIRONMENTAL

### Population Density
- **Correct:** Population density → DEMOGRAPHIC & SOCIAL
- **Incorrect:** → CONTROL or ENVIRONMENTAL

### Transportation Infrastructure
- **Correct:** Train stations, bus stops → DISTANCE & ACCESSIBILITY
- **Incorrect:** → ENVIRONMENTAL (unless specifically about crime attractors at stations)

## Formatting Standards

### Variable Entry Format:
```
Variable_Name | Description | Unit | Data_Source
```

### Example:
```
Distance_to_CBD | Distance from residential unit to central business district | Kilometers | Calculated using GIS
```

### Guidelines:
- Use underscore_case for variable names
- Keep descriptions concise but specific
- Always include units where applicable
- Use consistent data source naming

## Quality Checks

Before finalizing extraction:
1. Check for duplicate variables across categories
2. Verify each variable is in the most appropriate category
3. Ensure consistent formatting
4. Confirm no variables are missing or miscategorized
5. Review theoretical coherence of each category
