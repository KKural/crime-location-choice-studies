# Correction Guide for Variable Repetition and Categorization Issues

This guide addresses the specific repetition errors and miscategorizations identified in the Elicit extraction outputs.

## Critical Issues Identified

### 1. Variable Duplication Across Categories

**Problem:** The same variables appear in multiple categories with different descriptions.

**Examples from Current Extractions:**
- "Schools" appears in both ENVIRONMENTAL and DEMOGRAPHIC categories
- "Population Density" appears in both DEMOGRAPHIC and TEMPORAL categories  
- "Distance from Home" appears in both ENVIRONMENTAL and DISTANCE categories
- "Deprivation (IMD)" appears in DEMOGRAPHIC, ECONOMIC, and TEMPORAL categories

### 2. Inconsistent Variable Naming

**Problem:** Same variables have different names across studies.

**Examples:**
- "Journey-to-Crime Distance" vs. "Distance from Home Location" vs. "Distance to Offender's Home"
- "Retail Establishments" vs. "Retail Floor Space" vs. "Commercial Area"
- "Public Transport Hubs" vs. "Underground Station" vs. "Transit Stations"

### 3. Categorization Errors

**Problem:** Variables are placed in inappropriate categories.

**Common Errors:**
- Transportation infrastructure in ENVIRONMENTAL instead of DISTANCE & ACCESSIBILITY
- Economic deprivation indices in DEMOGRAPHIC instead of ECONOMIC
- Population characteristics in CONTROL instead of DEMOGRAPHIC

## Correction Rules

### Rule 1: Unique Placement
Each variable should appear in ONLY ONE category. Use this priority order:
1. What does the variable primarily measure?
2. How is it theoretically justified in the paper?
3. What is its main analytical purpose?

### Rule 2: Standard Variable Names
Use standardized naming conventions:

**Distance Variables:**
- `Home_to_Crime_Distance` (for all journey-to-crime measures)
- `Distance_to_CBD` (for city center distances)
- `Distance_to_[Feature]` (for specific destinations)

**Transportation Variables:**
- `Public_Transit_Access` (for any public transport accessibility)
- `Road_Network_Density` (for road infrastructure)
- `Transport_Station_Presence` (for stations/stops)

**Population Variables:**
- `Population_Density` (always in DEMOGRAPHIC)
- `Age_Structure` (demographic characteristics)
- `Ethnic_Diversity` (social characteristics)

**Economic Variables:**
- `Economic_Deprivation_Index` (any deprivation measure)
- `Income_Level` (income-related measures)
- `Employment_Rate` (employment measures)

### Rule 3: Category-Specific Guidelines

#### ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES
- **Include:** Land use, crime attractors, physical environment, built environment, crime opportunities
- **Common variables:** Retail density, entertainment venues, parks, building types, land use mix
- **Exclude:** Transportation networks, population characteristics, economic indicators

#### DISTANCE & ACCESSIBILITY VARIABLES  
- **Include:** ALL distance measures, transportation networks, accessibility indices
- **Common variables:** Home-to-crime distance, public transit access, road networks, travel time
- **Exclude:** Crime attractors at transport locations, economic aspects of transport

#### DEMOGRAPHIC & SOCIAL VARIABLES
- **Include:** Population characteristics, social structure, social processes
- **Common variables:** Population density, age structure, ethnic composition, social cohesion
- **Exclude:** Economic status measures, physical infrastructure

#### ECONOMIC VARIABLES
- **Include:** Economic status, employment, property values, business activity
- **Common variables:** Income, deprivation indices, unemployment, property values, business density
- **Exclude:** Physical business infrastructure, population demographics

#### TEMPORAL & CONTROL VARIABLES
- **Include:** Time effects, methodological controls, statistical controls
- **Common variables:** Year effects, spatial lags, fixed effects, control variables
- **Exclude:** Substantive variables with temporal dimensions

## Specific Corrections Needed

### For London Riots Study:
**Current Issues:**
- Schools appear in both ENVIRONMENTAL and DEMOGRAPHIC
- Population Density in both DEMOGRAPHIC and TEMPORAL
- Underground stations categorized inconsistently

**Corrections:**
- Move "Schools" → ENVIRONMENTAL (crime attractors)
- Keep "Population Density" → DEMOGRAPHIC only
- Move "Underground Stations" → DISTANCE & ACCESSIBILITY
- Move "Deprivation (IMD)" → ECONOMIC only

### For Burglary Study:
**Current Issues:**
- Distance variables scattered across categories
- Economic variables mixed with demographic

**Corrections:**
- Consolidate all distance measures → DISTANCE & ACCESSIBILITY
- Move "Built Surface Area" → ECONOMIC (property value indicator)
- Ensure housing characteristics → ENVIRONMENTAL

## Quality Assurance Checklist

Before finalizing extractions:

1. **Check for Duplicates**
   - [ ] No variable appears in multiple categories
   - [ ] Similar variables use consistent naming
   - [ ] All distance measures in DISTANCE category

2. **Verify Categories**
   - [ ] All economic status measures in ECONOMIC
   - [ ] All population characteristics in DEMOGRAPHIC  
   - [ ] All transportation infrastructure in DISTANCE
   - [ ] All crime attractors in ENVIRONMENTAL

3. **Standardize Format**
   - [ ] Variable names use underscore_case
   - [ ] Units are specified where applicable
   - [ ] Data sources are consistent
   - [ ] Descriptions are concise but specific

4. **Review Theoretical Coherence**
   - [ ] Category assignments match theoretical purpose
   - [ ] Variables within categories are conceptually related
   - [ ] No variables are orphaned or miscategorized

## Implementation for Future Extractions

1. **Use the Variable Categorization Guidelines** before starting extraction
2. **Apply this Correction Guide** to fix identified issues  
3. **Run the Duplicate Detection Script** to verify no repetitions
4. **Cross-check** variable placements against theoretical frameworks
5. **Standardize** variable names according to the conventions above

This systematic approach will eliminate the repetition and categorization errors currently present in the Elicit outputs.
