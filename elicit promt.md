#######################################
#  VARIABLE & SUoA EXTRACTION PROMPT  #
#######################################

🎯 **Goal**  
Return EVERY independent variable—and the exact reason the study chose its spatial unit of analysis (SUoA)—in the template below.  
Quote where you found the SUoA rationale and give a one-sentence summary.

────────────────────────────
SEARCH CHECKLIST (in order) 
────────────────────────────  
1. **Tables** (results, summary stats, appendices, robustness) - **MOST IMPORTANT: 80% of variables here**
2. **Methods** / **Data & Variables** sections  
3. Footnotes, table notes, captions  
4. Limitations / supplementary

Key SUoA phrases:  
"spatial unit", "geographic unit", "grid", "census tract", "postal code", "chosen because", "following previous research", "data availability".

Key variable cues:  
"control variables", "covariates", "predictors", "independent variables", "model includes".

**VARIABLE EXTRACTION RULES:**
- Find regression tables, summary statistics tables, correlation matrices
- Extract EXACT variable names as shown in table headers/rows
- Do NOT summarize or group (e.g., "Age variables" is WRONG, list "Age", "Age squared", "Age_group_18_25" separately)
- Include ALL variables: main predictors, controls, interactions, transformed variables
- Count each individual variable name once

────────────────────────────
TEMPLATE (copy EXACT headings)
────────────────────────────  

## STUDY IDENTIFICATION  
Title: […]  
Year: […]  
Authors: […]

## CONTEXT  
Country: […]  
City/Region: […]  
Study Area Size (km²): […]  
Crime Type(s): […]  
Study Period: […]  
Data Sources: […]

## SUoA  
Type: […]  
Size: […]  
#Units: […]  
Pop./Unit: […]  
Quoted Rationale: "[…]"  
Rationale Category: Prior research / Data availability / Admin convenience / Theory–method / Practical constraint / Not specified  
Rationale Summary: […]

## METHODS  
Design: […]  
Stat Method: […]  
Model Type: […]  
Software: […]  
Sampling: […]  
Sample Size (choices): […]  
Choice Set: […]

## VARIABLES  (list ONE per line under each heading)

**CRITICAL: List each INDIVIDUAL variable name exactly as used in the study's tables/models. Do NOT group or summarize.**

DEMOGRAPHIC:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual demographic variables...]
ECONOMIC:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual economic variables...]
CRIME ATTRACTORS:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual crime attractor variables...]
DISTANCE / ACCESSIBILITY:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual distance/accessibility variables...]
SOCIAL / BEHAVIOURAL:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual social/behavioral variables...]
TEMPORAL / CONTROL:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual temporal/control variables...]
OTHER:  
  - [Exact variable name from table]
  - [Exact variable name from table]
  - [Continue for ALL individual other variables...]

## VARIABLE COUNTS  
Demo: [#] | Econ: [#] | Crime Attractors: [#] | Distance: [#] | Social: [#] | Temporal: [#] | Other: [#]

## KEY FINDINGS  
Main Results: […]  
Significant Predictors: […]  
Model Performance: […]  
Scale Effects: […]

## DATA QUALITY  
Variable Info: Complete / Partial / Limited  
Missing Info: […]  
Confidence: High / Medium / Low
