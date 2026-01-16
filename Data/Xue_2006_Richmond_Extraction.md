# Data Extraction: Xue & Brown 2006 - Richmond, VA

## Study Identification

**Citation**: Xue, Y., & Brown, D. E. (2006). Spatial analysis with preference specification of latent decision makers for criminal event prediction. *Decision Support Systems*, 41(3), 560-573.

**DOI**: doi:10.1016/j.dss.2004.06.007

## Geographic Information

### Country
**Value**: United States
**Supporting Quotes**: "University of Virginia, 151 Engineer's Way, P. O. Box 400747, Charlottesville, VA 22904, USA" (Affiliation); "the city of Richmond, VA" (p. 566)
**Reasoning**: Study conducted in Virginia, United States.

### City
**Value**: Richmond, VA
**Supporting Quotes**: "the city of Richmond, VA" (p. 566); "We extracted the Residential 'Breaking and Entering' (B & E) criminal incidents between July 1, 1997 and October 31, 1997 in the city of Richmond, VA" (p. 566)
**Reasoning**: Explicitly stated study location.

### Geographic_Scope
**Value**: City-level
**Supporting Quotes**: "the city of Richmond, VA" (p. 566)
**Reasoning**: Analysis covers entire city of Richmond.

### Total_Study_Area_Size
**Value**: **[NEED FULL PAPER - Richmond city area]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Total area of Richmond city not provided in excerpt; Fig. 1 shows study area but no area measurement given.

## Spatial Unit Characteristics

### Spatial_Unit_Type
**Value**: Regular grid cells
**Supporting Quotes**: "we aggregated alternatives using 2517 regular grids, which were assumed to be fine enough to represent all spatial alternatives within this area" (p. 566)
**Reasoning**: Authors created regular grid tessellation rather than using administrative boundaries.

### Spatial_Unit_Name
**Value**: Grid cell
**Supporting Quotes**: "2517 regular grids" (p. 566)
**Reasoning**: Artificial grid units, not named administrative areas.

### Number_of_Units
**Value**: 2,517 regular grid cells
**Supporting Quotes**: "we aggregated alternatives using 2517 regular grids, which were assumed to be fine enough to represent all spatial alternatives within this area" (p. 566)
**Reasoning**: Explicitly stated total number of spatial alternatives.

### Unit_Size
**Value**: **[NEED FULL PAPER - grid cell dimensions not specified]**
**Supporting Quotes**: "2517 regular grids, which were assumed to be fine enough to represent all spatial alternatives" (p. 566)
**Reasoning**: Grid size not specified in excerpt; calculated from total study area ÷ 2,517 cells.

### Average_Population_per_Unit
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not discussed in excerpt.

### Unit_Selection_Rationale
**Value**: Computational tractability while maintaining fine spatial resolution to represent individual household locations
**Supporting Quotes**: "The analysis of B & E is related to locations of households in a city. However, it is difficult to represent all locations of individual houses in even a modest sized city, such as Richmond. Therefore, we aggregated alternatives using 2517 regular grids, which were assumed to be fine enough to represent all spatial alternatives within this area" (p. 566)
**Reasoning**: Grid aggregation balances computational feasibility with spatial precision; avoids enumerating every individual house.

**Rationale_Category**: Computational feasibility; Spatial precision

### Spatial_Aggregation
**Value**: Point crime locations aggregated to regular grid cells
**Supporting Quotes**: "we aggregated alternatives using 2517 regular grids" (p. 566); "The analysis of B & E is related to locations of households in a city. However, it is difficult to represent all locations of individual houses...Therefore, we aggregated alternatives" (p. 566)
**Reasoning**: Crime incidents geocoded to point locations, then assigned to grid cells; alternative to representing individual houses.

### Alternative_Units
**Value**: Individual household locations (theoretical); census block groups (for census data)
**Supporting Quotes**: 
- "The analysis of B & E is related to locations of households in a city" (p. 566)
- "The sub regions shown in Fig. 1 are block groups, which are the smallest areas, for which census counts are recorded" (p. 566)
**Reasoning**: Ideally would use individual houses but aggregated to grids; census data comes from block groups.

## Crime Data

### Crime_Type
**Value**: Residential Breaking and Entering (B&E) / Residential Burglary
**Supporting Quotes**: "Residential 'Breaking and Entering' (B & E) criminal incidents" (p. 566)
**Reasoning**: Specific crime type analyzed.

### Crime_Type_Group
**Value**: Property crime
**Supporting Quotes**: "Residential 'Breaking and Entering'" (p. 566)
**Reasoning**: B&E is a property crime category.

### Number_of_Crimes / Crime_Incidents
**Value**: 1,200+ total incidents (879 for training, remainder for validation)
**Supporting Quotes**: 
- "There are more than 1200 crime observations" (p. 566)
- "The data from the first three months are used for model estimation. The observations of October 1997 are used for model validation" (p. 566)
**Reasoning**: July-September 1997 = training; October 1997 = testing.

### Temporal_Scope / Data_Collection_Period
**Value**: July 1, 1997 - October 31, 1997 (4 months)
**Supporting Quotes**: "We extracted the Residential 'Breaking and Entering' (B & E) criminal incidents between July 1, 1997 and October 31, 1997 in the city of Richmond, VA" (p. 566)
**Reasoning**: Explicitly stated time period.

### Data_Source_Crime / Data_Sources
**Value**: Regional Crime Analysis Program (ReCAP); Richmond Police Department RAMS (criminal incident reporting mainframe system)
**Supporting Quotes**: 
- "The data for model evaluation came from the Regional Crime Analysis Program (ReCAP)" (p. 566)
- "The specific data set used for evaluating this work came from the criminal incident reporting mainframe system (RAMS). It is the data set used by the police department in Richmond, VA. The main table in RAMS is the 'Report' table. The information about the time, location, and narrative description of criminal incidents is stored in table 'Report'" (p. 566)
**Reasoning**: Police administrative data accessed through ReCAP system.

### Number_of_Data_Sources
**Value**: 2 (crime data from RAMS/ReCAP; environmental/census data)
**Supporting Quotes**: "The features of each spatial alternative come from the combination of census data and calculated distance values" (p. 566)
**Reasoning**: Police crime data + census demographic data + calculated spatial features.

## Offender Information

### Offender_Data_Available
**Value**: No - latent/unknown decision makers
**Supporting Quotes**: 
- "to analyze and predict the spatial behavior of criminals, and more generally, latent decision makers" (Abstract)
- "Our model differs from those used in consumer theory in that the decision makers for us are active criminals and hence, not accessible by survey instruments" (p. 561)
- "Since the criminals are unknown to the analyst we have no direct way to limit these numbers" (p. 564)
**Reasoning**: Key innovation is modeling preferences of unidentified offenders.

### Offender_Characteristics_Used
**Value**: N/A (latent decision makers - characteristics inferred from spatial patterns, not directly observed)
**Supporting Quotes**: "we exploit the rational choice perspective to model the site selection behavior of individual criminals...our model differs from those used in consumer theory in that the decision makers for us are active criminals and hence, not accessible by survey instruments" (p. 561)
**Reasoning**: No individual offender attributes used; preferences inferred from crime locations.

## Model Specification

### Model_Type
**Value**: Discrete spatial choice models (multinomial logit); Two variants: Uniform Spatial Choice Model and Distinct Spatial Choice Model
**Supporting Quotes**: 
- "we combine recent advances in discrete choice theory and data mining to develop point process models for spatial analysis" (Abstract)
- "This spatial choice model is a multinomial logit model where each spatial alternative's observable utility is weighted by the probability that the alternative is evaluated" (p. 563)
- "We call the spatial choice model in Eq. (2) with the estimated prior probability P(ai∈M)...the uniform spatial choice model" (p. 564)
- "The resulting model is called the distinct spatial choice model" (p. 564)
**Reasoning**: Multinomial logit framework adapted for latent decision makers with large choice sets.

### Dependent_Variable
**Value**: Probability that spatial alternative (grid cell) ai is chosen as crime location by criminal d
**Supporting Quotes**: "the probability that an individual d from D will choose alternative ai from available choice set A can be specified as P(ai|Ad,d)" (p. 562)
**Reasoning**: Binary outcome for each alternative in choice set.

### Unit_of_Analysis
**Value**: Criminal-grid cell dyad (latent offender × spatial alternative)
**Supporting Quotes**: "For an individual d, if any two spatial alternatives ai∈Ad, ai'∈Ad' have same attribute values, then they will have same utility values u(ai)=u(ai') for all ai∈Ad, ai'∈Ad'" (p. 562)
**Reasoning**: Choice model structure evaluates each alternative for each (latent) decision maker.

## Independent Variables

### Number_of_Variables
**Value**: 4 key features (from feature selection process)
**Supporting Quotes**: 
- "We picked the features D.HIGHWAY (distance to highway), FAM.DENSITY (Family density per unit area), P.CARE.PH (personal care expenditure per household) and D.HOSPITAL (distance to hospital). The first three were used by Brown et al. [7]" (p. 567)
**Reasoning**: Feature selection reduced attribute space from many candidates to 4 key features.

### Independent_Variables
**Value**: 

**Key features (after feature selection from larger attribute set):**
- D.HIGHWAY - Distance to highway
- D.HOSPITAL - Distance to hospital
- FAM.DENSITY - Family density per unit area  
- P.CARE.PH - Personal care expenditure per household

**Original feature space included (mentioned but not all selected):**
- COND1.DST - Number of condition 1 houses per unit area (rejected: uniformly distributed)
- Additional census variables (aggregated via clustering)
- Distance measurements to various features

**Supporting Quotes**: 
- "The features of each spatial alternative come from the combination of census data and calculated distance values" (p. 566)
- "Using the calculated correlation values as similarities, we computed hierarchical clusters for all features of observed spatial incidents" (p. 567)
- "We picked the features D.HIGHWAY (distance to highway), FAM.DENSITY (Family density per unit area), P.CARE.PH (personal care expenditure per household) and D.HOSPITAL (distance to hospital)" (p. 567)
- Figure 2 caption: "Clusters of features of observed spatial alternatives" (p. 567)

**Reasoning**: Hierarchical clustering of correlated features reduced dimensionality; selected 4 non-redundant key features representing different attribute clusters.

### Variable_Categories
**Value**: 
- Accessibility (distance to highways, hospitals)
- Socioeconomic characteristics (family density, personal care expenditure)
- **[NEED FULL PAPER for complete categorization of all original variables]**

**Supporting Quotes**: See Independent_Variables above
**Reasoning**: Selected features represent spatial access and demographic/economic attributes.

## Model Estimation and Results

### Model_Fit_Statistics
**Value**: Bayesian Information Criterion (BIC) used for model selection; **[NEED FULL PAPER for actual BIC values and model fit metrics]**
**Supporting Quotes**: 
- "BIC is called Bayesian Information Criterion, which has the definition below. BIC = 2l(ai; θ̂,C) - m log(n)" (p. 565)
- "Bayesian Information Criterion (BIC) is used as a criterion to compare different models and decide the number of clusters in the data set" (p. 565)
- Figure 3: "The trends of BIC values of different parameterized model-based clustering algorithms" (p. 568)
**Reasoning**: BIC used to determine optimal number of clusters (K=6); specific fit statistics for final models need full paper.

### Clustering Results
**Value**: 6 clusters identified representing distinct criminal preference groups
**Supporting Quotes**: 
- "From Fig. 3, it is clear that the BIC values become stable or decreasing after six clusters. The results shown in Fig. 3 suggest there are six clusters in this crime data set. Each cluster corresponds to a group of criminals that have similar preferences in their choices of spatial alternatives" (p. 567)
- Table 1 shows distribution: Cluster 1 (109), Cluster 2 (180), Cluster 3 (200), Cluster 4 (202), Cluster 5 (133), Cluster 6 (55)
**Reasoning**: Model-based clustering on key features identified 6 distinct preference patterns.

### Coefficients
**Value**: **[NEED FULL PAPER - utility function parameters not reported in excerpt]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Linear utility function V(d,si) = Σ βl·xil mentioned but parameters not shown in excerpt.

### Confidence_Intervals
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not reported in excerpt.

### Effect_Sizes
**Value**: **[NEED FULL PAPER]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Not reported in excerpt.

### Model_Comparison_Results
**Value**: Both spatial choice models significantly outperform hot spot (kernel density) model; Distinct spatial choice model significantly outperforms uniform spatial choice model
**Supporting Quotes**: 
- "The comparison results in Table 2 indicate that the two new spatial choice models significantly outperform the hot spot model. The distinct spatial choice model also significantly outperforms the uniform spatial choice model" (p. 572)
- Table 2 shows Z statistics and p-values for Wilcoxon signed rank tests
- Testing data set 1: Distinct vs. Uniform (Z=1.8055, p=0.0355); Distinct vs. Hot spot (Z=3.2876, p=0.0005); Uniform vs. Hot spot (Z=3.0619, p=0.0011)
- Testing data set 2: Distinct vs. Uniform (Z=2.407, p=0.008); Distinct vs. Hot spot (Z=3.1871, p=0.0007); Uniform vs. Hot spot (Z=2.1259, p=0.0168)
**Reasoning**: Wilcoxon signed rank test on paired predictions demonstrates statistical superiority of spatial choice models.

## Methodological Details

### Alternative_Set_Construction
**Value**: Full choice set of 2,517 grid cells; importance sampling used for computational efficiency
**Supporting Quotes**: 
- "the number of spatial alternatives available to the criminal is quite large and typically much larger than any alternative set used in analyzing consumer choices" (p. 561)
- "we adopted an importance sampling technique suggested by Ben-Akiva [1]. Sampling alternatives is a commonly applied technique for reducing the computational burden involved in the model estimation process" (p. 568)
**Reasoning**: All grids theoretically available but sampled for estimation efficiency.

### Computational_Constraints
**Value**: Yes - large alternative set (N=2,517) necessitated importance sampling for parameter estimation
**Supporting Quotes**: 
- "From both the analyst's and the criminal's perspectives the alternative set is large...there may be hundreds, if not thousands, of residences and commercial properties that represent potential targets" (p. 563)
- "Since the number of spatial alternatives is very large, it is possible that some alternatives can give higher utility values but they are never considered" (p. 563)
- "the number of spatial alternatives for crime analysis is very large. From an implementation standpoint, this makes the data preparation and computation time prohibitively expensive. To handle this problem, we adopted an importance sampling technique" (p. 568)
**Reasoning**: Large choice set requires sampling strategies for feasible computation.

### Data_Limitations
**Value**: 
- Criminal preferences must be inferred (cannot survey active criminals)
- Restricted choice sets unknown to analyst
- Some relevant attributes may not be measured or available
**Supporting Quotes**: 
- "Our model differs from those used in consumer theory in that the decision makers for us are active criminals and hence, not accessible by survey instruments" (p. 561)
- "the real choice set Ad is not known to the analyst. We need a method to identify or estimate the probability P(ai∈Ad)" (p. 563)
- "This inconsistency can derive from an incomplete set of attributes available to the decision maker or the analyst" (p. 562)
- "it is quite possible that any model we develop will not include the actual attributes used by the criminals in the choices because they are not even measured by anyone but the criminal" (p. 564)
**Reasoning**: Latent decision maker framework imposes unique data challenges.

### Feature_Selection_Method
**Value**: Hierarchical clustering of correlated features; selection of one representative feature per cluster
**Supporting Quotes**: 
- "Using the calculated correlation values as similarities, we computed hierarchical clusters for all features of observed spatial incidents. The resulting clustering structure of features of observed spatial alternatives is shown as Fig. 2" (p. 567)
- "From the clustering tree, we divided the features into five clusters. Each cluster included correlated features...We picked the features..." (p. 567)
**Reasoning**: Reduces multicollinearity and dimensionality while retaining information.

### Preference_Discovery_Method
**Value**: 
- **Uniform model**: Kernel density estimation in key feature space to estimate P(ai∈M)
- **Distinct model**: Model-based clustering with mixture of Gaussians to identify groups with different preferences
**Supporting Quotes**: 
- "To capture this probability we use kernel density estimation...P(ai∈M) = (1/L)ΣT((si¹-sl¹)/h₁, (si²-sl²)/h₂,...)" (p. 564)
- "We use a Gaussian function here because the Gaussian function provides very smooth estimates of the density values" (p. 564)
- "the data are viewed as coming from a mixture of probability distributions, each representing a different cluster. The resulting mixture density is shown below. f(ai) = ΣK pk fk(ai|θk)" (p. 565)
**Reasoning**: Density estimation reveals spatial patterns indicating criminal preferences.

## Software and Tools

### Software_Used
**Value**: **[NEED FULL PAPER - likely R or specialized GIS/statistical software; ReCAP system mentioned for data access]**
**Supporting Quotes**: 
- "ReCAP system uses databases, a geographic information system (GIS), and statistical tools to analyze, predict, and display future crime patterns" (p. 566)
**Reasoning**: GIS mentioned for data management; statistical software needed for model estimation but not specified in excerpt.

## Theoretical Framework

### Theoretical_Foundation
**Value**: Rational choice theory from criminology; Discrete choice theory from economics (McFadden)
**Supporting Quotes**: 
- "According to the rational choice perspective in criminology, criminal incidents, like many other human initiated events, involve a decision making and choice process" (p. 561)
- "we exploit the rational choice perspective to model the site selection behavior of individual criminals. This is similar to methods used in economics for consumer product selection" (p. 561)
- "we can then construct discrete choice models of criminal spatial choice using the theory of discrete choice pioneered by McFadden" (p. 561)
**Reasoning**: Combines criminological theory with econometric discrete choice methodology.

### Key_Assumptions
**Value**: 
- Criminals make rational utility-maximizing decisions
- Criminals use hierarchical information processing (evaluate subset before choosing)
- **Distinct model**: Criminal population contains groups with distinct preferences
- **Uniform model**: All criminals have same preferences and pre-evaluated choice sets
- Utility errors follow Type I extreme value distribution (multinomial logit)

**Supporting Quotes**: 
- "According to the assumptions of rational decision theory, individuals make choices that maximize their utility U over all alternatives" (p. 562)
- "Assumption 1. The two factors (i and ii) mentioned above are equally important to the individuals' choices" (p. 563)
- "Assumption 2. The error term of an individual decision maker's utility function ε(d, Si) is independently and identically distributed with Type I extreme value distribution" (p. 563)
- "Assumption 3. For criminal decision making the preferences of all individuals d∈D are same" (p. 564)
**Reasoning**: Formal statistical assumptions underpin model development.

## Methodological Innovations and Comparisons

### Novel_Contributions
**Value**: 
- Comparison framework using Wilcoxon signed rank tests
- First application of discrete choice models to latent/unknown decision makers
- Integration of data mining (clustering) with discrete choice theory
- Method for modeling restricted choice sets when true choice sets unknown
- Preference specification for criminals from observed incident patterns
**Supporting Quotes**: 
- "The approach we describe here for criminal event prediction using spatial choice analysis represents a new development in discrete choice theory. The alternative set is quite large and the decision makers are latent or unknown and cannot be surveyed for their preferences" (p. 561)
- "We require some method to extract the preferences of the latent or unknown decision makers in order to improve the predictions of future spatial choices" (p. 562)
- "we combine recent advances in discrete choice theory and data mining to develop point process models for spatial analysis" (Abstract)
**Reasoning**: Extends discrete choice methodology to new domain with unique constraints.

### Comparison_to_Hot_Spots
**Value**: Spatial choice models provide better predictive accuracy and behavioral insight compared to hot spot (kernel density) analysis
**Supporting Quotes**: 
- "The paper compares the performance of this methodology to more traditional hot spot methods of crime analysis" (Abstract)
- "The comparison results...indicate that the two new spatial choice models significantly outperform the hot spot model" (p. 572)
- "hot spot techniques alone cannot indicate how crime patterns might change. What is needed is a predictive approach that models the micro behavior and spatial choices of criminals" (p. 561)
**Reasoning**: Demonstrates advantage of behavioral models over descriptive density methods.

## Discussion of Spatial Issues

### MAUP_Discussion
**Value**: Acknowledged but not formally analyzed; grid aggregation chosen as compromise
**Supporting Quotes**: 
- "The analysis of B & E is related to locations of households in a city. However, it is difficult to represent all locations of individual houses in even a modest sized city, such as Richmond. Therefore, we aggregated alternatives using 2517 regular grids, which were assumed to be fine enough to represent all spatial alternatives" (p. 566)
**Reasoning**: Authors recognize aggregation trade-offs but do not test sensitivity to grid size.

### Sensitivity_Analysis
**Value**: Model comparison via multiple test periods; clustering number selection via BIC
**Supporting Quotes**: 
- "The predictions of the compared models are paired probabilities over the spatial alternatives in choice set A. The testing data set provides locations or spatial alternatives at the sites that the crimes really happened" (p. 572)
- Two testing periods: Oct 1-15, 1997 and Oct 1-31, 1997 (Table 2)
- "BIC is used as a criterion to compare different models and decide the number of clusters" (p. 565)
**Reasoning**: Multiple validation periods test temporal stability; BIC provides systematic cluster selection.

### Validation_Approach
**Value**: Out-of-sample temporal validation using October 1997 crimes; Wilcoxon signed rank test for model comparison
**Supporting Quotes**: 
- "The data from the first three months are used for model estimation. The observations of October 1997 are used for model validation" (p. 566)
- "We use (pai^p, pai^c) to represent the paired predictions at spatial alternative ai...The Wilcoxon signed rank test is designed to test whether a particular sample comes from a population with specified median and we use it here for our evaluation" (p. 570)
**Reasoning**: Proper train-test split with formal statistical comparison.

## Future Research

### Future_Suggestions
**Value**: 
- Improve prediction accuracy further
- Refine preference discovery methods
- Apply to other crime types and locations
- Explore dynamic preference changes over time
**Supporting Quotes**: 
- "The results demonstrate that the analysis of feature values attached to all spatial alternatives and the analysis of specified preferences of decision makers lead to improvement in the prediction of future crime locations" (p. 572)
- "These results suggest that the estimation of criminals' preferences and incorporation in spatial choice models provides a method for improving criminal event prediction" (p. 572)
**Reasoning**: Authors highlight potential for methodological refinement and broader application.

### Practical_Implications
**Value**: Better resource allocation for law enforcement; identification of high-risk areas based on criminal preferences rather than just historical hot spots
**Supporting Quotes**: 
- "Spatial analysis is of critical importance to law enforcement. It enables better planning and the use of scarce resources" (p. 572)
- "the analysis results have been presented to crime analysts and community residents. These evaluators agreed that the spatial choice models predict some crime areas that did have high Breaking and Entering crime rates in the past several years" (p. 572)
**Reasoning**: Model provides actionable intelligence for crime prevention.

## Additional Notes

### Study_Context
**Value**: Part of Regional Crime Analysis Program (ReCAP) - university-police partnership for data-driven crime analysis
**Supporting Quotes**: "ReCAP was created as a cooperative project between local police departments in Northern and Central Virginia and the researchers in the University of Virginia. ReCAP system uses databases, a geographic information system (GIS), and statistical tools to analyze, predict, and display future crime patterns" (p. 566)
**Reasoning**: Collaborative applied research context.

### Data_Availability
**Value**: Police administrative data; access through ReCAP partnership
**Supporting Quotes**: **[NEED FULL PAPER for data sharing policies]**
**Reasoning**: Sensitive police data typically restricted.

### Replication_Information
**Value**: **[NEED FULL PAPER for complete methodological details and code]**
**Supporting Quotes**: **[NEED FULL PAPER]**
**Reasoning**: Sufficient methodological detail provided for conceptual replication; implementation details and code not provided in excerpt.

---

## NOTES FOR COMPLETION

**Information available from excerpts (Abstract, Introduction, Sections 2-4). Full paper needed for:**
- Complete limitations discussion
- Complete list of original candidate variables before feature selection
- Complete model fit statistics beyond BIC
- Data availability and replication materials
- Detailed utility function parameter estimates (β coefficients)
- Full results interpretation
- Grid cell size specifications
- Software and code details
- Standard errors and confidence intervals for parameters
- Total study area size for Richmond

**Key methodological contributions confirmed:**
- Comparison of uniform vs. distinct preference specifications
- First discrete choice model for latent decision makers
- Integration of clustering with discrete choice for preference discovery
- Outperforms traditional hot spot methods statistically

**Key findings:**
- 4 key features selected: distance to highway, family density, personal care expenditure, distance to hospital
- 6 distinct criminal preference clusters identified
- Both spatial choice models outperform kernel density hot spots (p < 0.05)
- Distinct model outperforms uniform model (p < 0.05)

**Next steps: Obtain full paper for complete extraction of all fields requested in CSV format.**
