# Create a simple data frame with hypothetical values:
model_data <- data.frame(
  study_id = c("Bernasco2003", "Clare2009", "BernascoBlock2009",
               "Townsley2015", "Menting2016", "Long2018", 
               "Bernasco2019", "Yue2023"),
  
  unit_size_km2 = c(0.65, 6.70,  NA, 0.65, 2.96, 1.62, 0.20, 1.558),  
  num_units     = c(89,   291,   844,  89,   142,  1973, 4558, 2643),
  num_incidents = c(26,   1761, 12872, 1835, 12639, 527,  5092, 1540),
  
  unit_of_analysis = c("Neighborhood", "Residential suburb",
                       "Census tract", "Neighborhood", 
                       "Postal code area", "Community",
                       "Grid cell", "Community"),
  
  # Hypothetical effect sizes from the discrete choice model
  effect_size = c(0.25, 0.10, -0.05, 0.15, 0.02, 0.30, 0.50, 0.12),
  
  # Hypothetical standard errors for those effect sizes
  se = c(0.10, 0.15, 0.10, 0.08, 0.12, 0.18, 0.20, 0.09)
)

# Print to see your model data
model_data

install.packages("brms")  # if not installed
library(brms)

model <- brm(
  formula = effect_size | se(se) ~ 1 + (1|study_id),
  data    = model_data,
  family  = gaussian(), 
  prior   = c(
    # Prior for the overall (grand) mean effect (the intercept, mu)
    set_prior("normal(0, 1)", class = "Intercept"),
    # Prior for between-study variation (sd)
    set_prior("student_t(3, 0, 1)", class = "sd")
  ),
  chains  = 4, 
  cores   = 4, 
  iter    = 2000, 
  warmup  = 500
)


summary(model)
