data <- readr::read_csv(here::here("Unit of analysis table with citation.csv"))



test <- tibble(data$`Name of the unit`)|>
  summarise(n)


frequency_size_unit <- data |>
  dplyr::count(`Size of the unit`) |>
  mutate(Percentage = n / sum(n) * 100)

frequency_name_unit <- data |>
  dplyr::count(`Name of the unit`) |>
  mutate(Percentage = n / sum(n) * 100)

library(dplyr)
library(tidyr)

# Assuming 'data' is your dataframe

# Categorize 'Size of the unit KM' into specified ranges
data <- data %>% 
  mutate(Size_Category = case_when(
    `Size of the unit KM` < 1 ~ "less than 1km",
    `Size of the unit KM` >= 1 & `Size of the unit KM` < 2 ~ "1km to less than 2km",
    `Size of the unit KM` >= 2 & `Size of the unit KM` < 3 ~ "2km to less than 3km",
    `Size of the unit KM` >= 3 & `Size of the unit KM` < 4 ~ "3km to less than 4km",
    `Size of the unit KM` >= 4 & `Size of the unit KM` < 5 ~ "4km to less than 5km",
    `Size of the unit KM` >= 5 ~ "5km and more",
    is.na(`Size of the unit KM`) ~ "NA"
  ))

# Calculate frequency and percentage for 'Size_Category'
frequency_size_category <- data %>% 
  count(Size_Category) %>% 
  mutate(Percentage = n / sum(n) * 100)

# View the frequency and percentage table for 'Size_Category'
print(frequency_size_category)
