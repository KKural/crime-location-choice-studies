# Test loading variables
library(readxl)

# Set paths
table_path <- "20250712_Analysis & Results/Manuscript_All_Tables.xlsx"

# Define default values first
icc <- 0.328
area_beta <- 0.127
area_p <- ".145"
year_beta <- 0.033
year_p <- ".334"

# Try to load from Excel
if (file.exists(table_path)) {
  print("Excel file exists")
  model_table <- read_excel(table_path, sheet = "Table2_Model_Results")
  print("Excel file loaded successfully")
  print(head(model_table))
  
  # Extract values
  icc <- model_table$Effect_Size_Beta[model_table$Predictor == "Country clustering (ICC)"]
  area_beta <- model_table$Effect_Size_Beta[model_table$Predictor == "Study area size"]
  year_beta <- model_table$Effect_Size_Beta[model_table$Predictor == "Publication year"]
} else {
  print("Excel file not found")
}

# Print final values
print(paste("ICC:", icc))
print(paste("Area beta:", area_beta))
print(paste("Year beta:", year_beta))
print(paste("Area p:", area_p))
print(paste("Year p:", year_p))
