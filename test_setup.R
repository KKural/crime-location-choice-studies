# Test setup chunk without rendering
library(readxl)

# Initialize all variables with default fallback values
n_studies <- 51
median_unit_size <- "1.2 km²"
mean_unit_size <- "1.63 km²"
smallest_unit_m2 <- 136
largest_unit <- "8.48 km²"
std_dev <- "1.91 km²"
skewness_original <- 2.05
orders_magnitude <- "4.8"

table_path <- "20250712_Analysis & Results/Manuscript_All_Tables.xlsx"

# Try to load from Excel file if available
if (file.exists(table_path)) {
  tryCatch({
    summary_stats <- readxl::read_excel(table_path, sheet = "Table1_Summary_Statistics", .name_repair = "minimal")
    
    skewness_val <- summary_stats$Value[summary_stats$Statistic == "Skewness (original)"]
    if (!is.na(skewness_val) && length(skewness_val) > 0) skewness_original <- as.numeric(skewness_val)
    
  }, error = function(e) {
    message("Using fallback values")
  })
}

cat("Variables loaded:\n")
cat("skewness_original:", skewness_original, "\n")
cat("orders_magnitude:", orders_magnitude, "\n")
