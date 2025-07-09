# Screening Analysis Script for PRISMA Flow Diagram
# This script analyzes the screening process from the working CSV file

library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(tidyr)

# Load the data
screening_data <- read.csv("Data/20250521_Unique_retrieved_articles_wokring.csv", stringsAsFactors = FALSE)

# Clean the screening decision column
screening_data$screening_decision <- str_trim(screening_data$select_reject_by..Kural.)

# Create summary statistics
cat("=== SCREENING ANALYSIS SUMMARY ===\n")
cat("Total records in working file:", nrow(screening_data), "\n\n")

# Count by screening decision
screening_counts <- screening_data %>%
  count(screening_decision, sort = TRUE)

print("Screening decisions:")
print(screening_counts)
cat("\n")

# Extract inclusion/exclusion patterns
included_pattern <- "included|Included"
excluded_pattern <- "Excluded|excluded"

# Categorize decisions
screening_data$status <- case_when(
  str_detect(screening_data$screening_decision, included_pattern) ~ "Included",
  str_detect(screening_data$screening_decision, excluded_pattern) ~ "Excluded",
  screening_data$screening_decision == "" ~ "Not screened",
  str_detect(screening_data$screening_decision, "Not available") ~ "Not available",
  TRUE ~ "Other"
)

# Count by status
status_counts <- screening_data %>%
  count(status, sort = TRUE)

print("Status summary:")
print(status_counts)
cat("\n")

# For excluded articles, identify exclusion stage
screening_data$exclusion_stage <- case_when(
  str_detect(screening_data$screening_decision, "title") ~ "Title screening",
  str_detect(screening_data$screening_decision, "abstract") ~ "Abstract screening", 
  str_detect(screening_data$screening_decision, "full text") ~ "Full-text screening",
  str_detect(screening_data$screening_decision, included_pattern) ~ "Included",
  TRUE ~ "Other"
)

exclusion_stage_counts <- screening_data %>%
  count(exclusion_stage, sort = TRUE)

print("Exclusion stage summary:")
print(exclusion_stage_counts)
cat("\n")

# For included articles, identify inclusion stage
included_data <- screening_data[screening_data$status == "Included", ]
inclusion_stage_counts <- included_data %>%
  count(exclusion_stage, sort = TRUE)

print("Inclusion stage summary:")
print(inclusion_stage_counts)
cat("\n")

# Create detailed breakdown
cat("=== DETAILED BREAKDOWN ===\n")
cat("Records retrieved:", nrow(screening_data), "\n")
cat("Records excluded by title:", sum(screening_data$exclusion_stage == "Title screening"), "\n")
cat("Records excluded by abstract:", sum(screening_data$exclusion_stage == "Abstract screening"), "\n")
cat("Records excluded by full-text:", sum(screening_data$exclusion_stage == "Full-text screening"), "\n")
cat("Records included:", sum(screening_data$status == "Included"), "\n")
cat("Records not available:", sum(screening_data$status == "Not available"), "\n")
cat("Records not screened:", sum(screening_data$status == "Not screened"), "\n")

# Calculate percentages
total_screened <- sum(screening_data$status %in% c("Included", "Excluded"))
cat("\nTotal screened records:", total_screened, "\n")
cat("Inclusion rate:", round(sum(screening_data$status == "Included") / total_screened * 100, 1), "%\n")
cat("Exclusion rate:", round(sum(screening_data$status == "Excluded") / total_screened * 100, 1), "%\n")

# Create summary table for manuscript
summary_table <- data.frame(
  Stage = c("Records retrieved", "Excluded by title", "Excluded by abstract", 
            "Excluded by full-text", "Not available for download", "Final included studies"),
  Count = c(nrow(screening_data),
            sum(screening_data$exclusion_stage == "Title screening"),
            sum(screening_data$exclusion_stage == "Abstract screening"),
            sum(screening_data$exclusion_stage == "Full-text screening"),
            sum(screening_data$status == "Not available"),
            sum(screening_data$status == "Included"))
)

print("Summary table for manuscript:")
print(summary_table)

# Write results to file
write.csv(summary_table, "20250709_Analysis & Results/prisma_flow_numbers.csv", row.names = FALSE)

# Create a more detailed analysis by year if year data is available
if("year" %in% names(screening_data) && !all(is.na(screening_data$year))) {
  cat("\n=== TEMPORAL ANALYSIS ===\n")
  yearly_summary <- screening_data %>%
    filter(!is.na(year) & year != "") %>%
    group_by(year, status) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = status, values_from = count, values_fill = 0)
  
  print("Yearly breakdown:")
  print(yearly_summary)
}

# Database analysis if available
if("database" %in% names(screening_data) && !all(is.na(screening_data$database))) {
  cat("\n=== DATABASE ANALYSIS ===\n")
  database_summary <- screening_data %>%
    filter(!is.na(database) & database != "") %>%
    group_by(database, status) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = status, values_from = count, values_fill = 0)
  
  print("Database breakdown:")
  print(database_summary)
}

# Create visualization
screening_plot <- ggplot(status_counts, aes(x = reorder(status, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Screening Results Summary",
       x = "Screening Decision",
       y = "Number of Records") +
  theme_minimal() +
  theme(text = element_text(size = 12))

ggsave("20250709_Analysis & Results/screening_summary.png", screening_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to 20250709_Analysis & Results/\n")
