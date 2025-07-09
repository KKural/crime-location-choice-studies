# Examine the data structure of the clean combined dataset
library(readr)

# Read the clean combined dataset
data <- read_csv("Output/clean_combined_dataset.csv")

# Check dimensions
cat("Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

# Look at the first article's BASIC STUDY IDENTIFICATION field
cat("First article's BASIC STUDY IDENTIFICATION field:\n")
cat(data$`BASIC STUDY IDENTIFICATION`[1])
cat("\n\n")

# Look at a few more fields from the first article
cat("First article's TEMPORAL SCOPE & DATA SOURCES field:\n")
cat(data$`TEMPORAL SCOPE & DATA SOURCES`[1])
cat("\n\n")

# Check for any patterns in the data
sample_field <- data$`BASIC STUDY IDENTIFICATION`[1]
cat("Sample field structure analysis:\n")
cat("Contains '- **':", grepl("- \\*\\*", sample_field), "\n")
cat("Contains bullet points:", grepl("^-", sample_field), "\n")
cat("Contains key-value pairs:", grepl("\\*\\*.*:\\*\\*", sample_field), "\n")

# Count non-empty cells
non_empty_count <- sum(!is.na(data$`BASIC STUDY IDENTIFICATION`) & data$`BASIC STUDY IDENTIFICATION` != "")
cat("Non-empty BASIC STUDY IDENTIFICATION cells:", non_empty_count, "\n")
