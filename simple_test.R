library(readr)

# Check first file
df1 <- read_csv("Data/Study_Identification_Spatial_Units.csv", show_col_types = FALSE)
cat("File 1 columns:\n")
print(names(df1)[1:10])

# Check second file
df2 <- read_csv("Data/Theoretical_Framework_Methodology.csv", show_col_types = FALSE)
cat("File 2 columns:\n")
print(names(df2)[1:10])

# Check third file
df3 <- read_csv("Data/Temporal_Variables_Findings.csv", show_col_types = FALSE)
cat("File 3 columns:\n")
print(names(df3)[1:10])

# Check fourth file
df4 <- read_csv("Data/Scale_Effects_Limitations.csv", show_col_types = FALSE)
cat("File 4 columns:\n")
print(names(df4)[1:10])

# Check fifth file
df5 <- read_csv("Data/study area size.csv", show_col_types = FALSE)
cat("File 5 columns:\n")
print(names(df5)[1:10])
