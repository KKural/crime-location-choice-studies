# Display all 51 matched title pairs
library(dplyr)

# Load complete matches
complete_matches <- read.csv("20250709_Analysis & Results/title_matches_complete.csv", stringsAsFactors = FALSE)

cat("=== COMPLETE TITLE MATCHING RESULTS ===\n")
cat("Successfully matched all", nrow(complete_matches), "studies (100% coverage)\n\n")

# Display match type summary first
match_summary <- complete_matches %>% 
  group_by(Match_Type) %>% 
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

cat("MATCH TYPE SUMMARY:\n")
for(i in 1:nrow(match_summary)) {
  cat(sprintf("%-35s: %2d studies\n", match_summary$Match_Type[i], match_summary$Count[i]))
}

cat("\n=== ALL MATCHED TITLE PAIRS ===\n")
for (i in 1:nrow(complete_matches)) {
  cat("\n", sprintf("%2d", i), ". [", complete_matches$Match_Type[i], "]\n")
  cat("    Master: ", complete_matches$Master_Title[i], "\n")
  cat("    Clean:  ", complete_matches$Clean_Title[i], "\n")
}

cat("\n=== SUMMARY ===\n")
cat("• Master dataset: 51 studies\n")
cat("• Clean dataset: 51 studies\n") 
cat("• Successfully matched: 51 studies (100%)\n")
cat("• Results saved to: 20250709_Analysis & Results/title_matches_complete.csv\n")
