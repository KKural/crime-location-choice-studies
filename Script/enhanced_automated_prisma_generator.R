# Enhanced Automated PRISMA 2020 Flow Diagram Generator
# This script properly handles the screening flow from CSV data
# Authors: [Your Name]
# Date: 2025-01-17

# Load required libraries
library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(here)

# Clear environment
rm(list = ls())

# Set working directory and file paths
setwd(here())

# File paths
csv_file <- "Data/20250521_Unique_retrieved_articles_wokring.csv"
output_dir <- "20250710_Analysis & Results"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read the screening data
screening_data <- read.csv(csv_file, stringsAsFactors = FALSE)

# Clean and standardize the screening decisions
screening_data$kural_decision <- tolower(trimws(screening_data$select_reject_by..Kural.))

# Calculate PRISMA 2020 flow numbers following proper structure
# Total records from database search (before any duplicate removal)
total_records_from_databases <- 2325

# Duplicates removed by litsearchr (exact title matching)
litsearchr_duplicates_removed <- 651

# Records after litsearchr duplicate removal (this is what's in our CSV)
records_after_litsearchr <- total_records_from_databases - litsearchr_duplicates_removed
total_records_raw <- nrow(screening_data)  # Should be 1674

# Additional manual exclusions found during screening
additional_duplicates <- sum(screening_data$kural_decision == "duplicate", na.rm = TRUE)
other_language <- sum(screening_data$kural_decision == "other langauge", na.rm = TRUE)
not_available <- sum(screening_data$kural_decision == "not available for download", na.rm = TRUE)

# Main screening exclusions (title, abstract, full-text)
excluded_title <- sum(screening_data$kural_decision == "excluded by title", na.rm = TRUE)
excluded_abstract <- sum(screening_data$kural_decision == "excluded by abstract", na.rm = TRUE)
excluded_full_text <- sum(screening_data$kural_decision == "excluded by full text", na.rm = TRUE)

# Included studies - ALL studies that made it through the screening process
included_title <- sum(screening_data$kural_decision == "included by title", na.rm = TRUE)
included_abstract <- sum(screening_data$kural_decision == "included by abstract", na.rm = TRUE)
included_full_text <- sum(screening_data$kural_decision == "included by full text", na.rm = TRUE)

# For PRISMA flow, final included = all studies that were included at any stage
final_included <- included_title + included_abstract + included_full_text

# Calculate PRISMA flow following proper structure
# Total records identified from databases
total_records_identified <- total_records_from_databases

# Records after duplicates removed (litsearchr + additional manual duplicates + other languages)
total_duplicates_removed <- litsearchr_duplicates_removed + additional_duplicates
additional_exclusions_screening <- additional_duplicates + other_language
total_records_after_duplicates <- total_records_raw - additional_exclusions_screening

# Title/Abstract screening
records_screened_title_abstract <- total_records_after_duplicates
records_excluded_title_abstract <- excluded_title + excluded_abstract
records_after_title_abstract <- records_screened_title_abstract - records_excluded_title_abstract

# Reports sought for retrieval
reports_sought <- records_after_title_abstract
reports_not_retrieved <- not_available
reports_retrieved <- reports_sought - reports_not_retrieved

# Reports assessed for full-text eligibility
reports_assessed_full_text <- reports_retrieved
reports_excluded_full_text <- excluded_full_text

# For PRISMA flow: final included = all studies that were not excluded
# This includes studies included at title, abstract, or full-text stages
final_included <- included_title + included_abstract + included_full_text

# Create enhanced PRISMA 2020 flow diagram using custom function
create_automated_enhanced_prisma <- function() {
  
  # Create PNG with minimal white space - optimized dimensions
  png(paste0(output_dir, "/prisma_2020.png"), 
      width = 2200, height = 2800, res = 200, bg = "white", type = "cairo")
  
  # Set up the plot with minimal margins to reduce white space
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  plot(0, 0, type = "n", xlim = c(0, 18), ylim = c(0, 24), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Define grayscale colors
  light_gray <- "#F5F5F5"
  medium_gray <- "#E0E0E0"
  dark_gray <- "#606060"
  
  # Function to draw bigger boxes with better text fitting and proper formatting
  draw_box <- function(x, y, width, height, text_lines, color = "white", text_size = 1.0) {
    rect(x - width/2, y - height/2, x + width/2, y + height/2, 
         col = color, border = "black", lwd = 2)
    
    n_lines <- length(text_lines)
    if (n_lines == 1) {
      text(x, y, text_lines[1], cex = text_size, adj = 0.5, col = dark_gray)
    } else {
      # Better line spacing for multi-line text
      line_spacing <- 0.5
      start_y <- y + (n_lines - 1) * line_spacing / 2
      for (i in 1:n_lines) {
        text(x, start_y - (i - 1) * line_spacing, text_lines[i], 
             cex = text_size, adj = 0.5, col = dark_gray)
      }
    }
  }
  
  # IDENTIFICATION SECTION - Following correct flow from database search
  # Records identified from databases (original total before any duplicate removal)
  draw_box(6, 22, 5, 1.8, 
           c("Records identified from", "databases", paste0("(n = ", total_records_identified, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Records removed before screening (litsearchr duplicates + additional manual exclusions)
  draw_box(13, 22, 6.5, 3.0, 
           c("Records removed before screening:", 
             paste0("Duplicate records removed by litsearchr (n = ", litsearchr_duplicates_removed, ")"), 
             paste0("Additional duplicates found (n = ", additional_duplicates, ")"),
             paste0("Records in other language (n = ", other_language, ")"), 
             "Records marked as ineligible by automation tools (n = 0)"), 
           color = "white", text_size = 0.85)
  
  # Identification stage label
  rect(0.5, 20.5, 1.8, 23.5, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 22, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # SCREENING SECTION - Following proper PRISMA 2020 flow
  # Records screened (title and abstract)
  draw_box(6, 18, 5, 1.8, 
           c("Records screened", paste0("(n = ", records_screened_title_abstract, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Records excluded at title/abstract screening
  draw_box(13, 18, 6.5, 2.4, 
           c("Records excluded:", paste0("Title screening (n = ", excluded_title, ")"), paste0("Abstract screening (n = ", excluded_abstract, ")")), 
           color = "white", text_size = 0.95)
  
  # Reports sought for retrieval
  draw_box(6, 15, 5, 1.8, 
           c("Reports sought for retrieval", paste0("(n = ", reports_sought, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Reports not retrieved
  draw_box(13, 15, 6.5, 2.0, 
           c("Reports not retrieved", paste0("(n = ", reports_not_retrieved, ")")), 
           color = "white", text_size = 1.05)
  
  # Reports assessed for eligibility (full-text)
  draw_box(6, 12, 5, 1.8, 
           c("Reports assessed for eligibility", paste0("(n = ", reports_assessed_full_text, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Reports excluded at full-text stage
  draw_box(13, 12, 6.5, 2.0, 
           c("Reports excluded with reasons:", paste0("Full-text exclusions (n = ", excluded_full_text, ")")), 
           color = "white", text_size = 0.95)
  
  # Screening stage label
  rect(0.5, 10, 1.8, 19, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 14.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # INCLUDED SECTION - Minimal bottom space
  # Studies included in review - bigger box with automated data
  draw_box(6, 8, 5, 1.8, 
           c("Studies included in review", paste0("(n = ", final_included, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Reports of included studies - bigger box, positioned close to bottom
  draw_box(6, 4.5, 5, 1.8, 
           c("Reports of included studies", paste0("(n = ", final_included, ")")), 
           color = light_gray, text_size = 1.1)
  
  # Included stage label
  rect(0.5, 2.5, 1.8, 9, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 5.75, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # ARROWS - Precisely calculated positions based on box dimensions
  # Main flow arrows (vertical, down-pointing)
  arrows(6, 22 - 0.9, 6, 18 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 18 - 0.9, 6, 15 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 15 - 0.9, 6, 12 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 12 - 0.9, 6, 8 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 8 - 0.9, 6, 4.5 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  
  # Exclusion arrows (horizontal, right-pointing)
  arrows(6 + 2.5, 22, 13 - 3.25, 22, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 18, 13 - 3.25, 18, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 15, 13 - 3.25, 15, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 12, 13 - 3.25, 12, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  
  dev.off()
  
  # Create PDF version with minimal spacing
  pdf(paste0(output_dir, "/enhanced_automated_prisma_2020.pdf"), 
      width = 16, height = 20, bg = "white")
  
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  plot(0, 0, type = "n", xlim = c(0, 18), ylim = c(0, 24), 
       axes = FALSE, xlab = "", ylab = "")
  
  # All content boxes with correct PRISMA 2020 flow
  draw_box(6, 22, 5, 1.8, c("Records identified from", "databases", paste0("(n = ", total_records_identified, ")")), color = light_gray, text_size = 1.1)
  draw_box(13, 22, 6.5, 3.0, c("Records removed before screening:", paste0("Duplicate records removed by litsearchr (n = ", litsearchr_duplicates_removed, ")"), paste0("Additional duplicates found (n = ", additional_duplicates, ")"), paste0("Records in other language (n = ", other_language, ")"), "Records marked as ineligible by automation tools (n = 0)"), color = "white", text_size = 0.85)
  draw_box(6, 18, 5, 1.8, c("Records screened", paste0("(n = ", records_screened_title_abstract, ")")), color = light_gray, text_size = 1.1)
  draw_box(13, 18, 6.5, 2.4, c("Records excluded:", paste0("Title screening (n = ", excluded_title, ")"), paste0("Abstract screening (n = ", excluded_abstract, ")")), color = "white", text_size = 0.95)
  draw_box(6, 15, 5, 1.8, c("Reports sought for retrieval", paste0("(n = ", reports_sought, ")")), color = light_gray, text_size = 1.1)
  draw_box(13, 15, 6.5, 2.0, c("Reports not retrieved", paste0("(n = ", reports_not_retrieved, ")")), color = "white", text_size = 1.05)
  draw_box(6, 12, 5, 1.8, c("Reports assessed for eligibility", paste0("(n = ", reports_assessed_full_text, ")")), color = light_gray, text_size = 1.1)
  draw_box(13, 12, 6.5, 2.0, c("Reports excluded with reasons:", paste0("Full-text exclusions (n = ", excluded_full_text, ")")), color = "white", text_size = 0.95)
  draw_box(6, 8, 5, 1.8, c("Studies included in review", paste0("(n = ", final_included, ")")), color = light_gray, text_size = 1.1)
  draw_box(6, 4.5, 5, 1.8, c("Reports of included studies", paste0("(n = ", final_included, ")")), color = light_gray, text_size = 1.1)
  
  # Stage labels
  rect(0.5, 20.5, 1.8, 23.5, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 22, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  rect(0.5, 10, 1.8, 19, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 14.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  rect(0.5, 2.5, 1.8, 9, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 5.75, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # Arrows
  arrows(6, 22 - 0.9, 6, 18 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 18 - 0.9, 6, 15 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 15 - 0.9, 6, 12 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 12 - 0.9, 6, 8 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  arrows(6, 8 - 0.9, 6, 4.5 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)
  
  arrows(6 + 2.5, 22, 13 - 3.25, 22, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 18, 13 - 3.25, 18, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 15, 13 - 3.25, 15, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  arrows(6 + 2.5, 12, 13 - 3.25, 12, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)
  
  dev.off()
}

# Execute the function
create_automated_enhanced_prisma()

# Save the counts to a CSV file for reference (Correct PRISMA 2020 Structure)
prisma_counts <- data.frame(
  Stage = c("Total records identified", "Litsearchr duplicates removed", "Records after litsearchr", 
            "Additional duplicates found", "Other language", "Additional exclusions during screening",
            "Records after all duplicates removed", "Records screened (title/abstract)", 
            "Records excluded (title)", "Records excluded (abstract)", "Records excluded (title/abstract total)", 
            "Reports sought for retrieval", "Reports not retrieved", "Reports assessed (full-text)", 
            "Reports excluded (full-text)", "Final included studies"),
  Count = c(total_records_identified, litsearchr_duplicates_removed, records_after_litsearchr, 
            additional_duplicates, other_language, additional_exclusions_screening, 
            total_records_after_duplicates, records_screened_title_abstract, excluded_title, excluded_abstract,
            records_excluded_title_abstract, reports_sought, reports_not_retrieved, 
            reports_assessed_full_text, excluded_full_text, final_included)
)

write.csv(prisma_counts, paste0(output_dir, "/enhanced_automated_prisma_counts.csv"), row.names = FALSE)

# Save detailed breakdown
detailed_breakdown <- screening_data %>%
  group_by(kural_decision) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

write.csv(detailed_breakdown, paste0(output_dir, "/enhanced_screening_decisions_breakdown.csv"), row.names = FALSE)
