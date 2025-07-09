# Final Enhanced PRISMA 2020 Diagram with Maximum Spacing and Properly Formatted Arrows
# This version maximizes vertical spacing and fixes arrow formatting issues

create_final_enhanced_prisma <- function() {
  
  # Create PNG with maximum vertical space and resolution
  png("20250709_Analysis & Results/final_enhanced_prisma_2020.png", 
      width = 2400, height = 3200, res = 200, bg = "white", type = "cairo")
  
  # Set up the plot with maximum vertical space
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 18), ylim = c(0, 28), 
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
  
  # Header - at the top
  rect(3, 26, 15, 27, col = medium_gray, border = "black", lwd = 3)
  text(9, 26.5, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.6, font = 2, adj = 0.5, col = "black")
  
  # IDENTIFICATION SECTION - Maximum separation at top
  # Records identified - bigger box
  draw_box(6, 24, 5, 1.8, 
           c("Records identified from", "databases", "(n = 2,325)"), 
           color = light_gray, text_size = 1.1)
  
  # Records removed before screening - bigger box with better formatting
  draw_box(13, 24, 6.5, 2.6, 
           c("Records removed before screening:", "Duplicate records removed (n = 651)", "Records marked as ineligible", "by automation tools (n = 0)"), 
           color = "white", text_size = 0.95)
  
  # Identification stage label - ONLY for identification section
  rect(0.5, 22.5, 1.8, 25.5, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 24, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # SCREENING SECTION - Maximum separation with more space
  # Records screened - bigger box
  draw_box(6, 20, 5, 1.8, 
           c("Records screened", "(n = 1,674)"), 
           color = light_gray, text_size = 1.1)
  
  # Records excluded - bigger box with better formatting
  draw_box(13, 20, 6.5, 2.0, 
           c("Records excluded", "(n = 1,625)"), 
           color = "white", text_size = 1.05)
  
  # Reports sought for retrieval - bigger box
  draw_box(6, 17, 5, 1.8, 
           c("Reports sought for retrieval", "(n = 49)"), 
           color = light_gray, text_size = 1.1)
  
  # Reports not retrieved - bigger box with better formatting
  draw_box(13, 17, 6.5, 2.0, 
           c("Reports not retrieved", "(n = 4)"), 
           color = "white", text_size = 1.05)
  
  # Reports assessed for eligibility - bigger box
  draw_box(6, 14, 5, 1.8, 
           c("Reports assessed for eligibility", "(n = 49)"), 
           color = light_gray, text_size = 1.1)
  
  # Reports excluded with reasons - bigger box with better formatting
  draw_box(13, 14, 6.5, 2.8, 
           c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), 
           color = "white", text_size = 0.95)
  
  # Screening stage label - ONLY for screening section, maximum separation
  rect(0.5, 12, 1.8, 21, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 16.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # INCLUDED SECTION - Maximum separation at bottom
  # Studies included in review - bigger box
  draw_box(6, 10, 5, 1.8, 
           c("Studies included in review", "(n = 49)"), 
           color = light_gray, text_size = 1.1)
  
  # Reports of included studies - bigger box
  draw_box(6, 7, 5, 1.8, 
           c("Reports of included studies", "(n = 49)"), 
           color = light_gray, text_size = 1.1)
  
  # Included stage label - ONLY for included section, maximum separation
  rect(0.5, 5, 1.8, 11, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 8, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # ARROWS - Precisely calculated positions based on box dimensions
  # Main flow arrows (vertical, down-pointing) - calculated from box edges
  arrows(6, 24 - 0.9, 6, 20 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Records identified to Records screened
  arrows(6, 20 - 0.9, 6, 17 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Records screened to Reports sought
  arrows(6, 17 - 0.9, 6, 14 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Reports sought to Reports assessed
  arrows(6, 14 - 0.9, 6, 10 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Reports assessed to Studies included
  arrows(6, 10 - 0.9, 6, 7 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)   # From Studies included to Reports of studies
  
  # Exclusion arrows (horizontal, right-pointing) - calculated from box edges
  arrows(6 + 2.5, 24, 13 - 3.25, 24, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Records removed
  arrows(6 + 2.5, 20, 13 - 3.25, 20, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Records excluded
  arrows(6 + 2.5, 17, 13 - 3.25, 17, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Reports not retrieved
  arrows(6 + 2.5, 14, 13 - 3.25, 14, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Reports excluded with reasons
  
  dev.off()
  
  # Create PDF version with maximum spacing
  pdf("20250709_Analysis & Results/final_enhanced_prisma_2020.pdf", 
      width = 18, height = 26, bg = "white")
  
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 18), ylim = c(0, 28), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Header
  rect(3, 26, 15, 27, col = medium_gray, border = "black", lwd = 3)
  text(9, 26.5, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.6, font = 2, adj = 0.5, col = "black")
  
  # All content boxes with maximum spacing and better formatting
  draw_box(6, 24, 5, 1.8, c("Records identified from", "databases", "(n = 2,325)"), color = light_gray, text_size = 1.1)
  draw_box(13, 24, 6.5, 2.6, c("Records removed before screening:", "Duplicate records removed (n = 651)", "Records marked as ineligible", "by automation tools (n = 0)"), color = "white", text_size = 0.95)
  draw_box(6, 20, 5, 1.8, c("Records screened", "(n = 1,674)"), color = light_gray, text_size = 1.1)
  draw_box(13, 20, 6.5, 2.0, c("Records excluded", "(n = 1,625)"), color = "white", text_size = 1.05)
  draw_box(6, 17, 5, 1.8, c("Reports sought for retrieval", "(n = 49)"), color = light_gray, text_size = 1.1)
  draw_box(13, 17, 6.5, 2.0, c("Reports not retrieved", "(n = 4)"), color = "white", text_size = 1.05)
  draw_box(6, 14, 5, 1.8, c("Reports assessed for eligibility", "(n = 49)"), color = light_gray, text_size = 1.1)
  draw_box(13, 14, 6.5, 2.8, c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), color = "white", text_size = 0.95)
  draw_box(6, 10, 5, 1.8, c("Studies included in review", "(n = 49)"), color = light_gray, text_size = 1.1)
  draw_box(6, 7, 5, 1.8, c("Reports of included studies", "(n = 49)"), color = light_gray, text_size = 1.1)
  
  # Stage labels with maximum separation
  rect(0.5, 22.5, 1.8, 25.5, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 24, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  rect(0.5, 12, 1.8, 21, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 16.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  rect(0.5, 5, 1.8, 11, col = medium_gray, border = "black", lwd = 2)
  text(1.15, 8, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # Precisely calculated arrows based on box dimensions
  # Main flow arrows (vertical, down-pointing) - calculated from box edges
  arrows(6, 24 - 0.9, 6, 20 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Records identified to Records screened
  arrows(6, 20 - 0.9, 6, 17 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Records screened to Reports sought
  arrows(6, 17 - 0.9, 6, 14 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Reports sought to Reports assessed
  arrows(6, 14 - 0.9, 6, 10 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)  # From Reports assessed to Studies included
  arrows(6, 10 - 0.9, 6, 7 + 0.9, lwd = 3, length = 0.15, col = "black", angle = 20, code = 2)   # From Studies included to Reports of studies
  
  # Exclusion arrows (horizontal, right-pointing) - calculated from box edges
  arrows(6 + 2.5, 24, 13 - 3.25, 24, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Records removed
  arrows(6 + 2.5, 20, 13 - 3.25, 20, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Records excluded
  arrows(6 + 2.5, 17, 13 - 3.25, 17, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Reports not retrieved
  arrows(6 + 2.5, 14, 13 - 3.25, 14, lwd = 2, length = 0.12, col = "black", angle = 20, code = 2)  # To Reports excluded with reasons
  
  dev.off()
}

# Execute the function
create_final_enhanced_prisma()

cat("Final enhanced PRISMA 2020 diagram created successfully!\n")
cat("Files saved:\n")
cat("- final_enhanced_prisma_2020.png\n")
cat("- final_enhanced_prisma_2020.pdf\n")
cat("Updated with correct litsearchr duplicate removal numbers (651 duplicates from 2,325 total records)!\n")
