# Fixed PRISMA 2020 Diagram with Proper Text Positioning
# This version fixes all text overlap issues

create_fixed_prisma <- function() {
  
  # Create PNG with higher resolution
  png("20250709_Analysis & Results/fixed_prisma_2020.png", 
      width = 1600, height = 1800, res = 150, bg = "white")
  
  # Set up the plot with better margins
  par(mar = c(2, 2, 2, 2))
  plot(0, 0, type = "n", xlim = c(0, 16), ylim = c(0, 18), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Define colors
  id_color <- "#FFD700"      # Gold
  screen_color <- "#ADD8E6"  # Light blue
  include_color <- "#90EE90" # Light green
  exclude_color <- "#FFFFFF" # White
  
  # Function to draw boxes with proper text wrapping
  draw_box <- function(x, y, width, height, text_lines, color = "white", text_size = 1.0, bold = FALSE) {
    rect(x - width/2, y - height/2, x + width/2, y + height/2, 
         col = color, border = "black", lwd = 2)
    
    # Handle multiple lines of text
    n_lines <- length(text_lines)
    if (n_lines == 1) {
      text(x, y, text_lines[1], cex = text_size, adj = 0.5, font = if(bold) 2 else 1)
    } else {
      # Calculate line spacing
      line_spacing <- 0.4
      start_y <- y + (n_lines - 1) * line_spacing / 2
      
      for (i in 1:n_lines) {
        text(x, start_y - (i - 1) * line_spacing, text_lines[i], 
             cex = text_size, adj = 0.5, font = if(bold) 2 else 1)
      }
    }
  }
  
  # Header
  rect(3, 16, 13, 17, col = id_color, border = "black", lwd = 3)
  text(8, 16.5, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.4, font = 2, adj = 0.5)
  
  # Stage labels (vertical bars)
  # Identification
  rect(0.5, 14, 1.5, 16, col = id_color, border = "black", lwd = 2)
  text(1, 15, "Identification", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  # Screening
  rect(0.5, 8, 1.5, 13.5, col = screen_color, border = "black", lwd = 2)
  text(1, 10.75, "Screening", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  # Included
  rect(0.5, 2, 1.5, 7.5, col = include_color, border = "black", lwd = 2)
  text(1, 4.75, "Included", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  # IDENTIFICATION STAGE
  # Records identified
  draw_box(5, 14.5, 3.5, 1.2, 
           c("Records identified from", "databases", "(n = 1,674)"), 
           color = "white", text_size = 1.0)
  
  # Records removed before screening
  draw_box(11, 14.5, 4.5, 1.8, 
           c("Records removed before screening:", "Duplicate records removed (n = 0)", "Records marked as ineligible", "by automation tools (n = 0)"), 
           color = exclude_color, text_size = 0.9)
  
  # SCREENING STAGE
  # Records screened
  draw_box(5, 12.5, 3.5, 1.2, 
           c("Records screened", "(n = 1,674)"), 
           color = "white", text_size = 1.0)
  
  # Records excluded
  draw_box(11, 12.5, 4.5, 1.2, 
           c("Records excluded", "(n = 1,625)"), 
           color = exclude_color, text_size = 1.0)
  
  # Reports sought for retrieval
  draw_box(5, 10.5, 3.5, 1.2, 
           c("Reports sought for retrieval", "(n = 49)"), 
           color = "white", text_size = 1.0)
  
  # Reports not retrieved
  draw_box(11, 10.5, 4.5, 1.2, 
           c("Reports not retrieved", "(n = 4)"), 
           color = exclude_color, text_size = 1.0)
  
  # Reports assessed for eligibility
  draw_box(5, 8.5, 3.5, 1.2, 
           c("Reports assessed for eligibility", "(n = 49)"), 
           color = "white", text_size = 1.0)
  
  # Reports excluded with reasons
  draw_box(11, 8.5, 4.5, 1.8, 
           c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), 
           color = exclude_color, text_size = 0.9)
  
  # INCLUDED STAGE
  # Studies included in review
  draw_box(5, 5, 3.5, 1.2, 
           c("Studies included in review", "(n = 49)"), 
           color = "white", text_size = 1.0)
  
  # Reports of included studies
  draw_box(5, 3, 3.5, 1.2, 
           c("Reports of included studies", "(n = 49)"), 
           color = "white", text_size = 1.0)
  
  # ARROWS - Main flow
  arrows(5, 13.8, 5, 13.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 11.8, 5, 11.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 9.8, 5, 9.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 7.8, 5, 5.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 4.3, 5, 3.7, lwd = 3, length = 0.15, col = "black")
  
  # Exclusion arrows
  arrows(6.8, 14.5, 8.2, 14.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 12.5, 8.2, 12.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 10.5, 8.2, 10.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 8.5, 8.2, 8.5, lwd = 2, length = 0.12, col = "black")
  
  # Footer
  text(8, 1, 
       "From: Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews.\nBMJ 2021;372:n71. For more information, visit: http://www.prisma-statement.org/", 
       cex = 0.8, adj = 0.5, font = 3)
  
  dev.off()
  
  # Also create PDF version
  pdf("20250709_Analysis & Results/fixed_prisma_2020.pdf", 
      width = 16, height = 18, bg = "white")
  
  # Same plot for PDF
  par(mar = c(2, 2, 2, 2))
  plot(0, 0, type = "n", xlim = c(0, 16), ylim = c(0, 18), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Header
  rect(3, 16, 13, 17, col = id_color, border = "black", lwd = 3)
  text(8, 16.5, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.4, font = 2, adj = 0.5)
  
  # Stage labels
  rect(0.5, 14, 1.5, 16, col = id_color, border = "black", lwd = 2)
  text(1, 15, "Identification", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  rect(0.5, 8, 1.5, 13.5, col = screen_color, border = "black", lwd = 2)
  text(1, 10.75, "Screening", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  rect(0.5, 2, 1.5, 7.5, col = include_color, border = "black", lwd = 2)
  text(1, 4.75, "Included", cex = 1.1, font = 2, srt = 90, adj = 0.5)
  
  # All the same boxes and arrows as PNG version
  draw_box(5, 14.5, 3.5, 1.2, c("Records identified from", "databases", "(n = 1,674)"), color = "white", text_size = 1.0)
  draw_box(11, 14.5, 4.5, 1.8, c("Records removed before screening:", "Duplicate records removed (n = 0)", "Records marked as ineligible", "by automation tools (n = 0)"), color = exclude_color, text_size = 0.9)
  draw_box(5, 12.5, 3.5, 1.2, c("Records screened", "(n = 1,674)"), color = "white", text_size = 1.0)
  draw_box(11, 12.5, 4.5, 1.2, c("Records excluded", "(n = 1,625)"), color = exclude_color, text_size = 1.0)
  draw_box(5, 10.5, 3.5, 1.2, c("Reports sought for retrieval", "(n = 49)"), color = "white", text_size = 1.0)
  draw_box(11, 10.5, 4.5, 1.2, c("Reports not retrieved", "(n = 4)"), color = exclude_color, text_size = 1.0)
  draw_box(5, 8.5, 3.5, 1.2, c("Reports assessed for eligibility", "(n = 49)"), color = "white", text_size = 1.0)
  draw_box(11, 8.5, 4.5, 1.8, c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), color = exclude_color, text_size = 0.9)
  draw_box(5, 5, 3.5, 1.2, c("Studies included in review", "(n = 49)"), color = "white", text_size = 1.0)
  draw_box(5, 3, 3.5, 1.2, c("Reports of included studies", "(n = 49)"), color = "white", text_size = 1.0)
  
  # Arrows
  arrows(5, 13.8, 5, 13.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 11.8, 5, 11.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 9.8, 5, 9.2, lwd = 3, length = 0.15, col = "black")
  arrows(5, 7.8, 5, 5.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 4.3, 5, 3.7, lwd = 3, length = 0.15, col = "black")
  arrows(6.8, 14.5, 8.2, 14.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 12.5, 8.2, 12.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 10.5, 8.2, 10.5, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 8.5, 8.2, 8.5, lwd = 2, length = 0.12, col = "black")
  
  # Footer
  text(8, 1, 
       "From: Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews.\nBMJ 2021;372:n71. For more information, visit: http://www.prisma-statement.org/", 
       cex = 0.8, adj = 0.5, font = 3)
  
  dev.off()
}

# Execute the function
create_fixed_prisma()

cat("Fixed PRISMA 2020 diagram created successfully!\n")
cat("Files saved:\n")
cat("- fixed_prisma_2020.png\n")
cat("- fixed_prisma_2020.pdf\n")
cat("Text overlap issues should now be resolved!\n")
