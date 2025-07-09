# Reliable PRISMA 2020 Style Diagram using Base R
# This should definitely work and display correctly

# Create a PRISMA 2020 style diagram with base R
create_reliable_prisma <- function() {
  
  # Create PNG
  png("20250709_Analysis & Results/reliable_prisma_2020.png", 
      width = 1400, height = 1600, res = 150, bg = "white")
  
  # Set up the plot
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 14), ylim = c(0, 16), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Define colors
  id_color <- "gold"
  screen_color <- "lightblue"
  include_color <- "lightgreen"
  exclude_color <- "white"
  
  # Function to draw boxes with text
  draw_box <- function(x, y, width, height, text, color = "white", text_size = 1.2) {
    rect(x - width/2, y - height/2, x + width/2, y + height/2, 
         col = color, border = "black", lwd = 2)
    text(x, y, text, cex = text_size, adj = 0.5)
  }
  
  # Header
  rect(3, 14.5, 11, 15.5, col = id_color, border = "black", lwd = 2)
  text(7, 15, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.5, font = 2, adj = 0.5)
  
  # Stage labels (vertical bars)
  # Identification
  rect(0.5, 13, 1.5, 15, col = id_color, border = "black", lwd = 2)
  text(1, 14, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  # Screening
  rect(0.5, 7, 1.5, 12, col = screen_color, border = "black", lwd = 2)
  text(1, 9.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  # Included
  rect(0.5, 2, 1.5, 6, col = include_color, border = "black", lwd = 2)
  text(1, 4, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  # IDENTIFICATION STAGE
  # Records identified
  draw_box(4.5, 13.5, 3, 1, 
           "Records identified from\ndatabases\n(n = 1,674)", 
           color = "white", text_size = 1.1)
  
  # Records removed before screening
  draw_box(9, 13.5, 4, 1.5, 
           "Records removed before screening:\nDuplicate records removed (n = 0)\nRecords marked as ineligible\nby automation tools (n = 0)", 
           color = exclude_color, text_size = 0.9)
  
  # SCREENING STAGE
  # Records screened
  draw_box(4.5, 11.5, 3, 1, 
           "Records screened\n(n = 1,674)", 
           color = "white", text_size = 1.1)
  
  # Records excluded
  draw_box(9, 11.5, 4, 1, 
           "Records excluded\n(n = 1,625)", 
           color = exclude_color, text_size = 1.1)
  
  # Reports sought for retrieval
  draw_box(4.5, 10, 3, 1, 
           "Reports sought for retrieval\n(n = 49)", 
           color = "white", text_size = 1.1)
  
  # Reports not retrieved
  draw_box(9, 10, 4, 1, 
           "Reports not retrieved\n(n = 4)", 
           color = exclude_color, text_size = 1.1)
  
  # Reports assessed for eligibility
  draw_box(4.5, 8.5, 3, 1, 
           "Reports assessed for eligibility\n(n = 49)", 
           color = "white", text_size = 1.1)
  
  # Reports excluded with reasons
  draw_box(9, 8.5, 4, 1.5, 
           "Reports excluded:\nInsufficient spatial detail (n = 20)\nOffender residence studies (n = 10)\nNo discrete choice models (n = 8)", 
           color = exclude_color, text_size = 0.9)
  
  # INCLUDED STAGE
  # Studies included in review
  draw_box(4.5, 4, 3, 1, 
           "Studies included in review\n(n = 49)", 
           color = "white", text_size = 1.1)
  
  # Reports of included studies
  draw_box(4.5, 2.5, 3, 1, 
           "Reports of included studies\n(n = 49)", 
           color = "white", text_size = 1.1)
  
  # ARROWS
  # Main flow
  arrows(4.5, 13, 4.5, 12, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 11, 4.5, 10.5, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 9.5, 4.5, 9, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 8, 4.5, 4.5, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 3.5, 4.5, 3, lwd = 3, length = 0.15, col = "black")
  
  # Exclusion arrows
  arrows(6, 13.5, 7, 13.5, lwd = 2, length = 0.12, col = "black")
  arrows(6, 11.5, 7, 11.5, lwd = 2, length = 0.12, col = "black")
  arrows(6, 10, 7, 10, lwd = 2, length = 0.12, col = "black")
  arrows(6, 8.5, 7, 8.5, lwd = 2, length = 0.12, col = "black")
  
  # Footer
  text(7, 1, 
       "From: Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews.\nBMJ 2021;372:n71. For more information, visit: http://www.prisma-statement.org/", 
       cex = 0.8, adj = 0.5, font = 3)
  
  dev.off()
  
  # Also create PDF version
  pdf("20250709_Analysis & Results/reliable_prisma_2020.pdf", 
      width = 14, height = 16, bg = "white")
  
  # Same plot for PDF
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 14), ylim = c(0, 16), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Header
  rect(3, 14.5, 11, 15.5, col = id_color, border = "black", lwd = 2)
  text(7, 15, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.5, font = 2, adj = 0.5)
  
  # Stage labels
  rect(0.5, 13, 1.5, 15, col = id_color, border = "black", lwd = 2)
  text(1, 14, "Identification", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  rect(0.5, 7, 1.5, 12, col = screen_color, border = "black", lwd = 2)
  text(1, 9.5, "Screening", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  rect(0.5, 2, 1.5, 6, col = include_color, border = "black", lwd = 2)
  text(1, 4, "Included", cex = 1.2, font = 2, srt = 90, adj = 0.5)
  
  # All the same boxes and arrows as PNG version
  draw_box(4.5, 13.5, 3, 1, "Records identified from\ndatabases\n(n = 1,674)", color = "white", text_size = 1.1)
  draw_box(9, 13.5, 4, 1.5, "Records removed before screening:\nDuplicate records removed (n = 0)\nRecords marked as ineligible\nby automation tools (n = 0)", color = exclude_color, text_size = 0.9)
  draw_box(4.5, 11.5, 3, 1, "Records screened\n(n = 1,674)", color = "white", text_size = 1.1)
  draw_box(9, 11.5, 4, 1, "Records excluded\n(n = 1,625)", color = exclude_color, text_size = 1.1)
  draw_box(4.5, 10, 3, 1, "Reports sought for retrieval\n(n = 49)", color = "white", text_size = 1.1)
  draw_box(9, 10, 4, 1, "Reports not retrieved\n(n = 4)", color = exclude_color, text_size = 1.1)
  draw_box(4.5, 8.5, 3, 1, "Reports assessed for eligibility\n(n = 49)", color = "white", text_size = 1.1)
  draw_box(9, 8.5, 4, 1.5, "Reports excluded:\nInsufficient spatial detail (n = 20)\nOffender residence studies (n = 10)\nNo discrete choice models (n = 8)", color = exclude_color, text_size = 0.9)
  draw_box(4.5, 4, 3, 1, "Studies included in review\n(n = 49)", color = "white", text_size = 1.1)
  draw_box(4.5, 2.5, 3, 1, "Reports of included studies\n(n = 49)", color = "white", text_size = 1.1)
  
  # Arrows
  arrows(4.5, 13, 4.5, 12, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 11, 4.5, 10.5, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 9.5, 4.5, 9, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 8, 4.5, 4.5, lwd = 3, length = 0.15, col = "black")
  arrows(4.5, 3.5, 4.5, 3, lwd = 3, length = 0.15, col = "black")
  arrows(6, 13.5, 7, 13.5, lwd = 2, length = 0.12, col = "black")
  arrows(6, 11.5, 7, 11.5, lwd = 2, length = 0.12, col = "black")
  arrows(6, 10, 7, 10, lwd = 2, length = 0.12, col = "black")
  arrows(6, 8.5, 7, 8.5, lwd = 2, length = 0.12, col = "black")
  
  # Footer
  text(7, 1, 
       "From: Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews.\nBMJ 2021;372:n71. For more information, visit: http://www.prisma-statement.org/", 
       cex = 0.8, adj = 0.5, font = 3)
  
  dev.off()
}

# Execute the function
create_reliable_prisma()

cat("Reliable PRISMA 2020 diagram created successfully!\n")
cat("Files saved:\n")
cat("- reliable_prisma_2020.png\n")
cat("- reliable_prisma_2020.pdf\n")
cat("These should display correctly!\n")
