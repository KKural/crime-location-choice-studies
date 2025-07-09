# Clean Gray PRISMA 2020 Diagram (No Footer, Grayscale)
# Professional publication-ready version

create_clean_gray_prisma <- function() {
  
  # Create PNG with high resolution
  png("20250709_Analysis & Results/clean_gray_prisma_2020.png", 
      width = 1600, height = 1600, res = 150, bg = "white")
  
  # Set up the plot with better margins
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 16), ylim = c(0, 16), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Define grayscale colors
  light_gray <- "#F5F5F5"    # Very light gray for boxes
  medium_gray <- "#E0E0E0"   # Medium gray for stage labels
  dark_gray <- "#606060"     # Dark gray for text
  
  # Function to draw boxes with proper text wrapping
  draw_box <- function(x, y, width, height, text_lines, color = "white", text_size = 1.0, bold = FALSE) {
    rect(x - width/2, y - height/2, x + width/2, y + height/2, 
         col = color, border = "black", lwd = 2)
    
    # Handle multiple lines of text
    n_lines <- length(text_lines)
    if (n_lines == 1) {
      text(x, y, text_lines[1], cex = text_size, adj = 0.5, font = if(bold) 2 else 1, col = dark_gray)
    } else {
      # Calculate line spacing
      line_spacing <- 0.4
      start_y <- y + (n_lines - 1) * line_spacing / 2
      
      for (i in 1:n_lines) {
        text(x, start_y - (i - 1) * line_spacing, text_lines[i], 
             cex = text_size, adj = 0.5, font = if(bold) 2 else 1, col = dark_gray)
      }
    }
  }
  
  # Header
  rect(3, 14.5, 13, 15.5, col = medium_gray, border = "black", lwd = 3)
  text(8, 15, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.4, font = 2, adj = 0.5, col = "black")
  
  # Stage labels (vertical bars)
  # Identification
  rect(0.5, 12.5, 1.5, 14.5, col = medium_gray, border = "black", lwd = 2)
  text(1, 13.5, "Identification", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # Screening
  rect(0.5, 7.5, 1.5, 12, col = medium_gray, border = "black", lwd = 2)
  text(1, 9.75, "Screening", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # Included
  rect(0.5, 2.5, 1.5, 7, col = medium_gray, border = "black", lwd = 2)
  text(1, 4.75, "Included", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # IDENTIFICATION STAGE
  # Records identified
  draw_box(5, 13, 3.5, 1.2, 
           c("Records identified from", "databases", "(n = 1,674)"), 
           color = light_gray, text_size = 1.0)
  
  # Records removed before screening
  draw_box(11, 13, 4.5, 1.8, 
           c("Records removed before screening:", "Duplicate records removed (n = 0)", "Records marked as ineligible", "by automation tools (n = 0)"), 
           color = "white", text_size = 0.9)
  
  # SCREENING STAGE
  # Records screened
  draw_box(5, 11, 3.5, 1.2, 
           c("Records screened", "(n = 1,674)"), 
           color = light_gray, text_size = 1.0)
  
  # Records excluded
  draw_box(11, 11, 4.5, 1.2, 
           c("Records excluded", "(n = 1,625)"), 
           color = "white", text_size = 1.0)
  
  # Reports sought for retrieval
  draw_box(5, 9, 3.5, 1.2, 
           c("Reports sought for retrieval", "(n = 49)"), 
           color = light_gray, text_size = 1.0)
  
  # Reports not retrieved
  draw_box(11, 9, 4.5, 1.2, 
           c("Reports not retrieved", "(n = 4)"), 
           color = "white", text_size = 1.0)
  
  # Reports assessed for eligibility
  draw_box(5, 7, 3.5, 1.2, 
           c("Reports assessed for eligibility", "(n = 49)"), 
           color = light_gray, text_size = 1.0)
  
  # Reports excluded with reasons
  draw_box(11, 7, 4.5, 1.8, 
           c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), 
           color = "white", text_size = 0.9)
  
  # INCLUDED STAGE
  # Studies included in review
  draw_box(5, 4, 3.5, 1.2, 
           c("Studies included in review", "(n = 49)"), 
           color = light_gray, text_size = 1.0)
  
  # Reports of included studies
  draw_box(5, 2.5, 3.5, 1.2, 
           c("Reports of included studies", "(n = 49)"), 
           color = light_gray, text_size = 1.0)
  
  # ARROWS - Main flow
  arrows(5, 12.3, 5, 11.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 10.3, 5, 9.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 8.3, 5, 7.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 6.3, 5, 4.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 3.3, 5, 3.2, lwd = 3, length = 0.15, col = "black")
  
  # Exclusion arrows
  arrows(6.8, 13, 8.2, 13, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 11, 8.2, 11, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 9, 8.2, 9, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 7, 8.2, 7, lwd = 2, length = 0.12, col = "black")
  
  dev.off()
  
  # Also create PDF version
  pdf("20250709_Analysis & Results/clean_gray_prisma_2020.pdf", 
      width = 16, height = 16, bg = "white")
  
  # Same plot for PDF
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(0, 16), ylim = c(0, 16), 
       axes = FALSE, xlab = "", ylab = "")
  
  # Header
  rect(3, 14.5, 13, 15.5, col = medium_gray, border = "black", lwd = 3)
  text(8, 15, "PRISMA 2020 flow diagram for systematic reviews", 
       cex = 1.4, font = 2, adj = 0.5, col = "black")
  
  # Stage labels
  rect(0.5, 12.5, 1.5, 14.5, col = medium_gray, border = "black", lwd = 2)
  text(1, 13.5, "Identification", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  rect(0.5, 7.5, 1.5, 12, col = medium_gray, border = "black", lwd = 2)
  text(1, 9.75, "Screening", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  rect(0.5, 2.5, 1.5, 7, col = medium_gray, border = "black", lwd = 2)
  text(1, 4.75, "Included", cex = 1.1, font = 2, srt = 90, adj = 0.5, col = "black")
  
  # All the same boxes and arrows as PNG version
  draw_box(5, 13, 3.5, 1.2, c("Records identified from", "databases", "(n = 1,674)"), color = light_gray, text_size = 1.0)
  draw_box(11, 13, 4.5, 1.8, c("Records removed before screening:", "Duplicate records removed (n = 0)", "Records marked as ineligible", "by automation tools (n = 0)"), color = "white", text_size = 0.9)
  draw_box(5, 11, 3.5, 1.2, c("Records screened", "(n = 1,674)"), color = light_gray, text_size = 1.0)
  draw_box(11, 11, 4.5, 1.2, c("Records excluded", "(n = 1,625)"), color = "white", text_size = 1.0)
  draw_box(5, 9, 3.5, 1.2, c("Reports sought for retrieval", "(n = 49)"), color = light_gray, text_size = 1.0)
  draw_box(11, 9, 4.5, 1.2, c("Reports not retrieved", "(n = 4)"), color = "white", text_size = 1.0)
  draw_box(5, 7, 3.5, 1.2, c("Reports assessed for eligibility", "(n = 49)"), color = light_gray, text_size = 1.0)
  draw_box(11, 7, 4.5, 1.8, c("Reports excluded:", "Insufficient spatial detail (n = 20)", "Offender residence studies (n = 10)", "No discrete choice models (n = 8)"), color = "white", text_size = 0.9)
  draw_box(5, 4, 3.5, 1.2, c("Studies included in review", "(n = 49)"), color = light_gray, text_size = 1.0)
  draw_box(5, 2.5, 3.5, 1.2, c("Reports of included studies", "(n = 49)"), color = light_gray, text_size = 1.0)
  
  # Arrows
  arrows(5, 12.3, 5, 11.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 10.3, 5, 9.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 8.3, 5, 7.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 6.3, 5, 4.7, lwd = 3, length = 0.15, col = "black")
  arrows(5, 3.3, 5, 3.2, lwd = 3, length = 0.15, col = "black")
  arrows(6.8, 13, 8.2, 13, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 11, 8.2, 11, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 9, 8.2, 9, lwd = 2, length = 0.12, col = "black")
  arrows(6.8, 7, 8.2, 7, lwd = 2, length = 0.12, col = "black")
  
  dev.off()
}

# Execute the function
create_clean_gray_prisma()

cat("Clean gray PRISMA 2020 diagram created successfully!\n")
cat("Files saved:\n")
cat("- clean_gray_prisma_2020.png\n")
cat("- clean_gray_prisma_2020.pdf\n")
cat("Professional grayscale version without footer!\n")
