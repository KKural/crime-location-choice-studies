# PRISMA 2020 Compliant Flow Diagram
# Based on the official PRISMA 2020 template

library(ggplot2)
library(grid)
library(gridExtra)

# Create PRISMA 2020 compliant diagram
create_prisma_2020 <- function() {
  
  # Create base plot
  p <- ggplot() +
    xlim(0, 14) + 
    ylim(0, 16) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20))
  
  # PRISMA 2020 colors
  identification_color <- "#FFD700"  # Gold
  screening_color <- "#87CEEB"       # Sky blue
  included_color <- "#98FB98"        # Pale green
  excluded_color <- "#FFFFFF"        # White
  
  # Stage labels (vertical)
  p <- p +
    # Identification stage label
    geom_rect(xmin = 0.5, xmax = 1.5, ymin = 13, ymax = 15, 
              fill = identification_color, color = "black", size = 1) +
    geom_text(x = 1, y = 14, label = "Identification", 
              size = 4, fontface = "bold", angle = 90, hjust = 0.5) +
    
    # Screening stage label
    geom_rect(xmin = 0.5, xmax = 1.5, ymin = 7, ymax = 12, 
              fill = screening_color, color = "black", size = 1) +
    geom_text(x = 1, y = 9.5, label = "Screening", 
              size = 4, fontface = "bold", angle = 90, hjust = 0.5) +
    
    # Included stage label
    geom_rect(xmin = 0.5, xmax = 1.5, ymin = 2, ymax = 6, 
              fill = included_color, color = "black", size = 1) +
    geom_text(x = 1, y = 4, label = "Included", 
              size = 4, fontface = "bold", angle = 90, hjust = 0.5)
  
  # Header
  p <- p +
    geom_rect(xmin = 3, xmax = 11, ymin = 14.5, ymax = 15.5, 
              fill = identification_color, color = "black", size = 1) +
    geom_text(x = 7, y = 15, 
              label = "Identification of studies via databases and registers", 
              size = 4, fontface = "bold")
  
  # IDENTIFICATION STAGE
  p <- p +
    # Records identified
    geom_rect(xmin = 3, xmax = 6, ymin = 13, ymax = 14, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 13.5, 
              label = "Records identified from\ndatabases (n = 1,674)", 
              size = 3, hjust = 0.5) +
    
    # Records removed before screening
    geom_rect(xmin = 7, xmax = 11, ymin = 12.5, ymax = 14, 
              fill = excluded_color, color = "black", size = 1) +
    geom_text(x = 9, y = 13.25, 
              label = "Records removed before screening:\nDuplicate records removed (n = 0)\nRecords marked as ineligible\nby automation tools (n = 0)\nRecords removed for other\nreasons (n = 0)", 
              size = 2.5, hjust = 0.5)
  
  # SCREENING STAGE
  p <- p +
    # Records screened
    geom_rect(xmin = 3, xmax = 6, ymin = 11, ymax = 12, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 11.5, 
              label = "Records screened\n(n = 1,674)", 
              size = 3, hjust = 0.5) +
    
    # Records excluded
    geom_rect(xmin = 7, xmax = 11, ymin = 11, ymax = 12, 
              fill = excluded_color, color = "black", size = 1) +
    geom_text(x = 9, y = 11.5, 
              label = "Records excluded\n(n = 1,625)", 
              size = 3, hjust = 0.5) +
    
    # Reports sought for retrieval
    geom_rect(xmin = 3, xmax = 6, ymin = 9.5, ymax = 10.5, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 10, 
              label = "Reports sought for retrieval\n(n = 49)", 
              size = 3, hjust = 0.5) +
    
    # Reports not retrieved
    geom_rect(xmin = 7, xmax = 11, ymin = 9.5, ymax = 10.5, 
              fill = excluded_color, color = "black", size = 1) +
    geom_text(x = 9, y = 10, 
              label = "Reports not retrieved\n(n = 4)", 
              size = 3, hjust = 0.5) +
    
    # Reports assessed for eligibility
    geom_rect(xmin = 3, xmax = 6, ymin = 8, ymax = 9, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 8.5, 
              label = "Reports assessed for eligibility\n(n = 45)", 
              size = 3, hjust = 0.5) +
    
    # Reports excluded
    geom_rect(xmin = 7, xmax = 11, ymin = 7.5, ymax = 9, 
              fill = excluded_color, color = "black", size = 1) +
    geom_text(x = 9, y = 8.25, 
              label = "Reports excluded:\nInsufficient spatial detail (n = 20)\nOffender residence studies (n = 10)\nNo discrete choice models (n = 8)\netc.", 
              size = 2.5, hjust = 0.5)
  
  # INCLUDED STAGE
  p <- p +
    # Studies included in review
    geom_rect(xmin = 3, xmax = 6, ymin = 3, ymax = 4, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 3.5, 
              label = "Studies included in review\n(n = 49)", 
              size = 3, hjust = 0.5) +
    
    # Reports of included studies
    geom_rect(xmin = 3, xmax = 6, ymin = 2, ymax = 3, 
              fill = "white", color = "black", size = 1) +
    geom_text(x = 4.5, y = 2.5, 
              label = "Reports of included studies\n(n = 49)", 
              size = 3, hjust = 0.5)
  
  # ARROWS
  p <- p +
    # Main flow arrows
    geom_segment(x = 4.5, y = 12.9, xend = 4.5, yend = 12.1, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
    geom_segment(x = 4.5, y = 10.9, xend = 4.5, yend = 10.6, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
    geom_segment(x = 4.5, y = 9.4, xend = 4.5, yend = 9.1, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
    geom_segment(x = 4.5, y = 7.9, xend = 4.5, yend = 4.1, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
    geom_segment(x = 4.5, y = 2.9, xend = 4.5, yend = 2.1, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
    
    # Exclusion arrows
    geom_segment(x = 6.1, y = 13.5, xend = 6.9, yend = 13.5, 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    geom_segment(x = 6.1, y = 11.5, xend = 6.9, yend = 11.5, 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    geom_segment(x = 6.1, y = 10, xend = 6.9, yend = 10, 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    geom_segment(x = 6.1, y = 8.5, xend = 6.9, yend = 8.5, 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1)
  
  # Footnotes
  p <- p +
    geom_text(x = 7, y = 1, 
              label = "* Consider, if feasible to do so, reporting the number of records identified from each database or register searched\n(rather than the total number across all databases/registers).\n** If automation tools were used, indicate how many records were excluded by a human and how many were excluded by\nautomation tools.", 
              size = 2, hjust = 0.5, vjust = 0.5) +
    
    # Citation
    geom_text(x = 7, y = 0.3, 
              label = "From: Page MJ, McKenzie JE, Bossuyt PM, Boutron I, Hoffmann TC, Mulrow CD, et al. The PRISMA 2020 statement: an updated\nguideline for reporting systematic reviews. BMJ 2021;372:n71. doi: 10.1136/bmj.n71\n\nFor more information, visit: http://www.prisma-statement.org/", 
              size = 2, hjust = 0.5, vjust = 0.5, fontface = "italic")
  
  return(p)
}

# Create and save the diagram
p <- create_prisma_2020()

# Save with high quality
ggsave("20250709_Analysis & Results/prisma_2020_compliant.png", p, 
       width = 14, height = 16, dpi = 300, bg = "white")

ggsave("20250709_Analysis & Results/prisma_2020_compliant.pdf", p, 
       width = 14, height = 16, bg = "white")

cat("PRISMA 2020 compliant diagram created successfully!\n")
cat("Files saved:\n")
cat("- prisma_2020_compliant.png\n")
cat("- prisma_2020_compliant.pdf\n")

print(p)
