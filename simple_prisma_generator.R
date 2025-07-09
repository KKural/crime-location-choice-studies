# Simple but effective PRISMA diagram generator
# Focus on creating a working diagram with clear layout

library(ggplot2)
library(grid)

# Create a simple PRISMA flow diagram
create_simple_prisma <- function() {
  
  # Create base plot
  p <- ggplot() +
    xlim(0, 12) + 
    ylim(0, 14) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20))
  
  # Box dimensions
  box_width <- 3.5
  box_height <- 1.5
  
  # Main flow boxes
  p <- p +
    # Records identified
    geom_rect(xmin = 4.25, xmax = 7.75, ymin = 11.25, ymax = 12.75, 
              fill = "#E6F3FF", color = "black", size = 1) +
    geom_text(x = 6, y = 12, 
              label = "Records identified through\ndatabase searching\n(n = 1,674)", 
              size = 3.5, hjust = 0.5, vjust = 0.5) +
    
    # Title screening
    geom_rect(xmin = 4.25, xmax = 7.75, ymin = 9.25, ymax = 10.75, 
              fill = "#E6FFE6", color = "black", size = 1) +
    geom_text(x = 6, y = 10, 
              label = "Records screened\nby title\n(n = 1,674)", 
              size = 3.5, hjust = 0.5, vjust = 0.5) +
    
    # Abstract screening
    geom_rect(xmin = 4.25, xmax = 7.75, ymin = 7.25, ymax = 8.75, 
              fill = "#E6FFE6", color = "black", size = 1) +
    geom_text(x = 6, y = 8, 
              label = "Records screened\nby abstract\n(n = 317)", 
              size = 3.5, hjust = 0.5, vjust = 0.5) +
    
    # Full-text screening
    geom_rect(xmin = 4.25, xmax = 7.75, ymin = 5.25, ymax = 6.75, 
              fill = "#E6FFE6", color = "black", size = 1) +
    geom_text(x = 6, y = 6, 
              label = "Full-text articles\nassessed for eligibility\n(n = 49)", 
              size = 3.5, hjust = 0.5, vjust = 0.5) +
    
    # Final inclusion
    geom_rect(xmin = 4.25, xmax = 7.75, ymin = 3.25, ymax = 4.75, 
              fill = "#E6F0FF", color = "black", size = 1) +
    geom_text(x = 6, y = 4, 
              label = "Studies included in\nquantitative synthesis\n(n = 49)", 
              size = 3.5, hjust = 0.5, vjust = 0.5) +
    
    # Exclusion boxes
    # Title exclusions
    geom_rect(xmin = 8.5, xmax = 11.5, ymin = 8.5, ymax = 11.5, 
              fill = "#FFE6E6", color = "black", size = 1) +
    geom_text(x = 10, y = 10, 
              label = "Records excluded\nby title\n(n = 1,357)\n\n• Not crime location choice\n• No spatial choice models\n• Review/theoretical papers", 
              size = 3, hjust = 0.5, vjust = 0.5) +
    
    # Abstract exclusions
    geom_rect(xmin = 8.5, xmax = 11.5, ymin = 6.5, ymax = 9.5, 
              fill = "#FFE6E6", color = "black", size = 1) +
    geom_text(x = 10, y = 8, 
              label = "Records excluded\nby abstract\n(n = 268)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", 
              size = 3, hjust = 0.5, vjust = 0.5) +
    
    # Full-text exclusions
    geom_rect(xmin = 8.5, xmax = 11.5, ymin = 4.5, ymax = 7.5, 
              fill = "#FFE6E6", color = "black", size = 1) +
    geom_text(x = 10, y = 6, 
              label = "Full-text articles excluded\n(n = 38)\n\nArticles not available\n(n = 4)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", 
              size = 3, hjust = 0.5, vjust = 0.5) +
    
    # Arrows
    geom_segment(x = 6, y = 11.2, xend = 6, yend = 10.8, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
    geom_segment(x = 6, y = 9.2, xend = 6, yend = 8.8, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
    geom_segment(x = 6, y = 7.2, xend = 6, yend = 6.8, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
    geom_segment(x = 6, y = 5.2, xend = 6, yend = 4.8, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
    
    # Exclusion arrows
    geom_segment(x = 7.75, y = 10, xend = 8.5, yend = 10, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1) +
    geom_segment(x = 7.75, y = 8, xend = 8.5, yend = 8, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1) +
    geom_segment(x = 7.75, y = 6, xend = 8.5, yend = 6, 
                 arrow = arrow(length = unit(0.3, "cm")), size = 1) +
    
    # Stage labels
    geom_text(x = 2, y = 12, label = "IDENTIFICATION", size = 4, fontface = "bold", angle = 90) +
    geom_text(x = 2, y = 9, label = "SCREENING", size = 4, fontface = "bold", angle = 90) +
    geom_text(x = 2, y = 6, label = "ELIGIBILITY", size = 4, fontface = "bold", angle = 90) +
    geom_text(x = 2, y = 4, label = "INCLUDED", size = 4, fontface = "bold", angle = 90) +
    
    # Title
    geom_text(x = 6, y = 1.5, label = "PRISMA Flow Diagram", size = 5, fontface = "bold")
  
  return(p)
}

# Create and save the diagram
p <- create_simple_prisma()

# Save with multiple approaches to ensure it works
ggsave("20250709_Analysis & Results/simple_prisma_diagram.png", p, 
       width = 12, height = 10, dpi = 300, bg = "white")

# Also save as PDF and JPEG
ggsave("20250709_Analysis & Results/simple_prisma_diagram.pdf", p, 
       width = 12, height = 10, bg = "white")

ggsave("20250709_Analysis & Results/simple_prisma_diagram.jpg", p, 
       width = 12, height = 10, dpi = 300, bg = "white")

cat("Simple PRISMA diagram created successfully!\n")
cat("Files saved:\n")
cat("- simple_prisma_diagram.png\n")
cat("- simple_prisma_diagram.pdf\n")
cat("- simple_prisma_diagram.jpg\n")

# Display the plot
print(p)
