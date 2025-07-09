# Professional PRISMA Flow Diagram using ggplot2
# This creates a clean, publication-ready PRISMA diagram

library(ggplot2)
library(grid)
library(gridExtra)

# Create a custom PRISMA diagram function
create_prisma_box <- function(text, x, y, width, height, fill_color = "lightblue", text_size = 10) {
  list(
    geom_rect(xmin = x - width/2, xmax = x + width/2, 
              ymin = y - height/2, ymax = y + height/2, 
              fill = fill_color, color = "black", linewidth = 0.8),
    geom_text(x = x, y = y, label = text, size = text_size, hjust = 0.5, vjust = 0.5)
  )
}

create_prisma_arrow <- function(x1, y1, x2, y2) {
  geom_segment(x = x1, y = y1, xend = x2, yend = y2, 
               arrow = arrow(length = unit(0.3, "cm")), 
               linewidth = 1.2, color = "black")
}

# Set up the plot
p <- ggplot() +
  xlim(0, 10) + 
  ylim(0, 12) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Add boxes
p <- p + 
  # Identification
  create_prisma_box("Records identified through\ndatabase searching\n(n = 1,674)", 
                    x = 5, y = 11, width = 3, height = 1.2, fill_color = "lightcyan") +
  
  # Title screening
  create_prisma_box("Records screened\nby title\n(n = 1,674)", 
                    x = 5, y = 9, width = 3, height = 1.2, fill_color = "lightgreen") +
  
  create_prisma_box("Records excluded\nby title\n(n = 1,357)\n\n• Not crime location choice\n• No spatial choice models\n• Review/theoretical papers", 
                    x = 8.5, y = 9, width = 3, height = 2, fill_color = "lightcoral") +
  
  # Abstract screening
  create_prisma_box("Records screened\nby abstract\n(n = 317)", 
                    x = 5, y = 7, width = 3, height = 1.2, fill_color = "lightgreen") +
  
  create_prisma_box("Records excluded\nby abstract\n(n = 268)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", 
                    x = 8.5, y = 7, width = 3, height = 2, fill_color = "lightcoral") +
  
  # Full-text screening
  create_prisma_box("Full-text articles\nassessed for eligibility\n(n = 49)", 
                    x = 5, y = 5, width = 3, height = 1.2, fill_color = "lightgreen") +
  
  create_prisma_box("Full-text articles excluded (n = 38)\nArticles not available (n = 4)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", 
                    x = 8.5, y = 5, width = 3, height = 2, fill_color = "lightcoral") +
  
  # Final inclusion
  create_prisma_box("Studies included in\nquantitative synthesis\n(n = 49)", 
                    x = 5, y = 3, width = 3, height = 1.2, fill_color = "lightsteelblue") +
  
  # Add arrows
  create_prisma_arrow(5, 10.4, 5, 9.6) +  # identification to title
  create_prisma_arrow(5, 8.4, 5, 7.6) +   # title to abstract
  create_prisma_arrow(5, 6.4, 5, 5.6) +   # abstract to fulltext
  create_prisma_arrow(5, 4.4, 5, 3.6) +   # fulltext to final
  
  # Exclusion arrows
  create_prisma_arrow(6.5, 9, 7, 9) +     # title exclusion
  create_prisma_arrow(6.5, 7, 7, 7) +     # abstract exclusion
  create_prisma_arrow(6.5, 5, 7, 5) +     # fulltext exclusion
  
  # Add title
  geom_text(x = 5, y = 0.5, label = "PRISMA Flow Diagram", size = 14, fontface = "bold") +
  
  # Add stage labels
  geom_text(x = 1, y = 11, label = "IDENTIFICATION", size = 12, fontface = "bold", angle = 90) +
  geom_text(x = 1, y = 8, label = "SCREENING", size = 12, fontface = "bold", angle = 90) +
  geom_text(x = 1, y = 5, label = "ELIGIBILITY", size = 12, fontface = "bold", angle = 90) +
  geom_text(x = 1, y = 3, label = "INCLUDED", size = 12, fontface = "bold", angle = 90)

# Save the diagram
ggsave("20250709_Analysis & Results/professional_prisma_diagram.png", p, 
       width = 12, height = 14, dpi = 300, bg = "white")

ggsave("20250709_Analysis & Results/professional_prisma_diagram.pdf", p, 
       width = 12, height = 14, bg = "white")

print("Professional PRISMA diagram created!")
print("Files saved:")
print("- professional_prisma_diagram.png")
print("- professional_prisma_diagram.pdf")

# Display the plot
print(p)
