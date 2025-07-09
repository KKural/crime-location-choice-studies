# Basic PRISMA diagram using base R graphics
# This should definitely work

# Create a basic PRISMA diagram
png("20250709_Analysis & Results/basic_prisma_diagram.png", 
    width = 1200, height = 1400, res = 150)

# Set up the plot
par(mar = c(1, 1, 1, 1))
plot(0, 0, type = "n", xlim = c(0, 12), ylim = c(0, 14), 
     axes = FALSE, xlab = "", ylab = "")

# Function to draw boxes
draw_box <- function(x, y, width, height, text, color = "lightblue") {
  rect(x - width/2, y - height/2, x + width/2, y + height/2, 
       col = color, border = "black", lwd = 2)
  text(x, y, text, cex = 1.2, adj = 0.5)
}

# Draw boxes
draw_box(6, 12, 4, 1.5, "Records identified through\ndatabase searching\n(n = 1,674)", "lightcyan")
draw_box(6, 10, 4, 1.5, "Records screened\nby title\n(n = 1,674)", "lightgreen")
draw_box(6, 8, 4, 1.5, "Records screened\nby abstract\n(n = 317)", "lightgreen")
draw_box(6, 6, 4, 1.5, "Full-text articles\nassessed for eligibility\n(n = 49)", "lightgreen")
draw_box(6, 4, 4, 1.5, "Studies included in\nquantitative synthesis\n(n = 49)", "lightsteelblue")

# Exclusion boxes
draw_box(10, 10, 3, 2, "Records excluded\nby title\n(n = 1,357)\n\n• Not crime location choice\n• No spatial choice models\n• Review/theoretical papers", "lightcoral")
draw_box(10, 8, 3, 2, "Records excluded\nby abstract\n(n = 268)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", "lightcoral")
draw_box(10, 6, 3, 2, "Full-text articles excluded\n(n = 38)\n\nArticles not available\n(n = 4)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", "lightcoral")

# Arrows
arrows(6, 11.2, 6, 10.8, lwd = 3, length = 0.15)
arrows(6, 9.2, 6, 8.8, lwd = 3, length = 0.15)
arrows(6, 7.2, 6, 6.8, lwd = 3, length = 0.15)
arrows(6, 5.2, 6, 4.8, lwd = 3, length = 0.15)

# Exclusion arrows
arrows(8, 10, 8.5, 10, lwd = 2, length = 0.15)
arrows(8, 8, 8.5, 8, lwd = 2, length = 0.15)
arrows(8, 6, 8.5, 6, lwd = 2, length = 0.15)

# Stage labels
text(1.5, 12, "IDENTIFICATION", cex = 1.5, font = 2, srt = 90)
text(1.5, 9, "SCREENING", cex = 1.5, font = 2, srt = 90)
text(1.5, 6, "ELIGIBILITY", cex = 1.5, font = 2, srt = 90)
text(1.5, 4, "INCLUDED", cex = 1.5, font = 2, srt = 90)

# Title
text(6, 2, "PRISMA Flow Diagram", cex = 2, font = 2)

dev.off()

cat("Basic PRISMA diagram created successfully!\n")
cat("File saved: basic_prisma_diagram.png\n")

# Also create a PDF version
pdf("20250709_Analysis & Results/basic_prisma_diagram.pdf", 
    width = 12, height = 14)

# Set up the plot
par(mar = c(1, 1, 1, 1))
plot(0, 0, type = "n", xlim = c(0, 12), ylim = c(0, 14), 
     axes = FALSE, xlab = "", ylab = "")

# Draw the same diagram
draw_box(6, 12, 4, 1.5, "Records identified through\ndatabase searching\n(n = 1,674)", "lightcyan")
draw_box(6, 10, 4, 1.5, "Records screened\nby title\n(n = 1,674)", "lightgreen")
draw_box(6, 8, 4, 1.5, "Records screened\nby abstract\n(n = 317)", "lightgreen")
draw_box(6, 6, 4, 1.5, "Full-text articles\nassessed for eligibility\n(n = 49)", "lightgreen")
draw_box(6, 4, 4, 1.5, "Studies included in\nquantitative synthesis\n(n = 49)", "lightsteelblue")

# Exclusion boxes
draw_box(10, 10, 3, 2, "Records excluded\nby title\n(n = 1,357)\n\n• Not crime location choice\n• No spatial choice models\n• Review/theoretical papers", "lightcoral")
draw_box(10, 8, 3, 2, "Records excluded\nby abstract\n(n = 268)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", "lightcoral")
draw_box(10, 6, 3, 2, "Full-text articles excluded\n(n = 38)\n\nArticles not available\n(n = 4)\n\n• Insufficient spatial detail\n• Offender residence studies\n• No discrete choice models", "lightcoral")

# Arrows
arrows(6, 11.2, 6, 10.8, lwd = 3, length = 0.15)
arrows(6, 9.2, 6, 8.8, lwd = 3, length = 0.15)
arrows(6, 7.2, 6, 6.8, lwd = 3, length = 0.15)
arrows(6, 5.2, 6, 4.8, lwd = 3, length = 0.15)

# Exclusion arrows
arrows(8, 10, 8.5, 10, lwd = 2, length = 0.15)
arrows(8, 8, 8.5, 8, lwd = 2, length = 0.15)
arrows(8, 6, 8.5, 6, lwd = 2, length = 0.15)

# Stage labels
text(1.5, 12, "IDENTIFICATION", cex = 1.5, font = 2, srt = 90)
text(1.5, 9, "SCREENING", cex = 1.5, font = 2, srt = 90)
text(1.5, 6, "ELIGIBILITY", cex = 1.5, font = 2, srt = 90)
text(1.5, 4, "INCLUDED", cex = 1.5, font = 2, srt = 90)

# Title
text(6, 2, "PRISMA Flow Diagram", cex = 2, font = 2)

dev.off()

cat("Basic PRISMA diagram PDF created successfully!\n")
cat("File saved: basic_prisma_diagram.pdf\n")
