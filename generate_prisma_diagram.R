# PRISMA Flow Diagram Generator
# This script creates a publication-quality PRISMA flow diagram

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Load the screening numbers
screening_numbers <- read.csv("20250709_Analysis & Results/prisma_flow_numbers.csv")

# Extract numbers for the flow diagram
total_retrieved <- screening_numbers$Count[screening_numbers$Stage == "Records retrieved"]
excluded_title <- screening_numbers$Count[screening_numbers$Stage == "Excluded by title"]
excluded_abstract <- screening_numbers$Count[screening_numbers$Stage == "Excluded by abstract"]
excluded_fulltext <- screening_numbers$Count[screening_numbers$Stage == "Excluded by full-text"]
not_available <- screening_numbers$Count[screening_numbers$Stage == "Not available for download"]
final_included <- screening_numbers$Count[screening_numbers$Stage == "Final included studies"]

# Calculate intermediate numbers
after_title <- total_retrieved - excluded_title
after_abstract <- after_title - excluded_abstract
after_fulltext <- after_abstract - excluded_fulltext - not_available

# Create PRISMA flow diagram
prisma_diagram <- DiagrammeR::grViz("
digraph PRISMA {
  graph [layout = dot, rankdir = TB, splines = false]
  
  node [shape = box, style = filled, fillcolor = lightblue, fontname = Arial, fontsize = 11]
  
  # Identification
  records_retrieved [label = 'Records retrieved from databases\\n(n = 1,674)', fillcolor = lightcyan]
  
  # Screening - Title
  title_screening [label = 'Records screened by title\\n(n = 1,674)', fillcolor = lightgreen]
  title_excluded [label = 'Records excluded by title\\n(n = 1,357)', fillcolor = lightcoral]
  
  # Screening - Abstract  
  abstract_screening [label = 'Records screened by abstract\\n(n = 317)', fillcolor = lightgreen]
  abstract_excluded [label = 'Records excluded by abstract\\n(n = 268)', fillcolor = lightcoral]
  
  # Screening - Full-text
  fulltext_screening [label = 'Full-text articles assessed\\nfor eligibility\\n(n = 49)', fillcolor = lightgreen]
  fulltext_excluded [label = 'Full-text articles excluded\\n(n = 38)', fillcolor = lightcoral]
  not_available [label = 'Articles not available\\n(n = 4)', fillcolor = lightyellow]
  
  # Final inclusion
  final_included [label = 'Studies included in\\nquantitative synthesis\\n(n = 49)', fillcolor = lightsteelblue]
  
  # Connections
  records_retrieved -> title_screening
  title_screening -> title_excluded
  title_screening -> abstract_screening
  abstract_screening -> abstract_excluded
  abstract_screening -> fulltext_screening
  fulltext_screening -> fulltext_excluded
  fulltext_screening -> not_available
  fulltext_screening -> final_included
  
  # Invisible edges for alignment
  {rank = same; title_screening; title_excluded}
  {rank = same; abstract_screening; abstract_excluded}
  {rank = same; fulltext_screening; fulltext_excluded; not_available}
}
")

# Display the diagram
print(prisma_diagram)

# Save as SVG
diagram_svg <- DiagrammeRsvg::export_svg(prisma_diagram)
writeLines(diagram_svg, "20250709_Analysis & Results/prisma_flow_diagram.svg")

# Convert to PNG
rsvg::rsvg_png("20250709_Analysis & Results/prisma_flow_diagram.svg", 
               "20250709_Analysis & Results/prisma_flow_diagram.png", 
               width = 800, height = 1000)

# Create a text-based version for the manuscript
cat("PRISMA Flow Diagram (Text Version):\n")
cat("=====================================\n")
cat("Records retrieved from databases: n =", total_retrieved, "\n")
cat("                     ↓\n")
cat("Records screened by title: n =", total_retrieved, "\n")
cat("                     ↓\n")
cat("Records excluded by title: n =", excluded_title, "\n")
cat("                     ↓\n")
cat("Records screened by abstract: n =", after_title, "\n")
cat("                     ↓\n")
cat("Records excluded by abstract: n =", excluded_abstract, "\n")
cat("                     ↓\n")
cat("Full-text articles assessed: n =", after_abstract, "\n")
cat("                     ↓\n")
cat("Records excluded by full-text: n =", excluded_fulltext, "\n")
cat("Articles not available: n =", not_available, "\n")
cat("                     ↓\n")
cat("Studies included in synthesis: n =", final_included, "\n")

# Create summary for manuscript
cat("\n\nFor manuscript:\n")
cat("Initial search yielded", total_retrieved, "records.")
cat("After title screening,", after_title, "records remained.")
cat("Abstract screening further reduced this to", after_abstract, "records.")
cat("Following full-text assessment,", final_included, "studies met inclusion criteria.")
cat("The inclusion rate was", round(final_included/total_retrieved*100, 1), "%.")

cat("\n\nPRISMA diagram saved as PNG and SVG files.\n")
