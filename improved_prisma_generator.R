# Improved PRISMA Flow Diagram Generator
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
total_fulltext <- after_abstract

# Create improved PRISMA flow diagram
improved_prisma <- DiagrammeR::grViz("
digraph PRISMA {
  graph [layout = dot, rankdir = TB, splines = ortho, nodesep = 0.8, ranksep = 1.2]
  
  # Node styles
  node [shape = box, style = 'filled,rounded', fontname = 'Arial', fontsize = 12, margin = 0.2]
  
  # Identification stage
  identification [label = 'Records identified through\\ndatabase searching\\n(n = 1,674)', 
                  fillcolor = 'lightblue', width = 2.5, height = 1]
  
  # Screening stage - Title
  title_screen [label = 'Records screened\\nby title\\n(n = 1,674)', 
                fillcolor = 'lightgreen', width = 2.5, height = 1]
  
  title_exclude [label = 'Records excluded\\nby title\\n(n = 1,357)\\n\\n• Not crime location choice\\n• No spatial choice models\\n• Review/theoretical papers', 
                 fillcolor = 'lightcoral', width = 3, height = 1.5]
  
  # Screening stage - Abstract
  abstract_screen [label = 'Records screened\\nby abstract\\n(n = 317)', 
                   fillcolor = 'lightgreen', width = 2.5, height = 1]
  
  abstract_exclude [label = 'Records excluded\\nby abstract\\n(n = 268)\\n\\n• Insufficient spatial detail\\n• Offender residence studies\\n• No discrete choice models', 
                    fillcolor = 'lightcoral', width = 3, height = 1.5]
  
  # Eligibility stage
  fulltext_screen [label = 'Full-text articles\\nassessed for eligibility\\n(n = 49)', 
                   fillcolor = 'lightgreen', width = 2.5, height = 1]
  
  fulltext_exclude [label = 'Full-text articles\\nexcluded\\n(n = 38)\\n\\n• Insufficient spatial detail\\n• Offender residence studies\\n• No discrete choice models', 
                    fillcolor = 'lightcoral', width = 3, height = 1.5]
  
  not_available [label = 'Articles not\\navailable\\n(n = 4)', 
                 fillcolor = 'lightyellow', width = 2, height = 1]
  
  # Inclusion stage
  final_studies [label = 'Studies included in\\nquantitative synthesis\\n(n = 49)', 
                 fillcolor = 'lightsteelblue', width = 2.5, height = 1]
  
  # Connections - main flow
  identification -> title_screen
  title_screen -> abstract_screen
  abstract_screen -> fulltext_screen
  fulltext_screen -> final_studies
  
  # Exclusion connections
  title_screen -> title_exclude
  abstract_screen -> abstract_exclude
  fulltext_screen -> fulltext_exclude
  fulltext_screen -> not_available
  
  # Ranking for alignment
  {rank = same; title_screen; title_exclude}
  {rank = same; abstract_screen; abstract_exclude}
  {rank = same; fulltext_screen; fulltext_exclude; not_available}
  
  # Edge styling
  edge [color = black, arrowsize = 0.8, penwidth = 2]
}
")

# Display the diagram
print(improved_prisma)

# Save as SVG
diagram_svg <- DiagrammeRsvg::export_svg(improved_prisma)
writeLines(diagram_svg, "20250709_Analysis & Results/improved_prisma_flow_diagram.svg")

# Convert to PNG with higher resolution
rsvg::rsvg_png("20250709_Analysis & Results/improved_prisma_flow_diagram.svg", 
               "20250709_Analysis & Results/improved_prisma_flow_diagram.png", 
               width = 1200, height = 1600)

# Create a second version with PRISMA-style layout
prisma_standard <- DiagrammeR::grViz("
digraph PRISMA_Standard {
  graph [layout = dot, rankdir = TB, bgcolor = white, splines = ortho]
  
  node [shape = box, style = 'filled,rounded', fontname = 'Arial', fontsize = 11, margin = 0.15]
  
  # IDENTIFICATION
  subgraph cluster_identification {
    label = 'IDENTIFICATION'
    fontsize = 14
    fontname = 'Arial Bold'
    style = 'filled,rounded'
    fillcolor = 'lightgray'
    
    records_id [label = 'Records identified through\\ndatabase searching\\n(n = 1,674)', 
                fillcolor = 'white', width = 2.8]
  }
  
  # SCREENING
  subgraph cluster_screening {
    label = 'SCREENING'
    fontsize = 14
    fontname = 'Arial Bold'
    style = 'filled,rounded'
    fillcolor = 'lightgray'
    
    title_screen [label = 'Records screened by title\\n(n = 1,674)', 
                  fillcolor = 'white', width = 2.8]
    
    abstract_screen [label = 'Records screened by abstract\\n(n = 317)', 
                     fillcolor = 'white', width = 2.8]
  }
  
  # ELIGIBILITY
  subgraph cluster_eligibility {
    label = 'ELIGIBILITY'
    fontsize = 14
    fontname = 'Arial Bold'
    style = 'filled,rounded'
    fillcolor = 'lightgray'
    
    fulltext_assess [label = 'Full-text articles assessed\\nfor eligibility\\n(n = 49)', 
                     fillcolor = 'white', width = 2.8]
  }
  
  # INCLUDED
  subgraph cluster_included {
    label = 'INCLUDED'
    fontsize = 14
    fontname = 'Arial Bold'
    style = 'filled,rounded'
    fillcolor = 'lightgray'
    
    final_included [label = 'Studies included in\\nquantitative synthesis\\n(n = 49)', 
                    fillcolor = 'white', width = 2.8]
  }
  
  # Exclusion boxes
  title_excl [label = 'Records excluded by title\\n(n = 1,357)\\n\\n• Not crime location choice\\n• No spatial choice models\\n• Review/theoretical papers', 
              fillcolor = 'mistyrose', width = 3.2, height = 1.2]
  
  abstract_excl [label = 'Records excluded by abstract\\n(n = 268)\\n\\n• Insufficient spatial detail\\n• Offender residence studies\\n• No discrete choice models', 
                 fillcolor = 'mistyrose', width = 3.2, height = 1.2]
  
  fulltext_excl [label = 'Full-text articles excluded (n = 38)\\nArticles not available (n = 4)\\n\\n• Insufficient spatial detail\\n• Offender residence studies\\n• No discrete choice models', 
                 fillcolor = 'mistyrose', width = 3.2, height = 1.2]
  
  # Main flow
  records_id -> title_screen
  title_screen -> abstract_screen
  abstract_screen -> fulltext_assess
  fulltext_assess -> final_included
  
  # Exclusions
  title_screen -> title_excl
  abstract_screen -> abstract_excl
  fulltext_assess -> fulltext_excl
  
  # Alignment
  {rank = same; title_screen; title_excl}
  {rank = same; abstract_screen; abstract_excl}
  {rank = same; fulltext_assess; fulltext_excl}
  
  edge [color = black, arrowsize = 0.6, penwidth = 1.5]
}
")

# Display the standard version
print(prisma_standard)

# Save the standard version
diagram_svg_standard <- DiagrammeRsvg::export_svg(prisma_standard)
writeLines(diagram_svg_standard, "20250709_Analysis & Results/prisma_standard_flow_diagram.svg")

# Convert to PNG
rsvg::rsvg_png("20250709_Analysis & Results/prisma_standard_flow_diagram.svg", 
               "20250709_Analysis & Results/prisma_standard_flow_diagram.png", 
               width = 1200, height = 1600)

cat("Improved PRISMA diagrams created!\n")
cat("Files saved:\n")
cat("- improved_prisma_flow_diagram.png\n")
cat("- prisma_standard_flow_diagram.png\n")
cat("- Both SVG versions also available\n")
