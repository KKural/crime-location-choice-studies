# Regenerate temporal analysis figure with country differentiation
library(tidyverse)
library(lme4)

# Load data
data <- read_csv("20250710_Analysis & Results/20250710_standardized_unit_sizes_with_groups_merged.csv", 
                 show_col_types = FALSE)

# Prepare temporal data
temporal_data <- data %>%
  filter(!is.na(as.numeric(Year)) & as.numeric(Year) >= 2003) %>%
  mutate(
    Publication_Year = as.numeric(Year),
    Log_Unit_size = log10(Unit_size_km2),
    Jurisdiction = case_when(
      !is.na(Country) & Country != "" ~ Country,
      TRUE ~ "Other"
    ),
    Country_Simple = case_when(
      Jurisdiction %in% c("Netherlands", "United States", "China", "United Kingdom", "Belgium") ~ Jurisdiction,
      TRUE ~ "Other"
    )
  )

# Create the plot
p2_temporal <- ggplot(temporal_data, aes(x = Publication_Year, y = Log_Unit_size)) +
  geom_point(aes(shape = Country_Simple), color = "grey20", fill = "white", 
             alpha = 0.9, size = 3.5, stroke = 1.5) +
  geom_smooth(method = "lm", color = "#2D2D2D", fill = "grey80", alpha = 0.4, linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#4A4A4A", linetype = "dashed", se = FALSE, linewidth = 1) +
  scale_shape_manual(
    values = c("Netherlands" = 21, "United States" = 22, "China" = 23, 
               "United Kingdom" = 24, "Belgium" = 25, "Other" = 21),
    name = "Country"
  ) +
  labs(
    x = "Publication Year",
    y = "Log10(Unit Size in km²)",
    caption = "Solid line: linear trend with 95% CI | Dashed line: LOESS smoothing"
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(face = "bold"))

# Save the plot
ggsave("20250710_Analysis & Results/temporal_analysis.png", p2_temporal,
       width = 10, height = 7, dpi = 300, bg = "white")

print("Temporal analysis figure regenerated successfully")
