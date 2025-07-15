# Regenerate Cross-National Variation Figure
# Simple script to generate the rq3a_country_comparison_new.png figure

# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(here)

# Set working directory
setwd("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")

# Load the available data file
data_raw <- read_csv("20250704_standardized_unit_sizes_with_groups.csv", show_col_types = FALSE)

# Process data with unit conversion and derived variables
data <- data_raw |>
  mutate(
    # Convert all areas to square meters
    standardized_area_m2 = case_when(
      standardized_unit %in% c("m", "meters", "meter") ~ standardized_area,
      standardized_unit %in% c("km", "kilometers", "kilometer") ~ standardized_area * 1000000,
      standardized_unit %in% c("m2", "sq m", "square meters") ~ standardized_area,
      standardized_unit %in% c("km2", "sq km", "square kilometers") ~ standardized_area * 1000000,
      standardized_unit %in% c("ha", "hectares", "hectare") ~ standardized_area * 10000,
      TRUE ~ standardized_area
    ),
    # Log transformation for better visualization
    log_area = log10(standardized_area_m2),
    # Create area categories
    area_category = case_when(
      standardized_area_m2 < 1000 ~ "< 1,000 m²",
      standardized_area_m2 < 10000 ~ "1,000 - 10,000 m²", 
      standardized_area_m2 < 100000 ~ "10,000 - 100,000 m²",
      standardized_area_m2 < 1000000 ~ "100,000 m² - 1 km²",
      TRUE ~ "> 1 km²"
    )
  ) |>
  filter(!is.na(standardized_area_m2), standardized_area_m2 > 0)

# Create country comparison figure (RQ3a)
country_data <- data |>
  filter(!is.na(country)) |>
  group_by(country) |>
  summarise(
    study_count = n(),
    median_area = median(standardized_area_m2, na.rm = TRUE),
    q25_area = quantile(standardized_area_m2, 0.25, na.rm = TRUE),
    q75_area = quantile(standardized_area_m2, 0.75, na.rm = TRUE),
    min_area = min(standardized_area_m2, na.rm = TRUE),
    max_area = max(standardized_area_m2, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  arrange(desc(study_count)) |>
  slice_head(n = 15)  # Top 15 countries by study count

# Create the plot
p3a <- ggplot(country_data, aes(x = reorder(country, study_count))) +
  geom_segment(aes(xend = country, y = min_area, yend = max_area), 
               color = "gray60", size = 0.5) +
  geom_segment(aes(xend = country, y = q25_area, yend = q75_area), 
               color = "steelblue", size = 2, alpha = 0.8) +
  geom_point(aes(y = median_area), color = "darkred", size = 2) +
  geom_text(aes(y = max_area, label = paste0("n=", study_count)), 
            hjust = -0.2, size = 3, color = "black") +
  scale_y_log10(
    name = "Spatial Unit Area (m²)",
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = 10^(0:8)
  ) +
  scale_x_discrete(name = "Country") +
  coord_flip() +
  labs(
    title = "Cross-National Variation in Spatial Unit Sizes",
    subtitle = "Distribution of spatial unit areas by country (top 15 by study count)",
    caption = "Points show median values; thick lines show IQR; thin lines show full range"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, color = "gray60")
  )

# Ensure the Figures directory exists
if (!dir.exists("Figures")) {
  dir.create("Figures", recursive = TRUE)
}

# Save the figure
ggsave("Figures/20250714_rq3a_country_comparison_new.png", 
       plot = p3a, 
       width = 10, 
       height = 8, 
       dpi = 300,
       bg = "white")

cat("Cross-national variation figure regenerated successfully!\n")
cat("Saved to: Figures/20250714_rq3a_country_comparison_new.png\n")
