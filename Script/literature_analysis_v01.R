# These are the required libraries
library(tidyverse)
library(forcats)
library(here)
library(csvread)


# This function makes it easy to print a plot to a PNG file
# e.g. ggsave_png(ggp=PLOT, output = here("Output") 
#      saves PLOT to "./Output/PLOT.png"
ggsave_png <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".png", sep=""), 
         device = "png", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

# Create the "Output"folder if it does not yet exist
if (dir.exists(here("Output")) == FALSE) {
  dir.create(here("Output"))
} 


# Define location and name of the data file
file <-
  here("data", "Reworked sheet.csv")

# Read the data file (each row is a study)
prior_studies <- 
  read_delim(file, delim = ";")
  mutate(location_size = 
           case_when(size_unit=="km" ~ location_size,
                     size_unit=="m"  ~ location_size / 10^6,
                     TRUE            ~ as.numeric(NA)
           )
  )
view(prior_studies)

# frequency table of location unit labels
label_freq_table <-
prior_studies |> 
  group_by(unit_label) |>
  summarize(frequency = n()) |>
  mutate(unit_label = 
           fct_reorder(unit_label, frequency)
  ) 

# frequency bar of location unit labels
label_freq_bar <-
  label_freq_table  |>
  ggplot() +
  geom_col(aes(x=frequency, y = unit_label), 
           color= "black", fill= "lightblue") +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ylab("label given to spatial unit")
label_freq_bar

# Save as PNG file
ggsave_png(ggp=label_freq_bar,
           output = here("Output"),
           scale=3)

# Boxplot surface size unit of analysis
size_boxplot <- 
  prior_studies |> 
  ggplot() +
    geom_boxplot(aes(x=location_size),
                 color="black", fill="yellow") +
    scale_x_continuous(breaks = seq(0,10,1)) +
    xlab("size of spatial unit in km2") 
size_boxplot

# Save as PNG file
ggsave_png(ggp=size_boxplot,
           output = here("Output"),
           scale=2)


prior_studies |> 
  group_by(location_size) |>
  summarize(count = n()) |>
  mutate(constant_y = 1) |>
  ggplot() +
  geom_point(aes(x=location_size, 
                 y=constant_y, 
                 size= count,
                 color = count), alpha = .3) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = NULL, limits=c(1,1)) +
  ylab("") +
  xlab("size of spatial unit")
  
             
             
             
             
             
             
             
             
             
             
             
             


