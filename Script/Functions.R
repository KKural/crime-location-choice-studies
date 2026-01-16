# Call library for required fonts----- 
library(extrafont)
# Load fonts into R
loadfonts(device = "win") 

# Function: create sub folder ---------------------------------------------------
# Function to create a folder with a date argument---------
make_folder <- function(date = Sys.Date()) {
  # Convert the provided date to "YYYYMMDD" format
  folder_name <- format(as.Date(date), "%Y%m%d")
  
  # Define the full folder name with additional text
  full_folder_name <- paste0(folder_name, "_output_SUoA")
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(here::here(full_folder_name))) {
    dir.create(here::here(full_folder_name))
    message("Folder created: ", full_folder_name)
  } else {
    message("Folder already exists: ", full_folder_name)
  }
  
  return(full_folder_name)  # Return the folder name to use later
}

# Create the folder
folder_name <- make_folder()

# custom theme for ggplot 
custom_theme <- function() {
  ggplot2::theme(
    legend.position = "none",  # Remove legend
    plot.background = ggplot2::element_rect(fill = "white", color = NA),  # White background
    text = ggplot2::element_text(family = "Times New Roman", size = 12),  # Times New Roman font for all text
    axis.title = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),  # Bold axis titles
    axis.text = ggplot2::element_text(size = 12, family = "Times New Roman"),  # Consistent axis text font size
    axis.line = ggplot2::element_line(color = "black"),  # Black lines for x and y axes
    plot.title = ggplot2::element_text(family = "Times New Roman", size = 12, face = "bold", hjust = 0.5)  # Bold, centered title
  )
}

