# Check supporting quotes in the enhanced dataset
library(dplyr)
library(readr)

# Read the enhanced dataset
data <- read_csv('20250710_Analysis & Results/20250710_standardized_unit_sizes_with_all_evidence.csv', show_col_types = FALSE)

# Check which supporting quote columns have data
quote_cols <- names(data)[grepl('Supporting quotes', names(data))]
cat('Total supporting quote columns:', length(quote_cols), '\n\n')

# Check how many studies have non-empty quotes for each column
for (col in quote_cols) {
  non_empty <- sum(!is.na(data[[col]]) & data[[col]] != '' & data[[col]] != '-', na.rm = TRUE)
  cat(col, ':', non_empty, 'studies with quotes\n')
}

# Check specific important columns
cat('\n--- KEY COLUMNS ANALYSIS ---\n')
spatial_col <- 'Supporting quotes for "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION"'
if (spatial_col %in% names(data)) {
  spatial_quotes <- data[[spatial_col]]
  non_empty_spatial <- sum(!is.na(spatial_quotes) & spatial_quotes != '' & spatial_quotes != '-', na.rm = TRUE)
  cat('Spatial Units quotes:', non_empty_spatial, 'out of', nrow(data), 'studies\n')
  
  # Show first few non-empty examples
  non_empty_examples <- data[!is.na(spatial_quotes) & spatial_quotes != '' & spatial_quotes != '-', c('Citation', spatial_col)]
  if (nrow(non_empty_examples) > 0) {
    cat('First example:\n')
    cat('Study:', non_empty_examples$Citation[1], '\n')
    cat('Quote:', substr(non_empty_examples[[spatial_col]][1], 1, 200), '...\n')
  }
  
  # Show which studies have empty spatial quotes
  empty_examples <- data[is.na(spatial_quotes) | spatial_quotes == '' | spatial_quotes == '-', c('Citation', 'Evidence_Match_Type')]
  if (nrow(empty_examples) > 0) {
    cat('\nStudies with EMPTY spatial quotes:\n')
    for (i in 1:min(5, nrow(empty_examples))) {
      cat('-', empty_examples$Citation[i], '(Match type:', empty_examples$Evidence_Match_Type[i], ')\n')
    }
  }
}

# Check if there are any studies that didn't get matched at all
unmatched <- data[is.na(data$Evidence_Match_Type), ]
if (nrow(unmatched) > 0) {
  cat('\nStudies that did NOT match at all:\n')
  for (i in 1:nrow(unmatched)) {
    cat('-', unmatched$Citation[i], '\n')
  }
} else {
  cat('\nAll studies were successfully matched!\n')
}

cat('\nProcessing complete!\n')
