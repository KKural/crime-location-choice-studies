# ──────────────────────────────────────────────────────────────
#  INVENTORY: objects, tables, columns  ←  ADD THIS CHUNK
# ──────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)

## 1. Objects currently in the script’s environment -------------
env_to_scan <- environment()   #  <- use .GlobalEnv if you ran interactively

object_inventory <- tibble(
  object_name  = ls(envir = env_to_scan, all.names = TRUE),
  object_class = map_chr(object_name, ~ class(get(.x, envir = env_to_scan))[1]),
  object_dim   = map_chr(object_name, function(.x) {
    obj <- get(.x, envir = env_to_scan)
    if (is.data.frame(obj)) {
      paste0(nrow(obj), " × ", ncol(obj))
    } else if (!is.null(dim(obj))) {
      paste(dim(obj), collapse = " × ")
    } else {
      length(obj)
    }
  }),
  columns      = map_chr(object_name, function(.x) {
    obj <- get(.x, envir = env_to_scan)
    if (is.data.frame(obj)) paste(names(obj), collapse = ", ") else ""
  })
)

print(object_inventory, n = 50, width = Inf)
custom_save(object_inventory,
            output_folder,
            "00_object_inventory",
            readr::write_csv)

## 2. Tables inside the Excel results file ----------------------
excel_path <- file.path(
  output_folder,
  paste0(format(Sys.Date(), "%Y%m%d"), "_Comprehensive_Manuscript_Tables.xlsx")
)

if (file.exists(excel_path)) {
  sheet_names <- excel_sheets(excel_path)

  table_inventory <- map_dfr(sheet_names, function(sh) {
    cols <- names(read_excel(excel_path,
                             sheet      = sh,
                             n_max      = 0,      # read header only
                             .name_repair = "minimal"))
    tibble(table_name = sh,
           n_columns  = length(cols),
           column_names = paste(cols, collapse = ", "))
  })

  print(table_inventory, n = Inf, width = Inf)
  custom_save(table_inventory,
              output_folder,
              "00_table_inventory",
              readr::write_csv)
} else {
  warning("Excel workbook not found – table inventory skipped.")
}
# ──────────────────────────────────────────────────────────────
