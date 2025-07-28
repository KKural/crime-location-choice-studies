# Simple BibTeX Parser for Appendix References
# This script reads and parses the export.bib file to format references in Nature Human Behaviour style

# Load required libraries (using only base R and common packages)
library(dplyr)
library(stringr)

#' Parse BibTeX file and return formatted references
#' @param bib_file Path to the BibTeX file
#' @return List with parsed entries and formatted references
parse_bibtex_file <- function(bib_file) {
  
  # Read the file
  bib_content <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
  
  # Find entry boundaries
  entry_starts <- grep("^@", bib_content)
  
  if (length(entry_starts) == 0) {
    return(list(raw_data = list(), formatted = character()))
  }
  
  entry_ends <- c(entry_starts[-1] - 1, length(bib_content))
  
  entries <- list()
  formatted_refs <- character()
  
  for (i in 1:length(entry_starts)) {
    start_line <- entry_starts[i]
    end_line <- entry_ends[i]
    
    # Find the actual end of the entry (look for closing brace)
    entry_content <- bib_content[start_line:end_line]
    
    # Find where the entry actually ends by counting braces
    brace_count <- 0
    actual_end <- 1
    
    for (j in 1:length(entry_content)) {
      line <- entry_content[j]
      brace_count <- brace_count + str_count(line, "\\{") - str_count(line, "\\}")
      
      if (j > 1 && brace_count <= 0) {
        actual_end <- j
        break
      }
    }
    
    # Extract entry content up to actual end
    entry_content <- entry_content[1:actual_end]
    
    # Parse entry
    parsed_entry <- parse_single_entry(entry_content)
    entries[[i]] <- parsed_entry
    
    # Format reference
    formatted_refs[i] <- format_single_reference_nhb(parsed_entry)
  }
  
  return(list(
    raw_data = entries,
    formatted = formatted_refs
  ))
}

#' Parse a single BibTeX entry
#' @param entry_lines Character vector of lines for one entry
#' @return List with entry fields
parse_single_entry <- function(entry_lines) {
  
  # Initialize entry
  entry <- list(
    type = "",
    key = "",
    author = "",
    year = "",
    title = "",
    journal = "",
    volume = "",
    pages = "",
    doi = "",
    url = ""
  )
  
  # Extract entry type and key from first line
  first_line <- entry_lines[1]
  if (grepl("^@", first_line)) {
    # Extract type
    type_match <- str_extract(first_line, "(?<=@)\\w+")
    if (!is.na(type_match)) {
      entry$type <- type_match
    }
    
    # Extract key
    key_match <- str_extract(first_line, "(?<=\\{)[^,}]+")
    if (!is.na(key_match)) {
      entry$key <- str_trim(key_match)
    }
  }
  
  # Combine all lines and extract fields
  content <- paste(entry_lines, collapse = " ")
  
  # Extract fields using improved regex patterns
  entry$author <- extract_field(content, "author")
  entry$year <- extract_field(content, "year")
  entry$title <- extract_field(content, "title")
  entry$journal <- extract_field(content, "journal")
  entry$volume <- extract_field(content, "volume")
  entry$pages <- extract_field(content, "pages")
  entry$doi <- extract_field(content, "doi")
  entry$url <- extract_field(content, "url")
  
  return(entry)
}

#' Extract a field value from BibTeX content
#' @param content BibTeX entry content
#' @param field_name Field to extract
#' @return Field value or empty string
extract_field <- function(content, field_name) {
  
  # More flexible pattern to handle various BibTeX formats
  patterns <- c(
    paste0("\\b", field_name, "\\s*=\\s*\\{([^{}]*(?:\\{[^{}]*\\}[^{}]*)*)\\}"),
    paste0("\\b", field_name, "\\s*=\\s*\"([^\"]*)\",?"),
    paste0("\\b", field_name, "\\s*=\\s*([^,}]+),?")
  )
  
  for (pattern in patterns) {
    match <- str_extract(content, pattern)
    if (!is.na(match)) {
      # Extract the value part
      if (grepl("\\{", match)) {
        value <- str_extract(match, "(?<=\\{)[^}]*(?=\\})")
      } else if (grepl("\"", match)) {
        value <- str_extract(match, "(?<=\")[^\"]*(?=\")")
      } else {
        value <- str_extract(match, paste0("(?<=", field_name, "\\s*=\\s*)[^,}]+"))
      }
      
      if (!is.na(value)) {
        # Clean up the value
        value <- str_trim(value)
        value <- str_replace_all(value, "\\s+", " ")
        value <- str_replace_all(value, ",$", "")  # Remove trailing comma
        return(value)
      }
    }
  }
  
  return("")
}

#' Format a single reference in Nature Human Behaviour style
#' @param entry Parsed entry list
#' @return Formatted reference string
format_single_reference_nhb <- function(entry) {
  
  ref_parts <- c()
  
  # Authors and year
  if (entry$author != "" && entry$year != "") {
    # Format authors (handle multiple authors)
    authors <- format_authors_nhb(entry$author)
    ref_parts <- c(ref_parts, paste0(authors, " (", entry$year, ")"))
  } else if (entry$author != "") {
    authors <- format_authors_nhb(entry$author)
    ref_parts <- c(ref_parts, authors)
  }
  
  # Title
  if (entry$title != "") {
    # Clean title and add period
    title <- str_trim(entry$title)
    title <- str_replace_all(title, "\\{|\\}", "")  # Remove BibTeX braces
    if (!str_ends(title, "\\.")) {
      title <- paste0(title, ".")
    }
    ref_parts <- c(ref_parts, title)
  }
  
  # Journal, volume, pages
  if (entry$journal != "") {
    journal_part <- paste0("*", entry$journal, "*")
    
    if (entry$volume != "") {
      journal_part <- paste0(journal_part, " **", entry$volume, "**")
    }
    
    if (entry$pages != "") {
      # Clean up pages format
      pages <- str_replace_all(entry$pages, "–", "-")  # Replace en-dash with hyphen
      journal_part <- paste0(journal_part, ", ", pages)
    }
    
    ref_parts <- c(ref_parts, paste0(journal_part, "."))
  }
  
  # DOI (preferred) or URL
  if (entry$doi != "") {
    doi_clean <- str_replace(entry$doi, ",.*$", "")  # Remove trailing comma and text
    doi_clean <- str_replace(doi_clean, "^https?://doi.org/", "")  # Remove URL prefix if present
    ref_parts <- c(ref_parts, paste0("https://doi.org/", doi_clean))
  } else if (entry$url != "") {
    url_clean <- str_replace(entry$url, ",.*$", "")  # Remove trailing comma and text
    ref_parts <- c(ref_parts, url_clean)
  }
  
  if (length(ref_parts) == 0) {
    return("Reference formatting error - no extractable information")
  }
  
  return(paste(ref_parts, collapse = " "))
}

#' Format authors for Nature Human Behaviour style
#' @param authors String of author names from BibTeX
#' @return Formatted author string
format_authors_nhb <- function(authors) {
  if (authors == "" || is.null(authors)) {
    return("Unknown Author")
  }
  
  # Split authors by "and"
  author_list <- str_split(authors, " and ", simplify = TRUE)
  author_list <- str_trim(author_list)
  author_list <- author_list[author_list != ""]
  
  if (length(author_list) == 0) {
    return("Unknown Author")
  } else if (length(author_list) == 1) {
    return(clean_author_name(author_list[1]))
  } else if (length(author_list) == 2) {
    return(paste(clean_author_name(author_list[1]), "&", clean_author_name(author_list[2])))
  } else {
    # For 3+ authors, use "First Author et al."
    return(paste0(clean_author_name(author_list[1]), " et al."))
  }
}

#' Clean individual author name
#' @param author Single author name
#' @return Cleaned author name
clean_author_name <- function(author) {
  # Remove extra whitespace
  author <- str_trim(author)
  
  # Handle "Last, First" format
  if (grepl(",", author)) {
    parts <- str_split(author, ",", simplify = TRUE)
    if (length(parts) >= 2) {
      last_name <- str_trim(parts[1])
      first_name <- str_trim(parts[2])
      # For Nature style, typically "Last, F." format
      if (nchar(first_name) > 0) {
        first_initial <- substr(first_name, 1, 1)
        return(paste0(last_name, ", ", first_initial, "."))
      } else {
        return(last_name)
      }
    }
  }
  
  return(author)
}

# Test function to verify the parser works
test_bibtex_parser <- function(bib_file = "export.bib") {
  if (file.exists(bib_file)) {
    result <- parse_bibtex_file(bib_file)
    
    cat("Successfully parsed", length(result$formatted), "references\n\n")
    
    # Show first few references
    for (i in 1:min(5, length(result$formatted))) {
      cat("Reference", i, ":\n")
      cat(result$formatted[i], "\n\n")
    }
    
    return(result)
  } else {
    cat("BibTeX file not found:", bib_file, "\n")
    return(NULL)
  }
}
