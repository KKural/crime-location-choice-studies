# Test rendering the systematic review manuscript
library(rmarkdown)
library(flextable)

# Try to render the document to Word
tryCatch({
  render("Systematic_Review_Manuscript.Rmd", 
         output_format = "word_document", 
         output_file = "test_output.docx")
  cat("SUCCESS: Document rendered successfully!\n")
}, error = function(e) {
  cat("ERROR during rendering:\n")
  cat(conditionMessage(e), "\n")
})
