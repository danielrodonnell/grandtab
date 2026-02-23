# CI entry point for weekly GrandTab update
# Sources the update functions and runs non-interactively.

library(reticulate)
library(httr2)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(pdftools)
library(base64enc)

source("data-raw/update_stored_grandtab.R")

# 1. Download latest PDF
pdf_path <- download_grandtab()

# 2. Load baseline
load("data/grandtab_detail.rda")

# 3. Run update — capture cat() output and warnings
warnings_log <- character(0)
result <- capture.output(
  withCallingHandlers(
    updated <- update_baseline(pdf_path, grandtab_detail),
    warning = function(w) {
      warnings_log <<- c(warnings_log, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ),
  type = "output"
)

# 4. Write warnings to file for the workflow to pick up
if (length(warnings_log) > 0) {
  writeLines(warnings_log, "warnings.txt")
  message("Warnings captured:\n", paste("  -", warnings_log, collapse = "\n"))
}

if (any(grepl("already up to date", result, ignore.case = TRUE))) {
  message("No update needed.")
  writeLines("UPDATE_NEEDED=false", Sys.getenv("GITHUB_OUTPUT"))
} else {
  # 5. Save updated detail (sections and summary already saved by update_baseline)
  grandtab_detail <- updated
  save(grandtab_detail, file = "data/grandtab_detail.rda", compress = "xz")
  message("Saved updated grandtab_detail.rda")

  # 6. Update intro documentation RTF
  update_grandtab_info(pdf_path)

  writeLines("UPDATE_NEEDED=true", Sys.getenv("GITHUB_OUTPUT"))
}
