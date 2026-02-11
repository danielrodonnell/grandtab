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

# 3. Run update — capture cat() output to detect "up to date"
result <- capture.output(
  updated <- update_baseline(pdf_path, grandtab_detail),
  type = "output"
)

if (any(grepl("already up to date", result, ignore.case = TRUE))) {
  message("No update needed.")
  writeLines("UPDATE_NEEDED=false", Sys.getenv("GITHUB_OUTPUT"))
} else {
  # 4. Save updated detail (sections and summary already saved by update_baseline)
  grandtab_detail <- updated
  save(grandtab_detail, file = "data/grandtab_detail.rda", compress = "xz")
  message("Saved updated grandtab_detail.rda")
  writeLines("UPDATE_NEEDED=true", Sys.getenv("GITHUB_OUTPUT"))
}
