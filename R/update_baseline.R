# =============================================================================
# update_baseline.R
#
# Updates the GrandTab baseline data by extracting new rows from the latest
# PDF using PyMuPDF (rendering) + Claude API (vision-based extraction).
#
# Workflow:
#   1. Download the PDF
#   2. Render relevant pages to PNGs via PyMuPDF
#   3. Auto-detect which pages contain new data
#   4. Send each page image + baseline column structure to Claude
#   5. Append new rows to baseline data frames
#
# Requires:
#   - ANTHROPIC_API_KEY in .Renviron
#   - Python with pymupdf installed (via reticulate)
#   - R packages: reticulate, httr2, dplyr, readr, purrr, stringr
# =============================================================================

# -- Configuration ------------------------------------------------------------

GRANDTAB_URL <- "https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=84381"
MODEL <- "claude-sonnet-4-5-20250929"
API_DELAY <- 2  # seconds between API calls

# -- Download PDF -------------------------------------------------------------

#' Download GrandTab PDF to a local path
#' @param url URL to the GrandTab PDF
#' @param dest Local destination path
download_grandtab <- function(url = GRANDTAB_URL,
                              dest = "data-raw/grandtab.pdf") {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  download.file(url, dest, mode = "wb")
  message("Downloaded PDF to ", dest)
  dest
}

# -- Render PDF pages to PNGs via PyMuPDF ------------------------------------

#' Render specific PDF pages to PNG images
#' @param pdf_path Local path to PDF
#' @param pages Integer vector of page numbers
#' @param out_dir Output directory for PNGs
#' @param dpi Rendering resolution
#' @return Named character vector of PNG file paths
render_pages <- function(pdf_path, pages, out_dir = "data-raw/pages",
                         dpi = 300) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fitz  <- import("fitz")
  doc   <- fitz$open(pdf_path)
  scale <- dpi / 72
  
  paths <- vapply(pages, \(p) {
    out  <- file.path(out_dir, sprintf("grandtab_page_%02d.png", p))
    page <- doc$load_page(p - 1L)
    pix  <- page$get_pixmap(matrix = fitz$Matrix(scale, scale))
    pix$save(out)
    out
  }, character(1))
  
  doc$close()
  message("Rendered ", length(paths), " pages to ", out_dir)
  setNames(paths, pages)
}

# -- Detect page map ----------------------------------------------------------

#' Auto-detect which PDF pages correspond to each baseline table
#' @param pdf_path Local path to PDF
#' @param baseline Named list of 10 elements, each list(title, data_frame)
#' @return Integer vector of page numbers
detect_page_map <- function(pdf_path, baseline) {
  all_data <- pdf_data(pdf_path)
  
  # Determine new year from baseline
  last_year <- max(sapply(baseline, \(bl) {
    as.numeric(str_extract(tail(bl[[2]][[1]], 1), "\\d{4}"))
  }))
  new_year <- as.character(last_year + 1)
  target <- paste0(new_year, "]")
  
  # Find pages containing the target string
  pages <- which(sapply(all_data, \(pg) {
    any(str_detect(pg$text, fixed(target)))
  }))
  
  if(length(pages) == 0) {
    return(NULL)
  }else {
    message("Found '", target, "' on pages: ", paste(pages, collapse = ", "))
  }
  if (length(pages) != length(baseline)) {
    warning("Expected ", length(baseline), " pages, found ", length(pages))
  }
  
  pages
}

# -- Update baseline via Claude API ------------------------------------------

#' Update baseline data by extracting new rows via Claude vision
#' @param pdf_path Local path to GrandTab PDF
#' @param baseline Named list of 10 elements, each list(title, data_frame)
#' @param img_dir Directory containing rendered page PNGs
#' @return Updated baseline list with new rows appended
update_baseline <- function(pdf_path, baseline, img_dir = "data-raw/pages") {
  
  # Auto-detect page mapping
  page_map <- detect_page_map(pdf_path, baseline)
  
  if(is.null(page_map)) {
    return(cat("Stored GrandTab database is already up to date!"))
  }else{
    message("Detected page map: ", paste(page_map, collapse = ", "))
  }
  
  if (length(page_map) != length(baseline)) {
    stop("Page map length (", length(page_map),
         ") does not match baseline length (", length(baseline), ")")
  }
  
  # Render only the pages we need
  render_pages(pdf_path, page_map, out_dir = img_dir)
  
  map2(baseline, page_map, \(bl, pg) {
    df <- bl[[2]]
    col_names <- names(df)
    
    # Last 3 rows as CSV for context
    tail_csv <- df |>
      tail(3) |>
      format_csv()
    
    # Determine expected new years dynamically
    last_baseline_year <- as.numeric(str_extract(tail(df[[1]], 1), "\\d{4}"))
    next_years <- paste((last_baseline_year + 1):(last_baseline_year + 2),
                        collapse = " and ")
    
    # Read and encode the page image
    img_path <- file.path(img_dir, sprintf("grandtab_page_%02d.png", pg))
    img_b64 <- base64enc::base64encode(
      readBin(img_path, "raw", file.info(img_path)$size)
    )
    
    prompt <- sprintf("
I have an existing data frame with these exact columns:
%s

Here are the last 3 rows:
%s

Look at the table in this image. Find ALL rows after the last row shown above.
I expect rows for years %s to exist in the image.
Return them as CSV with the exact column names above as the header row.
The columns must align exactly to my existing structure.

Rules:
- Remove commas from numbers (e.g., 12,479 -> 12479)
- Use NA for blank/empty cells
- Preserve year format exactly as shown (e.g., [2024] or [Nov 2023 - Apr 2024])
- ALWAYS include rows even if every cell except YEAR is blank — fill non-YEAR
  columns with NA but still include the row
- If a row has ONLY a year value and nothing else, still include it with NA
  for all other columns
- No commentary, no markdown fences — just the CSV header + data rows
", paste(col_names, collapse = ", "), tail_csv, next_years)
    
    resp <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        `x-api-key`         = Sys.getenv("ANTHROPIC_API_KEY"),
        `anthropic-version`  = "2023-06-01",
        `content-type`       = "application/json"
      ) |>
      req_body_json(list(
        model      = MODEL,
        max_tokens = 4096L,
        messages   = list(list(
          role    = "user",
          content = list(
            list(type = "image",
                 source = list(type = "base64", media_type = "image/png",
                               data = img_b64)),
            list(type = "text", text = prompt)
          )
        ))
      )) |>
      req_perform()
    
    csv_text <- resp_body_json(resp)$content[[1]]$text
    
    new_rows <- tryCatch(
      read_csv(I(csv_text), show_col_types = FALSE, na = "NA",
               col_types = cols(.default = col_character())),
      error = function(e) {
        warning("Parse failed on page ", pg, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(new_rows)) return(bl)
    
    if (!identical(names(new_rows), col_names)) {
      warning("Column mismatch on page ", pg, ":\n  Expected: ",
              paste(col_names, collapse = ", "), "\n  Got: ",
              paste(names(new_rows), collapse = ", "))
    }
    
    message("Page ", pg, ": ", nrow(new_rows), " new rows extracted")
    Sys.sleep(API_DELAY)
    
    list(bl[[1]], bind_rows(
      mutate(df, across(everything(), as.character)),
      new_rows
    ))
  })
}

# -- Run everything -----------------------------------------------------------

if (interactive()) {
  # 1. Download latest PDF
  pdf_path <- download_grandtab()

  # 2. Load your baseline (adjust path as needed)
  load("data/grandtab_detail.rda")  # loads grandtab_detail2

  # 3. Update
  updated <- update_baseline(pdf_path, grandtab_detail2)

  # 4. Verify
  walk(updated, \(bl) {
    message(bl[[1]])
    print(tail(bl[[2]], 4))
    message("---")
  })

  # 5. Save
  grandtab_detail <- updated
  save(grandtab_detail, file = "data/grandtab_detail.rda", compress = "xz")
  message("Saved updated baseline.")
}

