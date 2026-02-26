# =============================================================================
# update_stored_grandtab.R
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
#   6. Rebuild grandtab_sections and grandtab_summary from updated detail
#
# Requires:
#   - ANTHROPIC_API_KEY in .Renviron
#   - Python with pymupdf installed (via reticulate)
#   - R packages: reticulate, httr2, dplyr, readr, purrr, stringr
# =============================================================================

# -- Configuration ------------------------------------------------------------

# Fallback URL — used when the CalFish page cannot be reached.
# The live URL is resolved dynamically by get_latest_grandtab_url().
GRANDTAB_URL  <- "https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=84381"
CALFISH_URL   <- "https://www.calfish.org/ProgramsData/Species/CDFWAnadromousResourceAssessment.aspx"
MODEL         <- "claude-sonnet-4-5-20250929"
API_DELAY     <- 2  # seconds between API calls

# Number of distinct data tables in the GrandTab PDF (10).
# grandtab_detail has 11 entries because Table 3 (Winter main) and Table 4
# (Winter extras) are both derived from the same PDF pages.
.N_PDF_TABLES <- 10L

# Maps each grandtab_detail index (1-11) to its PDF table index (1-10).
# Table 4 (winter extras) shares PDF pages with Table 3 (winter main).
.PDF_TABLE_IDX <- c(1L, 2L, 3L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)

# -- Resolve latest PDF URL from CalFish --------------------------------------

#' Fetch the most current GrandTab PDF URL from the CalFish data access page
#'
#' Scrapes the CalFish GrandTab data access page, which lists all GrandTab PDF
#' releases with named links in the form \code{GrandTab.YYYY.MM.DD}. Returns
#' the URL of the entry with the most recent date. Falls back to
#' \code{fallback_url} if the page cannot be fetched or no dated links are
#' found.
#'
#' @param calfish_url URL of the CalFish GrandTab data access page
#' @param fallback_url URL to return if the page cannot be scraped
#' @return Character scalar: the URL of the most recent GrandTab PDF
get_latest_grandtab_url <- function(calfish_url = CALFISH_URL,
                                    fallback_url = GRANDTAB_URL) {
  tryCatch({
    html <- request(calfish_url) |>
      req_headers(`User-Agent` = "R/grandtab") |>
      req_perform() |>
      resp_body_string()

    # Links on the page have the form:
    #   href="https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=XXXXXX&inline=1"
    # with link text "GrandTab.YYYY.MM.DD"
    pattern <- paste0(
      'href="(https?://nrm\\.dfg\\.ca\\.gov/FileHandler\\.ashx',
      '\\?DocumentID=(\\d+)[^"]*)"[^>]*>\\s*(GrandTab\\.\\d{4}\\.\\d{2}\\.\\d{2})\\s*<'
    )
    m <- str_match_all(html, pattern)[[1]]

    if (nrow(m) == 0) {
      message("No dated GrandTab links found on CalFish page; using fallback URL.")
      return(fallback_url)
    }

    # Columns: full match, URL, DocumentID, link text (GrandTab.YYYY.MM.DD)
    urls    <- m[, 2]
    doc_ids <- as.integer(m[, 3])
    dates   <- as.Date(sub("GrandTab\\.", "", m[, 4]), format = "%Y.%m.%d")

    best_idx  <- which.max(dates)
    best_url  <- urls[best_idx]
    best_date <- dates[best_idx]
    best_id   <- doc_ids[best_idx]

    current_id <- as.integer(str_extract(fallback_url, "(?<=DocumentID=)\\d+"))

    if (is.na(current_id) || best_id != current_id) {
      message("Using GrandTab PDF dated ", format(best_date),
              " (DocumentID=", best_id, "): ", best_url)
    } else {
      message("GrandTab PDF URL confirmed current (",
              format(best_date), ", DocumentID=", best_id, ").")
    }

    best_url
  }, error = function(e) {
    warning("Could not resolve latest GrandTab URL from CalFish: ",
            conditionMessage(e), "; using fallback URL.")
    fallback_url
  })
}

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
#'
#' Finds the pages containing the newest year, then checks whether the
#' preceding page also contains bracketed (provisional) data. Returns a
#' list of integer vectors — one per table — each of length 1 or 2.
#'
#' @param pdf_path Local path to PDF
#' @param baseline Named list of 11 elements, each list(title, data_frame)
#' @return List of integer vectors (page numbers per table), or NULL if up to date
detect_page_map <- function(pdf_path, baseline) {
  all_data <- pdf_data(pdf_path)

  # Determine new year from baseline
  last_year <- max(sapply(baseline, \(bl) {
    as.numeric(str_extract(tail(bl[[2]][[1]], 1), "\\d{4}"))
  }))
  new_year <- as.character(last_year + 1)
  target <- paste0(new_year, "]")

  # Find pages containing the target string (these are the "last" pages per table)
  target_pages <- which(sapply(all_data, \(pg) {
    any(str_detect(pg$text, fixed(target)))
  }))

  if(length(target_pages) == 0) {
    return(NULL)
  }else {
    message("Found '", target, "' on pages: ", paste(target_pages, collapse = ", "))
  }
  if (length(target_pages) != .N_PDF_TABLES) {
    warning("Expected ", .N_PDF_TABLES, " PDF data pages, found ",
            length(target_pages))
  }

  # For each target page, check if adjacent pages also have bracketed data
  page_has_brackets <- function(pg) {
    if (pg < 1 || pg > length(all_data)) return(FALSE)
    any(startsWith(all_data[[pg]]$text, "["))
  }

  page_map <- lapply(target_pages, \(pg) {
    prev <- pg > 1 && page_has_brackets(pg - 1)
    next_ <- pg < length(all_data) && page_has_brackets(pg + 1)
    if (prev && next_) {
      c(pg - 1L, pg, pg + 1L)
    } else if (prev) {
      c(pg - 1L, pg)
    } else if (next_) {
      c(pg, pg + 1L)
    } else {
      pg
    }
  })

  message("Page map: ", paste(sapply(page_map, \(x) paste(x, collapse="-")), collapse = ", "))
  page_map
}

# -- Cross-table consistency validation --------------------------------------

#' Validate that Table 1 summary totals match detail table totals
#'
#' Compares LATE-FALL_TOTAL, WINTER_TOTAL, SPRING_TOTAL, and FALL_TOTAL from
#' Table 1 (ALL RUNS) against the corresponding TOTAL columns in Tables 2, 3,
#' 5, and 6. Only checks rows with bracketed (provisional) years. Warns on
#' mismatches.
#'
#' @param detail The grandtab_detail list (11 elements)
#' Check cross-table consistency and return indices of mismatched tables
#'
#' Returns a sorted integer vector of table indices (from 1-6) involved in
#' mismatches. Empty integer(0) means everything is consistent.
#'
#' @param detail The grandtab_detail list (11 elements)
#' @return Integer vector of table indices that have mismatches
validate_cross_table <- function(detail) {
  parse_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

  t1 <- detail[[1]][[2]]
  t2 <- detail[[2]][[2]]
  t3 <- detail[[3]][[2]]
  t5 <- detail[[5]][[2]]
  t6 <- detail[[6]][[2]]

  extract_year <- function(x) str_extract(x, "\\d{4}$|\\d{4}(?=\\]$)")

  t1_year <- extract_year(t1[["Late-Fall YEAR"]])

  checks <- list(
    list(name = "LATE-FALL", t1_col = "Late-Fall TOTAL", detail_idx = 2L,
         detail_tbl = t2, detail_col = "TOTAL LATE-FALL RUN",
         detail_year = extract_year(t2[["Late-Fall YEAR"]])),
    list(name = "WINTER", t1_col = "Winter TOTAL", detail_idx = 3L,
         detail_tbl = t3, detail_col = "WINTER TOTAL CV SYSTEM",
         detail_year = extract_year(t3[["Winter YEAR"]])),
    list(name = "SPRING", t1_col = "Spring TOTAL", detail_idx = 5L,
         detail_tbl = t5, detail_col = "TOTAL SPRING RUN 7/",
         detail_year = extract_year(t5[["YEAR"]])),
    list(name = "FALL", t1_col = "Fall TOTAL", detail_idx = 6L,
         detail_tbl = t6, detail_col = "Sac SJ System TOTAL",
         detail_year = extract_year(t6[["YEAR"]]))
  )

  bad_indices <- integer(0)

  for (chk in checks) {
    bracketed <- startsWith(t1[["Late-Fall YEAR"]], "[")
    if (!any(bracketed)) next

    t1_sub <- t1[bracketed, ]
    t1_years <- extract_year(t1_sub[["Late-Fall YEAR"]])
    t1_vals <- parse_num(t1_sub[[chk$t1_col]])

    detail_idx <- match(t1_years, chk$detail_year)
    matched <- !is.na(detail_idx)

    for (j in which(matched)) {
      t1_val <- t1_vals[j]
      d_val <- parse_num(chk$detail_tbl[[chk$detail_col]][detail_idx[j]])

      if (is.na(t1_val) && is.na(d_val)) next
      if (!identical(t1_val, d_val)) {
        message("Cross-table mismatch [", chk$name, " ", t1_years[j], "]: ",
                "T1 ", chk$t1_col, "=", t1_val,
                " vs T", chk$detail_idx, " ", chk$detail_col, "=", d_val)
        bad_indices <- c(bad_indices, 1L, chk$detail_idx)
      }
    }
  }

  sort(unique(bad_indices))
}

# -- Update baseline via Claude API ------------------------------------------

#' Update baseline data by extracting new rows via Claude vision
#' @param pdf_path Local path to GrandTab PDF
#' @param baseline Named list of 11 elements, each list(title, data_frame)
#' @param img_dir Directory containing rendered page PNGs
#' @return Updated baseline list with new rows appended
update_baseline <- function(pdf_path, baseline, img_dir = "data-raw/pages") {

  # Auto-detect page mapping — returns one entry per PDF data table (10 total)
  page_map <- detect_page_map(pdf_path, baseline)

  if(is.null(page_map)) {
    return(cat("Stored GrandTab database is already up to date!"))
  }

  # Expand to one entry per baseline table (11 total).
  # Table 4 (winter extras) shares PDF pages with Table 3 (winter main).
  full_page_map <- page_map[.PDF_TABLE_IDX]

  # Render all unique pages we need
  all_pages <- sort(unique(unlist(page_map)))
  render_pages(pdf_path, all_pages, out_dir = img_dir)

  # Extract a single table's provisional rows via Claude API
  extract_table <- function(bl, pages) {
    df <- bl[[2]]
    col_names <- names(df)
    year_col <- df[[1]]

    # Find rows with bracketed years (provisional data)
    bracketed <- startsWith(year_col, "[")

    if (any(bracketed)) {
      first_bracket_idx <- which(bracketed)[1]

      # Use up to 3 rows before the first bracketed row as context
      context_end <- first_bracket_idx - 1
      context_start <- max(1, context_end - 2)
      context_rows <- if (context_end >= 1) df[context_start:context_end, ] else df[0, ]

      # Year range to extract: first bracketed year through end of table
      first_bracket_year <- str_extract(year_col[first_bracket_idx], "\\d{4}")
      year_instruction <- paste0("starting from year ", first_bracket_year,
                                 " through the very last row of the table")
    } else {
      # No bracketed years — fall back to extracting new rows only
      first_bracket_idx <- nrow(df) + 1
      context_rows <- tail(df, 3)
      last_year <- as.numeric(str_extract(tail(year_col, 1), "\\d{4}"))
      year_instruction <- paste0("for years ",
                                 paste((last_year + 1):(last_year + 2), collapse = " and "),
                                 " through the very last row of the table")
    }

    context_csv <- context_rows |> format_csv()

    # Encode page image(s)
    img_content <- lapply(pages, \(pg) {
      img_path <- file.path(img_dir, sprintf("grandtab_page_%02d.png", pg))
      img_b64 <- base64enc::base64encode(
        readBin(img_path, "raw", file.info(img_path)$size)
      )
      list(type = "image",
           source = list(type = "base64", media_type = "image/png",
                         data = img_b64))
    })

    if (length(pages) == 1) {
      image_note <- "Look at the table in this image."
    } else {
      image_note <- paste0("The table spans ", length(pages),
                           " pages shown in these images (in order).")
    }

    prompt <- sprintf("
I have an existing data frame with these exact columns:
%s

Here are the last few confirmed (non-provisional) rows for context:
%s

%s Extract ALL rows %s.
You MUST include every row from the starting year to the very end of the table —
do not stop early.
Return them as CSV with the exact column names above as the header row.
The columns must align exactly to my existing structure.

Rules:
- Remove commas from numbers (e.g., 12,479 -> 12479)
- Use NA for blank/empty cells
- CRITICAL: Preserve the year/period format EXACTLY as printed in the image,
  including square brackets. If the image shows [2024], return [2024] not 2024.
  If it shows [Nov 2023 - Apr 2024], return [Nov 2023 - Apr 2024] not
  Nov 2023 - Apr 2024. Brackets indicate provisional data and must be kept.
- ALWAYS include rows even if every cell except YEAR is blank — fill non-YEAR
  columns with NA but still include the row
- If a row has ONLY a year value and nothing else, still include it with NA
  for all other columns
- No commentary, no markdown fences — just the CSV header + data rows
", paste(col_names, collapse = ", "), context_csv, image_note, year_instruction)

    # Build message content: image(s) first, then the text prompt
    msg_content <- c(img_content, list(list(type = "text", text = prompt)))

    # Extract with validation and retry
    extract_rows <- function(attempt = 1) {
      resp <- request("https://api.anthropic.com/v1/messages") |>
        req_headers(
          `x-api-key`         = Sys.getenv("ANTHROPIC_API_KEY"),
          `anthropic-version`  = "2023-06-01",
          `content-type`       = "application/json"
        ) |>
        req_body_json(list(
          model      = MODEL,
          max_tokens = 8192L,
          messages   = list(list(
            role    = "user",
            content = msg_content
          ))
        )) |>
        req_perform()

      csv_text <- resp_body_json(resp)$content[[1]]$text

      new_rows <- tryCatch(
        read_csv(I(csv_text), show_col_types = FALSE, na = "NA",
                 col_types = cols(.default = col_character())),
        error = function(e) {
          warning("Parse failed on pages ", paste(pages, collapse="-"),
                  " (attempt ", attempt, "): ", e$message)
          return(NULL)
        }
      )

      if (is.null(new_rows)) return(NULL)

      # Validation: check column names
      if (!identical(names(new_rows), col_names)) {
        warning("Column mismatch on pages ", paste(pages, collapse="-"),
                " (attempt ", attempt, "):\n  Expected: ",
                paste(col_names, collapse = ", "),
                "\n  Got: ", paste(names(new_rows), collapse = ", "))
      }

      # Validation: extracted rows should have at least as many rows as
      # the existing bracketed rows (we're replacing them + possibly adding new)
      if (any(bracketed) && nrow(new_rows) < sum(bracketed)) {
        if (attempt < 2) {
          warning("Pages ", paste(pages, collapse="-"), ": extracted ",
                  nrow(new_rows), " rows but expected at least ", sum(bracketed),
                  ". Retrying...")
          Sys.sleep(API_DELAY)
          return(extract_rows(attempt + 1))
        } else {
          warning("Pages ", paste(pages, collapse="-"), ": extracted ",
                  nrow(new_rows), " rows but expected at least ", sum(bracketed),
                  " after ", attempt, " attempts.")
        }
      }

      # Validation: if original had brackets, extracted rows should too
      if (any(bracketed) && !any(startsWith(new_rows[[1]], "["))) {
        if (attempt < 2) {
          warning("Pages ", paste(pages, collapse="-"),
                  ": no bracketed years in extraction — brackets may have been dropped. Retrying...")
          Sys.sleep(API_DELAY)
          return(extract_rows(attempt + 1))
        } else {
          warning("Pages ", paste(pages, collapse="-"),
                  ": brackets still missing after ", attempt, " attempts.")
        }
      }

      new_rows
    }

    new_rows <- extract_rows()
    if (is.null(new_rows)) return(bl)

    # Keep confirmed rows (before bracketed years) and replace with fresh extraction
    confirmed_rows <- df[seq_len(first_bracket_idx - 1), ]

    message("Pages ", paste(pages, collapse="-"), ": ", nrow(new_rows),
            " rows extracted (replacing ", sum(bracketed), " provisional rows)")
    Sys.sleep(API_DELAY)

    list(bl[[1]], bind_rows(
      mutate(confirmed_rows, across(everything(), as.character)),
      new_rows
    ))
  }

  # Initial extraction of all 11 baseline tables
  updated <- map2(baseline, full_page_map, extract_table)

  # Cross-table consistency check with auto-correction
  bad <- validate_cross_table(updated)

  if (length(bad) > 0) {
    message("Re-extracting tables ", paste(bad, collapse = ", "),
            " to fix cross-table mismatches...")
    for (idx in bad) {
      updated[[idx]] <- extract_table(baseline[[idx]], full_page_map[[idx]])
    }

    bad2 <- validate_cross_table(updated)
    if (length(bad2) > 0) {
      warning("Cross-table mismatches persist for tables ",
              paste(bad2, collapse = ", "),
              " after re-extraction. These may reflect discrepancies in the source PDF.")
    }
  }

  message("Cross-table validation complete")

  # Rebuild derived datasets from updated detail
  message("Rebuilding grandtab_sections and grandtab_summary...")
  grandtab_sections <- build_sections(updated)
  grandtab_summary  <- build_summary(updated)

  save(grandtab_sections, file = "data/grandtab_sections.rda", compress = "xz")
  save(grandtab_summary, file = "data/grandtab_summary.rda", compress = "xz")
  message("Saved grandtab_sections.rda and grandtab_summary.rda")

  updated
}

# -- Derive grandtab_sections from grandtab_detail ---------------------------

#' Build grandtab_sections from the detail list
#'
#' Combines detail tables 2-11 (all except "ALL RUNS") into a single wide
#' data frame with a section_title column and the first column renamed to YEAR.
#' @param detail The grandtab_detail list (11 elements)
#' @return A tibble
build_sections <- function(detail) {
  lapply(detail[2:11], \(x) {
    df <- x[[2]]
    names(df)[1] <- "YEAR"
    df$section_title <- x[[1]]
    relocate(df, section_title)
  }) |> bind_rows()
}

# -- Derive grandtab_summary from grandtab_detail ---------------------------

#' Build grandtab_summary from the detail list
#'
#' Extracts the four run summaries (LATE-FALL, WINTER, SPRING, FALL) from
#' detail table 1 ("ALL RUNS").
#' @param detail The grandtab_detail list (11 elements)
#' @return A named list of 4 tibbles
build_summary <- function(detail) {
  all_runs <- detail[[1]][[2]]

  parse_numeric <- function(x) {
    suppressWarnings(as.numeric(gsub(",", "", x)))
  }

  late_fall <- tibble(
    run        = "LATE-FALL",
    YEAR       = all_runs[["Late-Fall YEAR"]],
    Hatcheries = parse_numeric(all_runs[["Late-Fall Hatch"]]),
    `In-River` = parse_numeric(all_runs[["Late-Fall In-R"]]),
    TOTAL      = parse_numeric(all_runs[["Late-Fall TOTAL"]])
  )

  winter <- tibble(
    run   = "WINTER",
    YEAR  = all_runs[["Winter YEAR"]],
    TOTAL = parse_numeric(all_runs[["Winter TOTAL"]]),
    RBDD  = parse_numeric(all_runs[["Winter RBDD"]])
  )

  spring <- tibble(
    run        = "SPRING",
    YEAR       = all_runs[["YEAR"]],
    Hatcheries = parse_numeric(all_runs[["Spring Hatch"]]),
    `In-River` = parse_numeric(all_runs[["Spring In-R"]]),
    TOTAL      = parse_numeric(all_runs[["Spring TOTAL"]])
  )

  fall <- tibble(
    run        = "FALL",
    YEAR       = all_runs[["YEAR"]],
    Hatcheries = parse_numeric(all_runs[["Fall Hatch"]]),
    `In-River` = parse_numeric(all_runs[["Fall In-R"]]),
    TOTAL      = parse_numeric(all_runs[["Fall TOTAL"]])
  )

  list(
    `LATE-FALL` = late_fall,
    WINTER      = winter,
    SPRING      = spring,
    FALL        = fall
  )
}

# -- Update GrandTab intro documentation RTF ---------------------------------

#' Update the GrandTab documentation RTF to match the current PDF
#'
#' Renders the first two pages of the PDF (intro text) and sends them to Claude
#' along with the existing RTF source. Claude updates only the text content
#' (words, dates, numbers) while preserving all RTF control codes and formatting.
#'
#' @param pdf_path Local path to the GrandTab PDF
#' @param img_dir Directory for rendered page PNGs
update_grandtab_info <- function(pdf_path, img_dir = "data-raw/pages") {
  rtf_path <- "inst/extdata/GrandTab Documentation.rtf"

  if (!file.exists(rtf_path)) {
    warning("RTF file not found at ", rtf_path, "; skipping documentation update.")
    return(invisible(NULL))
  }

  # 1. Render intro pages (1-2) to PNGs
  render_pages(pdf_path, 1:2, out_dir = img_dir)

  # 2. Read existing RTF
  rtf_text <- paste(readLines(rtf_path, warn = FALSE), collapse = "\n")

  # 3. Encode page images to base64
  img_content <- lapply(1:2, \(pg) {
    img_path <- file.path(img_dir, sprintf("grandtab_page_%02d.png", pg))
    img_b64 <- base64enc::base64encode(
      readBin(img_path, "raw", file.info(img_path)$size)
    )
    list(type = "image",
         source = list(type = "base64", media_type = "image/png",
                       data = img_b64))
  })

  # 4. Build prompt
  prompt <- sprintf("
I have an existing RTF file whose complete source is shown below. The RTF contains
the introductory text from a GrandTab PDF document.

I am also providing PNG images of the first two pages of the CURRENT version of the
GrandTab PDF. The text content may have changed (new dates, updated wording, etc.).

Your task: update the RTF so its TEXT content matches the PDF images, while
preserving EVERY RTF control code exactly as-is.

Rules:
- Return the COMPLETE updated RTF file, starting with {\\rtf1 and ending with }
- Change ONLY text content (words, dates, numbers, names) to match the PDF images
- Preserve every RTF control code, font definition, color table, paragraph format
- Keep the exact same structural layout (line breaks, paragraph breaks, formatting)
- If the text is already up to date, return the RTF unchanged
- No commentary, no markdown fences — just the raw RTF source

Existing RTF source:
%s
", rtf_text)

  # 5. Build message and call Claude API
  msg_content <- c(img_content, list(list(type = "text", text = prompt)))

  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      `x-api-key`         = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version`  = "2023-06-01",
      `content-type`       = "application/json"
    ) |>
    req_body_json(list(
      model      = MODEL,
      max_tokens = 8192L,
      messages   = list(list(
        role    = "user",
        content = msg_content
      ))
    )) |>
    req_perform()

  new_rtf <- resp_body_json(resp)$content[[1]]$text

  # 6. Validate response
  if (!startsWith(trimws(new_rtf), "{\\rtf1")) {
    warning("Claude response does not look like valid RTF; skipping update.")
    return(invisible(NULL))
  }

  # Check balanced braces
  n_open  <- nchar(gsub("[^{]", "", new_rtf))
  n_close <- nchar(gsub("[^}]", "", new_rtf))
  if (n_open != n_close) {
    warning("RTF has unbalanced braces (", n_open, " open, ", n_close,
            " close); skipping update.")
    return(invisible(NULL))
  }

  # 7. Write updated RTF
  writeLines(new_rtf, rtf_path, useBytes = TRUE)
  message("Updated GrandTab documentation RTF.")
  invisible(rtf_path)
}

# -- Run everything -----------------------------------------------------------

if (interactive()) {
  # 1. Resolve URL and download latest PDF
  pdf_path <- download_grandtab(url = get_latest_grandtab_url())

  # 2. Load your baseline (adjust path as needed)
  load("data/grandtab_detail.rda")

  # 3. Update
  updated <- update_baseline(pdf_path, grandtab_detail)

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

  # 6. Update intro documentation RTF
  update_grandtab_info(pdf_path)
}
