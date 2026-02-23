# ============================================================================
# get_column_notes.R
#
# Retrieve footnote documentation for GrandTab tables.
# Can be used standalone or piped from get_grandtab_raw().
# ============================================================================

# -- Internal footnote text per table ----------------------------------------
# Positional list (indices 1-11) matching grandtab_detail table numbers.
# Each element is a named character vector keyed by footnote number.

.footnotes <- list(
  # Table 1: ALL RUNS — no footnotes
  character(0),

  # Table 2: LATE-FALL RUN
  c(
    `1` = "Red Bluff Diversion Dam",
    `2` = "May include numbers of fish for tributaries where estimates were not made",
    `3` = paste(
      "Coleman National Fish Hatchery. Transferred to Coleman National Fish",
      "Hatchery from Keswick Dam and/or RBDD."
    ),
    `4` = "Transferred to Tehama Colusa Fish Facility from Red Bluff Diversion Dam",
    `5` = paste(
      "In 2009 USFWS conducted a comprehensive analysis of Battle Creek coded",
      "wire tag data from 2000-2008 to estimate numbers of fall and late-fall",
      "Chinook returning to Battle Creek. Previously, a cutoff date of December",
      "1st was used to assign run. This changed some Battle Creek estimates."
    )
  ),

  # Table 3: WINTER RUN (trimmed — cols 1-11 only)
  c(
    `1` = "Fish transferred from Red Bluff Diversion Dam (RBDD) and Keswick Trap to hatcheries",
    `2` = paste(
      "Upstream mainstem in-river estimates prior to 2001 were based on RBDD counts.",
      "Subsequent estimates are based on carcass surveys. Numbers using RBDD data are",
      "adjusted for angler harvest."
    ),
    `3` = "Downstream mainstem numbers based on upstream estimates and redd distribution.",
    `4` = "Not a stable breeding population",
    `5` = "Fish passed upstream of Coleman Weir",
    `6` = paste(
      "Total winter estimate includes mainstem in-river, tributaries, hatcheries,",
      "and angler harvest."
    ),
    `7` = "Not a stable breeding population"
  ),

  # Table 4: WINTER RUN additional data
  c(
    `1` = "Data used (when appropriate) to determine mainstem in-river estimate.",
    `2` = "Based on aerial survey",
    `3` = "Estimated from fish counts at Red Bluff Diversion Dam (RBDD)",
    `4` = paste(
      "Estimated angler harvest upstream of RBDD. After 1995, it was assumed that",
      "there was no harvest due to winter-run salmon angling closure."
    )
  ),

  # Table 5: SPRING RUN
  c(
    `1` = paste(
      "Red Bluff Diversion Dam. Estimates for 1960-1968 spring run are included",
      "with fall run. 2022 includes 378 fish transferred from Keswick Dam to",
      "Clear Creek."
    ),
    `2` = paste(
      "Feather River Survey does not provide separate estimates for fall and spring",
      "escapement. Spring-run estimates are included with fall-run estimates. Fish",
      "were transported above Oroville Dam in 1964-1966."
    ),
    `3` = paste(
      "Feather River Hatchery implemented a methodology change in 2005 for",
      "distinguishing spring-run from fall-run. Fish arriving prior to the",
      "spring-run spawning period were tagged and returned to the river. The",
      "spring-run escapement was the number of these tagged fish that subsequently",
      "returned to the hatchery during spawning."
    ),
    `4` = paste(
      "In 2009 USFWS conducted a comprehensive analysis of Battle Creek coded",
      "wire tag data from 2000-2008 to estimate numbers of fall and late-fall",
      "Chinook returning to Battle Creek. Previously, a cutoff date of December",
      "1st was used to assign run. This changed some Battle Creek estimates."
    ),
    `5` = paste(
      "Spawner estimates are shown when available for selected locations. These",
      "are shown for comparison and are not included in totals."
    ),
    `6` = paste(
      "Butte Creek shows the snorkel survey estimate prior to 2001, in 2023,",
      "and 2024, otherwise the carcass survey estimate is shown."
    ),
    `7` = paste(
      "Thomes Ck. and the Calaveras R. are no longer shown, but are included in",
      "totals. In 1972 500 fish were reported in the Calaveras R. In 1998 one fish",
      "and in 2002 two fish were reported in Thomes Ck."
    )
  ),

  # Table 6: FALL RUN Summary I
  c(
    `1` = paste(
      "Coleman National Fish Hatchery, Tehama-Colusa Fish Facility, Feather River",
      "Hatchery, and Nimbus Hatchery"
    ),
    `2` = "Mokelumne River Fish Installation and Merced River Fish Facility"
  ),

  # Table 7: FALL RUN Summary II
  c(
    `1` = paste(
      "Data for 1952-1959 may include some estimates from tributaries between",
      "Keswick Dam and Red Bluff."
    )
  ),

  # Table 8: FALL RUN Section 1
  c(
    `1` = paste(
      "For 1952-1955, the original data provided a total for the Sacramento",
      "mainstem. Data presented is that total multiplied by the average % of the",
      "fall-run estimate above Red Bluff, from 1956-2007."
    ),
    `2` = paste(
      "Salmon from the mainstem population that were trapped at Keswick or",
      "Anderson-Cottonwood dams and transferred to Coleman National Fish",
      "Hatchery (CNFH)"
    ),
    `3` = "Salmon in Battle Creek upstream of the Hatchery",
    `4` = "The data source did not provide a breakdown of individual tributaries.",
    `5` = paste(
      "In 2009 USFWS edited some 2000-2008 estimates of fall and late-fall",
      "Chinook returning to Battle Creek based on CWT data. Previously, a",
      "December 1st cutoff was used to assign run."
    )
  ),

  # Table 9: FALL RUN Section 2
  c(
    `1` = paste(
      "For 1952-1955, the original data source only provided a total estimate",
      "for the entire Sacramento mainstem, above and below Red Bluff combined.",
      "The data presented here is that total estimate multiplied by the average",
      "% of the fall-run estimate below Red Bluff, from 1956-2007."
    ),
    `2` = paste(
      "Salmon from the mainstem population that were trapped at Red Bluff",
      "Diversion Dam (RBDD) and transferred to Tehama-Colusa Fish Facility (TCFF)"
    ),
    `3` = paste(
      "The data source did not provide a breakdown of individual tributaries.",
      "NOTE: The number of fish listed may include those for tributaries from",
      "Keswick Dam to RBDD."
    )
  ),

  # Table 10: FALL RUN Section 3
  c(
    `1` = "No Butte Creek estimate was made in 2018 due to fire conditions.",
    `2` = paste(
      "Feather River Survey does not provide separate estimates for fall and",
      "spring escapement. Spring-run estimates are included with fall-run estimates."
    ),
    `3` = paste(
      "Yuba River Survey does not provide separate estimates for fall and spring",
      "escapement. Spring-run estimates are included with fall-run estimates.",
      "Non fresh carcasses not in report may be included in total."
    ),
    `4` = paste(
      "In 2009 CDFW conducted a reanalysis of historic American River Fall data",
      "from 1990-2008 to consistently include fish taken at Nimbus Racks and",
      "exclude angler estimates."
    ),
    `5` = paste(
      "From 2018 on, the area between Nimbus Weir and Nimbus Dam is included in",
      "the American River Fall escapement estimate. In 2018 this area was closed",
      "to angling."
    )
  ),

  # Table 11: FALL RUN Section 4
  c(
    `1` = paste(
      "Fish were trapped in San Joaquin mainstem in 1977. Only 389 of these were",
      "transported to the hatchery spawning channel."
    )
  )
)


# -- Main function -----------------------------------------------------------

#' Retrieve footnotes for GrandTab tables
#'
#' Returns footnote documentation extracted from the GrandTab PDF source
#' document. Can be used standalone or piped from \code{get_grandtab_raw()}.
#'
#' @param table A table number (1-11), a tibble from \code{get_grandtab_raw()},
#'   a list of tibbles from \code{get_grandtab_raw()}, or NULL (default) for all 
#'   tables. When piped from \code{get_grandtab_raw()}, the tibble/list is 
#'   suppressed and only footnotes are returned.
#' @param footnote Footnote number(s) to retrieve. Accepts an integer vector
#'   (e.g., \code{3} or \code{c(1, 3)}) or a character vector using GrandTab
#'   notation (e.g., \code{"3/"} or \code{c("1/", "3/")}). \code{NULL} (default)
#'   returns all footnotes for the indicated table(s).
#'
#' @return A character string (single table) or named list of character strings
#'   (multiple tables). Multi-footnote results are returned as a single character 
#'   string, with individual footnotes separated by line breaks.
#' @export
#'
#' @examples
#' # Standalone
#' get_column_notes(2, 3) # Retrieve footnote 3 for Table 2
#' get_column_notes(table = 4) # Retrieve all footnotes for Table 4
#' get_column_notes(footnote = "1/") # Retrieve footnote 1 for all tables that have it
#'
#' # Piped from get_grandtab_raw()
#' # get_grandtab_raw(3) %>% get_column_notes() # Retrieve all footnotes for Table 3 
#' # get_grandtab_raw(2) %>% get_column_notes("3/") # Retrieve footnote 3 for Table 2
#' # get_grandtab_raw() %>% get_column_notes() # Retrieve all footnotes for all tables
get_column_notes <- function(table = NULL, footnote = NULL) {

  titles <- vapply(grandtab_detail, `[[`, character(1), 1L)

  # -- Parse footnote argument ------------------------------------------------
  if (!is.null(footnote)) {
    if (is.character(footnote)) {
      footnote <- as.integer(gsub("/.*$", "", footnote))
    }
    footnote <- as.integer(footnote)
    if (any(is.na(footnote))) stop("Could not parse footnote argument to integer(s)")
  }


  # -- Resolve table number(s) ------------------------------------------------
  # Strip grandtab_raw class (from get_grandtab_raw print method) before matching
  if (inherits(table, "grandtab_raw"))
    class(table) <- setdiff(class(table), "grandtab_raw")

  if (is.data.frame(table)) {
    tbl_num <- attr(table, "grandtab_table")
    if (is.null(tbl_num)) {
      # Match piped tibble against grandtab_detail tables
      for (i in seq_along(grandtab_detail)) {
        if (identical(table, grandtab_detail[[i]][[2]])) {
          tbl_num <- i
          break
        }
      }
    }
    if (is.null(tbl_num))
      stop("Cannot determine table number from piped data.")
    tbl_nums <- as.integer(tbl_num)

  } else if (is.list(table) && !is.data.frame(table)) {
    # List of tibbles from get_grandtab_raw()
    tbl_nums <- vapply(table, function(x) {
      n <- attr(x, "grandtab_table")
      if (!is.null(n)) return(as.integer(n))
      # Match against grandtab_detail tables
      for (i in seq_along(grandtab_detail)) {
        if (identical(x, grandtab_detail[[i]][[2]])) return(as.integer(i))
      }
      NA_integer_
    }, integer(1))

    if (any(is.na(tbl_nums))) {
      # Fall back to list names
      nms <- names(table)
      if (!is.null(nms)) {
        num_nms <- suppressWarnings(as.integer(nms))
        if (!any(is.na(num_nms))) {
          tbl_nums <- num_nms
        } else {
          matches <- match(nms, titles)
          if (!any(is.na(matches))) {
            tbl_nums <- matches
          } else {
            stop("Cannot determine table numbers from piped list.")
          }
        }
      } else {
        stop("Cannot determine table numbers from piped list.")
      }
    }

  } else if (is.numeric(table)) {
    tbl_nums <- as.integer(table)

  } else if (is.null(table)) {
    tbl_nums <- seq_along(.footnotes)

  } else {
    stop("'table' must be a table number, a tibble from get_grandtab_raw(), or NULL")
  }

  # Validate table numbers
  bad <- tbl_nums[tbl_nums < 1 | tbl_nums > length(.footnotes)]
  if (length(bad) > 0) stop("Invalid table number(s): ", paste(bad, collapse = ", "))


  # -- Build output -----------------------------------------------------------
  result <- lapply(tbl_nums, function(tn) {
    fns <- .footnotes[[tn]]

    # Table with no footnotes
    if (length(fns) == 0) {
      return(paste0("Table ", tn, " - `", titles[tn], "` has no footnotes."))
    }

    if (!is.null(footnote)) {
      available <- as.integer(names(fns))
      found <- intersect(footnote, available)
      missing <- setdiff(footnote, available)

      if (length(found) == 0) {
        return(paste0(
          "Table ", tn, " does not have footnote(s) ",
          paste(footnote, collapse = ", "), ". ",
          "Available: ", paste(available, collapse = ", "), "."
        ))
      }
      if (length(missing) > 0) {
        warning(
          "Table ", tn, " does not have footnote(s) ",
          paste(missing, collapse = ", "),
          call. = FALSE
        )
      }
      selected <- fns[as.character(found)]
      paste(paste0(found, "/ ", selected), collapse = "\n\n")
    } else {
      nums <- as.integer(names(fns))
      paste(paste0(nums, "/ ", fns), collapse = "\n\n")
    }
  })

  names(result) <- titles[tbl_nums]

  if (length(result) == 1) {
    cat(result[[1]], "\n")
    invisible(result[[1]])
  } else {
    for (nm in names(result)) {
      cat(nm, "\n", result[[nm]], "\n\n", sep = "")
    }
    invisible(result)
  }
}
