#' Retrieve raw GrandTab tables
#'
#' Returns unaltered GrandTab tables (almost) exactly as they appear in the
#' online GrandTab PDF. When called with no arguments, \code{get_grandtab_raw()}
#' is equivalent to typing \code{\link{grandtab_raw}} directly; use the
#' \code{table} argument to retrieve one or more specific tables. Can be piped
#' to \code{\link{get_column_notes}} to retrieve column footnotes as they
#' appear in the GrandTab PDF.
#'
#' @param table A table identifier. Accepts a \code{numeric} table number or
#'   vector of table numbers of length < 12. If \code{table} is \code{NULL}
#'   (default), returns \code{\link{grandtab_raw}} — all 11 GrandTab tables.
#'
#' @return \code{\link{grandtab_raw}} when \code{table = NULL}; a named list of
#'   tibbles when \code{table} is a vector; a named list of length 1 when
#'   \code{table} is a scalar.
#' @export
#'
#' @examples
#' # Single table
#' get_grandtab_raw(2)
#'
#' # Selected tables
#' get_grandtab_raw(2:4)
#'
#' # All tables — equivalent to typing grandtab_raw
#' get_grandtab_raw()
#'
#' # Pipe to get_column_notes
#' # get_grandtab_raw(2) %>% get_column_notes("3/")
get_grandtab_raw <- function(table = NULL) {

  # Treat NA as NULL (return all tables)
  if (!is.null(table) && all(is.na(table))) table <- NULL

  # No table specified: return grandtab_raw directly (same object, same print)
  if (is.null(table)) return(grandtab_raw)

  # Coerce character to integer if possible
  if (is.character(table)) {
    coerced <- suppressWarnings(as.integer(table))
    if (any(is.na(coerced))) {
      stop("'table' must be an integer between 1 and 11, or NULL for all tables.",
           call. = FALSE)
    }
    table <- coerced
  }
  if (!is.numeric(table) && !is.integer(table)) {
    stop("'table' must be an integer between 1 and 11, or NULL for all tables.",
         call. = FALSE)
  }
  table <- as.integer(table)
  bad <- table[table < 1L | table > 11L]
  if (length(bad) > 0)
    stop("'table' must be between 1 and 11. Invalid value(s): ",
         paste(bad, collapse = ", "), ".", call. = FALSE)

  gt <- try(lapply(grandtab_raw[table], \(x) x[[2]]))
  names(gt) <- try(lapply(grandtab_raw[table], \(x) x[[1]]))

  if (inherits(gt, "try-error") | inherits(names(gt), "try-error") |
      is.null(gt) | is.null(names(gt))) {
    message(
      "There is a problem with the GrandTab dataset. Please submit a\n",
      "GitHub issue at https://github.com/danielrodonnell/grandtab/issues\n",
      "with the error message below and a description of what you were\n",
      "doing when the error occurred."
    )
    return(NULL)
  }

  class(gt) <- c("grandtab_raw", class(gt))
  gt
}


#' @export
print.grandtab_raw <- function(x, ...) {
  message(
    "These are the raw, unaltered tables as they appear on the GrandTab ",
    "website. Type grandtab:: to see available functions for pulling curated, ",
    "combined, or individual Central Valley Chinook population time series. Use",
    " get_column_notes() to retrieve footnotes (indicated by '*/' in column ",
    "headers) for these tables. Data for [Year in brackets] are preliminary. ",
    "NA values indicate no data."
  )
  # grandtab_raw is stored as a nested list(list(title, df), ...).
  # get_grandtab_raw(table=...) returns a flat named list of DFs.
  # Normalise to flat named list of DFs before printing.
  if (length(x) > 0 && is.list(x[[1]]) && !is.data.frame(x[[1]])) {
    nms <- vapply(x, `[[`, character(1), 1L)
    dfs <- lapply(x, `[[`, 2L)
    names(dfs) <- nms
    print(dfs, ...)
  } else {
    class(x) <- setdiff(class(x), "grandtab_raw")
    print(x, ...)
  }
}
