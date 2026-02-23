#' Retrieve raw GrandTab tables
#'
#' Returns unaltered GrandTab tables (almost) exactly as they appear in the 
#' online GrandTab PDF. Can be piped to \code{\link{get_column_notes}} to 
#' retrieve column footnotes as they appear in the GrandTab PDF.
#'
#' @param table A table identifier. Accepts a \code{numeric} table number or 
#'   vector of table numbers of length < 12. If \code{table} is \code{NULL}
#'   (default), returns all 11 GrandTab tables as a named list of tibbles. 
#'
#' @return A tibble (if \code{length(table)} = 1) or a named list of tibbles.
#' @export
#'
#' @examples
#' # Single table
#' get_grandtab_raw(2)
#' 
#' # Selected tables
#' get_grandtab_raw(2:4)
#'
#' # All tables
#' get_grandtab_raw()
#'
#' # Pipe to get_column_notes
#' # get_grandtab_raw(2) %>% get_column_notes("3/)
get_grandtab_raw <- function(table = NULL) {

  if(!is.null(table)) {
      
    gt <- try(lapply(grandtab_detail[table], \(x) x[[2]]))
    names(gt) <- try(lapply(grandtab_detail[table], \(x) x[[1]]))
    
  }else{
    gt <- try(lapply(grandtab_detail, \(x) x[[2]]))
    names(gt) <- try(lapply(grandtab_detail, \(x) x[[1]]))
  }

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
  
  out <- gt

  # if (is.null(table)) {
  #   out <- gt
  # } else {
  #   idx <- if (is.numeric(table)) as.integer(table) else readr::parse_number(table)
  #   out <- gt[[idx]]
  # }

  class(out) <- c("grandtab_raw", class(out))
  out
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
  class(x) <- setdiff(class(x), "grandtab_raw")
  print(x, ...)
}
