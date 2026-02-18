#' Look up a GrandTab table by number or keyword
#'
#' @param table A number (1-10) or a character keyword to match against table
#'   titles. Keywords are case-insensitive and support partial matching.
#' @return A data frame for the matched table (invisibly if printed with title)
#' @export
#' @examples
#' grandtab("winter")
#' grandtab(3)
#' grandtab("section 2")
grandtab <- function(table) {
  titles <- vapply(grandtab_detail, `[[`, character(1), 1L)

  if (is.numeric(table)) {
    idx <- as.integer(table)
    if (idx < 1 || idx > length(titles)) {
      stop("Table number must be between 1 and ", length(titles))
    }
    message(titles[idx])
    return(grandtab_detail[[idx]][[2]])
  }

  if (!is.character(table) || length(table) != 1) {
    stop("'table' must be a single number or keyword")
  }

  # Exclude late-fall from "fall" matches (late-fall requires explicit mention)
  matches <- grep(table, titles, ignore.case = TRUE)
  if (grepl("^fall", table, ignore.case = TRUE)) {
    matches <- matches[!grepl("late-fall", titles[matches], ignore.case = TRUE)]
  }

  if (length(matches) == 1) {
    message(titles[matches])
    return(grandtab_detail[[matches]][[2]])
  }

  if (length(matches) == 0) {
    message("No tables matched '", table, "'. Available tables:")
    for (i in seq_along(titles)) message("  ", i, ": ", titles[i])
    return(invisible(NULL))
  }

  if (!interactive()) {
    message("'", table, "' matched multiple tables. Please be more specific:")
    for (i in matches) message("  ", i, ": ", titles[i])
    return(invisible(NULL))
  }

  choice <- utils::menu(
    choices = titles[matches],
    title = paste0("'", table, "' matched multiple tables:")
  )

  if (choice == 0) return(invisible(NULL))

  selected <- matches[choice]
  message(titles[selected])
  grandtab_detail[[selected]][[2]]
}
