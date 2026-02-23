#' Open GrandTab documentation
#'
#' Opens the GrandTab introductory documentation (RTF file) in the system
#' default application. The text is extracted from the front matter of the
#' GrandTab PDF and is automatically updated when the PDF changes.
#'
#' @return The file path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' get_grandtab_info()
#' }
get_grandtab_info <- function() {
  path <- system.file("extdata", "GrandTab Documentation.rtf",
                      package = "grandtab")

  # Fall back for development mode (working directory is package root)
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "GrandTab Documentation.rtf")
  }

  if (!nzchar(path) || !file.exists(path)) {
    stop("GrandTab Documentation.rtf not found. ",
         "Please reinstall the grandtab package.", call. = FALSE)
  }

  os <- .Platform$OS.type
  if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", shQuote(path))
  } else if (os == "windows") {
    shell.exec(path)
  } else {
    system2("xdg-open", shQuote(path))
  }

  invisible(path)
}
