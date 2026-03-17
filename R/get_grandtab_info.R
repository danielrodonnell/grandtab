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
  path <- system.file("extdata", "GrandTab_Documentation.txt",
                      package = "grandtab")

  # Fall back for development mode (working directory is package root)
  if (!nzchar(path) || !file.exists(path)) {
    path <- file.path("inst", "extdata", "GrandTab_Documentation.txt")
  }

  if (!nzchar(path) || !file.exists(path)) {
    stop("GrandTab_Documentation.txt not found. ",
         "Please reinstall the grandtab package.", call. = FALSE)
  }

  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    system2("open", c("-t", shQuote(path)))
  } else if (.Platform$OS.type == "windows") {
    shell.exec(path)
  } else {
    # Linux and other Unix-like systems
    system2("xdg-open", shQuote(path))
  }

  invisible(path)
}
