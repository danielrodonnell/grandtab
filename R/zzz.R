.onAttach <- function(libname, pkgname) {
  if (!interactive()) return(invisible())

  tryCatch(
    check_grandtab_updates(),
    error = function(e) invisible()
  )
}

check_grandtab_updates <- function() {
  installed_sha <- utils::packageDescription("grandtab")$RemoteSha
  if (is.null(installed_sha)) return(invisible())

  # Query GitHub API for latest commit SHA on main
  api_url <- "https://api.github.com/repos/danielrodonnell/grandtab/commits/main"

  pat <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  headers <- if (nzchar(pat)) {
    c("User-Agent" = "grandtab", "Authorization" = paste("token", pat))
  } else {
    c("User-Agent" = "grandtab")
  }

  con <- url(api_url, headers = headers)
  on.exit(try(close(con), silent = TRUE))
  resp <- paste(readLines(con, warn = FALSE), collapse = "")

  # Extract SHA from JSON response
  sha_match <- regmatches(resp, regexpr('"sha"\\s*:\\s*"[a-f0-9]{40}"', resp))
  if (length(sha_match) == 0) return(invisible())
  latest_sha <- sub('.*"([a-f0-9]{40})"', "\\1", sha_match[1])

  # Compare installed SHA with latest
  if (identical(installed_sha, latest_sha)) return(invisible())

  packageStartupMessage(
    "The GrandTab database has been updated since you installed the grandtab package!\n",
    "Would you like to update your grandtab package to contain the most current data?"
  )

  choice <- utils::menu(c("Yes", "No"))

  if (choice == 1) {
    remotes::install_github("danielrodonnell/grandtab")
    # Unload and reload to pick up the updated data
    unloadNamespace("grandtab")
    library(grandtab)
    packageStartupMessage("Your local GrandTab database is now current!")
  } else {
    packageStartupMessage(
      "Your local Grandtab database has NOT been updated and may not contain the most current data."
    )
  }
}
