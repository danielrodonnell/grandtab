#' Raw GrandTab tables
#'
#' A list of 11 elements, each representing one GrandTab data table. Element
#' names (index 1--11) correspond to GrandTab table numbers. Each element is
#' itself a list with two components:
#' \describe{
#'   \item{[[1]]}{Character scalar: the table title.}
#'   \item{[[2]]}{Data frame: the table data with original GrandTab column headers.}
#' }
#' Tables 1--11 cover all four Central Valley Chinook salmon runs:
#' \describe{
#'   \item{Table 1}{ALL RUNS: Central Valley summary}
#'   \item{Table 2}{LATE-FALL RUN detail}
#'   \item{Table 3}{WINTER RUN detail}
#'   \item{Table 4}{WINTER RUN supplementary data (fish passing RBDD, redd distribution)}
#'   \item{Table 5}{SPRING RUN detail}
#'   \item{Table 6}{FALL RUN Summary I: Sacramento and San Joaquin River Systems}
#'   \item{Table 7}{FALL RUN Summary II: Sacramento River System}
#'   \item{Table 8}{FALL RUN Section 1: Keswick Dam to Red Bluff Diversion Dam}
#'   \item{Table 9}{FALL RUN Section 2: Red Bluff Diversion Dam to Princeton Ferry}
#'   \item{Table 10}{FALL RUN Section 3: Princeton Ferry to Sacramento}
#'   \item{Table 11}{FALL RUN Section 4: San Joaquin River System}
#' }
#'
#' @source California Department of Fish and Wildlife GrandTab
#'   (\url{https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Escapement})
"grandtab_detail"

#' Condensed raw GrandTab tables (most recent 10 years)
#'
#' Same structure as \code{\link{grandtab_detail}} but containing only the
#' 10 most recent run years of data. Used internally for faster rendering during
#' the CI update process.
#'
#' @source California Department of Fish and Wildlife GrandTab
"grandtab_detail_short"

#' GrandTab data in wide tidy format
#'
#' All 11 GrandTab tables joined into a single wide tibble with 677 rows (one
#' per run year and spawning period combination across all runs) and 112 columns.
#' Column names use the original GrandTab headers. This is a convenience dataset
#' for users who prefer a single wide table; for most analytical use cases,
#' \code{\link{get_escapement}} is recommended.
#'
#' @source California Department of Fish and Wildlife GrandTab
"grandtab_sections"

#' GrandTab Central Valley run summaries
#'
#' A named list with four elements (\code{LATE-FALL}, \code{WINTER},
#' \code{SPRING}, \code{FALL}), each a tibble containing the annual total
#' escapement for that run from Table 1 (ALL RUNS: Central Valley).
#'
#' @source California Department of Fish and Wildlife GrandTab
"grandtab_summary"
