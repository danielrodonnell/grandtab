#' @include get_escapement.R
NULL

#' Retrieve Winter Run supplementary data
#'
#' Returns the supplementary Winter Run data from Table 4 (fish passing RBDD,
#' angler harvest, and redd distribution above/below RBDD). These columns were
#' split from the main Winter Run table (Table 3) because they are supporting
#' data used to derive the mainstem in-river estimate.
#'
#' @param run_years Numeric vector of years to filter results to, or
#'   \code{NULL} (default) to return all years.
#' @return A tibble with columns: run_year, w_spawn_period,
#'   w_fish_passing_rbdd, w_angler_harvest, w_redd_dist_up_rbdd,
#'   w_redd_dist_dwn_rbdd, provisional_data.
#' @export
get_winter_extras <- function(run_years = NULL) {
  raw <- grandtab_detail[[4]][[2]]

  out <- tibble::tibble(
    run_year            = .derive_run_year(raw[["Winter YEAR"]]),
    w_spawn_period      = raw[["Winter YEAR"]],
    w_fish_passing_rbdd = raw[["Fish Passing RBDD 1/3/"]],
    w_angler_harvest    = raw[["Angler Harvest 1/4/"]],
    w_redd_dist_up_rbdd = raw[["Redd Dist2/ Up RBDD"]],
    w_redd_dist_dwn_rbdd = raw[["Redd Dist2/ Dwn RBDD"]]
  )

  .filter_run_years(.finalize(out), run_years)
}
