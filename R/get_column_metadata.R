# ============================================================================
# get_column_metadata.R
#
# Returns a tibble of column metadata for get_escapement() output.
# Can be used standalone (same arguments as get_escapement()) or piped from
# get_escapement().
# ============================================================================

#' @include get_escapement.R

# -- Column descriptions per table -------------------------------------------
# Each element is a named character vector: output_col_name -> description.
# Common columns (run_year, provisional_data, spawn_period) are defined once.

.common_meta <- c(
  run_year         = "Run year",
  provisional_data = paste(
    "1 if data are preliminary (provisional) and subject to revision,",
    "0 otherwise"
  ),
  lf_spawn_period  = "Late-fall run spawning period",
  w_spawn_period   = "Winter run spawning period"
)

.col_desc <- list(

  # ---- Table 1, all runs (summary = TRUE with no run filter) ----
  t1_all = c(
    lf_hatch = "Late-fall run total hatchery escapement (Central Valley system)",
    lf_inr   = "Late-fall run total in-river escapement (Central Valley system)",
    lf_total = "Late-fall run total escapement (Central Valley system)",
    w_total  = "Winter run total escapement (Central Valley system)",
    w_rbdd   = "Winter run fish counted passing Red Bluff Diversion Dam",
    s_hatch  = "Spring run total hatchery escapement (Central Valley system)",
    s_inr    = "Spring run total in-river escapement (Central Valley system)",
    s_total  = "Spring run total escapement (Central Valley system)",
    f_hatch  = "Fall run total hatchery escapement (Central Valley system)",
    f_inr    = "Fall run total in-river escapement (Central Valley system)",
    f_total  = "Fall run total escapement (Central Valley system)"
  ),

  # ---- Table 1, single run summaries ----
  t1_lf = c(
    lf_hatch = "Late-fall run total hatchery escapement (Central Valley system)",
    lf_inr   = "Late-fall run total in-river escapement (Central Valley system)",
    lf_total = "Late-fall run total escapement (Central Valley system)"
  ),
  t1_w = c(
    w_total = "Winter run total escapement (Central Valley system)",
    w_rbdd  = "Winter run fish counted passing Red Bluff Diversion Dam"
  ),
  t1_s = c(
    s_hatch = "Spring run total hatchery escapement (Central Valley system)",
    s_inr   = "Spring run total in-river escapement (Central Valley system)",
    s_total = "Spring run total escapement (Central Valley system)"
  ),
  t1_f = c(
    f_hatch = "Fall run total hatchery escapement (Central Valley system)",
    f_inr   = "Fall run total in-river escapement (Central Valley system)",
    f_total = "Fall run total escapement (Central Valley system)"
  ),

  # ---- Table 2: Late-Fall Run ----
  t2 = c(
    lf_sac_main_up_rbdd_inr       = "Late-fall run in-river escapement in the Sacramento mainstem upstream of Red Bluff Diversion Dam",
    lf_sac_main_up_rbdd_trans_cnfh= "Late-fall run fish transferred from upstream of RBDD to Coleman National Fish Hatchery",
    lf_sac_main_up_rbdd_total     = "Late-fall run total escapement upstream of Red Bluff Diversion Dam",
    lf_sac_main_dwn_rbdd_trans_tcff="Late-fall run fish transferred from downstream of RBDD to Tehama-Colusa Fish Facility",
    lf_sac_main_dwn_rbdd_inr      = "Late-fall run in-river escapement in the Sacramento mainstem downstream of Red Bluff Diversion Dam",
    lf_sac_main_dwn_rbdd_total    = "Late-fall run total escapement downstream of Red Bluff Diversion Dam",
    lf_battle_up_cnfh_inr         = "Late-fall run in-river escapement in Battle Creek upstream of Coleman National Fish Hatchery",
    lf_battle_cnfh                = "Late-fall run escapement at Coleman National Fish Hatchery (Battle Creek component)",
    lf_battle_total               = "Late-fall run total escapement in Battle Creek",
    lf_clear                      = "Late-fall run escapement in Clear Creek",
    lf_cottonwood                 = "Late-fall run escapement in Cottonwood Creek",
    lf_salt                       = "Late-fall run escapement in Salt Creek",
    lf_craig                      = "Late-fall run escapement in Craig Creek",
    lf_feather_inr                = "Late-fall run in-river escapement in the Feather River",
    lf_feather_hatch              = "Late-fall run escapement at Feather River Fish Hatchery",
    lf_total                      = "Late-fall run total escapement (Central Valley system)"
  ),

  # ---- Table 3: Winter Run ----
  t3 = c(
    w_sac_main_up_rbdd    = "Winter run in-river escapement in the Sacramento mainstem upstream of Red Bluff Diversion Dam",
    w_sac_main_dwn_rbdd   = "Winter run in-river escapement in the Sacramento mainstem downstream of Red Bluff Diversion Dam",
    w_sac_main_total_inr  = "Winter run total in-river escapement in the Sacramento mainstem",
    w_sac_main_trans_cnfh = "Winter run fish transferred to Coleman National Fish Hatchery",
    w_sac_main_trans_lsnfh= "Winter run fish transferred to Livingston Stone National Fish Hatchery",
    w_clear               = "Winter run escapement in Clear Creek",
    w_battle              = "Winter run escapement in Battle Creek",
    w_sac_sys_total       = "Winter run total escapement in the Sacramento River system",
    w_calaveras           = "Winter run escapement in the Calaveras River (San Joaquin system)",
    w_cv_sys_total        = "Winter run total escapement in the Central Valley system"
  ),

  # ---- Table 4: Winter Run supplementary data ----
  t4 = c(
    w_fish_passing_rbdd  = "Winter run fish counted passing Red Bluff Diversion Dam (supporting data used to derive mainstem in-river estimate)",
    w_angler_harvest     = "Estimated angler harvest of winter run",
    w_redd_dist_up_rbdd  = "Proportion of winter run redds distributed upstream of Red Bluff Diversion Dam",
    w_redd_dist_dwn_rbdd = "Proportion of winter run redds distributed downstream of Red Bluff Diversion Dam"
  ),

  # ---- Table 5: Spring Run ----
  t5 = c(
    s_sac_main_up_rbdd  = "Spring run in-river escapement upstream of Red Bluff Diversion Dam",
    s_sac_main_dwn_rbdd = "Spring run in-river escapement downstream of Red Bluff Diversion Dam",
    s_sac_main_total_inr= "Spring run total in-river escapement in the Sacramento mainstem",
    s_battle            = "Spring run escapement in Battle Creek",
    s_clear             = "Spring run escapement in Clear Creek",
    s_cottonwood        = "Spring run escapement in Cottonwood Creek",
    s_antelope          = "Spring run escapement in Antelope Creek",
    s_mill              = "Spring run escapement in Mill Creek",
    s_mill_spwn_snrkl   = "Spring run snorkel survey spawner estimate in Mill Creek",
    s_deer              = "Spring run escapement in Deer Creek",
    s_deer_spwn_snrkl   = "Spring run snorkel survey spawner estimate in Deer Creek",
    s_big_chico         = "Spring run escapement in Big Chico Creek",
    s_butte             = "Spring run escapement in Butte Creek",
    s_butte_spwn_snrkl  = "Spring run snorkel survey spawner estimate in Butte Creek",
    s_feather_inr       = "Spring run in-river escapement in the Feather River",
    s_feather_hatch     = "Spring run escapement at Feather River Fish Hatchery",
    s_feather_total     = "Spring run total escapement in the Feather River",
    s_yuba              = "Spring run escapement in the Yuba River",
    s_total             = "Spring run total escapement (Central Valley system)"
  ),

  # ---- Table 6: Fall Run Summary I ----
  t6 = c(
    f_sac_sys_hatch     = "Fall run hatchery escapement in the Sacramento River system",
    f_sac_sys_inr       = "Fall run in-river escapement in the Sacramento River system",
    f_sac_sys_tribs_inr = "Fall run tributary in-river escapement in the Sacramento River system",
    f_sac_sys_total     = "Fall run total escapement in the Sacramento River system",
    f_sj_sys_hatch      = "Fall run hatchery escapement in the San Joaquin River system",
    f_sj_sys_tribs_inr  = "Fall run tributary in-river escapement in the San Joaquin River system",
    f_sj_sys_total      = "Fall run total escapement in the San Joaquin River system",
    f_sac_sj_sys_hatch  = "Fall run hatchery escapement combined across Sacramento and San Joaquin systems",
    f_sac_sj_sys_inr    = "Fall run in-river escapement combined across Sacramento and San Joaquin systems",
    f_sac_sj_sys_total  = "Fall run total escapement combined across Sacramento and San Joaquin systems"
  ),

  # ---- Table 7: Fall Run Summary II ----
  t7 = c(
    f_kesw_rbdd_cnfh        = "Fall run escapement at Coleman National Fish Hatchery (Keswick Dam to RBDD section)",
    f_kesw_rbdd_main_inr    = "Fall run Sacramento mainstem in-river escapement from Keswick Dam to RBDD",
    f_kesw_rbdd_tribs_inr   = "Fall run tributary in-river escapement from Keswick Dam to RBDD",
    f_kesw_rbdd_total       = "Fall run total escapement from Keswick Dam to Red Bluff Diversion Dam",
    f_rbdd_pferry_tcff      = "Fall run escapement at Tehama-Colusa Fish Facility (RBDD to Princeton Ferry section)",
    f_rbdd_pferry_main_inr  = "Fall run Sacramento mainstem in-river escapement from RBDD to Princeton Ferry",
    f_rbdd_pferry_tribs_inr = "Fall run tributary in-river escapement from RBDD to Princeton Ferry",
    f_rbdd_pferry_total     = "Fall run total escapement from Red Bluff Diversion Dam to Princeton Ferry",
    f_feather_nimbus_hatch  = "Fall run combined escapement at Feather River Hatchery and Nimbus Hatchery (Princeton Ferry to Sacramento section)",
    f_pferry_sac_tribs_inr  = "Fall run tributary in-river escapement from Princeton Ferry to Sacramento",
    f_pferry_sac_total      = "Fall run total escapement from Princeton Ferry to Sacramento"
  ),

  # ---- Table 8: Fall Run Section 1 (Keswick Dam to RBDD) ----
  t8 = c(
    f_sac_main_inr        = "Fall run in-river escapement in the Sacramento mainstem (Keswick Dam to RBDD)",
    f_sac_main_trans_cnfh = "Fall run fish transferred from Sacramento mainstem to Coleman National Fish Hatchery",
    f_sac_main_total      = "Fall run total escapement in the Sacramento mainstem (Keswick Dam to RBDD)",
    f_battle_cnfh         = "Fall run escapement at Coleman National Fish Hatchery (Battle Creek component)",
    f_battle_dwn_cnfh_inr = "Fall run in-river escapement in Battle Creek downstream of Coleman NFH",
    f_battle_up_cnfh_inr  = "Fall run in-river escapement in Battle Creek upstream of Coleman NFH",
    f_battle_total        = "Fall run total escapement in Battle Creek",
    f_clear               = "Fall run escapement in Clear Creek",
    f_spring_gulch        = "Fall run escapement in Spring Gulch",
    f_china_gulch         = "Fall run escapement in China Gulch",
    f_olney               = "Fall run escapement in Olney Creek",
    f_cottonwood          = "Fall run escapement in Cottonwood Creek",
    f_paynes              = "Fall run escapement in Paynes Creek",
    f_cow                 = "Fall run escapement in Cow Creek",
    f_bear_ck             = "Fall run escapement in Bear Creek",
    f_ash                 = "Fall run escapement in Ash Creek",
    f_stillwater          = "Fall run escapement in Stillwater Creek",
    f_inks                = "Fall run escapement in Inks Creek",
    f_other               = "Fall run escapement in other unnamed tributaries (Keswick Dam to RBDD)"
  ),

  # ---- Table 9: Fall Run Section 2 (RBDD to Princeton Ferry) ----
  t9 = c(
    f_sac_main_inr        = "Fall run in-river escapement in the Sacramento mainstem (RBDD to Princeton Ferry)",
    f_sac_main_trans_tcff = "Fall run fish transferred from Sacramento mainstem to Tehama-Colusa Fish Facility",
    f_sac_main_total      = "Fall run total escapement in the Sacramento mainstem (RBDD to Princeton Ferry)",
    f_tcff                = "Fall run escapement at Tehama-Colusa Fish Facility",
    f_salt                = "Fall run escapement in Salt Creek",
    f_antelope            = "Fall run escapement in Antelope Creek",
    f_craig               = "Fall run escapement in Craig Creek",
    f_dye                 = "Fall run escapement in Dye Creek",
    f_mill                = "Fall run escapement in Mill Creek",
    f_toomes              = "Fall run escapement in Toomes Creek",
    f_thomes              = "Fall run escapement in Thomes Creek",
    f_deer                = "Fall run escapement in Deer Creek",
    f_coyote              = "Fall run escapement in Coyote Creek",
    f_stoney              = "Fall run escapement in Stoney Creek",
    f_singer              = "Fall run escapement in Singer Creek",
    f_big_chico           = "Fall run escapement in Big Chico Creek",
    f_other               = "Fall run escapement in other unnamed tributaries (RBDD to Princeton Ferry)"
  ),

  # ---- Table 10: Fall Run Section 3 (Princeton Ferry to Sacramento) ----
  t10 = c(
    f_butte               = "Fall run escapement in Butte Creek",
    f_feather_hatch       = "Fall run escapement at Feather River Fish Hatchery",
    f_feather_inr         = "Fall run in-river escapement in the Feather River",
    f_feather_total       = "Fall run total escapement in the Feather River",
    f_yuba                = "Fall run escapement in the Yuba River",
    f_bear_r              = "Fall run escapement in the Bear River",
    f_dry                 = "Fall run escapement in Dry Creek",
    f_american_nimbus_hatch = "Fall run escapement at Nimbus Hatchery on the American River",
    f_american_inr        = "Fall run in-river escapement in the American River",
    f_american_total      = "Fall run total escapement in the American River",
    f_natomas_drain       = "Fall run escapement in Natomas Drain"
  ),

  # ---- Table 11: Fall Run Section 4 (San Joaquin system) ----
  t11 = c(
    f_cosumnes      = "Fall run escapement in the Cosumnes River",
    f_mokelumne_hatch = "Fall run escapement at Mokelumne River Fish Hatchery",
    f_mokelumne_inr   = "Fall run in-river escapement in the Mokelumne River",
    f_mokelumne_total = "Fall run total escapement in the Mokelumne River",
    f_stanislaus    = "Fall run escapement in the Stanislaus River",
    f_tuolumne      = "Fall run escapement in the Tuolumne River",
    f_merced_hatch  = "Fall run escapement at Merced River Fish Hatchery",
    f_merced_inr    = "Fall run in-river escapement in the Merced River",
    f_merced_total  = "Fall run total escapement in the Merced River"
  )
)


# -- Raw name lookup (built at load time from .col_maps) ---------------------
# .raw_lookup[[tkey]][[col_name]] -> grandtab_raw column header

.raw_lookup <- local({
  run_map     <- c(`2`="lf", `3`="w", `4`="w", `5`="s", `6`="f", `7`="f",
                   `8`="f",  `9`="f", `10`="f", `11`="f")
  year_raw    <- c(`2`="Late-Fall YEAR", `3`="Winter YEAR", `4`="Winter YEAR",
                   `5`="YEAR", `6`="YEAR", `7`="YEAR",
                   `8`="YEAR", `9`="YEAR", `10`="YEAR", `11`="YEAR")
  result <- list()

  for (tnum in names(.col_maps)) {
    ra   <- run_map[[tnum]]
    tkey <- paste0("t", tnum)
    cm   <- .col_maps[[tnum]]
    # run_year is derived (not a raw column) when the first col_map entry is
    # "spawn_period" (Late-Fall and Winter tables); otherwise it maps to "YEAR".
    first_snake <- unname(cm[1])
    tbl  <- list(run_year = if (first_snake == "spawn_period") NA_character_
                            else year_raw[[tnum]])

    for (i in seq_along(cm)) {
      raw   <- names(cm)[i]
      snake <- unname(cm[i])
      if (snake == "run_year") next          # already captured above
      full  <- if (snake == "spawn_period") paste0(ra, "_spawn_period")
               else                         paste0(ra, "_", snake)
      tbl[[full]] <- raw
    }
    result[[tkey]] <- tbl
  }

  # Table 1 (ALL RUNS) is not in .col_maps — add raw name mappings manually
  # from .prepare_full_t1() and .t1_run_cols.
  result[["t1_all"]] <- list(
    run_year        = "YEAR",
    lf_spawn_period = "Late-Fall YEAR",
    lf_hatch        = "Late-Fall Hatch",
    lf_inr          = "Late-Fall In-R",
    lf_total        = "Late-Fall TOTAL",
    w_spawn_period  = "Winter YEAR",
    w_total         = "Winter TOTAL",
    w_rbdd          = "Winter RBDD",
    s_hatch         = "Spring Hatch",
    s_inr           = "Spring In-R",
    s_total         = "Spring TOTAL",
    f_hatch         = "Fall Hatch",
    f_inr           = "Fall In-R",
    f_total         = "Fall TOTAL"
  )
  result[["t1_lf"]] <- list(
    run_year        = NA_character_,
    lf_spawn_period = "Late-Fall YEAR",
    lf_hatch        = "Late-Fall Hatch",
    lf_inr          = "Late-Fall In-R",
    lf_total        = "Late-Fall TOTAL"
  )
  result[["t1_w"]] <- list(
    run_year       = NA_character_,
    w_spawn_period = "Winter YEAR",
    w_total        = "Winter TOTAL",
    w_rbdd         = "Winter RBDD"
  )
  result[["t1_s"]] <- list(
    run_year = "YEAR",
    s_hatch  = "Spring Hatch",
    s_inr    = "Spring In-R",
    s_total  = "Spring TOTAL"
  )
  result[["t1_f"]] <- list(
    run_year = "YEAR",
    f_hatch  = "Fall Hatch",
    f_inr    = "Fall In-R",
    f_total  = "Fall TOTAL"
  )

  result
})


# -- Table identification ----------------------------------------------------
# Identify which .col_desc key best matches a set of column names.
# Uses fingerprint columns that uniquely distinguish each table.

.fingerprints <- list(
  t1_all = c("lf_hatch", "w_total", "s_hatch", "f_hatch"),
  t1_lf  = c("lf_hatch",  "lf_inr",  "lf_total"),
  t1_w   = c("w_total",   "w_rbdd"),
  t1_s   = c("s_hatch",   "s_inr",   "s_total"),
  t1_f   = c("f_hatch",   "f_inr",   "f_total"),
  t2  = c("lf_sac_main_up_rbdd_inr"),
  t3  = c("w_sac_main_up_rbdd", "w_cv_sys_total"),
  t4  = c("w_fish_passing_rbdd", "w_redd_dist_up_rbdd"),
  t5  = c("s_sac_main_up_rbdd", "s_total"),
  t6  = c("f_sac_sys_hatch", "f_sj_sys_total"),
  t7  = c("f_kesw_rbdd_cnfh", "f_rbdd_pferry_tcff"),
  t8  = c("f_battle_cnfh",    "f_spring_gulch"),
  t9  = c("f_tcff",           "f_dye"),
  t10 = c("f_american_inr",   "f_bear_r"),
  t11 = c("f_stanislaus",     "f_cosumnes")
)

.identify_table_key <- function(col_names) {
  # Score each known table by how many of its fingerprint columns appear
  scores <- vapply(.fingerprints, function(fp) {
    sum(fp %in% col_names)
  }, integer(1))
  best <- which(scores == max(scores) & scores > 0)
  if (length(best) == 0) return(NULL)
  names(best)[1]
}


# -- Per-column lookup helpers -----------------------------------------------

.lookup_raw_name <- function(col, tkey) {
  if (col %in% c("run_year", "provisional_data")) {
    if (col == "run_year") {
      if (is.null(tkey)) return(NA_character_)
      return(.raw_lookup[[tkey]][["run_year"]] %||% NA_character_)
    }
    if (col == "provisional_data") return(NA_character_)
  }
  if (!is.null(tkey) && col %in% names(.raw_lookup[[tkey]])) {
    return(.raw_lookup[[tkey]][[col]])
  }
  # Fall back: search all tables
  for (tk in names(.raw_lookup)) {
    if (col %in% names(.raw_lookup[[tk]])) return(.raw_lookup[[tk]][[col]])
  }
  NA_character_
}

.lookup_description <- function(col, tkey) {
  if (col %in% names(.common_meta))           return(unname(.common_meta[[col]]))
  if (!is.null(tkey)) {
    descs <- .col_desc[[tkey]]
    if (!is.null(descs) && col %in% names(descs)) return(unname(descs[[col]]))
  }
  # Fall back: search all tables (ambiguous, but better than NA)
  for (tk in names(.col_desc)) {
    if (col %in% names(.col_desc[[tk]])) return(unname(.col_desc[[tk]][[col]]))
  }
  NA_character_
}

# Null-coalescing helper (avoids importing rlang)
`%||%` <- function(a, b) if (!is.null(a)) a else b


# -- Build metadata tibble from a single escapement tibble -------------------

.build_col_meta <- function(tbl) {
  col_names <- names(tbl)
  tkey      <- .identify_table_key(col_names)

  tibble::tibble(
    column_name       = col_names,
    column_position   = seq_along(col_names),
    grandtab_raw_name = vapply(col_names, .lookup_raw_name,  character(1), tkey = tkey),
    description       = vapply(col_names, .lookup_description, character(1), tkey = tkey)
  )
}


# -- Main function -----------------------------------------------------------

#' Retrieve column metadata for get_escapement() output
#'
#' Returns a tibble of column metadata describing the columns that
#' \code{\link{get_escapement}} would return for the same arguments. Can be
#' used standalone or piped from \code{get_escapement()}.
#'
#' @param x A tibble or named list of tibbles from \code{get_escapement()},
#'   when piped. Leave \code{NULL} (default) for standalone use.
#' @param run Character. Salmon run: \code{"lf"}, \code{"w"}, \code{"s"},
#'   \code{"f"}, \code{"all"}, or \code{NULL}. Passed to
#'   \code{get_escapement()}.
#' @param river_system Character. River system filter. Passed to
#'   \code{get_escapement()}.
#' @param location Character. Location name(s). Passed to
#'   \code{get_escapement()}.
#' @param summary Logical. Summary-level data. Passed to
#'   \code{get_escapement()}.
#' @param section Integer 1--4. Fall run section. Passed to
#'   \code{get_escapement()}.
#' @param run_years Ignored (column structure does not depend on year). Kept
#'   for consistency with \code{get_escapement()}.
#'
#' @return A tibble with columns \code{column_name}, \code{column_position},
#'   \code{grandtab_raw_name}, and \code{description}. Returns a named list of
#'   such tibbles when the underlying query returns multiple tables.
#' @export
#'
#' @examples
#' # Standalone
#' get_column_metadata(run = "lf")
#' get_column_metadata(run = "s", location = "battle")
#' get_column_metadata(run = "f", section = 1)
#' get_column_metadata(summary = TRUE)
#'
#' # Piped from get_escapement()
#' # get_escapement(run = "w") |> get_column_metadata()
#' # get_escapement(run = "f", section = 2) |> get_column_metadata()
get_column_metadata <- function(x = NULL, run = NULL, river_system = NULL,
                                location = NULL, summary = NULL,
                                section = NULL, run_years = NULL) {

  if (!is.null(x) && (is.data.frame(x) ||
                       (is.list(x) && !is.data.frame(x)))) {
    result <- x
  } else {
    # run_years intentionally omitted — column structure is year-independent
    result <- get_escapement(run = run, river_system = river_system,
                             location = location, summary = summary,
                             section = section)
  }

  if (is.null(result)) return(invisible(NULL))

  if (is.data.frame(result)) {
    return(.build_col_meta(result))
  }

  lapply(result, .build_col_meta)
}
