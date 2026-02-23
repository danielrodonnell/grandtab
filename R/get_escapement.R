# ============================================================================
# get_escapement.R
#
# User-facing function for retrieving GrandTab Chinook salmon escapement data.
# Supports flexible querying by run, river system, location, summary level,
# and fall run section.
# ============================================================================

# -- Column name mappings per table -------------------------------------------
# Named vectors: grandtab_detail column name -> snake_case base name
# Run prefix (lf_, w_, s_, f_) added dynamically. Special values:
#   "spawn_period" -> {run}_spawn_period
#   "run_year"     -> run_year (no prefix)

.col_maps <- list(
  `2` = c(
    "Late-Fall YEAR"                       = "spawn_period",
    "Sac Main Up RBDD1/ In-R 2/"          = "sac_main_up_rbdd_inr",
    "Sac Main Up RBDD trans CNFH 3/"      = "sac_main_up_rbdd_trans_cnfh",
    "Sac Main Up RBDD TOTAL"               = "sac_main_up_rbdd_total",
    "Sac Main Dwn RBDD trans TCFF 4/"     = "sac_main_dwn_rbdd_trans_tcff",
    "Sac Main Dwn RBDD In-R 2/"           = "sac_main_dwn_rbdd_inr",
    "Sac Main Dwn RBDD TOTAL"              = "sac_main_dwn_rbdd_total",
    "Battle5/ Up CNFH In-R"               = "battle_up_cnfh_inr",
    "Battle CNFH"                           = "battle_cnfh",
    "Battle TOTAL"                          = "battle_total",
    "Clear Ck"                              = "clear",
    "Cottonwood Ck"                         = "cottonwood",
    "Salt Ck"                               = "salt",
    "Craig Ck"                              = "craig",
    "Feather In-R"                          = "feather_inr",
    "Feather Hatch"                         = "feather_hatch",
    "TOTAL LATE-FALL RUN"                   = "total"
  ),
  `3` = c(
    "Winter YEAR"                  = "spawn_period",
    "Sac Main Up RBDD In-R 2/"     = "sac_main_up_rbdd",
    "Sac Main Dwn RBDD In-R 3/"   = "sac_main_dwn_rbdd",
    "Sac Main TOTAL In-R"          = "sac_main_total_inr",
    "Sac Main trans1/ CNFH"       = "sac_main_trans_cnfh",
    "Sac Main trans1/ LSNFH"      = "sac_main_trans_lsnfh",
    "Clear Ck 4/"                  = "clear",
    "Battle Ck 5/"                 = "battle",
    "WINTER TOTAL SAC SYSTEM 6/"  = "sac_sys_total",
    "SJ Calaveras R 7/"           = "calaveras",
    "WINTER TOTAL CV SYSTEM"       = "cv_sys_total"
  ),
  `4` = c(
    "Winter YEAR"              = "spawn_period",
    "Fish Passing RBDD 1/3/"   = "fish_passing_rbdd",
    "Angler Harvest 1/4/"      = "angler_harvest",
    "Redd Dist2/ Up RBDD"      = "redd_dist_up_rbdd",
    "Redd Dist2/ Dwn RBDD"     = "redd_dist_dwn_rbdd"
  ),
  `5` = c(
    "YEAR"                         = "run_year",
    "Sac Main Up RBDD1/ In-R"    = "sac_main_up_rbdd",
    "Sac Main Dwn RBDD In-R"      = "sac_main_dwn_rbdd",
    "Sac Main In-R TOTAL"          = "sac_main_total_inr",
    "Battle Ck 4/"                 = "battle",
    "Clear Ck"                      = "clear",
    "Cottonwood Ck"                 = "cottonwood",
    "Antelope Ck"                   = "antelope",
    "Mill Ck"                       = "mill",
    "Mill Ck Spawn 5/"             = "mill_spwn_snrkl",
    "Deer Ck"                       = "deer",
    "Deer Ck Spawn 5/"             = "deer_spwn_snrkl",
    "Big Chico Ck"                  = "big_chico",
    "Butte Ck 6/"                   = "butte",
    "Butte Ck Spawn 5/"            = "butte_spwn_snrkl",
    "Feather In-R 2/"              = "feather_inr",
    "Feather Hatch 3/"             = "feather_hatch",
    "Feather TOTAL"                 = "feather_total",
    "Yuba R"                        = "yuba",
    "TOTAL SPRING RUN 7/"          = "total"
  ),
  `6` = c(
    "YEAR"                          = "run_year",
    "Sac System Hatch 1/"          = "sac_sys_hatch",
    "Sac System In-R"               = "sac_sys_inr",
    "Sac System Tribs In-R"         = "sac_sys_tribs_inr",
    "Sac System TOTAL"              = "sac_sys_total",
    "SJ System Hatch 2/"           = "sj_sys_hatch",
    "SJ System Tribs In-R"          = "sj_sys_tribs_inr",
    "SJ System TOTAL"               = "sj_sys_total",
    "Sac SJ System Hatch"           = "sac_sj_sys_hatch",
    "Sac SJ System In-R"            = "sac_sj_sys_inr",
    "Sac SJ System TOTAL"           = "sac_sj_sys_total"
  ),
  `7` = c(
    "YEAR"                                  = "run_year",
    "Kesw_RBDD CNFH"                        = "kesw_rbdd_cnfh",
    "Kesw_RBDD Main In-R"                   = "kesw_rbdd_main_inr",
    "Kesw_RBDD Tribs In-R"                  = "kesw_rbdd_tribs_inr",
    "Kesw_RBDD TOTAL"                       = "kesw_rbdd_total",
    "RBDD_PFerry TCFF"                      = "rbdd_pferry_tcff",
    "RBDD_PFerry Main In-R"                 = "rbdd_pferry_main_inr",
    "RBDD_PFerry Tribs In-R 1/"            = "rbdd_pferry_tribs_inr",
    "RBDD_PFerry TOTAL"                     = "rbdd_pferry_total",
    "PFerry_Sacr Feather_Nimbus Hatch"      = "feather_nimbus_hatch",
    "PFerry_Sacr Tribs In-R"                = "pferry_sac_tribs_inr",
    "PFerry_Sacr TOTAL"                     = "pferry_sac_total"
  ),
  `8` = c(
    "YEAR"                      = "run_year",
    "Sac Main In-R 1/"         = "sac_main_inr",
    "Sac Main trans CNFH 2/"   = "sac_main_trans_cnfh",
    "Sac Main TOTAL"            = "sac_main_total",
    "Battle5/ CNFH"             = "battle_cnfh",
    "Battle Dwn CNFH In-R"      = "battle_dwn_cnfh_inr",
    "Battle Up CNFH In-R 3/"   = "battle_up_cnfh_inr",
    "Battle TOTAL"               = "battle_total",
    "Clear Ck"                   = "clear",
    "Spring Gulch"               = "spring_gulch",
    "China Gulch"                = "china_gulch",
    "Olney Ck"                   = "olney",
    "Cottonwood Ck"              = "cottonwood",
    "Paynes Ck"                  = "paynes",
    "Cow Ck"                     = "cow",
    "Bear Ck"                    = "bear_ck",
    "Ash Ck"                     = "ash",
    "Stillwater Ck"              = "stillwater",
    "Inks Ck"                    = "inks",
    "Other 4/"                   = "other"
  ),
  `9` = c(
    "YEAR"                     = "run_year",
    "Sac Main In-R 1/"        = "sac_main_inr",
    "Sac Main trans TCFF 2/"  = "sac_main_trans_tcff",
    "Sac Main TOTAL"           = "sac_main_total",
    "TCFF"                     = "tcff",
    "Salt Ck"                  = "salt",
    "Antelope Ck"              = "antelope",
    "Craig Ck"                 = "craig",
    "Dye Ck"                   = "dye",
    "Mill Ck"                  = "mill",
    "Toomes Ck"                = "toomes",
    "Thomes Ck"                = "thomes",
    "Deer Ck"                  = "deer",
    "Coyote Ck"                = "coyote",
    "Stoney Ck"                = "stoney",
    "Singer Ck"                = "singer",
    "Big Chico Ck"             = "big_chico",
    "Other 3/"                 = "other"
  ),
  `10` = c(
    "YEAR"                       = "run_year",
    "Butte Ck 1/"                = "butte",
    "Feather Hatch"               = "feather_hatch",
    "Feather In-R 2/"            = "feather_inr",
    "Feather TOTAL"               = "feather_total",
    "Yuba R 3/"                   = "yuba",
    "Bear R"                      = "bear_r",
    "Dry Ck"                      = "dry",
    "American Nimbus Hatch"       = "american_nimbus_hatch",
    "American In-R 4/5/"         = "american_inr",
    "American TOTAL"              = "american_total",
    "Natomas Drain"               = "natomas_drain"
  ),
  `11` = c(
    "YEAR"              = "run_year",
    "Cosumnes R"        = "cosumnes",
    "Mokelumne Hatch"   = "mokelumne_hatch",
    "Mokelumne In-R"    = "mokelumne_inr",
    "Mokelumne TOTAL"   = "mokelumne_total",
    "Stanislaus R"      = "stanislaus",
    "Tuolumne R"        = "tuolumne",
    "Merced Hatch 1/"   = "merced_hatch",
    "Merced In-R"       = "merced_inr",
    "Merced TOTAL"      = "merced_total"
  )
)

# Table 1 column mapping by run
.t1_run_cols <- list(
  lf = list(
    year_col  = "Late-Fall YEAR",
    data_cols = c("Late-Fall Hatch" = "hatch",
                  "Late-Fall In-R"  = "inr",
                  "Late-Fall TOTAL" = "total")
  ),
  w = list(
    year_col  = "Winter YEAR",
    data_cols = c("Winter TOTAL" = "total",
                  "Winter RBDD"  = "rbdd")
  ),
  s = list(
    year_col  = "YEAR",
    data_cols = c("Spring Hatch" = "hatch",
                  "Spring In-R"  = "inr",
                  "Spring TOTAL" = "total")
  ),
  f = list(
    year_col  = "YEAR",
    data_cols = c("Fall Hatch" = "hatch",
                  "Fall In-R"  = "inr",
                  "Fall TOTAL" = "total")
  )
)

# -- Location -> table/column mapping -----------------------------------------
.location_cols <- list(
  sac_main = list(
    `2` = c("Sac Main Up RBDD1/ In-R 2/", "Sac Main Up RBDD trans CNFH 3/",
            "Sac Main Up RBDD TOTAL", "Sac Main Dwn RBDD trans TCFF 4/",
            "Sac Main Dwn RBDD In-R 2/", "Sac Main Dwn RBDD TOTAL"),
    `3` = c("Sac Main Up RBDD In-R 2/", "Sac Main Dwn RBDD In-R 3/",
            "Sac Main TOTAL In-R", "Sac Main trans1/ CNFH",
            "Sac Main trans1/ LSNFH"),
    `5` = c("Sac Main Up RBDD1/ In-R", "Sac Main Dwn RBDD In-R",
            "Sac Main In-R TOTAL"),
    `8` = c("Sac Main In-R 1/", "Sac Main trans CNFH 2/", "Sac Main TOTAL"),
    `9` = c("Sac Main In-R 1/", "Sac Main trans TCFF 2/", "Sac Main TOTAL")
  ),
  battle = list(
    `2` = c("Battle5/ Up CNFH In-R", "Battle CNFH", "Battle TOTAL"),
    `3` = c("Battle Ck 5/"),
    `5` = c("Battle Ck 4/"),
    `8` = c("Battle5/ CNFH", "Battle Dwn CNFH In-R", "Battle Up CNFH In-R 3/",
            "Battle TOTAL")
  ),
  clear = list(
    `2` = c("Clear Ck"), `3` = c("Clear Ck 4/"),
    `5` = c("Clear Ck"), `8` = c("Clear Ck")
  ),
  cottonwood = list(
    `2` = c("Cottonwood Ck"), `5` = c("Cottonwood Ck"),
    `8` = c("Cottonwood Ck")
  ),
  salt       = list(`2` = c("Salt Ck"), `9` = c("Salt Ck")),
  craig      = list(`2` = c("Craig Ck"), `9` = c("Craig Ck")),
  feather = list(
    `2` = c("Feather In-R", "Feather Hatch"),
    `5` = c("Feather In-R 2/", "Feather Hatch 3/", "Feather TOTAL"),
    `10` = c("Feather Hatch", "Feather In-R 2/", "Feather TOTAL")
  ),
  antelope   = list(`5` = c("Antelope Ck"), `9` = c("Antelope Ck")),
  mill = list(
    `5` = c("Mill Ck", "Mill Ck Spawn 5/"), `9` = c("Mill Ck")
  ),
  deer = list(
    `5` = c("Deer Ck", "Deer Ck Spawn 5/"), `9` = c("Deer Ck")
  ),
  big_chico  = list(`5` = c("Big Chico Ck"), `9` = c("Big Chico Ck")),
  butte = list(
    `5` = c("Butte Ck 6/", "Butte Ck Spawn 5/"), `10` = c("Butte Ck 1/")
  ),
  yuba       = list(`5` = c("Yuba R"), `10` = c("Yuba R 3/")),
  american   = list(`10` = c("American Nimbus Hatch", "American In-R 4/5/",
                              "American TOTAL")),
  bear_r     = list(`10` = c("Bear R")),
  bear_ck    = list(`8` = c("Bear Ck")),
  dry        = list(`10` = c("Dry Ck")),
  natomas    = list(`10` = c("Natomas Drain")),
  cosumnes   = list(`11` = c("Cosumnes R")),
  mokelumne  = list(`11` = c("Mokelumne Hatch", "Mokelumne In-R",
                              "Mokelumne TOTAL")),
  stanislaus = list(`11` = c("Stanislaus R")),
  tuolumne   = list(`11` = c("Tuolumne R")),
  merced     = list(`11` = c("Merced Hatch 1/", "Merced In-R", "Merced TOTAL")),
  calaveras  = list(`3` = c("SJ Calaveras R 7/")),
  spring_gulch = list(`8` = c("Spring Gulch")),
  china_gulch  = list(`8` = c("China Gulch")),
  olney      = list(`8` = c("Olney Ck")),
  paynes     = list(`8` = c("Paynes Ck")),
  cow        = list(`8` = c("Cow Ck")),
  ash        = list(`8` = c("Ash Ck")),
  stillwater = list(`8` = c("Stillwater Ck")),
  inks       = list(`8` = c("Inks Ck")),
  dye        = list(`9` = c("Dye Ck")),
  toomes     = list(`9` = c("Toomes Ck")),
  thomes     = list(`9` = c("Thomes Ck")),
  coyote     = list(`9` = c("Coyote Ck")),
  stoney     = list(`9` = c("Stoney Ck")),
  singer     = list(`9` = c("Singer Ck"))
)

# -- Constants ----------------------------------------------------------------

.section_to_table <- c(`1` = 8L, `2` = 9L, `3` = 10L, `4` = 11L)

.table_run <- c(`2` = "lf", `3` = "w", `4` = "w", `5` = "s", `6` = "f",
                `7` = "f", `8` = "f", `9` = "f", `10` = "f", `11` = "f")

.feather_system <- c("feather", "yuba", "bear_r")

.fall_table_titles <- c(
  `1`  = "ALL RUNS: Central Valley (fall columns only)",
  `6`  = "FALL RUN Summary I: Sacramento and San Joaquin River Systems",
  `7`  = "FALL RUN Summary II: Sacramento River System",
  `8`  = "FALL RUN Section 1: Keswick Dam to Red Bluff Diversion Dam (RBDD)",
  `9`  = "FALL RUN Section 2: Red Bluff Diversion Dam (RBDD) to Princeton Ferry",
  `10` = "FALL RUN Section 3: Princeton Ferry to Sacramento",
  `11` = "FALL RUN Section 4: San Joaquin River System"
)

# -- Normalization functions --------------------------------------------------

.normalize_run <- function(run) {
  if (is.null(run)) return(NULL)
  run <- tolower(trimws(run))
  vapply(run, function(r) {
    if (r == "all") return("all")
    if (grepl("^l|late", r)) return("lf")
    if (grepl("^w", r)) return("w")
    if (grepl("^sp|^s$", r)) return("s")
    if (grepl("^fa|^f$", r)) return("f")
    stop("Unrecognized run: '", r, "'. Valid: 'lf', 'w', 's', 'f', 'all'.",
         call. = FALSE)
  }, character(1), USE.NAMES = FALSE)
}

.normalize_location <- function(loc) {
  if (is.null(loc)) return(NULL)
  loc <- tolower(trimws(loc))

  patterns <- c(
    sac_main     = "^sac|sacra.*main|mainstem|main.*stem",
    battle       = "^bat",
    clear        = "^clea",
    cottonwood   = "^cot",
    salt         = "^salt",
    craig        = "^crai",
    feather      = "^feat",
    antelope     = "^ant",
    mill         = "^mill",
    deer         = "^deer",
    big_chico    = "chico|^big.*chi",
    butte        = "^but",
    yuba         = "^yub",
    american     = "^amer",
    bear_r       = "^bear$|bear.*riv",
    bear_ck      = "bear.*cr|bear.*ck",
    dry          = "^dry",
    natomas      = "^nat",
    cosumnes     = "^cos",
    mokelumne    = "^mok",
    stanislaus   = "^stan",
    tuolumne     = "^tuol",
    merced       = "^merc",
    calaveras    = "^cal",
    spring_gulch = "^spring",
    china_gulch  = "^china",
    olney        = "^oln",
    paynes       = "^pay",
    cow          = "^cow",
    ash          = "^ash",
    stillwater   = "^still",
    inks         = "^ink",
    dye          = "^dye",
    toomes       = "^too",
    thomes       = "^tho",
    coyote       = "^coy",
    stoney       = "^ston",
    singer       = "^sing"
  )

  vapply(loc, function(l) {
    hits <- names(patterns)[vapply(patterns, function(p) grepl(p, l),
                                   logical(1))]
    if (length(hits) == 0)
      stop("Unrecognized location: '", l, "'.", call. = FALSE)
    if (length(hits) > 1)
      stop("Ambiguous location: '", l, "' matched: ",
           paste(hits, collapse = ", "), ". Be more specific.", call. = FALSE)
    hits
  }, character(1), USE.NAMES = FALSE)
}

.normalize_river_system <- function(rs) {
  if (is.null(rs)) return(NULL)
  rs <- tolower(trimws(rs))
  if (grepl("^sac|sacra", rs)) return("sacramento")
  if (grepl("^san|^sj|joaquin", rs)) return("san_joaquin")
  if (grepl("^feat", rs)) return("feather")
  stop("Unrecognized river system: '", rs,
       "'. Valid: 'sacramento', 'san joaquin', 'feather'.", call. = FALSE)
}

# -- Helper functions ---------------------------------------------------------

.derive_run_year <- function(spawn_period) {
  year <- stringr::str_extract(spawn_period, "\\d{4}(?=[^\\d]*$)")
  ifelse(startsWith(spawn_period, "["), paste0("[", year, "]"), year)
}

.prepare_table <- function(table_idx, run_abbrev) {
  raw <- grandtab_detail[[table_idx]][[2]]
  col_map <- .col_maps[[as.character(table_idx)]]
  first_snake <- unname(col_map[1])

  if (first_snake == "spawn_period") {
    first_orig <- names(col_map)[1]
    out <- tibble::tibble(run_year = .derive_run_year(raw[[first_orig]]))
    out[[paste0(run_abbrev, "_spawn_period")]] <- raw[[first_orig]]
  } else {
    out <- tibble::tibble(run_year = raw[[names(col_map)[1]]])
  }

  for (i in 2:length(col_map)) {
    orig  <- names(col_map)[i]
    snake <- unname(col_map[i])
    out[[paste0(run_abbrev, "_", snake)]] <- raw[[orig]]
  }
  out
}

.extract_t1_run <- function(run_abbrev) {
  t1   <- grandtab_detail[[1]][[2]]
  info <- .t1_run_cols[[run_abbrev]]

  if (info$year_col %in% c("Late-Fall YEAR", "Winter YEAR")) {
    out <- tibble::tibble(run_year = .derive_run_year(t1[[info$year_col]]))
    out[[paste0(run_abbrev, "_spawn_period")]] <- t1[[info$year_col]]
  } else {
    out <- tibble::tibble(run_year = t1[[info$year_col]])
  }

  for (orig in names(info$data_cols)) {
    snake <- info$data_cols[orig]
    out[[paste0(run_abbrev, "_", unname(snake))]] <- t1[[orig]]
  }
  out
}

.prepare_full_t1 <- function() {
  t1 <- grandtab_detail[[1]][[2]]
  tibble::tibble(
    run_year        = t1[["YEAR"]],
    lf_spawn_period = t1[["Late-Fall YEAR"]],
    lf_hatch        = t1[["Late-Fall Hatch"]],
    lf_inr          = t1[["Late-Fall In-R"]],
    lf_total        = t1[["Late-Fall TOTAL"]],
    w_spawn_period  = t1[["Winter YEAR"]],
    w_total         = t1[["Winter TOTAL"]],
    w_rbdd          = t1[["Winter RBDD"]],
    s_hatch         = t1[["Spring Hatch"]],
    s_inr           = t1[["Spring In-R"]],
    s_total         = t1[["Spring TOTAL"]],
    f_hatch         = t1[["Fall Hatch"]],
    f_inr           = t1[["Fall In-R"]],
    f_total         = t1[["Fall TOTAL"]]
  )
}

.get_table_for_run_location <- function(run_abbrev, loc_name) {
  loc_info <- .location_cols[[loc_name]]
  if (is.null(loc_info)) return(NULL)
  run_tables <- switch(run_abbrev,
    lf = "2", w = "3", s = "5", f = c("8", "9", "10", "11"))
  matching <- intersect(names(loc_info), run_tables)
  if (length(matching) == 0) return(NULL)
  as.integer(matching)
}

.extract_location_from_table <- function(table_idx, run_abbrev, loc_names) {
  raw     <- grandtab_detail[[table_idx]][[2]]
  col_map <- .col_maps[[as.character(table_idx)]]
  first_snake <- unname(col_map[1])

  if (first_snake == "spawn_period") {
    first_orig <- names(col_map)[1]
    out <- tibble::tibble(run_year = .derive_run_year(raw[[first_orig]]))
    out[[paste0(run_abbrev, "_spawn_period")]] <- raw[[first_orig]]
  } else {
    out <- tibble::tibble(run_year = raw[[names(col_map)[1]]])
  }

  tbl_key <- as.character(table_idx)
  for (loc in loc_names) {
    loc_info <- .location_cols[[loc]]
    if (!tbl_key %in% names(loc_info)) next
    for (orig in loc_info[[tbl_key]]) {
      snake <- unname(col_map[orig])
      out[[paste0(run_abbrev, "_", snake)]] <- raw[[orig]]
    }
  }
  out
}

.extract_across_runs <- function(runs, locs, as_list = FALSE) {
  parts     <- list()
  run_names <- c(lf = "LATE-FALL", w = "WINTER", s = "SPRING", f = "FALL")

  for (ra in runs) {
    valid <- locs[vapply(locs, function(l)
      !is.null(.get_table_for_run_location(ra, l)), logical(1))]
    if (length(valid) == 0) next

    if (ra == "f") {
      tbl <- .extract_fall_locations(valid)
    } else {
      detail_idx <- switch(ra, lf = 2L, w = 3L, s = 5L)
      tbl <- .extract_location_from_table(detail_idx, ra, valid)
    }

    if (as_list) {
      parts[[run_names[ra]]] <- tbl
    } else {
      parts <- c(parts, list(tbl))
    }
  }

  if (as_list) return(parts)
  .join_on_run_year(parts)
}

.extract_fall_locations <- function(locs) {
  all_tables <- unique(unlist(lapply(locs, function(l)
    .get_table_for_run_location("f", l))))

  tbl_parts <- lapply(all_tables, function(ti) {
    locs_in <- locs[vapply(locs, function(l)
      ti %in% .get_table_for_run_location("f", l), logical(1))]
    .extract_location_from_table(ti, "f", locs_in)
  })
  .join_on_run_year(tbl_parts)
}

.join_on_run_year <- function(parts) {
  if (length(parts) == 0) return(NULL)
  if (length(parts) == 1) return(parts[[1]])
  result <- parts[[1]]
  for (i in seq_along(parts)[-1]) {
    result <- merge(result, parts[[i]], by = "run_year", all = TRUE)
  }
  tibble::as_tibble(result)
}

.fall_menu <- function() {
  choices <- c("All Fall Run Tables",
               .fall_table_titles[["1"]],
               .fall_table_titles[["6"]],
               .fall_table_titles[["7"]],
               .fall_table_titles[["8"]],
               .fall_table_titles[["9"]],
               .fall_table_titles[["10"]],
               .fall_table_titles[["11"]])

  indices <- c(0L, 1L, 6L, 7L, 8L, 9L, 10L, 11L)

  if (!interactive()) {
    stop("Multiple fall run tables available. Specify 'section', 'summary', ",
         "or 'river_system' to narrow the selection.", call. = FALSE)
  }

  selected <- utils::select.list(choices, multiple = TRUE,
                                 title = "Select fall run table(s):")
  if (length(selected) == 0) return(invisible(NULL))

  sel_idx <- indices[match(selected, choices)]
  if (0L %in% sel_idx) sel_idx <- indices[indices > 0L]

  result <- list()
  for (idx in sel_idx) {
    if (idx == 1L) {
      tbl  <- .extract_t1_run("f")
      name <- grandtab_detail[[1]][[1]]
    } else {
      tbl  <- .prepare_table(idx, "f")
      name <- grandtab_detail[[idx]][[1]]
    }
    result[[name]] <- tbl
  }
  if (length(result) == 1) return(result[[1]])
  result
}

.sac_main_fall_menu <- function() {
  if (!interactive()) {
    return(list(
      "FALL RUN Section 1" = .extract_location_from_table(8L, "f", "sac_main"),
      "FALL RUN Section 2" = .extract_location_from_table(9L, "f", "sac_main")
    ))
  }
  choices <- c("All Fall Run Tables",
               .fall_table_titles[["8"]],
               .fall_table_titles[["9"]])
  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Which mainstem Sacramento tables should get_escapement() return?")
  if (length(selected) == 0) return(invisible(NULL))
  if ("All Fall Run Tables" %in% selected) selected <- choices[-1]

  result <- list()
  if (.fall_table_titles[["8"]] %in% selected)
    result[[grandtab_detail[[8]][[1]]]] <-
      .extract_location_from_table(8L, "f", "sac_main")
  if (.fall_table_titles[["9"]] %in% selected)
    result[[grandtab_detail[[9]][[1]]]] <-
      .extract_location_from_table(9L, "f", "sac_main")
  if (length(result) == 1) return(result[[1]])
  result
}

# -- Provisional data handling ------------------------------------------------

.finalize <- function(tbl) {
  sp_cols <- grep("_spawn_period$", names(tbl), value = TRUE)
  check_cols <- intersect(c("run_year", sp_cols), names(tbl))

  provisional <- rep(0L, nrow(tbl))
  for (col in check_cols) {
    vals <- tbl[[col]]
    is_prov <- !is.na(vals) & startsWith(vals, "[")
    provisional <- pmax(provisional, as.integer(is_prov))
  }

  for (col in check_cols) {
    tbl[[col]] <- gsub("^\\[|\\]$", "", tbl[[col]])
  }

  tbl$provisional_data <- provisional
  tbl
}

.finalize_output <- function(result) {
  if (is.null(result)) return(invisible(NULL))
  if (is.data.frame(result)) return(.finalize(result))
  if (is.list(result)) {
    return(lapply(result, function(x) {
      if (is.data.frame(x)) .finalize(x) else x
    }))
  }
  result
}

.filter_run_years <- function(result, run_years) {
  if (is.null(run_years)) return(result)
  run_years <- as.numeric(run_years)
  keep <- function(tbl) {
    yrs <- suppressWarnings(as.numeric(tbl$run_year))
    tbl[!is.na(yrs) & yrs %in% run_years, , drop = FALSE]
  }
  if (is.data.frame(result)) return(keep(result))
  if (is.list(result)) {
    return(lapply(result, function(x) {
      if (is.data.frame(x)) keep(x) else x
    }))
  }
  result
}

# -- Routing handlers ---------------------------------------------------------

.route_single_run <- function(run_abbrev, rs, loc, summary) {
  detail_idx <- switch(run_abbrev, lf = 2L, w = 3L, s = 5L)

  if (isTRUE(summary)) {
    if (!is.null(loc)) {
      warning("'summary' ignored for individual locations.", call. = FALSE)
    } else {
      return(.extract_t1_run(run_abbrev))
    }
  }

  if (!is.null(loc)) {
    return(.extract_location_from_table(detail_idx, run_abbrev, loc))
  }

  if (!is.null(rs)) {
    full <- .prepare_table(detail_idx, run_abbrev)
    if (run_abbrev == "w") {
      if (rs == "sacramento") {
        full <- full[, !names(full) %in% "w_calaveras", drop = FALSE]
      } else if (rs == "san_joaquin") {
        keep <- intersect(c("run_year", "w_spawn_period", "w_calaveras"),
                          names(full))
        full <- full[, keep, drop = FALSE]
      } else if (rs == "feather") {
        stop("No Feather River data available for winter run.", call. = FALSE)
      }
    } else if (run_abbrev == "lf") {
      if (rs == "feather") {
        return(.extract_location_from_table(detail_idx, run_abbrev, "feather"))
      }
    } else if (run_abbrev == "s") {
      if (rs == "feather") {
        fl <- intersect(.feather_system,
          names(which(vapply(.location_cols,
                             function(x) "5" %in% names(x), logical(1)))))
        return(.extract_location_from_table(detail_idx, run_abbrev, fl))
      }
    }
    return(full)
  }

  .prepare_table(detail_idx, run_abbrev)
}

.route_fall <- function(rs, loc, summary, section) {
  if (isTRUE(summary)) {
    if (!is.null(loc)) {
      warning("'summary' ignored for individual locations.", call. = FALSE)
    } else {
      return(.fall_menu())
    }
  }

  if (!is.null(section)) {
    tbl_idx <- .section_to_table[as.character(section)]
    return(.prepare_table(tbl_idx, "f"))
  }

  if (!is.null(loc)) {
    if (length(loc) == 1 && loc == "sac_main") {
      return(.sac_main_fall_menu())
    }
    return(.extract_fall_locations(loc))
  }

  if (!is.null(rs)) {
    if (rs == "sacramento") {
      t6 <- .prepare_table(6L, "f")
      t6 <- t6[, 1:5]
      t7 <- .prepare_table(7L, "f")
      message("Note: these are summary tables for the whole Sacramento River ",
              "System. For greater specification, use the 'section' and ",
              "'location' arguments.")
      return(list(
        "FALL RUN Summary I: Sacramento and San Joaquin River Systems" = t6,
        "FALL RUN Summary II: Sacramento River System" = t7
      ))
    }
    if (rs == "san_joaquin") {
      return(.prepare_table(11L, "f"))
    }
    if (rs == "feather") {
      fl <- intersect(.feather_system,
        names(which(vapply(.location_cols,
                           function(x) "10" %in% names(x), logical(1)))))
      return(.extract_location_from_table(10L, "f", fl))
    }
  }

  .fall_menu()
}

.route_all_runs <- function(rs, loc, summary, section) {
  if (isTRUE(summary)) return(.prepare_full_t1())

  if (!is.null(loc)) {
    return(.extract_across_runs(c("lf", "w", "s", "f"), loc, as_list = TRUE))
  }

  if (!is.null(rs)) {
    result <- list()
    for (ra in c("lf", "w", "s", "f")) {
      tbl <- tryCatch({
        if (ra == "f") .route_fall(rs, NULL, NULL, section)
        else .route_single_run(ra, rs, NULL, NULL)
      }, error = function(e) NULL)
      if (!is.null(tbl)) {
        rn <- switch(ra, lf="LATE-FALL", w="WINTER", s="SPRING", f="FALL")
        if (is.data.frame(tbl)) {
          result[[rn]] <- tbl
        } else if (is.list(tbl)) {
          for (nm in names(tbl)) result[[nm]] <- tbl[[nm]]
        }
      }
    }
    return(result)
  }

  result <- list()
  for (idx in c(2L, 3L, 5L, 8L, 9L, 10L, 11L)) {
    ra   <- .table_run[as.character(idx)]
    name <- grandtab_detail[[idx]][[1]]
    result[[name]] <- .prepare_table(idx, ra)
  }
  result
}

.route_multi_run <- function(runs, rs, loc, summary, section) {
  if (!is.null(loc)) {
    return(.extract_across_runs(runs, loc, as_list = FALSE))
  }

  result    <- list()
  run_names <- c(lf = "LATE-FALL", w = "WINTER", s = "SPRING", f = "FALL")

  for (ra in runs) {
    tbl <- tryCatch({
      if (isTRUE(summary)) {
        .extract_t1_run(ra)
      } else if (ra == "f") {
        if (!is.null(section)) {
          .prepare_table(.section_to_table[as.character(section)], "f")
        } else if (!is.null(rs)) {
          .route_fall(rs, NULL, NULL, NULL)
        } else {
          # Multi-run context: return all fall section tables
          fl <- list()
          for (idx in c(8L, 9L, 10L, 11L)) {
            fl[[grandtab_detail[[idx]][[1]]]] <- .prepare_table(idx, "f")
          }
          fl
        }
      } else {
        .route_single_run(ra, rs, NULL, NULL)
      }
    }, error = function(e) NULL)

    if (!is.null(tbl)) {
      if (is.data.frame(tbl)) {
        result[[run_names[ra]]] <- tbl
      } else if (is.list(tbl)) {
        for (nm in names(tbl)) result[[nm]] <- tbl[[nm]]
      }
    }
  }
  result
}

.route_null_run <- function(rs, loc, summary, section) {
  if (!is.null(section)) {
    message("Note: only FALL RUN data is organized by section.")
    tbl_idx <- .section_to_table[as.character(section)]
    return(.prepare_table(tbl_idx, "f"))
  }

  if (isTRUE(summary)) {
    if (!is.null(loc)) {
      warning("'summary' ignored for individual locations.", call. = FALSE)
    } else {
      return(.prepare_full_t1())
    }
  }

  if (!is.null(loc)) {
    complex <- any(loc %in% c("feather", "sac_main"))
    if (complex) {
      return(.extract_across_runs(c("lf", "w", "s", "f"), loc,
                                  as_list = TRUE))
    }
    return(.extract_across_runs(c("lf", "w", "s", "f"), loc,
                                as_list = FALSE))
  }

  if (!is.null(rs)) {
    result <- list()
    for (ra in c("lf", "w", "s", "f")) {
      tbl <- tryCatch({
        if (ra == "f") .route_fall(rs, NULL, NULL, NULL)
        else .route_single_run(ra, rs, NULL, NULL)
      }, error = function(e) NULL)
      if (!is.null(tbl)) {
        rn <- switch(ra, lf="LATE-FALL", w="WINTER", s="SPRING", f="FALL")
        if (is.data.frame(tbl)) result[[rn]] <- tbl
        else if (is.list(tbl)) for (nm in names(tbl)) result[[nm]] <- tbl[[nm]]
      }
    }
    message("Note: these summary counts include the tributaries of each ",
            "river system. Sacramento summary includes counts from the ",
            "Feather River.")
    return(result)
  }

  # No filters — return all 11 tables in order with snake_case column names
  result <- list()
  result[[grandtab_detail[[1]][[1]]]] <- .prepare_full_t1()
  for (idx in 2:11) {
    ra   <- .table_run[as.character(idx)]
    name <- grandtab_detail[[idx]][[1]]
    result[[name]] <- .prepare_table(idx, ra)
  }
  result
}

# -- Main function ------------------------------------------------------------

#' Retrieve GrandTab Chinook salmon escapement data
#'
#' Flexible query interface for GrandTab escapement data. Supports filtering
#' by run, river system, location, summary level, and fall run section.
#'
#' @param run Character. Salmon run: \code{"lf"} (late-fall), \code{"w"}
#'   (winter), \code{"s"} (spring), \code{"f"} (fall), \code{"all"}, or
#'   \code{NULL}. A vector of run codes is also accepted.
#' @param river_system Character. \code{"sacramento"}, \code{"san joaquin"},
#'   or \code{"feather"}. Partial matching supported.
#' @param location Character. Location name(s), e.g. \code{"clear"},
#'   \code{"battle"}, \code{"feather"}. Flexible matching via substrings.
#'   Vectors accepted for multiple locations.
#' @param summary Logical or \code{"all"}. If \code{TRUE} or \code{"all"},
#'   return summary-level data from Table 1. If \code{FALSE} or \code{NULL},
#'   return detail data.
#' @param section Integer 1--4. Fall run geographic section (maps to
#'   Tables 8--11). Only valid for fall run.
#' @param run_years Numeric vector of years to filter results to, or
#'   \code{NULL} (default) to return all years.
#' @return A tibble or named list of tibbles.
#' @export
get_escapement <- function(run = NULL, river_system = NULL, location = NULL,
                           summary = NULL, section = NULL, run_years = NULL) {

  run_norm <- .normalize_run(run)
  rs_norm  <- .normalize_river_system(river_system)
  loc_norm <- .normalize_location(location)

  if (isFALSE(summary)) summary <- NULL
  if (identical(tolower(as.character(summary)), "all")) summary <- TRUE

  # -- Validate ---------------------------------------------------------------

  if (!is.null(section)) {
    section <- as.integer(section)
    if (!section %in% 1:4)
      stop("'section' must be 1-4.", call. = FALSE)
    if (!is.null(run_norm) && length(run_norm) == 1 &&
        !run_norm %in% c("f", "all"))
      stop("'section' only applies to fall run. Only fall run data is ",
           "organized by geographic section.", call. = FALSE)
  }

  if (isTRUE(summary) && !is.null(section))
    stop("'summary' and 'section' cannot both be specified. If 'section' is ",
         "specified, 'summary' must be NULL or FALSE. If 'summary' is ",
         "specified, 'section' must be NULL.", call. = FALSE)

  if (!is.null(rs_norm) && !is.null(run_norm) && length(run_norm) == 1) {
    if (run_norm == "lf" && rs_norm != "sacramento")
      stop("Late-fall run are only found in the Sacramento River System.",
           call. = FALSE)
    if (run_norm == "s" && rs_norm == "san_joaquin")
      stop("Spring run are only found in the Sacramento River System.",
           call. = FALSE)
  }

  if (!is.null(loc_norm) && !is.null(run_norm) && length(run_norm) == 1 &&
      run_norm != "all") {
    for (loc in loc_norm) {
      if (is.null(.get_table_for_run_location(run_norm, loc)))
        stop("'", loc, "' does not exist for ",
             switch(run_norm, lf="late-fall", w="winter", s="spring", f="fall"),
             " run.", call. = FALSE)
    }
  }

  # -- Route ------------------------------------------------------------------

  result <- if (!is.null(run_norm) && length(run_norm) == 1) {
    if (run_norm == "all")
      .route_all_runs(rs_norm, loc_norm, summary, section)
    else if (run_norm == "f")
      .route_fall(rs_norm, loc_norm, summary, section)
    else
      .route_single_run(run_norm, rs_norm, loc_norm, summary)
  } else if (!is.null(run_norm) && length(run_norm) > 1) {
    .route_multi_run(run_norm, rs_norm, loc_norm, summary, section)
  } else {
    .route_null_run(rs_norm, loc_norm, summary, section)
  }

  .filter_run_years(.finalize_output(result), run_years)
}
