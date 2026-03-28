# ============================================================================
# get_escapement.R
#
# User-facing function for retrieving GrandTab Chinook salmon escapement data.
# Supports flexible querying by run, river system, location, summary level,
# and fall run section.
# ============================================================================

# -- Column name mappings per table -------------------------------------------
# Named vectors: grandtab_raw column name -> snake_case base name
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
  singer     = list(`9` = c("Singer Ck")),
  tcff       = list(`9` = c("TCFF"))
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

.bear_disambiguate <- function() {
  choices <- c(
    "Bear River  (Feather River System)",
    "Bear Creek  (Sacramento River System)"
  )
  if (!interactive())
    stop("'bear' is ambiguous. Use 'bear river' (Feather River System) or ",
         "'bear creek' (Sacramento River System).", call. = FALSE)
  selected <- utils::select.list(choices, multiple = FALSE,
    title = "Which Bear location?")
  if (length(selected) == 0 || nchar(selected) == 0) return(invisible(NULL))
  if (grepl("River", selected)) "bear river" else "bear creek"
}

.normalize_location <- function(loc) {
  if (is.null(loc)) return(NULL)
  loc <- tolower(trimws(loc))

  # Intercept bare "bear" before pattern matching and prompt user to choose.
  loc <- vapply(loc, function(l) {
    if (l == "bear") .bear_disambiguate() else l
  }, character(1), USE.NAMES = FALSE)

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
    bear_r       = "bear.*riv",
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
    singer       = "^sing",
    san_joaquin  = "^san|^sj"
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


.location_display_names <- c(
  sac_main     = "Sacramento River",  battle      = "Battle Creek",
  clear        = "Clear Creek",       cottonwood  = "Cottonwood Creek",
  salt         = "Salt Creek",        craig       = "Craig Creek",
  feather      = "Feather River",     antelope    = "Antelope Creek",
  mill         = "Mill Creek",        deer        = "Deer Creek",
  big_chico    = "Big Chico Creek",   butte       = "Butte Creek",
  yuba         = "Yuba River",        american    = "American River",
  bear_r       = "Bear River",        bear_ck     = "Bear Creek",
  dry          = "Dry Creek",         natomas     = "Natomas Drain",
  cosumnes     = "Cosumnes River",    mokelumne   = "Mokelumne River",
  stanislaus   = "Stanislaus River",  tuolumne    = "Tuolumne River",
  merced       = "Merced River",      calaveras   = "Calaveras River",
  spring_gulch = "Spring Gulch",      china_gulch = "China Gulch",
  olney        = "Olney Creek",       paynes      = "Paynes Creek",
  cow          = "Cow Creek",         ash         = "Ash Creek",
  stillwater   = "Stillwater Creek",  inks        = "Inks Creek",
  dye          = "Dye Creek",         toomes      = "Toomes Creek",
  thomes       = "Thomes Creek",      coyote      = "Coyote Creek",
  stoney       = "Stoney Creek",      singer      = "Singer Creek",
  san_joaquin  = "San Joaquin River", tcff        = "Tehama-Colusa Fish Facility"
)

.hatchery_patterns <- list(
  lsnfh = list(assoc_loc = "sac_main", trans_loc = NULL,
               col_suffix = "_lsnfh$",
               valid_runs = c("w"),
               display    = "Livingston Stone National Fish Hatchery"),
  cnfh = list(assoc_loc = "battle",    trans_loc = "sac_main",
              col_suffix = "_cnfh$",
              valid_runs = c("lf", "f"),
              display    = "Coleman National Fish Hatchery"),
  tcff = list(assoc_loc = "tcff",      trans_loc = "sac_main",
              col_suffix = "_tcff$",
              valid_runs = c("f"),
              display    = "Tehama-Colusa Fish Facility"),
  frh  = list(assoc_loc = "feather",   trans_loc = NULL,
              col_suffix = "_feather_hatch$",
              valid_runs = c("lf", "s", "f"),
              display    = "Feather River Hatchery"),
  nfh  = list(assoc_loc = "american",  trans_loc = NULL,
              col_suffix = "_nimbus_hatch$",
              valid_runs = c("f"),
              display    = "Nimbus Hatchery"),
  mrh  = list(assoc_loc = "mokelumne", trans_loc = NULL,
              col_suffix = "_mokelumne_hatch$",
              valid_runs = c("f"),
              display    = "Mokelumne River Hatchery"),
  merh = list(assoc_loc = "merced",    trans_loc = NULL,
              col_suffix = "_merced_hatch$",
              valid_runs = c("f"),
              display    = "Merced River Hatchery")
)

# Build a location phrase: prep + optional "the" + display name.
# "the" is added only when the name ends in "River".
# Use prep="in"  for escapement / hatchery-return errors.
# Use prep="on"  for hatchery-presence errors ("no hatchery on X").
.loc_phrase <- function(display_name, prep = "in") {
  has_the <- grepl("[Rr]iver$", display_name)
  paste0(prep, if (has_the) " the " else " ", display_name)
}

.resolve_hatchery <- function(loc_str) {
  l <- tolower(trimws(loc_str))
  patterns <- c(
    lsnfh = "^lsnfh$|livingston|stone.*hatch",
    cnfh  = "^cnfh$|coleman",
    tcff  = "^tcff$|tehama",
    frh   = "^frh$|feather.*hatch|hatch.*feather",
    nfh   = "^nfh$|nimbus",
    mrh   = "^mrh$|mokelumne.*hatch|hatch.*mok",
    merh  = "^merh$|merced.*hatch|hatch.*merced"
  )
  for (key in names(patterns)) {
    if (grepl(patterns[[key]], l)) return(.hatchery_patterns[[key]])
  }
  NULL
}

# Return the hatchery display name(s) associated with a loc_norm, or NULL.
.location_hatchery_display <- function(loc_norm) {
  matches <- Filter(function(h) h$assoc_loc == loc_norm, .hatchery_patterns)
  if (length(matches) == 0) return(NULL)
  paste(vapply(matches, `[[`, character(1), "display"), collapse = " and ")
}

# Return display names of hatcheries that have transfer columns at loc_norm
# for the given run(s), using the column-metadata structures (no data fetch).
.find_transfer_displays <- function(loc_norm, run_norm = NULL) {
  tables <- .location_cols[[loc_norm]]
  if (is.null(tables)) return(character(0))
  if (!is.null(run_norm) && !identical(run_norm, "all")) {
    keep   <- names(tables)[.table_run[names(tables)] %in% run_norm]
    tables <- tables[keep]
  }
  hatch_names <- character(0)
  for (tbl_key in names(tables)) {
    snake_cols <- unname(.col_maps[[tbl_key]][tables[[tbl_key]]])
    trans_cols <- snake_cols[!is.na(snake_cols) & grepl("_trans_", snake_cols)]
    for (col in unique(trans_cols)) {
      for (h in .hatchery_patterns) {
        if (grepl(h$col_suffix, col)) {
          hatch_names <- c(hatch_names, h$display)
          break
        }
      }
    }
  }
  unique(hatch_names)
}

# Detect which hatcheries have return (non-transfer) columns in a result object.
.detect_hatcheries_in_result <- function(raw) {
  all_cols <- if (is.data.frame(raw)) {
    names(raw)
  } else if (is.list(raw)) {
    unlist(lapply(raw, function(t) if (is.data.frame(t)) names(t) else character(0)))
  } else character(0)
  hatch_display <- character(0)
  for (h in .hatchery_patterns) {
    ret_cols <- all_cols[grepl(h$col_suffix, all_cols) & !grepl("_trans_", all_cols)]
    if (length(ret_cols) > 0) hatch_display <- c(hatch_display, h$display)
  }
  unique(hatch_display)
}

.location_has_origin <- function(loc_norm, suffix, run_norm = NULL) {
  tables <- .location_cols[[loc_norm]]
  if (is.null(tables)) return(FALSE)
  if (!is.null(run_norm)) {
    keep <- names(tables)[.table_run[names(tables)] %in% run_norm]
    tables <- tables[keep]
  }
  any(vapply(names(tables), function(tbl_key) {
    raw_cols   <- tables[[tbl_key]]
    col_map    <- .col_maps[[tbl_key]]
    snake_cols <- unname(col_map[raw_cols])
    any(grepl(suffix, snake_cols), na.rm = TRUE)
  }, logical(1)))
}

.hatch_suffix_re <- "(_hatch|_cnfh|_tcff|_lsnfh)$"

.location_has_hatchery <- function(loc_norm, run_norm = NULL) {
  # Exclude _trans_ columns: they are transfers to hatcheries, not hatchery
  # returns, and hatchery=TRUE does not include them.
  tables <- .location_cols[[loc_norm]]
  if (is.null(tables)) return(FALSE)
  if (!is.null(run_norm)) {
    keep   <- names(tables)[.table_run[names(tables)] %in% run_norm]
    tables <- tables[keep]
  }
  any(vapply(names(tables), function(tbl_key) {
    raw_cols   <- tables[[tbl_key]]
    col_map    <- .col_maps[[tbl_key]]
    snake_cols <- unname(col_map[raw_cols])
    any(grepl(.hatch_suffix_re, snake_cols) & !grepl("_trans_", snake_cols),
        na.rm = TRUE)
  }, logical(1)))
}

.filter_by_origin <- function(result, hatchery) {
  if (is.null(hatchery)) return(result)
  suffix <- if (isFALSE(hatchery)) "_inr$" else .hatch_suffix_re
  filter_tbl <- function(tbl) {
    if (!is.data.frame(tbl)) return(tbl)
    meta  <- grepl("run_year|spawn_period|provisional", names(tbl))
    match <- grepl(suffix, names(tbl))
    # hatchery=TRUE: exclude transfer columns (not hatchery-origin fish)
    # hatchery=FALSE (in-river): include transfer columns
    if (isTRUE(hatchery))  match <- match & !grepl("_trans_", names(tbl))
    if (isFALSE(hatchery)) match <- match | grepl("_trans_",  names(tbl))
    tbl[, meta | match, drop = FALSE]
  }
  if (is.data.frame(result)) return(filter_tbl(result))
  if (is.list(result))       return(lapply(result, filter_tbl))
  result
}

# Raise an informative error if a filtered result contains no data columns
# (i.e. only run_year / spawn_period / provisional_data remain).
.assert_has_data <- function(result, hatchery, run_norm, rs_norm, loc_norm,
                             unfiltered = NULL, selected_hatch_display = NULL) {
  if (is.null(hatchery)) return(invisible(NULL))

  meta_re  <- "run_year|spawn_period|provisional"
  has_data <- function(tbl) is.data.frame(tbl) && any(!grepl(meta_re, names(tbl)))

  empty <- if (is.data.frame(result)) !has_data(result)
           else if (is.list(result))  !any(vapply(result, has_data, logical(1)))
           else FALSE

  if (!empty) return(invisible(NULL))

  type_str   <- if (isTRUE(hatchery)) "hatchery" else "in-river"
  run_labels <- c(lf = "Late-Fall Run", w = "Winter Run",
                  s  = "Spring Run",    f  = "Fall Run")
  run_str <- if (!is.null(run_norm) && !identical(run_norm, "all")) {
    known <- run_norm[run_norm %in% names(run_labels)]
    if (length(known) > 0) paste(unname(run_labels[known]), collapse = " or ")
    else NULL
  } else NULL

  loc_str        <- NULL
  transfer_clause <- NULL

  if (isTRUE(hatchery) && length(selected_hatch_display) > 0) {
    # A specific hatchery (or hatcheries) was selected from the menu.
    # Say "at [Hatchery Name]" rather than the river.
    hatch_str <- if (length(selected_hatch_display) == 1) {
      selected_hatch_display
    } else {
      paste0(paste(selected_hatch_display[-length(selected_hatch_display)], collapse = ", "),
             " or ", selected_hatch_display[length(selected_hatch_display)])
    }
    loc_str <- paste0("at ", hatch_str)

    # Check if sac_main has transfer columns for this hatchery + run.
    # (Transfer columns are only in sac_main, never the tributary tables.)
    if (!is.null(loc_norm) && length(loc_norm) == 1 && loc_norm == "sac_main") {
      trans_hatcheries <- .find_transfer_displays("sac_main", run_norm)
      matched <- selected_hatch_display[selected_hatch_display %in% trans_hatcheries]
      if (length(matched) > 0) {
        transfer_clause <- ", only transfers from the Sacramento River"
      }
    }

  } else {
    # No specific hatchery selected: use location / river-system name.
    loc_str <- if (!is.null(loc_norm) && length(loc_norm) == 1) {
      dn <- unname(.location_display_names[loc_norm])
      if (!is.na(dn)) .loc_phrase(dn) else NULL
    } else if (!is.null(rs_norm)) {
      sys_names <- c(sacramento  = "the Sacramento River system",
                     san_joaquin = "the San Joaquin River system",
                     feather     = "the Feather River system")
      paste0("in ", unname(sys_names[rs_norm]))
    } else NULL

    # When hatchery=TRUE and result is empty, check unfiltered for transfer
    # columns so we can say "only transfers to X".
    if (isTRUE(hatchery) && !is.null(unfiltered)) {
      all_cols <- if (is.data.frame(unfiltered)) {
        names(unfiltered)
      } else if (is.list(unfiltered)) {
        unlist(lapply(unfiltered, function(t) if (is.data.frame(t)) names(t) else character(0)))
      } else character(0)

      trans_cols <- all_cols[grepl("_trans_", all_cols) & !grepl(meta_re, all_cols)]
      if (length(trans_cols) > 0) {
        hatch_names <- character(0)
        for (col in unique(trans_cols)) {
          for (h in .hatchery_patterns) {
            if (grepl(h$col_suffix, col)) {
              hatch_names <- c(hatch_names, h$display)
              break
            }
          }
        }
        hatch_names <- unique(hatch_names)
        if (length(hatch_names) > 0) {
          ht_str <- if (length(hatch_names) == 1) {
            hatch_names
          } else {
            paste0(paste(hatch_names[-length(hatch_names)], collapse = ", "),
                   " and ", hatch_names[length(hatch_names)])
          }
          transfer_clause <- paste0(", only transfers to ", ht_str)
        }
      }
    }
  }

  stop("There are no ",
       if (!is.null(run_str)) paste0(run_str, " ") else "",
       type_str, " returns",
       if (!is.null(loc_str)) paste0(" ", loc_str) else "",
       if (!is.null(transfer_clause)) transfer_clause else "",
       ".", call. = FALSE)
}

.filter_hatch_only <- function(result, col_suffix) {
  filter_tbl <- function(tbl) {
    if (!is.data.frame(tbl)) return(tbl)
    meta  <- grepl("run_year|spawn_period|provisional", names(tbl))
    match <- grepl(col_suffix, names(tbl))
    tbl[, meta | match, drop = FALSE]
  }
  if (is.data.frame(result)) return(filter_tbl(result))
  if (is.list(result))       return(lapply(result, filter_tbl))
  result
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
  raw <- grandtab_raw[[table_idx]][[2]]
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
  t1   <- grandtab_raw[[1]][[2]]
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
  t1 <- grandtab_raw[[1]][[2]]
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
  raw     <- grandtab_raw[[table_idx]][[2]]
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
  # Deduplicate .x/.y suffix pairs produced by merge() when column names
  # collide (e.g. lf_spawn_period appearing in multiple tables).
  # Values should be identical across tables, so keep .x and drop .y.
  nms   <- names(result)
  x_cols <- nms[endsWith(nms, ".x")]
  if (length(x_cols) > 0) {
    base  <- sub("\\.x$", "", x_cols)
    names(result)[names(result) %in% x_cols] <- base
    result <- result[, !names(result) %in% paste0(base, ".y"), drop = FALSE]
  }
  tibble::as_tibble(result)
}

# -- Summary table helpers ----------------------------------------------------

# Table 1 with Sacramento-only fall and winter totals.
# Fall:   replace f_total with f_sac_sys_total from Table 6.
# Winter: replace w_total with w_sac_sys_total from Table 3.
.extract_t1_sac_adjusted <- function() {
  t1 <- .prepare_full_t1()
  t6 <- .prepare_table(6L, "f")
  t3 <- .prepare_table(3L, "w")

  sac_fall <- t6[, c("run_year", "f_sac_sys_total"), drop = FALSE]
  t1 <- merge(t1, sac_fall, by = "run_year", all.x = TRUE)
  t1$f_total        <- t1$f_sac_sys_total
  t1$f_sac_sys_total <- NULL

  sac_wint <- t3[, c("run_year", "w_sac_sys_total"), drop = FALSE]
  t1 <- merge(t1, sac_wint, by = "run_year", all.x = TRUE)
  t1$w_total        <- t1$w_sac_sys_total
  t1$w_sac_sys_total <- NULL

  tibble::as_tibble(t1)
}

# "ALL RUNS" table for San Joaquin system only.
# Combines winter Calaveras (Table 3) and fall SJ summary (Table 6).
.extract_t1_sj_only <- function() {
  t3 <- .prepare_table(3L, "w")
  t6 <- .prepare_table(6L, "f")

  wint <- t3[, c("run_year", "w_spawn_period", "w_calaveras"), drop = FALSE]
  sj_cols <- c("run_year", grep("^f_sj_", names(t6), value = TRUE))
  fall <- t6[, sj_cols, drop = FALSE]

  tibble::as_tibble(merge(wint, fall, by = "run_year", all = TRUE))
}

# Menu for run="f", summary=TRUE (no river_system filter).
# Offers: ALL RUNS fall columns / Summary I / Summary II / All.
.fall_summary_menu <- function() {
  t1_label <- .fall_table_titles[["1"]]
  t6_label <- .fall_table_titles[["6"]]
  t7_label <- .fall_table_titles[["7"]]
  choices  <- c(t1_label, t6_label, t7_label, "All fall summary tables")

  get_all <- function() {
    result_list <- list(
      .extract_t1_run("f"),
      .prepare_table(6L, "f"),
      .prepare_table(7L, "f")
    )
    names(result_list) <- c(t1_label, t6_label, t7_label)
    result_list
  }

  if (!interactive()) return(get_all())

  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Select fall run summary table(s):")
  if (length(selected) == 0) return(invisible(NULL))

  all_sel <- "All fall summary tables" %in% selected
  result  <- list()
  if (all_sel || t1_label %in% selected) result[[t1_label]] <- .extract_t1_run("f")
  if (all_sel || t6_label %in% selected) result[[t6_label]] <- .prepare_table(6L, "f")
  if (all_sel || t7_label %in% selected) result[[t7_label]] <- .prepare_table(7L, "f")

  if (length(result) == 1) return(result[[1]])
  result
}

# Full summary menu: ALL RUNS / Summary I / Summary II / All summary tables.
# rs_norm controls whether Sacramento-adjusted versions are used.
.all_summary_menu <- function(rs_norm = NULL) {
  sac <- identical(rs_norm, "sacramento")

  t1_label <- if (sac) {
    paste0(.fall_table_titles[["1"]],
           " (excluding San Joaquin Fall and Winter from totals)")
  } else {
    .fall_table_titles[["1"]]
  }
  t6_label <- .fall_table_titles[["6"]]
  t7_label <- .fall_table_titles[["7"]]
  choices  <- c(t1_label, t6_label, t7_label, "All summary tables")

  get_t1 <- function() if (sac) .extract_t1_sac_adjusted() else .prepare_full_t1()
  get_t6 <- function() {
    t6 <- .prepare_table(6L, "f")
    if (!sac) return(t6)
    keep <- c("run_year", grep("^f_sac_sys_", names(t6), value = TRUE))
    t6[, keep, drop = FALSE]
  }
  get_t7 <- function() .prepare_table(7L, "f")

  get_all <- function() {
    result_list <- list(get_t1(), get_t6(), get_t7())
    names(result_list) <- c(t1_label, t6_label, t7_label)
    result_list
  }

  if (!interactive()) return(get_all())

  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Select summary table(s):")
  if (length(selected) == 0) return(invisible(NULL))

  all_sel <- "All summary tables" %in% selected
  result  <- list()
  if (all_sel || t1_label %in% selected) result[[t1_label]] <- get_t1()
  if (all_sel || t6_label %in% selected) result[[t6_label]] <- get_t6()
  if (all_sel || t7_label %in% selected) result[[t7_label]] <- get_t7()

  if (length(result) == 1) return(result[[1]])
  result
}

# Master router for all summary=TRUE paths.
.route_summary <- function(run_norm, rs_norm, rs_all = FALSE) {

  run_labels <- c(lf = "Late-Fall Run", w = "Winter Run",
                  s  = "Spring Run",    f  = "Fall Run")

  # run="all" -> full summary menu (with optional Sacramento adjustment).
  if (identical(run_norm, "all")) {
    if (!is.null(rs_norm) && rs_norm == "feather") {
      stop("There is no summary table for the Feather River.", call. = FALSE)
    }
    if (!is.null(rs_norm) && rs_norm == "san_joaquin") {
      # Special case: SJ-specific tables only.
      t6  <- .prepare_table(6L, "f")
      sj_cols <- c("run_year", grep("^f_sj_", names(t6), value = TRUE))
      t3  <- .prepare_table(3L, "w")
      message("There are no Spring Run or Late-Fall Run in the ",
              "San Joaquin River System.")
      result <- list()
      result[["FALL RUN Summary I (San Joaquin System only)"]] <-
        t6[, sj_cols, drop = FALSE]
      result[["Winter Run (San Joaquin System)"]] <-
        t3[, c("run_year", "w_spawn_period", "w_calaveras"), drop = FALSE]
      return(result)
    }
    return(.all_summary_menu(rs_norm))
  }

  # run=NULL: rs="all" or no rs -> Table 1; rs specified -> filtered Table 1.
  if (is.null(run_norm)) {
    if (!is.null(rs_norm) && rs_norm == "feather")
      stop("There is no summary table for the Feather River.", call. = FALSE)
    if (rs_all || is.null(rs_norm)) {
      if (rs_all) return(.all_summary_menu(NULL))
      return(.prepare_full_t1())
    }
    if (rs_norm == "sacramento") return(.extract_t1_sac_adjusted())
    if (rs_norm == "san_joaquin") return(.extract_t1_sj_only())
  }

  # Single run.
  if (length(run_norm) == 1) {
    # Validate run/rs combos (mirrors existing validation).
    if (run_norm == "lf" && !is.null(rs_norm) && rs_norm != "sacramento")
      stop("Late-fall run are only found in the Sacramento River System.",
           call. = FALSE)
    if (run_norm %in% c("lf", "s") && !is.null(rs_norm) &&
        rs_norm == "san_joaquin")
      stop("There is no ", run_labels[[run_norm]], " escapement in the ",
           "San Joaquin River system.", call. = FALSE)

    if (run_norm == "f") {
      if (is.null(rs_norm)) return(.fall_summary_menu())
      if (rs_norm == "sacramento") return(.prepare_table(7L, "f"))
      if (rs_norm == "san_joaquin") {
        t6 <- .prepare_table(6L, "f")
        sj_cols <- c("run_year", grep("^f_sj_", names(t6), value = TRUE))
        return(t6[, sj_cols, drop = FALSE])
      }
      if (rs_norm == "feather") {
        # No dedicated summary: return Section 3 feather-system columns.
        t10 <- .prepare_table(10L, "f")
        feat <- c("run_year",
                  grep("^f_(feather|yuba|bear_r)", names(t10), value = TRUE))
        return(t10[, feat, drop = FALSE])
      }
    }

    if (run_norm == "w") {
      if (is.null(rs_norm)) return(.extract_t1_run("w"))
      if (rs_norm == "sacramento") {
        warning("Calaveras River Winter Run data are excluded from totals. ",
                "To include Calaveras River Winter Run data in totals, ",
                "use river_system = NULL.", call. = FALSE)
        t1w <- .extract_t1_run("w")
        t3  <- .prepare_table(3L, "w")
        sac <- t3[, c("run_year", "w_sac_sys_total"), drop = FALSE]
        t1w <- merge(t1w, sac, by = "run_year", all.x = TRUE)
        t1w$w_total       <- t1w$w_sac_sys_total
        t1w$w_sac_sys_total <- NULL
        return(tibble::as_tibble(t1w))
      }
      if (rs_norm == "san_joaquin") {
        t3 <- .prepare_table(3L, "w")
        return(tibble::as_tibble(
          t3[, c("run_year", "w_spawn_period", "w_calaveras"), drop = FALSE]
        ))
      }
    }

    # lf / s: feather has no summary table outside of run="f".
    if (!is.null(rs_norm) && rs_norm == "feather")
      stop("There is no summary table for the Feather River.", call. = FALSE)
    # lf / s with no rs or rs="sacramento": Table 1 for that run.
    return(.extract_t1_run(run_norm))
  }

  # Multi-run: return Table 1.
  .prepare_full_t1()
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
      name <- grandtab_raw[[1]][[1]]
    } else {
      tbl  <- .prepare_table(idx, "f")
      name <- grandtab_raw[[idx]][[1]]
    }
    result[[name]] <- tbl
  }
  if (length(result) == 1) return(result[[1]])
  result
}

# -- Per-hatchery data extractors (raw; origin filter applied by caller) ------

.extract_cnfh_data <- function() {
  # CNFH at Battle Creek: late-fall (table 2) and fall (table 8)
  lf_tbl <- .extract_location_from_table(2L, "lf", "battle")
  f_tbl  <- .extract_location_from_table(8L, "f",  "battle")
  .join_on_run_year(list(lf_tbl, f_tbl))
}

.extract_tcff_data <- function() {
  # TCFF hatchery escapement: standalone column in fall table 9
  raw <- grandtab_raw[[9L]][[2]]
  tibble::tibble(run_year = raw[["YEAR"]], f_tcff = raw[["TCFF"]])
}

.extract_frh_data <- function() {
  # Feather River Hatchery: late-fall (t2), spring (t5), fall (t10)
  .extract_across_runs(c("lf", "s", "f"), "feather", as_list = FALSE)
}

.extract_nfh_data <- function() {
  # Nimbus Hatchery: fall American River data (table 10)
  .extract_fall_locations("american")
}

.extract_sac_hatch_all <- function() {
  list(
    "CNFH - Coleman National Fish Hatchery" = .extract_cnfh_data(),
    "TCFF - Tehama-Colusa Fish Facility"    = .extract_tcff_data(),
    "FRH  - Feather River Hatchery"         = .extract_frh_data(),
    "NFH  - Nimbus Hatchery"                = .extract_nfh_data()
  )
}

# Helper: build a raw (pre-filter) TCFF fall tibble from table 9
.raw_tcff_fall <- function() {
  raw9 <- grandtab_raw[[9L]][[2]]
  tibble::tibble(run_year = raw9[["YEAR"]], f_tcff = raw9[["TCFF"]])
}

# Assemble run-keyed raw data for a set of selected hatcheries.
# Returns a named list with keys LATE-FALL / SPRING / FALL (only those runs
# that have data for the selected hatcheries), or a single tibble when only
# one run is represented.
.build_sac_hatch_result <- function(cnfh, tcff, frh, nfh) {
  lf_parts <- list()
  if (cnfh) lf_parts <- c(lf_parts,
    list(.extract_location_from_table(2L,  "lf", "battle")))
  if (frh)  lf_parts <- c(lf_parts,
    list(.extract_location_from_table(2L,  "lf", "feather")))

  s_parts <- list()
  if (frh)  s_parts <- c(s_parts,
    list(.extract_location_from_table(5L,  "s",  "feather")))

  f_parts <- list()
  if (cnfh) f_parts <- c(f_parts,
    list(.extract_location_from_table(8L,  "f",  "battle")))
  if (tcff) f_parts <- c(f_parts, list(.raw_tcff_fall()))
  if (frh)  f_parts <- c(f_parts,
    list(.extract_location_from_table(10L, "f",  "feather")))
  if (nfh)  f_parts <- c(f_parts, list(.extract_fall_locations("american")))

  result <- list()
  if (length(lf_parts) > 0) result[["LATE-FALL"]] <- .join_on_run_year(lf_parts)
  if (length(s_parts)  > 0) result[["SPRING"]]    <- .join_on_run_year(s_parts)
  if (length(f_parts)  > 0) result[["FALL"]]      <- .join_on_run_year(f_parts)

  if (length(result) == 1) result[[1]] else result
}

# Menu shown when location = "sacramento", section = NULL, origin = "hatch"
.sac_system_hatch_menu <- function() {
  choices <- c(
    "CNFH - Coleman National Fish Hatchery",
    "TCFF - Tehama-Colusa Fish Facility",
    "FRH  - Feather River Hatchery",
    "NFH  - Nimbus Hatchery",
    "All Sacramento System Hatcheries"
  )

  if (!interactive()) {
    return(.build_sac_hatch_result(TRUE, TRUE, TRUE, TRUE))
  }

  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Select Sacramento River system hatchery:")
  if (length(selected) == 0) return(invisible(NULL))

  all <- choices[5] %in% selected
  .build_sac_hatch_result(
    cnfh = all || choices[1] %in% selected,
    tcff = all || choices[2] %in% selected,
    frh  = all || choices[3] %in% selected,
    nfh  = all || choices[4] %in% selected
  )
}

# Menu shown when location = "sacramento" (any section), hatchery = TRUE
# (Sacramento River mainstem: CNFH and TCFF only)
.sac_main_hatch_menu <- function() {
  choices <- c(
    "CNFH - Coleman National Fish Hatchery",
    "TCFF - Tehama-Colusa Fish Facility",
    "All Sacramento River Hatchery Returns"
  )

  if (!interactive()) {
    return(.build_sac_hatch_result(TRUE, TRUE, FALSE, FALSE))
  }

  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Select Sacramento River hatchery:")
  if (length(selected) == 0) return(invisible(NULL))

  all <- choices[3] %in% selected
  .build_sac_hatch_result(
    cnfh = all || choices[1] %in% selected,
    tcff = all || choices[2] %in% selected,
    frh  = FALSE,
    nfh  = FALSE
  )
}

# Menu shown when run = "f", river_system = "sacramento", no section/location.
# Offers the three Sacramento sections (tables 8-10).
# Section 3 (table 10) includes Feather River, Yuba River, Bear River, etc.
.sac_fall_menu <- function() {
  choices <- c(
    "All Sacramento River Sections (1-3)",
    .fall_table_titles[["8"]],
    .fall_table_titles[["9"]],
    .fall_table_titles[["10"]]
  )
  indices <- c(0L, 8L, 9L, 10L)

  if (!interactive()) {
    result <- list()
    for (idx in c(8L, 9L, 10L)) {
      result[[grandtab_raw[[idx]][[1]]]] <- .prepare_table(idx, "f")
    }
    return(result)
  }

  selected <- utils::select.list(choices, multiple = TRUE,
    title = "Select Sacramento River fall run section(s):")
  if (length(selected) == 0) return(invisible(NULL))

  sel_idx <- indices[match(selected, choices)]
  if (0L %in% sel_idx) sel_idx <- c(8L, 9L, 10L)
  sel_idx <- sort(unique(sel_idx))

  result <- list()
  for (idx in sel_idx) {
    result[[grandtab_raw[[idx]][[1]]]] <- .prepare_table(idx, "f")
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
    result[[grandtab_raw[[8]][[1]]]] <-
      .extract_location_from_table(8L, "f", "sac_main")
  if (.fall_table_titles[["9"]] %in% selected)
    result[[grandtab_raw[[9]][[1]]]] <-
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

  # Convert run_year to integer (brackets already stripped above)
  tbl$run_year <- suppressWarnings(as.integer(tbl$run_year))

  # Coerce all non-meta columns to numeric
  meta_cols <- c("run_year", "provisional_data", grep("_spawn_period$", names(tbl), value = TRUE))
  for (col in setdiff(names(tbl), meta_cols)) {
    tbl[[col]] <- suppressWarnings(as.numeric(tbl[[col]]))
  }

  # Trim leading rows where ALL data columns are NA
  meta_re   <- "run_year|spawn_period|provisional"
  data_mask <- !grepl(meta_re, names(tbl))
  if (any(data_mask)) {
    all_na <- apply(tbl[, data_mask, drop = FALSE], 1,
                    function(row) all(is.na(row)))
    first_data <- which(!all_na)
    if (length(first_data) > 0 && first_data[1] > 1) {
      tbl <- tbl[first_data[1]:nrow(tbl), , drop = FALSE]
      row.names(tbl) <- NULL
    }
  }

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
    if (length(section) == 1L) {
      return(.prepare_table(.section_to_table[as.character(section)], "f"))
    }
    result <- list()
    for (s in section) {
      idx  <- .section_to_table[as.character(s)]
      result[[grandtab_raw[[idx]][[1]]]] <- .prepare_table(idx, "f")
    }
    return(result)
  }

  if (!is.null(loc)) {
    if (length(loc) == 1 && loc == "sac_main") {
      return(.sac_main_fall_menu())
    }
    return(.extract_fall_locations(loc))
  }

  if (!is.null(rs)) {
    if (rs == "sacramento") {
      return(.sac_fall_menu())
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
    name <- grandtab_raw[[idx]][[1]]
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
          if (length(section) == 1L) {
            .prepare_table(.section_to_table[as.character(section)], "f")
          } else {
            sec_list <- list()
            for (s in section) {
              idx <- .section_to_table[as.character(s)]
              sec_list[[grandtab_raw[[idx]][[1]]]] <- .prepare_table(idx, "f")
            }
            sec_list
          }
        } else if (!is.null(rs)) {
          .route_fall(rs, NULL, NULL, NULL)
        } else {
          # Multi-run context: return all fall section tables
          fl <- list()
          for (idx in c(8L, 9L, 10L, 11L)) {
            fl[[grandtab_raw[[idx]][[1]]]] <- .prepare_table(idx, "f")
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
    return(.route_fall(rs, loc, summary, section))
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
  result[[grandtab_raw[[1]][[1]]]] <- .prepare_full_t1()
  for (idx in 2:11) {
    ra   <- .table_run[as.character(idx)]
    name <- grandtab_raw[[idx]][[1]]
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
#' @param summary Logical. If \code{TRUE}, return summary-level data. Default \code{FALSE}.
#' @param section Fall run geographic section(s). Accepts integers 1--4,
#'   Roman numerals \code{"I"}--\code{"IV"}, \code{"all"} (all four sections),
#'   or a vector of any of the above. Implies \code{run = "f"} when \code{run}
#'   is \code{NULL}. When \code{river_system} is also specified, sections are
#'   silently restricted to those belonging to that system (sections 1--3 for
#'   Sacramento; section 4 for San Joaquin).
#' @param run_years Numeric vector of years to filter results to, or
#'   \code{NULL} (default) to return all years.
#' @param hatchery Logical or \code{NULL}. Filter columns by return type.
#'   \code{TRUE} returns only hatchery columns; \code{FALSE} returns only
#'   in-river columns (including transfer columns). If \code{hatchery = TRUE}
#'   and the specified location has no hatchery returns, an error is returned.
#'   Default \code{NULL} returns all columns.
#' @return A tibble or named list of tibbles.
#' @export
get_escapement <- function(run = NULL, river_system = NULL, location = NULL,
                           summary = FALSE, section = NULL, run_years = NULL,
                           hatchery = NULL) {

  if (!is.null(hatchery) && !is.logical(hatchery))
    stop("'hatchery' must be TRUE, FALSE, or NULL.", call. = FALSE)

  run_norm <- .normalize_run(run)

  # river_system="all" is a special sentinel: no system filter, but triggers
  # the full summary menu when summary=TRUE and run is NULL.
  rs_all  <- !is.null(river_system) &&
               tolower(trimws(as.character(river_system))) == "all"
  rs_norm <- if (rs_all) NULL else .normalize_river_system(river_system)

  # Detect hatchery abbreviations before generic location normalisation
  is_hatchery_loc <- FALSE
  hatch_info      <- NULL
  if (!is.null(location) && length(location) == 1) {
    hatch_info <- .resolve_hatchery(location)
    if (!is.null(hatch_info)) is_hatchery_loc <- TRUE
  }
  loc_norm <- if (is_hatchery_loc) hatch_info$assoc_loc else .normalize_location(location)

  if (!is.logical(summary) || length(summary) != 1L || is.na(summary))
    stop("'summary' must be TRUE or FALSE.", call. = FALSE)

  # -- Validate run_years -----------------------------------------------------
  if (!is.null(run_years)) {
    if (!is.numeric(run_years) && !all(!is.na(suppressWarnings(as.numeric(run_years))))) {
      stop("'run_years' must be numeric.", call. = FALSE)
    }
    run_years <- as.numeric(run_years)
    cur_year  <- as.integer(format(Sys.Date(), "%Y"))
    bad_yrs   <- run_years[run_years < 1952 | run_years > cur_year]
    if (length(bad_yrs) > 0)
      stop("'run_years' must be between 1952 and ", cur_year, ". ",
           "Invalid year(s): ", paste(bad_yrs, collapse = ", "), ".",
           call. = FALSE)
  }

  # -- Summary routing (early return) -----------------------------------------
  if (isTRUE(summary)) {
    if (!is.null(loc_norm)) {
      stop("location must be NULL if summary = TRUE.", call. = FALSE)
    }
    result    <- .route_summary(run_norm, rs_norm, rs_all)
    finalized <- .finalize_output(result)
    yr_filt   <- .filter_run_years(finalized, run_years)
    filtered  <- .filter_by_origin(yr_filt, hatchery)
    .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                     unfiltered = yr_filt)
    return(filtered)
  }

  # -- Hatchery location handling ---------------------------------------------

  if (is_hatchery_loc) {
    if (isFALSE(hatchery)) {
      stop('hatchery cannot be FALSE when location is a hatchery.', call. = FALSE)
    }
    if (!is.null(run_norm) && !identical(run_norm, "all")) {
      invalid <- run_norm[!run_norm %in% hatch_info$valid_runs]
      if (length(invalid) > 0) {
        run_labels <- c(lf = "Late-Fall Run", w = "Winter Run",
                        s  = "Spring Run",    f  = "Fall Run")
        run_str <- paste(
          unname(run_labels[invalid[invalid %in% names(run_labels)]]),
          collapse = " or "
        )
        stop("No ", hatch_info$display, " data for ", run_str, ".",
             call. = FALSE)
      }
    }
    # Default to the hatchery's valid runs when none specified
    if (is.null(run_norm) || identical(run_norm, "all")) {
      run_norm <- hatch_info$valid_runs
    }
    # When hatchery=NULL, also pull from the transfer source so _trans_ columns
    # are included. The col_suffix filter will keep only the relevant ones.
    if (is.null(hatchery) && !is.null(hatch_info$trans_loc)) {
      loc_norm <- c(loc_norm, hatch_info$trans_loc)
    }
  }

  # -- Validate ---------------------------------------------------------------

  if (!is.null(section)) {
    roman_map <- c(I = 1L, II = 2L, III = 3L, IV = 4L)
    # "all" / "ALL" / "a" expands to 1:4, then filtered by river_system below
    if (length(section) == 1L &&
        grepl("^all?$", trimws(tolower(as.character(section))))) {
      section <- 1:4
    } else {
      section <- vapply(trimws(as.character(section)), function(s) {
        rh <- roman_map[toupper(s)]
        if (!is.na(rh)) return(unname(rh))
        si <- suppressWarnings(as.integer(s))
        if (is.na(si) || as.character(si) != s)
          stop("'section' must be 1, 2, 3, or 4 (or Roman numeral I-IV), ",
               "or \"all\".", call. = FALSE)
        si
      }, integer(1))
      bad <- section[!section %in% 1:4]
      if (length(bad) > 0)
        stop("'section' must be 1, 2, 3, or 4 (or Roman numeral I-IV), ",
             "or \"all\".", call. = FALSE)
    }
    # Silently restrict sections to those matching river_system
    if (!is.null(rs_norm)) {
      if (rs_norm == "sacramento")
        section <- section[section %in% 1:3]
      else if (rs_norm == "san_joaquin")
        section <- section[section == 4L]
    }
    if (length(section) == 0)
      stop("No fall run sections match the specified 'river_system'.",
           call. = FALSE)
    # section implies fall run — if a non-fall run is explicitly specified, error
    if (!is.null(run_norm) && length(run_norm) == 1 &&
        !run_norm %in% c("f", "all"))
      stop("'section' only applies to fall run. Only fall run data is ",
           "organized by geographic section. Did you mean `run = 'f'`?",
           call. = FALSE)
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

  if (!is_hatchery_loc && !is.null(loc_norm) && !is.null(run_norm) &&
      length(run_norm) == 1 && run_norm != "all") {
    run_labels <- c(lf = "Late-Fall Run", w = "Winter Run",
                    s  = "Spring Run",    f  = "Fall Run")
    bad_locs  <- character(0)
    good_locs <- character(0)
    for (loc in loc_norm) {
      if (is.null(.get_table_for_run_location(run_norm, loc))) {
        bad_locs <- c(bad_locs, loc)
      } else {
        good_locs <- c(good_locs, loc)
      }
    }
    if (length(bad_locs) > 0) {
      if (length(good_locs) == 0) {
        # All locations are incompatible — error
        dn_all <- vapply(bad_locs, function(l) {
          dn <- unname(.location_display_names[l])
          if (!is.na(dn)) dn else l
        }, character(1))
        if (length(dn_all) == 1) {
          stop("There is no ", run_labels[[run_norm]], " escapement ",
               .loc_phrase(dn_all), ".", call. = FALSE)
        } else {
          stop("There is no ", run_labels[[run_norm]], " escapement at any of: ",
               paste(dn_all, collapse = ", "), ".", call. = FALSE)
        }
      } else {
        # Some locations are OK — warn about the bad ones and continue
        for (loc in bad_locs) {
          dn <- unname(.location_display_names[loc])
          dn <- if (!is.na(dn)) dn else loc
          warning("There is no ", run_labels[[run_norm]], " escapement ",
                  .loc_phrase(dn), ". Skipping.", call. = FALSE)
        }
        loc_norm <- good_locs
      }
    }
  }

  if (isTRUE(hatchery) && !is.null(loc_norm)) {
    run_labels <- c(lf = "Late-Fall Run", w = "Winter Run",
                    s  = "Spring Run",    f  = "Fall Run")
    # Treat run = "all" like NULL for hatchery checking (all runs requested)
    check_run <- if (identical(run_norm, "all")) NULL else run_norm

    for (loc in loc_norm) {
      # sac_main hatchery routing is special-cased in the intercept below;
      # run/hatchery validation for that path is handled there.
      if (loc == "sac_main") next

      dn <- unname(.location_display_names[loc])
      if (is.na(dn)) dn <- loc

      if (!.location_has_hatchery(loc, run_norm = NULL)) {
        stop("There is no hatchery ", .loc_phrase(dn, "on"), ".", call. = FALSE)
      }
      if (!is.null(check_run) && !.location_has_hatchery(loc, check_run)) {
        known     <- check_run[check_run %in% names(run_labels)]
        run_str   <- paste(unname(run_labels[known]), collapse = " or ")
        hatch_name <- .location_hatchery_display(loc)
        at_str    <- if (!is.null(hatch_name)) paste0("at ", hatch_name)
                     else paste0("in the ", dn)
        stop("There are no ", run_str, " hatchery returns ", at_str, ".",
             call. = FALSE)
      }
    }
  }

  # -- Special cases: Sacramento mainstem + hatch origin ---------------------
  # Transfer columns (sac_main_trans_cnfh etc.) are not hatchery-origin fish,
  # so they are excluded from the origin filter. Instead, route to per-hatchery
  # data pulled from the relevant tributary/table directly.

  if (isTRUE(hatchery) &&
      !is.null(loc_norm) && length(loc_norm) == 1 && loc_norm == "sac_main") {

    run_key_map <- c(lf = "LATE-FALL", w = "WINTER", s = "SPRING", f = "FALL")
    run_labels  <- c(lf = "Late-Fall Run", w = "Winter Run",
                     s  = "Spring Run",    f  = "Fall Run")

    # Validate run BEFORE showing any interactive menu.
    # Sac system hatcheries cover late-fall, spring, and fall only.
    if (!is.null(run_norm) && !identical(run_norm, "all")) {
      keep_keys <- unname(run_key_map[run_norm])
      keep_keys <- keep_keys[keep_keys %in% c("LATE-FALL", "SPRING", "FALL")]
      if (length(keep_keys) == 0) {
        known   <- run_norm[run_norm %in% names(run_labels)]
        run_str <- paste(unname(run_labels[known]), collapse = " or ")
        trans_names <- .find_transfer_displays("sac_main", run_norm)
        transfer_clause <- if (length(trans_names) > 0) {
          hatch_str <- if (length(trans_names) == 1) {
            trans_names
          } else {
            paste0(paste(trans_names[-length(trans_names)], collapse = ", "),
                   " and ", trans_names[length(trans_names)])
          }
          paste0(", only transfers to ", hatch_str)
        } else NULL
        stop("There are no ", run_str,
             " hatchery returns in the Sacramento River",
             if (!is.null(transfer_clause)) transfer_clause else "",
             ".", call. = FALSE)
      }
    }

    .filter_sac_hatch_runs <- function(raw) {
      if (is.null(run_norm) || identical(run_norm, "all") || !is.list(raw))
        return(raw)
      if (is.data.frame(raw)) {
        # .build_sac_hatch_result() collapses single-element lists to a data
        # frame.  Check that its columns are actually for the requested run;
        # if not (e.g. TCFF selected for spring but result is fall data),
        # treat as empty so .assert_has_data() can produce the right error.
        pfx <- paste0("^(", paste(run_norm, collapse = "|"), ")_")
        if (!any(grepl(pfx, names(raw)))) return(list())
        return(raw)
      }
      keep <- unname(run_key_map[run_norm])
      keep <- keep[keep %in% names(raw)]
      if (length(keep) == 1) raw[[keep]] else raw[keep]
    }

    # section = NULL  →  Sacramento River hatchery menu (CNFH / TCFF / All)
    if (is.null(section)) {
      raw_menu   <- .sac_main_hatch_menu()
      selected   <- .detect_hatcheries_in_result(raw_menu)
      raw        <- .filter_sac_hatch_runs(raw_menu)
      unfiltered <- .filter_run_years(.finalize_output(raw), run_years)
      filtered   <- .filter_by_origin(unfiltered, hatchery)
      .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                       unfiltered = unfiltered, selected_hatch_display = selected)
      return(filtered)
    }

    # section = 1  →  upper-Sacramento menu (CNFH / TCFF)
    if (section == 1L) {
      raw_menu   <- .sac_main_hatch_menu()
      selected   <- .detect_hatcheries_in_result(raw_menu)
      raw        <- .filter_sac_hatch_runs(raw_menu)
      unfiltered <- .filter_run_years(.finalize_output(raw), run_years)
      filtered   <- .filter_by_origin(unfiltered, hatchery)
      .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                       unfiltered = unfiltered, selected_hatch_display = selected)
      return(filtered)
    }
  }

  # -- Special case: Sacramento river_system + hatchery=TRUE ------------------
  # Show the full system hatchery menu (CNFH / TCFF / FRH / NFH / All).

  if (isTRUE(hatchery) && !is.null(rs_norm) && rs_norm == "sacramento" &&
      is.null(loc_norm)) {

    run_key_map <- c(lf = "LATE-FALL", w = "WINTER", s = "SPRING", f = "FALL")
    run_labels  <- c(lf = "Late-Fall Run", w = "Winter Run",
                     s  = "Spring Run",    f  = "Fall Run")

    # Validate run before showing menu (sac hatcheries cover lf, s, f only).
    if (!is.null(run_norm) && !identical(run_norm, "all")) {
      keep_keys <- unname(run_key_map[run_norm])
      keep_keys <- keep_keys[keep_keys %in% c("LATE-FALL", "SPRING", "FALL")]
      if (length(keep_keys) == 0) {
        known   <- run_norm[run_norm %in% names(run_labels)]
        run_str <- paste(unname(run_labels[known]), collapse = " or ")
        trans_names <- .find_transfer_displays("sac_main", run_norm)
        transfer_clause <- if (length(trans_names) > 0) {
          ht <- if (length(trans_names) == 1) {
            trans_names
          } else {
            paste0(paste(trans_names[-length(trans_names)], collapse = ", "),
                   " and ", trans_names[length(trans_names)])
          }
          paste0(", only transfers to ", ht)
        } else NULL
        stop("There are no ", run_str,
             " hatchery returns in the Sacramento River system",
             if (!is.null(transfer_clause)) transfer_clause else "",
             ".", call. = FALSE)
      }
    }

    filter_by_run <- function(raw) {
      if (is.null(run_norm) || identical(run_norm, "all") || !is.list(raw))
        return(raw)
      if (is.data.frame(raw)) {
        pfx <- paste0("^(", paste(run_norm, collapse = "|"), ")_")
        if (!any(grepl(pfx, names(raw)))) return(list())
        return(raw)
      }
      keep <- unname(run_key_map[run_norm])
      keep <- keep[keep %in% names(raw)]
      if (length(keep) == 1) raw[[keep]] else raw[keep]
    }

    raw_menu   <- .sac_system_hatch_menu()
    selected   <- .detect_hatcheries_in_result(raw_menu)
    raw        <- filter_by_run(raw_menu)
    unfiltered <- .filter_run_years(.finalize_output(raw), run_years)
    filtered   <- .filter_by_origin(unfiltered, hatchery)
    .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                     unfiltered = unfiltered, selected_hatch_display = selected)
    return(filtered)
  }

  # -- Special case: Sacramento mainstem + hatchery=FALSE + no section --------
  # Tables 8 and 9 both use the same snake_case name ("sac_main_inr") for the
  # mainstem in-river column. Merging them via .extract_fall_locations() adds
  # .x/.y suffixes that break the _inr$ filter, producing an empty result.
  # Use Fall Run Summary II (table 7) for fall instead — it carries clean
  # section-labelled in-river columns (kesw_rbdd_main_inr, etc.).

  if (isFALSE(hatchery) &&
      !is.null(loc_norm) && length(loc_norm) == 1 && loc_norm == "sac_main" &&
      is.null(section) &&
      (is.null(run_norm) || "f" %in% run_norm || identical(run_norm, "all"))) {

    fall_tbl <- .prepare_table(7L, "f")

    if (!is.null(run_norm) && length(run_norm) == 1 && run_norm == "f") {
      result <- fall_tbl
    } else {
      # Null run or multi-run: fetch lf/w/s normally, slot fall from table 7.
      other_runs <- if (is.null(run_norm) || identical(run_norm, "all"))
        c("lf", "w", "s")
      else
        run_norm[!run_norm %in% c("f", "all")]
      result <- if (length(other_runs) > 0)
        .extract_across_runs(other_runs, loc_norm, as_list = TRUE)
      else
        list()
      result[["FALL"]] <- fall_tbl
    }

    unfiltered <- .filter_run_years(.finalize_output(result), run_years)
    out        <- .filter_by_origin(unfiltered, hatchery)
    # location="sacramento" is the mainstem — drop tributary aggregates
    drop_tribs <- function(tbl) {
      if (!is.data.frame(tbl)) return(tbl)
      tbl[, !grepl("_tribs_", names(tbl)), drop = FALSE]
    }
    filtered <- if (is.data.frame(out)) drop_tribs(out) else lapply(out, drop_tribs)
    .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                     unfiltered = unfiltered)
    return(filtered)
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

  out <- .filter_run_years(.finalize_output(result), run_years)

  # Emit a message if current year was requested but has no data yet
  if (!is.null(run_years)) {
    cur_year <- as.integer(format(Sys.Date(), "%Y"))
    if (cur_year %in% run_years) {
      has_cur <- function(tbl) {
        if (!is.data.frame(tbl)) return(FALSE)
        any(!is.na(suppressWarnings(as.numeric(tbl$run_year))) &
              suppressWarnings(as.numeric(tbl$run_year)) == cur_year, na.rm = TRUE)
      }
      found_cur <- if (is.data.frame(out)) has_cur(out) else any(vapply(out, has_cur, logical(1)))
      if (!found_cur)
        message("Data for ", cur_year, " have not yet been posted to GrandTab.")
    }
  }

  filtered <- if (is_hatchery_loc) {
    .filter_hatch_only(out, hatch_info$col_suffix)
  } else {
    .filter_by_origin(out, hatchery)
  }
  .assert_has_data(filtered, hatchery, run_norm, rs_norm, loc_norm,
                   unfiltered = out)
  filtered
}
