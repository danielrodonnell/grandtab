# ============================================================================
# get_locations.R
#
# Returns metadata about GrandTab Chinook salmon monitoring locations,
# optionally filtered by run, river system, or specific location.
# ============================================================================

# -- Static location metadata -------------------------------------------------

.river_system_map <- c(
  sac_main     = "sacramento",  battle       = "sacramento",
  clear        = "sacramento",  cottonwood   = "sacramento",
  salt         = "sacramento",  craig        = "sacramento",
  feather      = "feather",     antelope     = "sacramento",
  mill         = "sacramento",  deer         = "sacramento",
  big_chico    = "sacramento",  butte        = "sacramento",
  yuba         = "feather",     american     = "sacramento",
  bear_r       = "feather",     bear_ck      = "sacramento",
  dry          = "sacramento",  natomas      = "sacramento",
  cosumnes     = "san_joaquin", mokelumne    = "san_joaquin",
  stanislaus   = "san_joaquin", tuolumne     = "san_joaquin",
  merced       = "san_joaquin", calaveras    = "san_joaquin",
  spring_gulch = "sacramento",  china_gulch  = "sacramento",
  olney        = "sacramento",  paynes       = "sacramento",
  cow          = "sacramento",  ash          = "sacramento",
  stillwater   = "sacramento",  inks         = "sacramento",
  dye          = "sacramento",  toomes       = "sacramento",
  thomes       = "sacramento",  coyote       = "sacramento",
  stoney       = "sacramento",  singer       = "sacramento"
)

# Approximate terminus / confluence coordinates (longitude, latitude)
.river_terminus_coords <- list(
  sac_main     = c(-121.828, 38.058),   # Sacramento River at Delta
  battle       = c(-122.149, 40.399),   # confluence with Sacramento
  clear        = c(-122.461, 40.556),   # confluence with Sacramento near Redding
  cottonwood   = c(-122.314, 40.393),   # confluence with Sacramento near Anderson
  salt         = c(-122.190, 39.843),   # confluence with Sacramento
  craig        = c(-121.980, 40.385),   # confluence with Sacramento
  feather      = c(-121.609, 38.773),   # confluence with Sacramento near Verona
  antelope     = c(-122.186, 40.059),   # confluence with Sacramento near Red Bluff
  mill         = c(-122.092, 39.916),   # confluence with Sacramento near Los Molinos
  deer         = c(-122.065, 39.975),   # confluence with Sacramento near Vina
  big_chico    = c(-121.859, 39.723),   # confluence with Sacramento near Nord
  butte        = c(-121.851, 39.535),   # confluence into Sutter Bypass
  yuba         = c(-121.594, 39.140),   # confluence with Feather near Marysville
  american     = c(-121.496, 38.588),   # confluence with Sacramento at Sacramento
  bear_r       = c(-121.604, 38.972),   # confluence with Feather near Wheatland
  bear_ck      = c(-122.265, 40.386),   # confluence with Sacramento, Tehama Co.
  dry          = c(-121.436, 38.858),   # confluence with Sacramento near Roseville
  natomas      = c(-121.514, 38.682),   # Natomas, Sacramento area
  cosumnes     = c(-121.404, 38.259),   # confluence with Mokelumne
  mokelumne    = c(-121.487, 38.207),   # Delta near Walnut Grove
  stanislaus   = c(-121.054, 37.742),   # confluence with San Joaquin near Ripon
  tuolumne     = c(-120.946, 37.645),   # confluence with San Joaquin near Modesto
  merced       = c(-120.978, 37.352),   # confluence with San Joaquin near Newman
  calaveras    = c(-121.312, 37.966),   # confluence with San Joaquin near Stockton
  spring_gulch = c(-122.255, 40.141),   # tributary near Red Bluff
  china_gulch  = c(-122.900, 40.140),   # Beegum Creek area
  olney        = c(-122.190, 40.228),   # confluence with Sacramento near Corning
  paynes       = c(-122.142, 40.237),   # confluence with Sacramento
  cow          = c(-122.215, 40.459),   # confluence with Sacramento near Anderson
  ash          = c(-122.340, 40.027),   # confluence with Sacramento
  stillwater   = c(-122.217, 40.087),   # confluence with Sacramento near Corning
  inks         = c(-121.938, 40.558),   # Cow Creek tributary near Millville
  dye          = c(-122.193, 40.097),   # tributary near Red Bluff
  toomes       = c(-122.193, 39.930),   # confluence with Sacramento near Corning
  thomes       = c(-122.494, 39.851),   # confluence with Sacramento near Newville
  coyote       = c(-122.540, 39.440),   # Stony Creek tributary
  stoney       = c(-122.208, 39.748),   # confluence with Sacramento near Orland
  singer       = c(-122.199, 40.161)    # tributary near Red Bluff
)

# Hatchery location metadata (coordinates from build_gpkg.R point_features)
.hatchery_location_meta <- data.frame(
  hatch_key    = c("lsnfh", "cnfh",    "tcff",    "frh",      "nfh",      "mrh",      "merh"),
  river_system = c("sacramento", "sacramento", "sacramento", "feather", "sacramento", "san_joaquin", "san_joaquin"),
  lon          = c(-122.4162, -122.1435, -122.2278, -121.5547, -121.2240, -121.0667, -120.4117),
  lat          = c( 40.7196,   40.3965,   40.1556,   39.5169,   38.6302,   38.2267,   37.5183),
  stringsAsFactors = FALSE
)

# -- Main function ------------------------------------------------------------

#' Get metadata for GrandTab monitoring locations
#'
#' Returns a named list of tibbles (one per run) or a single tidy tibble
#' describing the locations where Chinook salmon escapement is monitored.
#'
#' @param run Character. Run to filter by: \code{"lf"} (late-fall), \code{"w"}
#'   (winter), \code{"s"} (spring), \code{"f"} (fall), or \code{NULL} for all.
#' @param river_system Character. River system to filter by: \code{"sacramento"},
#'   \code{"san joaquin"}, \code{"feather"}, or \code{NULL} for all.
#' @param location Character scalar or vector. Location or hatchery name(s) to
#'   filter by, or \code{NULL} for all. Unrecognised names produce a warning and
#'   are skipped.
#' @param as_tibble Logical. If \code{TRUE}, return a single tidy tibble with
#'   a \code{run} column instead of a list of tibbles. Default \code{FALSE}.
#'
#' @return A named list of tibbles (default) or a single tibble
#'   (\code{as_tibble = TRUE}), with columns \code{river_system},
#'   \code{location_name}, \code{location_type}, \code{longitude},
#'   \code{latitude}.
#'
#' @export
get_locations <- function(run = NULL, river_system = NULL, location = NULL,
                          as_tibble = FALSE) {

  run_label_map <- c(lf = "LATE-FALL", w = "WINTER", s = "SPRING", f = "FALL")

  # -- Normalize inputs -------------------------------------------------------
  run_norm <- .normalize_run(run)
  rs_norm  <- .normalize_river_system(river_system)

  loc_norms  <- character(0)
  hatch_keys <- character(0)

  if (!is.null(location)) {
    for (loc in location) {
      hatch_info <- .resolve_hatchery(loc)
      if (!is.null(hatch_info)) {
        key <- names(Filter(
          function(h) h$col_suffix == hatch_info$col_suffix, .hatchery_patterns
        ))
        hatch_keys <- c(hatch_keys, key)
      } else {
        ln <- tryCatch(.normalize_location(loc), error = function(e) NULL)
        if (!is.null(ln)) {
          loc_norms <- c(loc_norms, ln)
        } else {
          warning("Location '", loc, "' not found in GrandTab. Skipping.",
                  call. = FALSE)
        }
      }
    }
    if (length(loc_norms) == 0 && length(hatch_keys) == 0)
      stop("None of the specified location(s) were found in GrandTab.",
           call. = FALSE)
  }

  # -- Build river rows -------------------------------------------------------
  # tcff is a hatchery-only location; exclude it from river rows.
  river_ids <- setdiff(names(.river_system_map), "tcff")

  river_long <- do.call(rbind, lapply(river_ids, function(lid) {
    tables <- .location_cols[[lid]]
    if (is.null(tables)) return(NULL)

    coords <- .river_terminus_coords[[lid]]
    rs     <- .river_system_map[[lid]]
    dn     <- unname(.location_display_names[lid])

    if (is.null(coords) || is.null(rs) || is.na(dn)) return(NULL)

    run_codes  <- unique(unname(.table_run[names(tables)]))
    run_labels <- run_label_map[run_codes]
    run_labels <- run_labels[!is.na(run_labels)]

    if (length(run_labels) == 0) return(NULL)

    data.frame(
      location_id   = lid,
      location_name = dn,
      location_type = "river",
      river_system  = rs,
      longitude     = coords[1],
      latitude      = coords[2],
      run           = unname(run_labels),
      run_code      = names(run_labels),
      stringsAsFactors = FALSE
    )
  }))

  # -- Build hatchery rows ----------------------------------------------------
  hatchery_long <- do.call(rbind, lapply(names(.hatchery_patterns), function(key) {
    h    <- .hatchery_patterns[[key]]
    meta <- .hatchery_location_meta[.hatchery_location_meta$hatch_key == key, ]
    if (nrow(meta) == 0) return(NULL)

    run_codes  <- h$valid_runs
    run_labels <- run_label_map[run_codes]
    run_labels <- run_labels[!is.na(run_labels)]

    if (length(run_labels) == 0) return(NULL)

    data.frame(
      location_id   = key,
      location_name = h$display,
      location_type = "hatchery",
      river_system  = meta$river_system,
      longitude     = meta$lon,
      latitude      = meta$lat,
      run           = unname(run_labels),
      run_code      = names(run_labels),
      stringsAsFactors = FALSE
    )
  }))

  # -- Combine ----------------------------------------------------------------
  all_rows <- rbind(river_long, hatchery_long)

  # -- Filter -----------------------------------------------------------------
  if (length(loc_norms) > 0 || length(hatch_keys) > 0) {
    river_match   <- all_rows$location_id %in% loc_norms
    hatchery_match <- all_rows$location_id %in% hatch_keys &
                        all_rows$location_type == "hatchery"
    all_rows <- all_rows[river_match | hatchery_match, ]
  }

  if (!is.null(rs_norm)) {
    all_rows <- all_rows[all_rows$river_system == rs_norm, ]
  }

  if (!is.null(run_norm) && !identical(run_norm, "all")) {
    all_rows <- all_rows[all_rows$run_code %in% run_norm, ]
  }

  # -- Check for empty results after run/river_system filters ----------------
  if (!is.null(location) && nrow(all_rows) == 0) {
    if (!is.null(run_norm) && !identical(run_norm, "all")) {
      run_label_map2 <- c(lf = "Late-Fall Run", w = "Winter Run",
                          s  = "Spring Run",    f  = "Fall Run")
      run_str  <- paste(unname(run_label_map2[run_norm]), collapse = " or ")
      loc_disp <- if (length(loc_norms) == 1) {
        dn <- unname(.location_display_names[loc_norms])
        if (!is.na(dn)) dn else loc_norms
      } else paste(location, collapse = ", ")
      stop("There is no ", run_str, " escapement ",
           .loc_phrase(loc_disp), ".", call. = FALSE)
    } else if (!is.null(rs_norm)) {
      rs_display <- c(sacramento = "Sacramento", san_joaquin = "San Joaquin",
                      feather = "Feather")
      stop("No matching locations found in the ",
           unname(rs_display[rs_norm]), " River system.", call. = FALSE)
    } else {
      stop("No locations found for the specified filters.", call. = FALSE)
    }
  } else if (is.null(location) && nrow(all_rows) == 0) {
    # No location filter but still empty (e.g. run + river_system combo)
    if (!is.null(run_norm) && !is.null(rs_norm)) {
      run_label_map2 <- c(lf = "Late-Fall Run", w = "Winter Run",
                          s  = "Spring Run",    f  = "Fall Run")
      run_str <- paste(unname(run_label_map2[run_norm[!is.na(run_norm)]]), collapse = " or ")
      rs_display <- c(sacramento = "Sacramento", san_joaquin = "San Joaquin",
                      feather = "Feather")
      rs_str <- unname(rs_display[rs_norm])
      stop("There is no ", run_str, " escapement in the ",
           rs_str, " River system.", call. = FALSE)
    }
  }

  all_rows <- unique(all_rows)

  # -- Capitalize river_system ------------------------------------------------
  rs_labels <- c(sacramento  = "Sacramento",
                 san_joaquin = "San Joaquin",
                 feather     = "Feather")
  all_rows$river_system <- unname(rs_labels[all_rows$river_system])

  # -- Format output ----------------------------------------------------------
  output_cols <- c("river_system", "location_name", "location_type",
                   "run", "longitude", "latitude")

  if (as_tibble) {
    result <- all_rows[, output_cols]
    result <- result[order(match(result$run, c("LATE-FALL", "WINTER", "SPRING", "FALL")),
                           result$river_system, result$location_name), ]
    row.names(result) <- NULL
    return(tibble::as_tibble(result))
  }

  ordered_runs  <- c("LATE-FALL", "WINTER", "SPRING", "FALL")
  runs_present  <- ordered_runs[ordered_runs %in% unique(all_rows$run)]

  result <- lapply(runs_present, function(r) {
    sub <- all_rows[all_rows$run == r, output_cols]
    sub <- sub[order(sub$river_system, sub$location_name), ]
    row.names(sub) <- NULL
    tibble::as_tibble(sub)
  })
  names(result) <- runs_present
  result
}
