#' @include get_escapement.R
# ============================================================================
# map_grandtab.R
#
# Interactive leaflet map of GrandTab Chinook salmon monitoring locations.
# Supports highlighting by location, river system, or fall section, with
# click-to-plot escapement time series via get_escapement().
# ============================================================================

# -- Dependency check --------------------------------------------------------

.check_map_deps <- function() {
  missing <- character(0)
  for (pkg in c("leaflet", "shiny", "sf", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  if (length(missing) > 0) {
    stop("map_grandtab() requires: ", paste(missing, collapse = ", "),
         "\nInstall with: install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }
}

# -- Load spatial data -------------------------------------------------------

.load_spatial <- function() {
  fname <- "grandtab_spatial.rds"
  path <- system.file("extdata", fname, package = "grandtab")

  if (nzchar(path) && file.exists(path)) return(readRDS(path))

  # Fall back for development mode — build candidate paths
  candidates <- c(
    file.path("inst", "extdata", fname),           # from package root
    file.path("..", "inst", "extdata", fname)       # from R/
  )

  # Also try to locate from the source file's own path
  for (i in rev(seq_len(sys.nframe()))) {
    ofile <- tryCatch(get("ofile", envir = sys.frame(i)), error = function(e) NULL)
    if (!is.null(ofile)) {
      script_dir <- normalizePath(dirname(ofile), mustWork = FALSE)
      # This file lives in R/, so go up one to package root
      pkg_root <- dirname(script_dir)
      candidates <- c(file.path(pkg_root, "inst", "extdata", fname), candidates)
      break
    }
  }

  # Walk up from getwd() looking for inst/extdata/
  wd <- getwd()
  for (d in c(wd, dirname(wd), dirname(dirname(wd)))) {
    p <- file.path(d, "inst", "extdata", fname)
    candidates <- c(candidates, p)
  }

  for (p in candidates) {
    if (file.exists(p)) return(readRDS(p))
  }

  stop("grandtab_spatial.rds not found. ",
       "Run data-raw/build_spatial.R to generate the spatial data, ",
       "or reinstall the grandtab package.", call. = FALSE)
}

# -- Normalize map location (extends .normalize_location for hatcheries) -----

.normalize_map_location <- function(location) {
  if (is.null(location)) return(NULL)
  loc <- tolower(trimws(location))

  # Hatchery patterns — try these first
  hatchery_patterns <- list(
    CNFH = c("^cnfh", "^coleman"),
    TCFF = c("^tcff", "^tehama"),
    FRFH = c("^frfh", "^frh", "feather.*hatch"),
    NFH  = c("^nfh", "^nimbus"),
    MRH  = c("^mrh", "mokelumne.*hatch", "^mok.*hatch"),
    MeRH = c("^merh", "merced.*hatch", "^merc.*hatch", "merced.*facil")
  )

  # Associated locations for each hatchery
  hatchery_assoc <- c(
    CNFH = "battle", TCFF = "sac_sec2",
    FRFH = "feather", NFH = "american",
    MRH = "mokelumne", MeRH = "merced"
  )

  for (hid in names(hatchery_patterns)) {
    for (pat in hatchery_patterns[[hid]]) {
      if (grepl(pat, loc)) {
        return(list(type = "hatchery", id = hid,
                    assoc_location = unname(hatchery_assoc[hid])))
      }
    }
  }

  # Sacramento River mainstem → highlight all 3 sections
  if (grepl("^sac(ramento)?($|\\s|_|r)", loc) || grepl("^sac_main$", loc)) {
    return(list(type = "sac_river", id = "sac_river",
                assoc_location = c("sac_sec1", "sac_sec2", "sac_sec3")))
  }

  # Bear disambiguation — "bear" alone matches both Bear Creek and Bear River
  if (grepl("^bear$", loc)) {
    choice <- utils::menu(
      choices = c("Bear Creek", "Bear River"),
      title = "Multiple matches for 'bear'. Please select:"
    )
    if (choice == 0) stop("No selection made.", call. = FALSE)
    loc_id <- c("bear_ck", "bear_r")[choice]
    return(list(type = "location", id = loc_id, assoc_location = loc_id))
  }

  # Not a hatchery — delegate to .normalize_location()
  loc_id <- .normalize_location(location)
  list(type = "location", id = loc_id, assoc_location = loc_id)
}

# -- Get available runs for a location ---------------------------------------

.get_available_runs <- function(loc_id) {
  # Sacramento sections map to sac_main for data queries
  query_id <- .map_loc_to_data_id(loc_id)
  tables <- names(.location_cols[[query_id]])
  if (is.null(tables)) return(character(0))
  runs <- unique(unname(.table_run[tables]))
  sort(runs)
}

# Map spatial location_id to get_escapement() location_id
.map_loc_to_data_id <- function(loc_id) {
  if (loc_id %in% c("sac_sec1", "sac_sec2", "sac_sec3")) return("sac_main")
  loc_id
}

# -- Resolve point features display logic ------------------------------------

.resolve_points <- function(loc_info, show_hatcheries, spatial) {
  points_sf <- spatial$points

  # Landmarks (e.g. RBDD) are always shown
  landmarks <- points_sf[points_sf$point_type == "landmark", ]
  hatcheries <- points_sf[points_sf$point_type == "hatchery", ]

  if (isFALSE(show_hatcheries)) {
    return(landmarks)
  }

  if (isTRUE(show_hatcheries)) {
    return(points_sf)
  }

  # show_hatcheries is NULL — show hatcheries associated with selected location
  if (!is.null(loc_info)) {
    assoc_locs <- loc_info$assoc_location
    assoc_hatch <- hatcheries[hatcheries$associated_location %in% assoc_locs, ]
    return(rbind(landmarks, assoc_hatch))
  }

  # Default: show all
  points_sf
}

# -- Build leaflet map -------------------------------------------------------

.add_flowline_layer <- function(map, data, layer_suffix, color, weight,
                                opacity, highlight_color) {
  if (nrow(data) == 0) return(map)

  # Split by geometry type — tiny creeks may be POINTs
  geom_types <- as.character(sf::st_geometry_type(data))

  is_line <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
  lines <- data[is_line, ]
  points <- data[!is_line, ]

  # Cast subsets to concrete types — the parent sfc_GEOMETRY class won't work
  # with leaflet, which needs sfc_LINESTRING/sfc_POINT specifically
  if (nrow(lines) > 0) {
    lines <- sf::st_cast(lines, "MULTILINESTRING")
    map <- map |>
      leaflet::addPolylines(
        data = lines,
        layerId = ~paste0(location_id, layer_suffix),
        label = ~display_name,
        color = color, weight = weight, opacity = opacity,
        highlightOptions = leaflet::highlightOptions(
          color = highlight_color, weight = max(weight + 1, 3),
          opacity = min(opacity + 0.4, 1.0), bringToFront = TRUE
        )
      )
  }

  if (nrow(points) > 0) {
    points <- sf::st_cast(points, "POINT")
    map <- map |>
      leaflet::addCircleMarkers(
        data = points,
        layerId = ~paste0(location_id, layer_suffix),
        label = ~display_name,
        radius = 5,
        color = color,
        fillColor = color,
        fillOpacity = opacity,
        stroke = TRUE,
        weight = 1
      )
  }

  map
}

.build_leaflet_map <- function(spatial, args) {
  flowlines <- spatial$flowlines

  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(args$base_map)

  # Color scheme
  default_color   <- "#4A90D9"
  highlight_color <- "#FF6B35"
  muted_color     <- "#CCCCCC"

  has_highlight <- !is.null(args$loc_info) || !is.null(args$river_system) ||
                   !is.null(args$section)

  if (has_highlight) {
    # Determine which location_ids to highlight
    highlight_ids <- character(0)

    if (!is.null(args$loc_info)) {
      # assoc_location may be a vector (e.g. all 3 sac sections)
      highlight_ids <- args$loc_info$assoc_location
    } else if (!is.null(args$river_system)) {
      highlight_ids <- flowlines$location_id[flowlines$river_system == args$river_system]
    } else if (!is.null(args$section)) {
      highlight_ids <- flowlines$location_id[
        !is.na(flowlines$fall_section) & flowlines$fall_section == args$section
      ]
    }

    # Muted background layer
    bg <- flowlines[!flowlines$location_id %in% highlight_ids, ]
    map <- .add_flowline_layer(map, bg, "", muted_color, 1, 0.3,
                               highlight_color)

    # Highlighted foreground layer
    fg <- flowlines[flowlines$location_id %in% highlight_ids, ]
    map <- .add_flowline_layer(map, fg, "_highlight", highlight_color, 4, 1.0,
                               "#FF4500")

    # Zoom to highlighted extent
    if (nrow(fg) > 0) {
      bb <- sf::st_bbox(fg)
      map <- map |>
        leaflet::fitBounds(
          lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
          lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
        )
    }
  } else {
    # No highlighting — show all flowlines in default color
    map <- .add_flowline_layer(map, flowlines, "", default_color, 2, 0.6,
                               highlight_color)

    # Fit to Central Valley bounds
    map <- map |>
      leaflet::fitBounds(
        lng1 = -123.0, lat1 = 36.8, lng2 = -120.0, lat2 = 41.0
      )
  }

  # Point markers (hatcheries and landmarks)
  if (!is.null(args$points_to_show) && nrow(args$points_to_show) > 0) {
    pts <- args$points_to_show

    # Hatcheries: red
    hatch <- pts[pts$point_type == "hatchery", ]
    if (nrow(hatch) > 0) {
      map <- map |>
        leaflet::addCircleMarkers(
          data = hatch,
          layerId = ~paste0("point_", point_id),
          label = ~display_name,
          radius = 7,
          color = "#E63946",
          fillColor = "#E63946",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2
        )
    }

    # Landmarks: dark blue
    lmk <- pts[pts$point_type == "landmark", ]
    if (nrow(lmk) > 0) {
      map <- map |>
        leaflet::addCircleMarkers(
          data = lmk,
          layerId = ~paste0("point_", point_id),
          label = ~display_name,
          radius = 7,
          color = "#2B4570",
          fillColor = "#2B4570",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2
        )
    }
  }

  map
}

# -- Extract plot data from get_escapement() ---------------------------------

.extract_plot_data <- function(loc_id, run_code, run_years) {
  run_prefix <- switch(run_code, lf = "lf", w = "w", s = "s", f = "f")
  run_label  <- switch(run_code,
    lf = "Late-Fall", w = "Winter", s = "Spring", f = "Fall"
  )

  tbl <- tryCatch(
    get_escapement(run = run_code, location = loc_id, run_years = run_years),
    error = function(e) NULL
  )

  if (is.null(tbl)) return(NULL)

  # Handle list results (e.g., sac_main fall returns multiple tables)
  if (is.list(tbl) && !is.data.frame(tbl)) {
    # Combine all data frames
    tbl <- tryCatch({
      dfs <- Filter(is.data.frame, tbl)
      if (length(dfs) == 0) return(NULL)
      # Use the first one that has a total column
      for (df in dfs) {
        total_col <- grep(paste0("^", run_prefix, "_.*total"), names(df),
                          value = TRUE, ignore.case = TRUE)
        if (length(total_col) > 0) {
          tbl <- df
          break
        }
      }
      if (is.list(tbl) && !is.data.frame(tbl)) tbl <- dfs[[1]]
      tbl
    }, error = function(e) NULL)
    if (is.null(tbl)) return(NULL)
  }

  # Find "total" column or first data column
  total_col <- grep(paste0("^", run_prefix, "_.*total$"), names(tbl),
                    value = TRUE, ignore.case = TRUE)
  if (length(total_col) == 0) {
    # Fall back to first data column (not run_year, not spawn_period)
    data_cols <- setdiff(names(tbl),
      c("run_year", grep("spawn_period|provisional", names(tbl), value = TRUE)))
    if (length(data_cols) == 0) return(NULL)
    total_col <- data_cols[1]
  } else {
    total_col <- total_col[1]
  }

  # Parse year and escapement
  year <- suppressWarnings(as.numeric(gsub("\\[|\\]", "", tbl$run_year)))
  escapement <- suppressWarnings(as.numeric(gsub(",", "", tbl[[total_col]])))

  valid <- !is.na(year)
  if (sum(valid) == 0) return(NULL)

  data.frame(
    year       = year[valid],
    escapement = escapement[valid],
    run        = run_label,
    is_missing = is.na(escapement[valid]),
    stringsAsFactors = FALSE
  )
}

# -- Build escapement plot ---------------------------------------------------

.build_escapement_plot <- function(plot_data, title) {
  run_colors <- c(
    "Late-Fall" = "#1B9E77",
    "Winter"    = "#D95F02",
    "Spring"    = "#7570B3",
    "Fall"      = "#E7298A"
  )

  present <- plot_data[!plot_data$is_missing, ]
  missing <- plot_data[plot_data$is_missing, ]

  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal()

  if (nrow(present) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = present,
        ggplot2::aes(x = .data$year, y = .data$escapement, color = .data$run),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = present,
        ggplot2::aes(x = .data$year, y = .data$escapement, color = .data$run),
        size = 1.5
      )
  }

  if (nrow(missing) > 0) {
    # Place X markers at y = 0 for missing data
    missing$escapement <- 0
    p <- p +
      ggplot2::geom_point(
        data = missing,
        ggplot2::aes(x = .data$year, y = .data$escapement),
        shape = 4, size = 2, color = "gray50"
      )
  }

  p <- p +
    ggplot2::scale_color_manual(values = run_colors, name = "Run") +
    ggplot2::labs(
      title = title,
      x = "Year",
      y = "Escapement"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )

  p
}

# -- Plot escapement for a single location -----------------------------------

.plot_location_escapement <- function(loc_id, runs, run_years) {
  meta <- tryCatch(.load_spatial()$location_meta, error = function(e) NULL)
  display <- if (!is.null(meta)) {
    nm <- meta$display_name[meta$location_id == loc_id]
    if (length(nm) > 0) nm[1] else loc_id
  } else {
    loc_id
  }

  for (r in runs) {
    pd <- .extract_plot_data(loc_id, r, run_years)
    if (is.null(pd) || nrow(pd) == 0) next

    run_label <- switch(r,
      lf = "Late-Fall", w = "Winter", s = "Spring", f = "Fall"
    )
    title <- paste0(display, " - ", run_label, " Run Escapement")
    p <- .build_escapement_plot(pd, title)
    grDevices::dev.new()
    print(p)
  }
}

# -- Plot system summary -----------------------------------------------------

.plot_system_summary <- function(river_system, run, run_years) {
  rs_label <- switch(river_system,
    sacramento = "Sacramento", san_joaquin = "San Joaquin",
    feather = "Feather"
  )

  runs_to_plot <- if (!is.null(run)) run else c("lf", "w", "s", "f")

  for (r in runs_to_plot) {
    tbl <- tryCatch(
      get_escapement(river_system = river_system, run = r, summary = TRUE,
                     run_years = run_years),
      error = function(e) NULL
    )
    if (is.null(tbl)) next

    run_label <- switch(r,
      lf = "Late-Fall", w = "Winter", s = "Spring", f = "Fall"
    )

    # Build plot data from summary table
    year <- suppressWarnings(as.numeric(gsub("\\[|\\]", "", tbl$run_year)))
    total_col <- grep("total$", names(tbl), value = TRUE, ignore.case = TRUE)
    if (length(total_col) == 0) next
    escapement <- suppressWarnings(as.numeric(gsub(",", "", tbl[[total_col[1]]])))

    valid <- !is.na(year)
    if (sum(valid) == 0) next

    pd <- data.frame(
      year       = year[valid],
      escapement = escapement[valid],
      run        = run_label,
      is_missing = is.na(escapement[valid]),
      stringsAsFactors = FALSE
    )

    title <- paste0(rs_label, " River System - ", run_label, " Run Escapement")
    p <- .build_escapement_plot(pd, title)
    grDevices::dev.new()
    print(p)
  }
}

# -- Plot CV summary ---------------------------------------------------------

.plot_cv_summary <- function(run, run_years) {
  runs_to_plot <- if (!is.null(run)) run else c("lf", "w", "s", "f")

  for (r in runs_to_plot) {
    tbl <- tryCatch(
      get_escapement(run = r, summary = TRUE, run_years = run_years),
      error = function(e) NULL
    )
    if (is.null(tbl)) next

    run_label <- switch(r,
      lf = "Late-Fall", w = "Winter", s = "Spring", f = "Fall"
    )

    # Extract year and total
    year_col <- if ("run_year" %in% names(tbl)) "run_year" else names(tbl)[1]
    year <- suppressWarnings(as.numeric(gsub("\\[|\\]", "", tbl[[year_col]])))
    total_col <- grep(paste0("^", r, "_.*total$"), names(tbl), value = TRUE)
    if (length(total_col) == 0) {
      total_col <- grep("total$", names(tbl), value = TRUE, ignore.case = TRUE)
    }
    if (length(total_col) == 0) next
    escapement <- suppressWarnings(as.numeric(gsub(",", "", tbl[[total_col[1]]])))

    valid <- !is.na(year)
    if (sum(valid) == 0) next

    pd <- data.frame(
      year       = year[valid],
      escapement = escapement[valid],
      run        = run_label,
      is_missing = is.na(escapement[valid]),
      stringsAsFactors = FALSE
    )

    title <- paste0("Central Valley - ", run_label, " Run Escapement")
    p <- .build_escapement_plot(pd, title)
    grDevices::dev.new()
    print(p)
  }
}

# -- Plot routing ------------------------------------------------------------

.plot_escapement <- function(args) {
  if (!is.null(args$loc_info)) {
    # Map to data location_id (e.g. sac sections -> sac_main)
    loc_id <- .map_loc_to_data_id(args$loc_info$assoc_location[1])
    runs <- .get_available_runs(loc_id)
    if (!is.null(args$run)) runs <- intersect(runs, args$run)
    if (length(runs) > 0) {
      .plot_location_escapement(loc_id, runs, args$run_years)
    }
  } else if (!is.null(args$river_system)) {
    .plot_system_summary(args$river_system, args$run, args$run_years)
  } else {
    .plot_cv_summary(args$run, args$run_years)
  }
}

# -- Shiny gadget wrapping the leaflet map -----------------------------------

.map_gadget <- function(spatial, args) {
  initial_map <- .build_leaflet_map(spatial, args)

  ui <- shiny::fillPage(
    leaflet::leafletOutput("map", height = "100%")
  )

  server <- function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      initial_map
    })

    # Click on polyline -> open escapement plot
    shiny::observeEvent(input$map_shape_click, {
      click_id <- input$map_shape_click$id
      if (is.null(click_id)) return()
      loc_id <- sub("_highlight$", "", click_id)

      # Map to data location_id (e.g. sac_sec1 -> sac_main)
      data_id <- .map_loc_to_data_id(loc_id)
      available_runs <- .get_available_runs(data_id)
      if (!is.null(args$run)) {
        available_runs <- intersect(available_runs, args$run)
      }
      if (length(available_runs) > 0) {
        .plot_location_escapement(data_id, available_runs, args$run_years)
      }
    })

    # Click on point marker -> open escapement plot for associated location
    shiny::observeEvent(input$map_marker_click, {
      click_id <- input$map_marker_click$id
      if (is.null(click_id)) return()
      pt_id <- sub("^point_", "", click_id)

      loc_id <- spatial$points$associated_location[
        spatial$points$point_id == pt_id
      ]
      if (length(loc_id) == 0) return()

      # Map to data location_id (e.g. sac_sec2 -> sac_main)
      data_id <- .map_loc_to_data_id(loc_id)
      available_runs <- .get_available_runs(data_id)
      if (!is.null(args$run)) {
        available_runs <- intersect(available_runs, args$run)
      }
      if (length(available_runs) > 0) {
        .plot_location_escapement(data_id, available_runs, args$run_years)
      }
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 400))
}

# -- Main function -----------------------------------------------------------

#' Interactive map of GrandTab monitoring locations
#'
#' Opens an interactive leaflet map of the Sacramento-San Joaquin River system
#' showing all GrandTab Chinook salmon monitoring locations. Supports
#' highlighting specific rivers, hatcheries, river systems, or fall run
#' sections. Clicking a river on the map opens an escapement time-series plot.
#'
#' @param run Character. Salmon run to filter: \code{"lf"} (late-fall),
#'   \code{"w"} (winter), \code{"s"} (spring), \code{"f"} (fall), or
#'   \code{NULL} (default). Affects which runs are plotted on click.
#' @param river_system Character. \code{"sacramento"} or \code{"san_joaquin"}.
#'   Highlights flowlines for that system and mutes the other.
#' @param location Character. A river/creek or hatchery name. Flexible
#'   matching supports short names (e.g. \code{"battle"}, \code{"Coleman"},
#'   \code{"CNFH"}). The matched location is highlighted on the map.
#' @param section Integer 1--4. Fall run geographic section. Highlights
#'   flowlines in that section.
#' @param run_years Numeric vector. Filter escapement plots to these years.
#' @param plot_escapement Logical. If \code{TRUE}, immediately open escapement
#'   plots based on the current filter arguments. Default \code{FALSE}.
#' @param base_map Character. Tile provider for the base map. One of
#'   \code{"CartoDB.Positron"} (default), \code{"Esri.WorldImagery"},
#'   \code{"Esri.WorldTopoMap"}, or \code{"OpenTopoMap"}.
#' @param show_hatcheries Logical or \code{NULL}. Whether to display hatchery
#'   markers. Default \code{NULL} shows hatcheries only when the selected
#'   location has one. Set \code{TRUE} to show all, \code{FALSE} to hide.
#'
#' @return Invisible \code{NULL}. Called for its side effects (interactive map
#'   and optional plots).
#'
#' @examples
#' \dontrun{
#' # Default: all rivers shown
#' map_grandtab()
#'
#' # Highlight Battle Creek with Coleman NFH marker
#' map_grandtab(location = "battle")
#'
#' # Highlight by hatchery name
#' map_grandtab(location = "Coleman")
#'
#' # Sacramento system on satellite imagery
#' map_grandtab(river_system = "sacramento", base_map = "Esri.WorldImagery")
#'
#' # Section 1 rivers highlighted
#' map_grandtab(section = 1)
#'
#' # Open plots immediately for Feather River
#' map_grandtab(location = "feather", plot_escapement = TRUE)
#' }
#' @export
map_grandtab <- function(run = NULL, river_system = NULL, location = NULL,
                          section = NULL, run_years = NULL,
                          plot_escapement = FALSE,
                          base_map = c("CartoDB.Positron", "Esri.WorldImagery",
                                       "Esri.WorldTopoMap", "OpenTopoMap"),
                          show_hatcheries = NULL) {

  .check_map_deps()

  spatial <- .load_spatial()

  # Normalize inputs
  run_norm <- .normalize_run(run)
  rs_norm  <- .normalize_river_system(river_system)
  loc_info <- .normalize_map_location(location)
  base_map <- match.arg(base_map)

  if (!is.null(section)) {
    section <- as.integer(section)
    if (!section %in% 1:4) {
      stop("'section' must be 1-4.", call. = FALSE)
    }
  }

  # Resolve point features to display
  points_to_show <- .resolve_points(loc_info, show_hatcheries, spatial)

  # Bundle args for passing to helpers
  args <- list(
    run              = run_norm,
    river_system     = rs_norm,
    loc_info         = loc_info,
    section          = section,
    run_years        = run_years,
    base_map         = base_map,
    points_to_show   = points_to_show
  )

  # Open plots if requested
  if (isTRUE(plot_escapement)) {
    .plot_escapement(args)
  }

  # Launch interactive map
  .map_gadget(spatial, args)

  invisible(NULL)
}
