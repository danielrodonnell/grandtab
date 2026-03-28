#' @include get_escapement.R
# ============================================================================
# map_grandtab.R
#
# Interactive leaflet map of GrandTab Chinook salmon monitoring locations.
# Supports highlighting by location, river system, or fall section. Hovering
# a river shows a tooltip with available salmon runs.
# ============================================================================

# -- Dependency check --------------------------------------------------------

.check_map_deps <- function() {
  missing <- character(0)
  for (pkg in c("htmltools", "htmlwidgets", "leaflet", "shiny", "sf")) {
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
  fname <- "grandtab_spatial.gpkg"
  path  <- system.file("extdata", fname, package = "grandtab")

  if (!nzchar(path) || !file.exists(path)) {
    # Fall back for development mode — build candidate paths
    candidates <- c(
      file.path("inst", "extdata", fname),
      file.path("..", "inst", "extdata", fname)
    )

    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- tryCatch(get("ofile", envir = sys.frame(i)), error = function(e) NULL)
      if (!is.null(ofile)) {
        pkg_root   <- dirname(normalizePath(dirname(ofile), mustWork = FALSE))
        candidates <- c(file.path(pkg_root, "inst", "extdata", fname), candidates)
        break
      }
    }

    wd <- getwd()
    for (d in c(wd, dirname(wd), dirname(dirname(wd)))) {
      candidates <- c(candidates, file.path(d, "inst", "extdata", fname))
    }

    path <- NULL
    for (p in candidates) {
      if (file.exists(p)) { path <- p; break }
    }
  }

  if (is.null(path) || !file.exists(path)) {
    stop("grandtab_spatial.gpkg not found. ",
         "Run data-raw/build_gpkg.R to generate the spatial data, ",
         "or reinstall the grandtab package.", call. = FALSE)
  }

  flowlines <- sf::st_read(path, layer = "flowlines", quiet = TRUE)
  points    <- sf::st_read(path, layer = "points",    quiet = TRUE)

  # Reconstruct location_meta from the flowlines attributes and points
  location_meta <- as.data.frame(sf::st_drop_geometry(flowlines))[
    , c("location_id", "display_name", "river_system", "fall_section")
  ]
  hatch_df <- sf::st_drop_geometry(points[points$point_type == "hatchery", ])
  location_meta$has_hatchery <- location_meta$location_id %in% hatch_df$associated_location
  location_meta$hatchery_id  <- NA_character_
  for (i in seq_len(nrow(hatch_df))) {
    idx <- which(location_meta$location_id == hatch_df$associated_location[i])
    if (length(idx) > 0) {
      prev <- location_meta$hatchery_id[idx]
      location_meta$hatchery_id[idx] <- if (is.na(prev)) hatch_df$point_id[i] else
        paste(prev, hatch_df$point_id[i], sep = ",")
    }
  }

  list(flowlines = flowlines, points = points, location_meta = location_meta)
}

# -- Resolve Sacramento River section identifier to location_id -------------

.resolve_sac_section <- function(section) {
  sec <- tolower(trimws(as.character(section)))
  # "all" / "a" / "all sections"
  if (grepl("^a", sec))
    return(list(id = "sac_all",
                assoc = c("sac_sec1", "sac_sec2", "sac_sec3", "sac_sec4")))
  # "lower" / "low" / "l" / 4
  if (grepl("^l|^4$", sec))
    return(list(id = "sac_sec4", assoc = "sac_sec4"))
  s <- suppressWarnings(as.integer(sec))
  if (!is.na(s) && s %in% 1:3)
    return(list(id = paste0("sac_sec", s), assoc = paste0("sac_sec", s)))
  stop(
    "When location = 'sacramento', section must be 1, 2, 3, \"lower\"/\"l\"/4, or \"all\"/\"a\".",
    call. = FALSE
  )
}

# -- Normalize map location (extends .normalize_location for hatcheries) -----

.normalize_map_location <- function(location, section = NULL) {
  if (is.null(location)) return(NULL)
  loc <- tolower(trimws(location))

  # Hatchery patterns -- try these first
  hatchery_patterns <- list(
    LSNFH = c("^lsnfh", "livingston", "stone.*hatch"),
    CNFH  = c("^cnfh", "^coleman", "^col"),
    TCFF  = c("^tcff", "^tehama", "^tc"),
    FRFH  = c("^frfh", "^frh", "feather.*hatch"),
    NFH   = c("^nfh", "^nimbus"),
    MRH   = c("^mrh", "mokelumne.*hatch", "^mok.*hatch"),
    MeRH  = c("^merh", "merced.*hatch", "^merc.*hatch", "merced.*facil")
  )

  # Associated locations for each hatchery
  hatchery_assoc <- c(
    LSNFH = "sac_sec1", CNFH = "battle", TCFF = "sac_sec1",
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

  # Sacramento River mainstem -- resolve section or prompt with a menu
  if (grepl("^sac", loc)) {
    if (!is.null(section)) {
      resolved <- .resolve_sac_section(section)
      return(list(type = "location", id = resolved$id, assoc_location = resolved$assoc))
    }
    choice <- utils::menu(
      choices = c(
        "1: Keswick to RBDD",
        "2: RBDD to Princeton Ferry",
        "3: Princeton Ferry to Sacramento (I St Bridge)",
        "lower: Lower Sacramento River (below I St Bridge)",
        "All Sacramento River"
      ),
      title = "Select Sacramento River section:"
    )
    if (choice == 0) {
      # Non-interactive: show all Sacramento sections
      return(list(type = "location", id = "sac_all",
                  assoc_location = c("sac_sec1", "sac_sec2", "sac_sec3", "sac_sec4")))
    }
    if (choice == 5) {
      return(list(type = "location", id = "sac_all",
                  assoc_location = c("sac_sec1", "sac_sec2", "sac_sec3", "sac_sec4")))
    }
    sec_id <- c("sac_sec1", "sac_sec2", "sac_sec3", "sac_sec4")[choice]
    return(list(type = "location", id = sec_id, assoc_location = sec_id))
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

# Map spatial location_id to get_escapement() location_id
.map_loc_to_data_id <- function(loc_id) {
  if (loc_id %in% c("sac_sec1", "sac_sec2", "sac_sec3")) return("sac_main")
  loc_id  # sac_sec4 (Lower Sacramento) has no escapement data; returns as-is
}

# Build a human-readable run string for a spatial location_id
.location_run_label <- function(loc_id) {
  data_id <- .map_loc_to_data_id(loc_id)
  tables  <- names(.location_cols[[data_id]])
  if (is.null(tables) || length(tables) == 0) return("No escapement data")
  runs <- sort(unique(unname(.table_run[tables])))
  run_labels <- c(lf = "Late-Fall Run", w = "Winter Run", s = "Spring Run", f = "Fall Run")
  labels <- run_labels[runs[runs %in% names(run_labels)]]
  if (length(labels) == 0) return("No escapement data")
  paste(labels, collapse = "<br/>")
}

# -- Resolve point features display logic ------------------------------------

.resolve_points <- function(loc_info, show_hatcheries, spatial, visible_ids = NULL) {
  # FALSE: hide all hatchery markers
  if (isFALSE(show_hatcheries)) return(spatial$points[0L, ])

  # NULL or TRUE: show hatcheries whose associated stream is visible
  hatcheries <- spatial$points[spatial$points$point_type == "hatchery", ]
  if (!is.null(visible_ids)) {
    hatcheries <- hatcheries[hatcheries$associated_location %in% visible_ids, ]
  }
  hatcheries
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
        label = ~hover_label,
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "11px", "padding" = "4px 8px"),
          direction = "auto"
        ),
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
        label = ~hover_label,
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

  # Precompute hover tooltip: name + available runs (rendered as HTML)
  flowlines$hover_label <- lapply(seq_len(nrow(flowlines)), function(i) {
    htmltools::HTML(paste0(
      "<b>", flowlines$display_name[i], "</b><br/>",
      .location_run_label(flowlines$location_id[i])
    ))
  })

  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(args$base_map)

  # Color scheme
  default_color   <- "#1A6BB5"
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
        !is.na(flowlines$fall_section) & flowlines$fall_section %in% args$section
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
    map <- .add_flowline_layer(map, flowlines, "", default_color, 2, 0.85,
                               highlight_color)

    # Fit to Central Valley bounds
    map <- map |>
      leaflet::fitBounds(
        lng1 = -123.0, lat1 = 36.8, lng2 = -120.0, lat2 = 41.0
      )
  }

  # Hatchery markers — permanent abbreviated labels + invisible hover layer
  if (!is.null(args$points_to_show) && nrow(args$points_to_show) > 0) {
    hatch <- args$points_to_show

    # Map point_id -> short label shown permanently on the map
    abbrev_map <- c(LSNFH = "LSNFH", CNFH = "CNFH", TCFF = "TCFF", FRFH = "FRH",
                    NFH   = "NFH",   MRH  = "MRH",  MeRH = "MeRH")
    hatch$abbrev <- unname(abbrev_map[hatch$point_id])
    hatch$abbrev[is.na(hatch$abbrev)] <- hatch$point_id[is.na(hatch$abbrev)]

    # Hover tooltip: full name + runs with returns to that hatchery
    hatch$hover_label <- lapply(seq_len(nrow(hatch)), function(i) {
      run_str <- .location_run_label(hatch$associated_location[i])
      htmltools::HTML(paste0(
        "<b>", hatch$display_name[i], "</b><br/>", run_str
      ))
    })

    # Per-feature label options: TCFF appears to the left, all others to the right.
    # Leaflet uses margin-left for right-direction labels, margin-right for left.
    label_opts <- lapply(seq_len(nrow(hatch)), function(i) {
      is_left <- hatch$point_id[i] == "TCFF"
      leaflet::labelOptions(
        permanent = TRUE,
        noHide    = TRUE,
        direction = if (is_left) "left" else "right",
        style     = list(
          "font-size"   = "11px",
          "font-weight" = "bold",
          "color"       = "#000000",
          "text-shadow" = paste(
            "-1px -1px 0 #E63946,",
            " 1px -1px 0 #E63946,",
            "-1px  1px 0 #E63946,",
            " 1px  1px 0 #E63946"
          ),
          "border"      = "none",
          "box-shadow"  = "none",
          "background"  = "transparent",
          "margin-left" = if (is_left) "0px" else "2px",
          "margin-right"= if (is_left) "2px" else "0px"
        )
      )
    })

    # Visible markers with permanent abbreviated labels
    map <- map |>
      leaflet::addCircleMarkers(
        data        = hatch,
        layerId     = ~paste0("point_", point_id),
        label       = ~abbrev,
        labelOptions = label_opts,
        radius      = 3,
        color       = "#E63946",
        fillColor   = "#E63946",
        fillOpacity = 0.9,
        stroke      = TRUE,
        weight      = 1
      )

    # Invisible overlay markers — capture hover over the dot to show full name + runs
    map <- map |>
      leaflet::addCircleMarkers(
        data         = hatch,
        layerId      = ~paste0("hover_", point_id),
        label        = ~hover_label,
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "11px", "padding" = "4px 8px")
        ),
        radius      = 8,
        fillOpacity = 0,
        opacity     = 0,
        stroke      = FALSE
      )

    # Wire abbreviation label elements to also trigger the hover tooltip.
    # Uses the 'tooltipopen' event so the listener is attached the instant each
    # permanent label is added to the DOM — more reliable than setTimeout.
    map <- htmlwidgets::onRender(map, "
      function(el, x) {
        var map = this;
        map.on('tooltipopen', function(e) {
          if (!e.tooltip || !e.tooltip.options.permanent) return;
          var layer = e.layer;
          var lid = (layer.options && layer.options.layerId) || '';
          if (lid.indexOf('point_') !== 0) return;
          var hoverLid = 'hover_' + lid.slice(6);
          var labelEl = e.tooltip.getElement();
          if (!labelEl || labelEl._hoverBound) return;
          labelEl._hoverBound = true;
          labelEl.addEventListener('mouseenter', function() {
            map.eachLayer(function(l) {
              if ((l.options && l.options.layerId) === hoverLid)
                l.openTooltip();
            });
          });
          labelEl.addEventListener('mouseleave', function() {
            map.eachLayer(function(l) {
              if ((l.options && l.options.layerId) === hoverLid)
                l.closeTooltip();
            });
          });
        });
      }
    ")
  }

  map
}

# -- Shiny gadget wrapping the leaflet map -----------------------------------

.map_gadget <- function(spatial, args) {
  initial_map <- .build_leaflet_map(spatial, args)

  ui <- shiny::fillPage(
    shiny::tags$style(".leaflet-tooltip::before { display: none !important; }"),
    leaflet::leafletOutput("map", height = "100%")
  )

  server <- function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      initial_map
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
#' sections. Hovering over a river shows a tooltip with the available salmon
#' runs for that location.
#'
#' @param run Character. Filter map to rivers with data for one or more runs.
#'   Accepts a scalar or vector of run names/codes: \code{"fall"} / \code{"f"},
#'   \code{"spring"} / \code{"s"}, \code{"winter"} / \code{"w"}, or
#'   \code{"late-fall"} / \code{"lf"}. A stream is shown if it has data for
#'   any of the specified runs. Default \code{NULL} shows all rivers.
#' @param river_system Character. \code{"sacramento"} or \code{"san_joaquin"}.
#'   Highlights flowlines for that system and mutes the other.
#' @param location Character. A river/creek or hatchery name. Flexible
#'   matching supports short names (e.g. \code{"battle"}, \code{"Coleman"},
#'   \code{"CNFH"}). The matched location is highlighted on the map.
#' @param section When \code{location = "sacramento"}: one of \code{1}, \code{2},
#'   \code{3} (numeric or character), \code{"lower"}, or \code{"all"} to go
#'   directly to that Sacramento River section without a menu. Otherwise:
#'   integer 1--4 selecting a fall-run geographic section to highlight across
#'   the whole map.
#' @param base_map Character. Tile provider for the base map. One of
#'   \code{"CartoDB.Positron"} (default), \code{"Esri.WorldImagery"},
#'   \code{"Esri.WorldTopoMap"}, or \code{"OpenTopoMap"}.
#' @param hatchery Logical or \code{NULL}. Filter displayed streams by return
#'   type. \code{TRUE} shows only streams with hatchery returns; \code{FALSE}
#'   shows only streams with in-river returns. If \code{location} is a hatchery
#'   and \code{hatchery = FALSE}, an error is returned. If \code{hatchery = TRUE}
#'   and the specified location has no hatchery returns, an error is returned.
#'   Default \code{NULL} shows all streams.
#' @param show_hatcheries Logical or \code{NULL} (default). Controls hatchery
#'   marker display. \code{NULL} (default) shows hatchery markers wherever they
#'   exist for visible streams, silently. \code{TRUE} does the same but prints a
#'   message when the specified location has no associated hatchery.
#'   \code{FALSE} hides all hatchery markers.
#'
#' @return Invisible \code{NULL}. Called for its side effect (interactive map).
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
#' # Only rivers with fall run data
#' map_grandtab(run = "fall")
#' }
#' @export
map_grandtab <- function(run = NULL, river_system = NULL, location = NULL,
                          section = NULL, hatchery = NULL,
                          base_map = c("CartoDB.Positron", "Esri.WorldImagery",
                                       "Esri.WorldTopoMap", "OpenTopoMap"),
                          show_hatcheries = NULL) {

  if (!is.null(show_hatcheries) &&
      (!is.logical(show_hatcheries) || length(show_hatcheries) != 1L ||
       is.na(show_hatcheries)))
    stop("'show_hatcheries' must be TRUE, FALSE, or NULL.", call. = FALSE)

  if (!is.null(hatchery) && !is.logical(hatchery))
    stop("'hatchery' must be TRUE, FALSE, or NULL.", call. = FALSE)

  .check_map_deps()

  spatial <- .load_spatial()
  # Keep a full copy so a specified location can always be added back after filters
  full_flowlines <- spatial$flowlines

  # Filter flowlines to streams that have data for any of the requested runs
  run_norm <- if (!is.null(run)) vapply(run, .normalize_run, character(1)) else NULL

  if (!is.null(run_norm)) {
    keep <- vapply(spatial$flowlines$location_id, function(lid) {
      data_id <- .map_loc_to_data_id(lid)
      tables  <- names(.location_cols[[data_id]])
      if (is.null(tables) || length(tables) == 0) return(FALSE)
      any(.table_run[tables] %in% run_norm, na.rm = TRUE)
    }, logical(1))
    spatial$flowlines <- spatial$flowlines[keep, ]
    if (nrow(spatial$flowlines) == 0) {
      warning("No streams found for the specified run filter. Displaying blank map.",
              call. = FALSE)
    }
  }

  # Filter flowlines by hatchery origin flag — only when browsing (no specific
  # location given). When a location is specified, hatchery is validated against
  # that location directly and the flowlines filter would hide it from the map.
  if (!is.null(hatchery) && is.null(location)) {
    suffix <- if (isTRUE(hatchery)) .hatch_suffix_re else "_inr$"
    keep <- vapply(spatial$flowlines$location_id, function(lid) {
      data_id <- .map_loc_to_data_id(lid)
      .location_has_origin(data_id, suffix)
    }, logical(1))
    spatial$flowlines <- spatial$flowlines[keep, ]
    if (nrow(spatial$flowlines) == 0) {
      warning("No streams found for the specified hatchery filter. Displaying blank map.",
              call. = FALSE)
    }
  }

  # Determine if this is a Sacramento-specific request (section has different
  # meaning there: 1/2/3/"lower" instead of fall monitoring section 1-4)
  is_sac <- !is.null(location) &&
    grepl("^sac", tolower(trimws(location)))

  # Normalize inputs
  rs_norm  <- .normalize_river_system(river_system)
  # For Sacramento, pass section so .normalize_map_location() can resolve it
  # directly (or show a menu if section is NULL).
  if (!is.null(location) && length(location) > 1) {
    # Normalize each location and combine their assoc_locations
    loc_infos <- lapply(location, function(l) {
      is_sac_l <- grepl("^sac", tolower(trimws(l)))
      .normalize_map_location(l, if (is_sac_l) section else NULL)
    })
    all_assoc <- unique(unlist(lapply(loc_infos, `[[`, "assoc_location")))
    types     <- vapply(loc_infos, `[[`, character(1), "type")
    loc_info  <- list(type           = if (all(types == "hatchery")) "hatchery" else "location",
                      id             = paste(vapply(loc_infos, `[[`, character(1), "id"), collapse="+"),
                      assoc_location = all_assoc)
  } else {
    loc_info <- .normalize_map_location(location, if (is_sac) section else NULL)
  }

  # Ensure the specified location's flowlines are always present, even if the
  # run or hatchery filter removed them. The map should always show the river.
  # Capture which locations were filtered out BEFORE restoring, so the warning
  # check below can still detect a run mismatch.
  loc_filtered_out <- character(0)
  if (!is.null(loc_info)) {
    assoc <- unlist(loc_info$assoc_location)
    missing <- assoc[!assoc %in% spatial$flowlines$location_id]
    loc_filtered_out <- missing
    if (length(missing) > 0) {
      spatial$flowlines <- rbind(
        spatial$flowlines,
        full_flowlines[full_flowlines$location_id %in% missing, ]
      )
    }
  }

  # Warn if the run filter removed the location's flowlines — check this before
  # the hatchery check so the right message fires first.
  run_mismatch_warned <- FALSE
  if (!is.null(loc_info) && !is.null(run_norm) && length(loc_filtered_out) > 0) {
    assoc_locs <- if (is.list(loc_info$assoc_location))
      unlist(loc_info$assoc_location) else loc_info$assoc_location
    run_label_map <- c(lf = "late-fall-run", w = "winter-run",
                       s  = "spring-run",    f  = "fall-run")
    run_str <- if (length(run_norm) == 1) run_label_map[run_norm]
               else paste(run_label_map[run_norm], collapse = " or ")
    meta <- spatial$location_meta
    dn_vals <- meta$display_name[
      meta$location_id %in% assoc_locs[assoc_locs %in% spatial$location_meta$location_id]]
    loc_name <- if (length(dn_vals) >= 1) dn_vals[1] else loc_info$id
    has_hatch <- any(meta$has_hatchery[meta$location_id %in% assoc_locs],
                     na.rm = TRUE)
    if (isTRUE(hatchery) && !has_hatch) {
      warning("There is no ", run_str, " escapement or hatchery ",
              .loc_phrase(loc_name, "on"), ".", call. = FALSE)
    } else {
      warning("There is no ", run_str, " escapement ",
              .loc_phrase(loc_name, "on"), ".", call. = FALSE)
    }
    run_mismatch_warned <- TRUE
  }

  # When show_hatcheries=TRUE and the location has no hatchery, message and continue.
  # show_hatcheries=NULL silently shows hatcheries only if they exist — no message.
  # Skip if a run-mismatch warning was already issued (it already covers hatchery absence).
  if (!run_mismatch_warned && isTRUE(show_hatcheries) && !is.null(loc_info) &&
      loc_info$type == "location") {
    assoc_locs <- if (is.list(loc_info$assoc_location))
      unlist(loc_info$assoc_location) else loc_info$assoc_location
    meta <- spatial$location_meta
    has_hatch <- any(meta$has_hatchery[meta$location_id %in% assoc_locs],
                     na.rm = TRUE)
    if (!has_hatch) {
      dn_vals <- meta$display_name[meta$location_id %in% assoc_locs]
      dn <- if (length(dn_vals) == 1) dn_vals
            else if (length(dn_vals) > 1) paste(dn_vals, collapse = " / ")
            else loc_info$id
      warning("There is no hatchery ", .loc_phrase(dn, "on"), ".", call. = FALSE)
    }
  }
  # show_hatcheries=NULL: treat as TRUE (show if present) with no messaging

  # For single Sacramento River location, restrict visible flowlines to Sacramento system
  if (!is.null(location) && length(location) == 1 &&
      !is.null(loc_info) && grepl("^sac", loc_info$id)) {
    spatial$flowlines <- spatial$flowlines[spatial$flowlines$river_system == "sacramento", ]
  }

  # Hatchery validation against location type.
  # Skip if a run-mismatch warning was already issued (it already covers hatchery absence).
  if (!run_mismatch_warned && !is.null(hatchery) && !is.null(loc_info)) {
    if (loc_info$type == "hatchery" && isFALSE(hatchery)) {
      stop(
        'hatchery cannot be FALSE when location is a hatchery.',
        call. = FALSE
      )
    }
    if (loc_info$type == "location" && isTRUE(hatchery)) {
      assoc_locs <- loc_info$assoc_location
      meta       <- spatial$location_meta
      has_hatch  <- any(meta$has_hatchery[meta$location_id %in% assoc_locs])
      if (!has_hatch) {
        dn_vals <- meta$display_name[meta$location_id %in% assoc_locs]
        dn <- if (length(dn_vals) == 1) dn_vals else loc_info$id
        warning("There is no hatchery ", .loc_phrase(dn, "on"), ".", call. = FALSE)
      }
    }
  }
  bm_valid <- c("CartoDB.Positron", "Esri.WorldImagery",
                "Esri.WorldTopoMap", "OpenTopoMap")
  if (length(base_map) == 1) {
    m <- pmatch(tolower(base_map), tolower(bm_valid))
    if (!is.na(m)) base_map <- bm_valid[m]
  }
  base_map <- match.arg(base_map, bm_valid)

  # For non-Sacramento requests, validate section as a fall monitoring section
  # (1-4). When location is Sacramento, section was consumed above.
  map_section <- NULL
  if (!is_sac && !is.null(section)) {
    roman_map <- c(I = 1L, II = 2L, III = 3L, IV = 4L)
    if (length(section) == 1L &&
        grepl("^all?$", trimws(tolower(as.character(section))))) {
      map_section <- 1:4
    } else {
      map_section <- vapply(trimws(as.character(section)), function(s) {
        rh <- roman_map[toupper(s)]
        if (!is.na(rh)) return(unname(rh))
        si <- suppressWarnings(as.integer(s))
        if (is.na(si) || as.character(si) != s || !si %in% 1:4)
          stop("'section' must be 1, 2, 3, or 4 (or Roman numeral I-IV), ",
               "or \"all\".", call. = FALSE)
        si
      }, integer(1))
    }
    # Restrict sections to those matching river_system
    if (!is.null(rs_norm)) {
      if (rs_norm == "sacramento")
        map_section <- map_section[map_section %in% 1:3]
      else if (rs_norm == "san_joaquin")
        map_section <- map_section[map_section == 4L]
    }
    if (length(map_section) == 0)
      stop("No fall run sections match the specified 'river_system'.",
           call. = FALSE)

    # Warn if a non-fall run was explicitly requested alongside a section
    if (!is.null(run_norm) && !"f" %in% run_norm) {
      warning('Fall-run "Section" tables contain only fall-run escapement data.',
              call. = FALSE)
    }
  }

  # Compute which location_ids are visible on the map (used to filter hatcheries)
  flowlines <- spatial$flowlines
  visible_ids <- if (!is.null(loc_info)) {
    loc_info$assoc_location
  } else if (!is.null(rs_norm)) {
    flowlines$location_id[flowlines$river_system == rs_norm]
  } else if (!is.null(map_section)) {
    flowlines$location_id[!is.na(flowlines$fall_section) &
                            flowlines$fall_section %in% map_section]
  } else {
    NULL  # all streams visible
  }

  # Resolve point features to display
  points_to_show <- .resolve_points(loc_info, show_hatcheries, spatial, visible_ids)

  # Bundle args for passing to helpers
  args <- list(
    river_system     = rs_norm,
    loc_info         = loc_info,
    section          = map_section,
    base_map         = base_map,
    points_to_show   = points_to_show
  )

  # Launch interactive map
  .map_gadget(spatial, args)

  invisible(NULL)
}
