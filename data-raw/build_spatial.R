# =============================================================================
# build_spatial.R
#
# Builds grandtab_spatial.rds for use by map_grandtab().
#
# Primary source: CalFish ds181 (Chinook Abundance - Linear Features)
# Gap-filling:    NHDPlus flowlines via nhdplusTools
#
# Requires: nhdplusTools, sf, dplyr
# =============================================================================

library(nhdplusTools)
library(sf)
library(dplyr)

# =============================================================================
# 1. DOWNLOAD AND LOAD ds181
# =============================================================================

ds181_dir <- file.path(tempdir(), "ds181")
ds181_zip <- file.path(tempdir(), "ds181.zip")

if (!file.exists(file.path(ds181_dir, "ds181.shp"))) {
  message("Downloading CalFish ds181 (Chinook Abundance - Linear Features)...")
  download.file(
    "https://filelib.wildlife.ca.gov/Public/BDB/GIS/BIOS/Public_Datasets/100_199/ds181.zip",
    ds181_zip, mode = "wb", quiet = TRUE
  )
  dir.create(ds181_dir, showWarnings = FALSE, recursive = TRUE)
  unzip(ds181_zip, exdir = ds181_dir)
}

ds181 <- st_read(file.path(ds181_dir, "ds181.shp"), quiet = TRUE)
ds181 <- st_transform(ds181, 4326)

message("Loaded ds181: ", nrow(ds181), " features")

# =============================================================================
# 2. FEATURE SELECTION BY TrendID
# =============================================================================
# Each location maps to specific ds181 TrendIDs (Central Valley features only).
# Common stream names (Bear Creek, Clear Creek, etc.) exist in multiple regions;
# we select only Central Valley features by TrendID.

# TrendID -> location_id mapping
# For each location, we union all listed TrendIDs into one geometry.
ds181_selections <- list(
  # --- Sacramento River (will be split into 3 sections later) ---
  sac_river = c("90121", "90122", "90123", "90124",  # Keswick-RBDD (harvest)
                 "90141",                              # RBDD-Colusa (adult return)
                 "91248",                              # Winter (Keswick-RBDD+)
                 "91282", "91283",                     # Fall, Fall/LF
                 "91286"),                             # Full Sacramento (harvest)

  # --- Tributaries OK as-is ---
  battle       = c("90010", "90131", "91615", "91616"),
  clear        = c("90128", "90129", "90160", "90632", "90702", "91284", "91609"),
  cottonwood   = c("90452", "90703", "90143"),
  salt         = c("90453", "91257"),
  craig        = c("90454", "90456"),
  antelope     = c("90158", "90671", "90137", "91608"),
  spring_gulch = c("91251"),
  olney        = c("91252"),
  paynes       = c("90451"),
  cow          = c("90130", "90704"),
  bear_ck      = c("90705"),
  ash          = c("91253"),
  stillwater   = c("91254"),
  inks         = c("91255"),
  toomes       = c("90458"),   # "Toomes and Dry creek" combined feature
  coyote       = c("90315"),
  stoney       = c("90706"),
  singer       = c("90637"),
  feather      = c("90324", "90467", "90556", "90659"),
  yuba         = c("90470", "90557", "90662"),
  dry          = c("90734"),
  american     = c("90558", "90658"),
  san_joaquin  = c("90560"),
  calaveras    = c("90463", "90828"),
  mokelumne    = c("90156", "90559"),
  stanislaus   = c("90653", "91376", "91392"),
  tuolumne     = c("90561", "90652"),
  merced       = c("90651"),

  # --- Tributaries that need gap-filling (ds181 features + NHDPlus) ---
  mill         = c("90305", "90449", "90667", "90668"),  # CV only, not 90768/90849

  deer         = c("90333", "90334", "90459", "90665"),
  big_chico    = c("90163", "90636", "90664"),
  butte        = c("90323", "90338", "90466", "90555", "90663", "91607"),
  dye          = c("90457"),
  thomes       = c("90162", "90469"),
  bear_r       = c("90460"),
  cosumnes     = c("90468", "90657")
)

# Locations that need gap-filling to connect to parent river
needs_gapfill <- c("mill", "deer", "big_chico", "butte", "dye", "thomes",
                    "bear_r", "cosumnes", "cow", "san_joaquin", "merced",
                    "stanislaus", "tuolumne", "mokelumne")

# Locations not in ds181 — must be created entirely from NHDPlus
needs_creation <- c("china_gulch", "natomas")

# =============================================================================
# 3. EXTRACT ds181 FEATURES
# =============================================================================

extract_ds181 <- function(loc_id, trend_ids) {
  matched <- ds181[ds181$TrendID %in% trend_ids, ]
  if (nrow(matched) == 0) {
    message("  WARNING: No ds181 features for ", loc_id)
    return(NULL)
  }
  geom <- st_union(matched)
  st_sf(location_id = loc_id, geometry = geom)
}

message("\nExtracting ds181 features...")
ds181_features <- list()
for (loc_id in names(ds181_selections)) {
  message("  ", loc_id, " (", length(ds181_selections[[loc_id]]), " TrendIDs)")
  ds181_features[[loc_id]] <- extract_ds181(loc_id, ds181_selections[[loc_id]])
}

# =============================================================================
# 4. SPLIT SACRAMENTO RIVER INTO 3 SECTIONS
# =============================================================================

message("\nSplitting Sacramento River into 3 sections...")

# RBDD latitude: ~40.155°N
# Section 2/3 boundary: ~39.414°N (natural break in ds181 data)
RBDD_LAT   <- 40.155
SEC23_LAT  <- 39.414

sac_geom <- ds181_features[["sac_river"]]

if (!is.null(sac_geom)) {
  # Create clipping rectangles — use unname() to avoid anyNA errors
  sac_bb <- st_bbox(sac_geom)
  xmin_val <- unname(sac_bb["xmin"]) - 0.1
  xmax_val <- unname(sac_bb["xmax"]) + 0.1
  ymin_val <- unname(sac_bb["ymin"]) - 0.1
  ymax_val <- unname(sac_bb["ymax"]) + 0.1

  clip_sec1 <- st_as_sfc(st_bbox(c(
    xmin = xmin_val, ymin = RBDD_LAT, xmax = xmax_val, ymax = ymax_val
  ), crs = 4326))

  clip_sec2 <- st_as_sfc(st_bbox(c(
    xmin = xmin_val, ymin = SEC23_LAT, xmax = xmax_val, ymax = RBDD_LAT
  ), crs = 4326))

  clip_sec3 <- st_as_sfc(st_bbox(c(
    xmin = xmin_val, ymin = ymin_val, xmax = xmax_val, ymax = SEC23_LAT
  ), crs = 4326))

  # Drop M/Z coordinates before intersection
  sac_flat <- st_zm(sac_geom, drop = TRUE)

  sec1 <- st_intersection(st_geometry(sac_flat), clip_sec1)
  sec2 <- st_intersection(st_geometry(sac_flat), clip_sec2)
  sec3 <- st_intersection(st_geometry(sac_flat), clip_sec3)

  ds181_features[["sac_sec1"]] <- st_sf(location_id = "sac_sec1", geometry = sec1)
  ds181_features[["sac_sec2"]] <- st_sf(location_id = "sac_sec2", geometry = sec2)
  ds181_features[["sac_sec3"]] <- st_sf(location_id = "sac_sec3", geometry = sec3)

  # Remove the unsplit version
  ds181_features[["sac_river"]] <- NULL

  message("  Section 1 features: ", length(sec1))
  message("  Section 2 features: ", length(sec2))
  message("  Section 3 features: ", length(sec3))
}

# =============================================================================
# 5. GAP-FILL INCOMPLETE FEATURES USING NHDPlus
# =============================================================================
# For streams where ds181 features don't connect to the Sacramento/San Joaquin,
# download NHDPlus flowlines in the gap region and union with ds181 geometry.

# Target confluence points (approximate lat/lon where each stream meets its parent)
# These are used to create a bounding box that covers the gap.
confluence_targets <- list(
  mill        = list(gnis = "Mill Creek",        target_lat = 40.028, target_lon = -122.098),
  deer        = list(gnis = "Deer Creek",        target_lat = 39.980, target_lon = -122.032),
  big_chico   = list(gnis = "Big Chico Creek",   target_lat = 39.700, target_lon = -121.95),
  butte       = list(gnis = c("Butte Creek", "Butte Slough"),
                                                  target_lat = 39.16,  target_lon = -121.95),
  dye         = list(gnis = "Dye Creek",         target_lat = 40.069, target_lon = -122.118),
  thomes      = list(gnis = "Thomes Creek",      target_lat = 39.854, target_lon = -122.346),
  bear_r      = list(gnis = "Bear River",        target_lat = 38.958, target_lon = -121.559),
  cosumnes    = list(gnis = "Cosumnes River",    target_lat = 38.253, target_lon = -121.50),
  cow         = list(gnis = "Cow Creek",         target_lat = 40.463, target_lon = -122.23),
  san_joaquin = list(gnis = "San Joaquin River", target_lat = 37.29,  target_lon = -120.85),
  merced      = list(gnis = "Merced River",      target_lat = 37.35,  target_lon = -121.00),
  stanislaus  = list(gnis = "Stanislaus River",   target_lat = 37.74,  target_lon = -121.24),
  tuolumne    = list(gnis = "Tuolumne River",    target_lat = 37.63,  target_lon = -121.17),
  mokelumne   = list(gnis = c("Mokelumne River", "South Mokelumne River",
                               "North Mokelumne River"),
                                                  target_lat = 38.05,  target_lon = -121.58)
)

gapfill_feature <- function(loc_id, ds181_sf, target_info) {
  message("  Gap-filling: ", loc_id)

  ds181_bb <- st_bbox(ds181_sf)

  # Build a bbox that covers from the ds181 feature to the confluence target
  all_lats <- c(ds181_bb["ymin"], ds181_bb["ymax"], target_info$target_lat)
  all_lons <- c(ds181_bb["xmin"], ds181_bb["xmax"], target_info$target_lon)
  # Add padding
  pad <- 0.02
  query_bb <- c(
    xmin = min(all_lons) - pad,
    ymin = min(all_lats) - pad,
    xmax = max(all_lons) + pad,
    ymax = max(all_lats) + pad
  )

  aoi <- st_as_sfc(st_bbox(query_bb, crs = 4326))

  nhd <- tryCatch(
    get_nhdplus(AOI = aoi),
    error = function(e) {
      message("    NHDPlus query failed: ", e$message)
      return(NULL)
    }
  )

  if (is.null(nhd) || nrow(nhd) == 0) {
    message("    No NHDPlus data — using ds181 only")
    return(ds181_sf)
  }

  # Filter NHDPlus by GNIS name (exact match, supports multiple patterns)
  gnis_col <- if ("gnis_name" %in% names(nhd)) "gnis_name" else "GNIS_NAME"
  if (gnis_col %in% names(nhd)) {
    gnis_patterns <- target_info$gnis
    pattern <- paste0("^(", paste(gnis_patterns, collapse = "|"), ")$")
    matched <- nhd[grepl(pattern, nhd[[gnis_col]], ignore.case = TRUE), ]
    if (nrow(matched) == 0) {
      message("    No GNIS match for '", paste(gnis_patterns, collapse = ", "),
              "' — using ds181 only")
      return(ds181_sf)
    }
    nhd <- matched
  }

  message("    Found ", nrow(nhd), " NHDPlus flowlines for gap-fill")

  # Union NHDPlus geometry with ds181 geometry
  nhd_union <- st_union(nhd)
  combined <- st_union(st_geometry(ds181_sf), nhd_union)

  st_sf(location_id = loc_id, geometry = combined)
}

message("\nGap-filling incomplete features...")
for (loc_id in needs_gapfill) {
  if (is.null(ds181_features[[loc_id]])) next
  target <- confluence_targets[[loc_id]]
  if (is.null(target)) next

  result <- tryCatch(
    gapfill_feature(loc_id, ds181_features[[loc_id]], target),
    error = function(e) {
      message("    Error gap-filling ", loc_id, ": ", e$message)
      ds181_features[[loc_id]]
    }
  )
  ds181_features[[loc_id]] <- result
  Sys.sleep(1)
}

# =============================================================================
# 6. CONNECT TRIBUTARIES TO PARENT RIVERS
# =============================================================================
# After gap-filling, some tributaries still don't quite reach their parent river.
# Add connector lines from each tributary's nearest point to the parent river's
# nearest point.

connector_targets <- list(
  dye        = "sac_sec2",
  big_chico  = "sac_sec2",
  butte      = "sac_sec3",
  cosumnes   = "mokelumne",
  mokelumne  = "san_joaquin",
  stanislaus = "san_joaquin"
)

message("\nConnecting tributaries to parent rivers...")
for (trib_id in names(connector_targets)) {
  parent_id <- connector_targets[[trib_id]]
  trib <- ds181_features[[trib_id]]
  parent <- ds181_features[[parent_id]]
  if (is.null(trib) || is.null(parent)) next

  # Find nearest points between tributary and parent
  nearest_line <- st_nearest_points(trib, parent)
  pts <- st_cast(nearest_line, "POINT")
  dist_m <- as.numeric(st_distance(pts[1], pts[2]))

  if (dist_m < 50) {
    message("  ", trib_id, " -> ", parent_id, ": already connected (", round(dist_m), "m)")
    next
  }

  message("  ", trib_id, " -> ", parent_id, ": adding connector (", round(dist_m), "m)")

  # Create connector LINESTRING
  coords <- st_coordinates(pts)
  connector <- st_linestring(matrix(coords[, c("X", "Y")], ncol = 2))
  connector_sfc <- st_sfc(connector, crs = 4326)

  # Union connector with tributary geometry
  combined <- st_union(st_geometry(trib), connector_sfc)
  ds181_features[[trib_id]] <- st_sf(location_id = trib_id, geometry = combined)
}

# =============================================================================
# 7. CREATE FEATURES NOT IN ds181
# =============================================================================

message("\nCreating manual line features...")

# --- China Gulch ---
# Coordinates from data-raw/kmz/China Gulch.kmz (Google Earth Pro trace)
message("  china_gulch (from KMZ)")
china_gulch_line <- st_linestring(matrix(c(
  -122.4441, 40.4128,
  -122.4437, 40.4145,
  -122.4432, 40.4155,
  -122.4436, 40.4168,
  -122.4442, 40.4184,
  -122.4446, 40.4195,
  -122.4450, 40.4208,
  -122.4446, 40.4219,
  -122.4450, 40.4231
), ncol = 2, byrow = TRUE))
china_gulch_sf <- st_sf(
  location_id = "china_gulch",
  geometry = st_sfc(china_gulch_line, crs = 4326)
)
ds181_features[["china_gulch"]] <- china_gulch_sf

# --- Natomas East Main Drain ---
# Not reliably in NHDPlus; create manual line from Elkhorn south to
# Discovery Park outlet at Sacramento River
message("  natomas (manual line)")
natomas_line <- st_linestring(matrix(c(
  -121.49,  38.69,     # northern extent (Elkhorn area)
  -121.50,  38.65,     # mid-point
  -121.52,  38.62,     # near outlet bend
  -121.525, 38.604     # Discovery Park outlet at Sacramento River
), ncol = 2, byrow = TRUE))
natomas_sf <- st_sf(
  location_id = "natomas",
  geometry = st_sfc(natomas_line, crs = 4326)
)
ds181_features[["natomas"]] <- natomas_sf

# =============================================================================
# 8. SIMPLIFY GEOMETRIES
# =============================================================================

message("\nSimplifying geometries...")

simplify_feature <- function(sf_obj) {
  # Transform to CA Albers (meters) for proper simplification
  proj <- st_transform(sf_obj, 3310)
  proj <- st_simplify(proj, dTolerance = 50)  # 50m tolerance
  st_transform(proj, 4326)
}

# Skip simplification for tiny manual features (would lose detail)
skip_simplify <- c("china_gulch", "natomas")

for (loc_id in names(ds181_features)) {
  if (!is.null(ds181_features[[loc_id]]) && !loc_id %in% skip_simplify) {
    gt <- as.character(st_geometry_type(ds181_features[[loc_id]]))
    if (gt != "POINT") {
      ds181_features[[loc_id]] <- simplify_feature(ds181_features[[loc_id]])
    }
  }
}

# =============================================================================
# 9. ASSEMBLE FLOWLINES sf
# =============================================================================

message("\nAssembling flowlines...")
flowlines_sf <- do.call(rbind, ds181_features)
message("Total features: ", nrow(flowlines_sf))

# =============================================================================
# 10. LOCATION METADATA
# =============================================================================

# Display names and metadata for all locations
location_meta <- data.frame(
  location_id  = c("sac_sec1", "sac_sec2", "sac_sec3",
                     "battle", "clear", "cottonwood", "salt", "craig",
                     "antelope", "mill", "deer", "big_chico", "butte",
                     "spring_gulch", "china_gulch", "olney", "paynes",
                     "cow", "bear_ck", "ash", "stillwater", "inks",
                     "dye", "toomes", "thomes", "coyote", "stoney", "singer",
                     "feather", "yuba", "bear_r", "dry", "american", "natomas",
                     "san_joaquin", "calaveras", "cosumnes", "mokelumne",
                     "stanislaus", "tuolumne", "merced"),
  display_name = c("Sacramento River Section 1", "Sacramento River Section 2",
                     "Sacramento River Section 3",
                     "Battle Creek", "Clear Creek", "Cottonwood Creek",
                     "Salt Creek", "Craig Creek", "Antelope Creek",
                     "Mill Creek", "Deer Creek", "Big Chico Creek",
                     "Butte Creek", "Spring Gulch", "China Gulch",
                     "Olney Creek", "Paynes Creek", "Cow Creek",
                     "Bear Creek", "Ash Creek", "Stillwater Creek",
                     "Inks Creek", "Dye Creek", "Toomes Creek",
                     "Thomes Creek", "Coyote Creek", "Stoney Creek",
                     "Singer Creek", "Feather River", "Yuba River",
                     "Bear River", "Dry Creek", "American River",
                     "Natomas East Main Drain",
                     "San Joaquin River", "Calaveras River",
                     "Cosumnes River", "Mokelumne River",
                     "Stanislaus River", "Tuolumne River", "Merced River"),
  river_system = c("sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento", "sacramento", "sacramento",
                     "sacramento",
                     "san_joaquin", "san_joaquin",
                     "san_joaquin", "san_joaquin",
                     "san_joaquin", "san_joaquin", "san_joaquin"),
  has_hatchery = c(TRUE, FALSE, FALSE,
                    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                    FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
                    FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                    FALSE, TRUE),
  fall_section = c(1L, 2L, 3L,
                    1L, 1L, 1L, 1L, 1L, 2L,
                    2L, 2L, 2L, 3L, 1L, 1L,
                    1L, 1L, 1L, 1L, 1L, 1L,
                    1L, 2L, 2L, 2L, 2L, 2L,
                    2L, 3L, 3L, 3L, 3L, 3L,
                    3L, 4L, NA_integer_, 4L, 4L, 4L, 4L, 4L),
  stringsAsFactors = FALSE
)

# Merge display metadata into flowlines
flowlines_sf <- merge(
  flowlines_sf,
  location_meta[, c("location_id", "display_name", "river_system", "fall_section")],
  by = "location_id", all.x = TRUE
)

# =============================================================================
# 11. POINT FEATURES: HATCHERIES AND LANDMARKS
# =============================================================================

point_features <- data.frame(
  point_id            = c("RBDD", "CNFH", "TCFF", "FRFH", "NFH", "MRH", "MeRH"),
  display_name        = c("Red Bluff Diversion Dam",
                           "Coleman National Fish Hatchery",
                           "Tehama Colusa Fish Facility",
                           "Feather River Hatchery",
                           "Nimbus Hatchery",
                           "Mokelumne River Hatchery",
                           "Merced River Hatchery"),
  point_type          = c("landmark",
                           "hatchery", "hatchery", "hatchery",
                           "hatchery", "hatchery", "hatchery"),
  associated_location = c("sac_sec1",
                           "battle", "sac_sec2", "feather",
                           "american", "mokelumne", "merced"),
  lat                 = c(40.1550,
                           40.3965, 40.1556, 39.5169,
                           38.6302, 38.2267, 37.5183),
  lon                 = c(-122.1880,
                           -122.1435, -122.2278, -121.5547,
                           -121.2240, -121.0667, -120.4117),
  stringsAsFactors    = FALSE
)

points_sf <- st_as_sf(
  point_features,
  coords = c("lon", "lat"),
  crs = 4326
)

# =============================================================================
# 12. AVAILABLE RUNS (for location_meta)
# =============================================================================

# Table -> run mapping (from get_escapement.R)
table_run <- c(`2` = "lf", `3` = "w", `5` = "s", `6` = "f", `7` = "f",
               `8` = "f", `9` = "f", `10` = "f", `11` = "f")

location_cols_tables <- list(
  sac_sec1     = c("2", "3", "5", "8"),
  sac_sec2     = c("2", "3", "5", "9"),
  sac_sec3     = c("9"),
  battle       = c("2", "3", "5", "8"),
  clear        = c("2", "3", "5", "8"),
  cottonwood   = c("2", "5", "8"),
  salt         = c("2", "9"),
  craig        = c("2", "9"),
  antelope     = c("5", "9"),
  mill         = c("5", "9"),
  deer         = c("5", "9"),
  big_chico    = c("5", "9"),
  butte        = c("5", "10"),
  spring_gulch = c("8"),
  china_gulch  = c("8"),
  olney        = c("8"),
  paynes       = c("8"),
  cow          = c("8"),
  bear_ck      = c("8"),
  ash          = c("8"),
  stillwater   = c("8"),
  inks         = c("8"),
  dye          = c("9"),
  toomes       = c("9"),
  thomes       = c("9"),
  coyote       = c("9"),
  stoney       = c("9"),
  singer       = c("9"),
  feather      = c("2", "5", "10"),
  yuba         = c("5", "10"),
  bear_r       = c("10"),
  dry          = c("10"),
  american     = c("10"),
  natomas      = c("10"),
  san_joaquin  = c("11"),
  calaveras    = c("3"),
  cosumnes     = c("11"),
  mokelumne    = c("11"),
  stanislaus   = c("11"),
  tuolumne     = c("11"),
  merced       = c("11")
)

available_runs_list <- lapply(location_cols_tables, function(tables) {
  runs <- unique(unname(table_run[tables]))
  sort(runs)
})

# Add hatchery_id and available_runs to location_meta
location_meta$hatchery_id <- NA_character_
for (i in seq_len(nrow(point_features))) {
  if (point_features$point_type[i] != "hatchery") next
  idx <- which(location_meta$location_id == point_features$associated_location[i])
  if (length(idx) > 0) {
    existing <- location_meta$hatchery_id[idx]
    if (is.na(existing)) {
      location_meta$hatchery_id[idx] <- point_features$point_id[i]
    } else {
      location_meta$hatchery_id[idx] <- paste(existing, point_features$point_id[i],
                                               sep = ",")
    }
  }
}

location_meta$available_runs <- available_runs_list[location_meta$location_id]

# =============================================================================
# 13. SAVE
# =============================================================================

grandtab_spatial <- list(
  flowlines     = flowlines_sf,
  points        = points_sf,
  location_meta = location_meta
)

out_path <- "inst/extdata/grandtab_spatial.rds"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(grandtab_spatial, out_path, compress = "xz")

message("\nSaved grandtab_spatial.rds (",
        round(file.size(out_path) / 1024, 1), " KB)")
message("Flowlines: ", nrow(flowlines_sf), " features")
message("Points: ", nrow(points_sf), " features")
message("Location meta: ", nrow(location_meta), " rows")
