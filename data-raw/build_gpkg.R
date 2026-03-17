# =============================================================================
# build_gpkg.R
#
# Converts KMZ files in data-raw/GrandTab_kmz/ into a GeoPackage for use by
# map_grandtab(). Multiple KMZ files for the same location are unioned.
# The Sacramento River KMZ is split into 4 sections by latitude:
#   Section 1: Keswick to RBDD             (above RBDD_LAT)
#   Section 2: RBDD to Princeton Ferry     (PRINCETON_LAT to RBDD_LAT)
#   Section 3: Princeton Ferry to I St Br  (ISACBRIDGE_LAT to PRINCETON_LAT)
#   Section 4: Lower Sacramento            (below ISACBRIDGE_LAT)
#
# Output: inst/extdata/grandtab_spatial.gpkg
#   Layer "flowlines" : river/creek geometries with display metadata
#   Layer "points"    : hatchery and landmark point features
#
# Requires: sf
# =============================================================================

library(sf)

# =============================================================================
# 1. KMZ -> LOCATION_ID MAPPING
# =============================================================================

kmz_to_location_id <- c(
  "American River"           = "american",
  "Antelope Creek"           = "antelope",
  "Antelope Creek2"          = "antelope",
  "Ash Creek"                = "ash",
  "Battle Creek South"       = "battle",
  "Battle Creek"             = "battle",
  "Bear Creek"               = "bear_ck",
  "Bear River"               = "bear_r",
  "Big Chico Creek1"         = "big_chico",
  "Big Chico Creek2"         = "big_chico",
  "Butte Creek"              = "butte",
  "Calaveras River"          = "calaveras",
  "China Gulch_Beegum Creek" = "china_gulch",
  "Clear Creek"              = "clear",
  "Cosumnes River"           = "cosumnes",
  "Cottonwood Creek1"        = "cottonwood",
  "Cottonwood Creek2"        = "cottonwood",
  "Cottonwood Creek3"        = "cottonwood",
  "Cow Creek"                = "cow",
  "Cow Creek2"               = "cow",
  "Cow Creek3"               = "cow",
  "Coyote Creek"             = "coyote",
  "Craig Creek"              = "craig",
  "Deer Creek"               = "deer",
  "Dry Creek_Sacramento"     = "dry",
  "Dry Creek"                = "dry",
  "Dye Creek"                = "dye",
  "Feather River"            = "feather",
  "Inks Creek"               = "inks",
  "Merced River"             = "merced",
  "Mill Creek"               = "mill",
  "Mokelumne River"          = "mokelumne",
  "Mokelumne River2"         = "mokelumne",
  "Natomas Drain"            = "natomas",
  "Olney Creek"              = "olney",
  "Olney Creek2"             = "olney",
  "Paynes Creek"             = "paynes",
  "Sacramento River"         = "sac_river",
  "Salt Creek"               = "salt",
  "San Joaquin River"        = "san_joaquin",
  "Singer Creek"             = "singer",
  "Spring Gulch"             = "spring_gulch",
  "Stanislaus River"         = "stanislaus",
  "Stillwater Creek"         = "stillwater",
  "Stony Creek"              = "stoney",
  "Thomes Creek"             = "thomes",
  "Toomes Creek"             = "toomes",
  "Tuolumne River"           = "tuolumne",
  "Yuba River"               = "yuba"
)

# =============================================================================
# 2. READ KMZ FILES AND GROUP BY LOCATION_ID
# =============================================================================

kmz_dir   <- "data-raw/GrandTab_kmz"
kmz_files <- list.files(kmz_dir, pattern = "\\.kmz$", full.names = TRUE)

message("Reading ", length(kmz_files), " KMZ files...")

# Helper: read all geometry from a KML path
read_kml_geoms <- function(kml_path) {
  layers <- tryCatch(st_layers(kml_path)$name, error = function(e) character(0))
  result <- list()
  for (lyr in layers) {
    tryCatch({
      feat <- st_read(kml_path, layer = lyr, quiet = TRUE)
      if (nrow(feat) > 0) {
        feat <- st_transform(feat, 4326)
        feat <- st_zm(feat, drop = TRUE)
        result <- c(result, list(st_geometry(feat)))
      }
    }, error = function(e) NULL)
  }
  result
}

features_by_loc <- list()

for (f in kmz_files) {
  base   <- tools::file_path_sans_ext(basename(f))
  loc_id <- kmz_to_location_id[base]

  if (is.na(loc_id)) {
    message("  WARNING: no mapping for '", base, "' -- skipping")
    next
  }

  message("  ", base, " -> ", loc_id)

  # KMZ is a ZIP archive containing a KML file. Unzip and read.
  tmp_dir   <- tempfile()
  dir.create(tmp_dir)
  extracted <- tryCatch(unzip(f, exdir = tmp_dir), error = function(e) character(0))
  kml_paths <- extracted[grepl("\\.kml$", extracted, ignore.case = TRUE)]

  geoms <- list()
  for (kml in kml_paths) {
    geoms <- c(geoms, read_kml_geoms(kml))
  }
  unlink(tmp_dir, recursive = TRUE)

  if (length(geoms) == 0) {
    message("    WARNING: no geometry found")
    next
  }

  combined <- do.call(c, geoms)
  unioned  <- st_union(combined)

  if (is.null(features_by_loc[[loc_id]])) {
    features_by_loc[[loc_id]] <- unioned
  } else {
    features_by_loc[[loc_id]] <- st_union(features_by_loc[[loc_id]], unioned)
  }
}

message("\nCollected ", length(features_by_loc), " unique locations from KMZ files")

# =============================================================================
# 3. SPLIT SACRAMENTO RIVER INTO 4 SECTIONS
# =============================================================================
#   Section 1: Keswick to RBDD                  (above RBDD_LAT)
#   Section 2: RBDD to Princeton Ferry          (PRINCETON_LAT to RBDD_LAT)
#   Section 3: Princeton Ferry to I St Bridge   (ISACBRIDGE_LAT to PRINCETON_LAT)
#   Section 4: Lower Sacramento River           (below ISACBRIDGE_LAT)

RBDD_LAT       <- 40.155   # Red Bluff Diversion Dam
PRINCETON_LAT  <- 39.414   # Princeton Ferry (Colusa County)
ISACBRIDGE_LAT <- 38.582   # Sacramento I Street Bridge

if (!is.null(features_by_loc[["sac_river"]])) {
  message("\nSplitting Sacramento River into 4 sections...")

  sac  <- features_by_loc[["sac_river"]]
  bb   <- st_bbox(sac)
  xmin <- unname(bb["xmin"]) - 0.1
  xmax <- unname(bb["xmax"]) + 0.1
  ymin <- unname(bb["ymin"]) - 0.1
  ymax <- unname(bb["ymax"]) + 0.1

  clip <- function(y1, y2) {
    st_as_sfc(st_bbox(c(xmin = xmin, ymin = y1, xmax = xmax, ymax = y2), crs = 4326))
  }

  features_by_loc[["sac_sec1"]] <- st_intersection(sac, clip(RBDD_LAT,       ymax))
  features_by_loc[["sac_sec2"]] <- st_intersection(sac, clip(PRINCETON_LAT,  RBDD_LAT))
  features_by_loc[["sac_sec3"]] <- st_intersection(sac, clip(ISACBRIDGE_LAT, PRINCETON_LAT))
  features_by_loc[["sac_sec4"]] <- st_intersection(sac, clip(ymin,           ISACBRIDGE_LAT))
  features_by_loc[["sac_river"]] <- NULL

  message("  Section 1 (Keswick to RBDD): done")
  message("  Section 2 (RBDD to Princeton Ferry): done")
  message("  Section 3 (Princeton Ferry to I St Bridge): done")
  message("  Section 4 (Lower Sacramento): done")
}

# =============================================================================
# 4. LOCATION METADATA
# =============================================================================

location_meta <- data.frame(
  location_id  = c("sac_sec1", "sac_sec2", "sac_sec3", "sac_sec4",
                   "battle", "clear", "cottonwood", "salt", "craig",
                   "antelope", "mill", "deer", "big_chico", "butte",
                   "spring_gulch", "china_gulch", "olney", "paynes",
                   "cow", "bear_ck", "ash", "stillwater", "inks",
                   "dye", "toomes", "thomes", "coyote", "stoney", "singer",
                   "feather", "yuba", "bear_r", "dry", "american", "natomas",
                   "san_joaquin", "calaveras", "cosumnes", "mokelumne",
                   "stanislaus", "tuolumne", "merced"),
  display_name = c("Sacramento River Section 1",
                   "Sacramento River Section 2",
                   "Sacramento River Section 3",
                   "Lower Sacramento River",
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
                   "Natomas Drain",
                   "San Joaquin River", "Calaveras River",
                   "Cosumnes River", "Mokelumne River",
                   "Stanislaus River", "Tuolumne River", "Merced River"),
  river_system = c(rep("sacramento", 35),
                   rep("san_joaquin", 7)),
  fall_section = c(1L, 2L, 3L, 3L,
                   1L, 1L, 1L, 1L, 1L, 2L,
                   2L, 2L, 2L, 3L, 1L, 1L,
                   1L, 1L, 1L, 1L, 1L, 1L,
                   1L, 2L, 2L, 2L, 2L, 2L,
                   2L, 3L, 3L, 3L, 3L, 3L,
                   3L, 4L, NA_integer_, 4L, 4L, 4L, 4L, 4L),
  stringsAsFactors = FALSE
)

# =============================================================================
# 5. ASSEMBLE FLOWLINES sf
# =============================================================================

message("\nAssembling flowlines sf...")

flowline_rows <- lapply(location_meta$location_id, function(lid) {
  geom <- features_by_loc[[lid]]
  if (is.null(geom)) {
    message("  WARNING: no geometry for ", lid)
    return(NULL)
  }
  meta_row <- location_meta[location_meta$location_id == lid, ]
  st_sf(
    location_id  = lid,
    display_name = meta_row$display_name,
    river_system = meta_row$river_system,
    fall_section = meta_row$fall_section,
    geometry     = st_sfc(geom, crs = 4326)
  )
})

flowline_rows <- Filter(Negate(is.null), flowline_rows)
flowlines_sf  <- do.call(rbind, flowline_rows)

message("Flowlines: ", nrow(flowlines_sf), " features")

# =============================================================================
# 6. POINT FEATURES: HATCHERIES AND LANDMARKS
# =============================================================================

point_features <- data.frame(
  point_id            = c("RBDD", "LSNFH", "CNFH", "TCFF", "FRFH", "NFH", "MRH", "MeRH"),
  display_name        = c("Red Bluff Diversion Dam",
                          "Livingston Stone National Fish Hatchery",
                          "Coleman National Fish Hatchery",
                          "Tehama Colusa Fish Facility",
                          "Feather River Hatchery",
                          "Nimbus Hatchery",
                          "Mokelumne River Hatchery",
                          "Merced River Hatchery"),
  point_type          = c("landmark",
                          "hatchery", "hatchery", "hatchery", "hatchery",
                          "hatchery", "hatchery", "hatchery"),
  associated_location = c("sac_sec1",
                          "sac_sec1", "battle", "sac_sec1", "feather",
                          "american", "mokelumne", "merced"),
  lat                 = c(40.1550,
                          40.7196, 40.3965, 40.1556, 39.5169,
                          38.6302, 38.2267, 37.5183),
  lon                 = c(-122.1880,
                          -122.4162, -122.1435, -122.2278, -121.5547,
                          -121.2240, -121.0667, -120.4117),
  stringsAsFactors    = FALSE
)

points_sf <- st_as_sf(point_features, coords = c("lon", "lat"), crs = 4326)

message("Points: ", nrow(points_sf), " features")

# =============================================================================
# 7. SAVE GEOPACKAGE
# =============================================================================

out_path <- "inst/extdata/grandtab_spatial.gpkg"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(out_path)) file.remove(out_path)

st_write(flowlines_sf, out_path, layer = "flowlines", quiet = TRUE)
st_write(points_sf,    out_path, layer = "points",    append = TRUE, quiet = TRUE)

message("\nSaved grandtab_spatial.gpkg (",
        round(file.size(out_path) / 1024, 1), " KB)")
message("Layers: flowlines (", nrow(flowlines_sf), " features), ",
        "points (", nrow(points_sf), " features)")
