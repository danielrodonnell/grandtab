# =============================================================================
# restructure_tables.R
#
# One-time migration script to:
#   1. Split Winter Run Table 3 (cols 12-15 become new Table 4)
#   2. Add footnote indicators to grandtab_detail column headers
#   3. Shift old Tables 4-10 to 5-11
#   4. Apply same split+shift to grandtab_detail_short
#   5. Rebuild grandtab_sections and grandtab_summary
#   6. Save all 4 .rda files
# =============================================================================

library(dplyr)
library(stringr)

source("data-raw/update_stored_grandtab.R")

# -- Load current data --------------------------------------------------------
load("data/grandtab_detail.rda")
load("data/grandtab_detail_short.rda")

# -- 1. Split Table 3 in grandtab_detail -------------------------------------

t3_full <- grandtab_detail[[3]][[2]]
t3_title <- grandtab_detail[[3]][[1]]

# Table 3 keeps cols 1-11, Table 4 (new) gets col 1 + cols 12-15
t3_trimmed <- t3_full[, 1:11]
t4_new <- t3_full[, c(1, 12:15)]

# -- 2. Add footnote indicators to grandtab_detail column headers ------------

# Table 2: RBDD1/, In-R 2/, CNFH 3/, TCFF 4/, Battle5/
names(grandtab_detail[[2]][[2]]) <- c(
  "Late-Fall YEAR",
  "Sac Main Up RBDD1/ In-R 2/",      # was "Sac Main Up RBDD In-R"
  "Sac Main Up RBDD trans CNFH 3/",   # was "Sac Main Up RBDD trans CNFH"
  "Sac Main Up RBDD TOTAL",
  "Sac Main Dwn RBDD trans TCFF 4/",  # was "Sac Main Dwn RBDD trans TCFF"
  "Sac Main Dwn RBDD In-R 2/",        # was "Sac Main Dwn RBDD In-R"
  "Sac Main Dwn RBDD TOTAL",
  "Battle5/ Up CNFH In-R",            # was "Battle Up CNFH In-R"
  "Battle CNFH",
  "Battle TOTAL",
  "Clear Ck",
  "Cottonwood Ck",
  "Salt Ck",
  "Craig Ck",
  "Feather In-R",
  "Feather Hatch",
  "TOTAL LATE-FALL RUN"
)

# Table 3 (trimmed, cols 1-11): In-R 2/, Dwn RBDD In-R 3/, trans1/,
# Clear Ck 4/, Battle Ck 5/, SAC SYSTEM 6/, Calaveras R 7/
names(t3_trimmed) <- c(
  "Winter YEAR",
  "Sac Main Up RBDD In-R 2/",         # was "Sac Main Up RBDD In-R"
  "Sac Main Dwn RBDD In-R 3/",        # was "Sac Main Dwn RBDD In-R"
  "Sac Main TOTAL In-R",
  "Sac Main trans1/ CNFH",            # was "Sac Main trans CNFH"
  "Sac Main trans1/ LSNFH",           # was "Sac Main trans LSNFH"
  "Clear Ck 4/",                       # was "Clear Ck"
  "Battle Ck 5/",                      # was "Battle Ck"
  "WINTER TOTAL SAC SYSTEM 6/",       # was "WINTER TOTAL SAC SYSTEM"
  "SJ Calaveras R 7/",                # was "SJ Calaveras R"
  "WINTER TOTAL CV SYSTEM"
)

# New Table 4: Fish Passing RBDD 1/3/, Angler Harvest 1/4/, Dist2/
names(t4_new) <- c(
  "Winter YEAR",
  "Fish Passing RBDD 1/3/",           # was "Fish Passing RBDD"
  "Angler Harvest 1/4/",              # was "Angler Harvest"
  "Redd Dist2/ Up RBDD",              # was "Redd Dist Up RBDD"
  "Redd Dist2/ Dwn RBDD"              # was "Redd Dist Dwn RBDD"
)

# Table 4 (old, becoming 5): RBDD1/, Battle Ck 4/, Spawn 5/, Butte Ck 6/,
# Feather In-R 2/, Feather Hatch 3/, TOTAL SPRING RUN 7/
names(grandtab_detail[[4]][[2]]) <- c(
  "YEAR",
  "Sac Main Up RBDD1/ In-R",          # was "Sac Main Up RBDD In-R"
  "Sac Main Dwn RBDD In-R",
  "Sac Main In-R TOTAL",
  "Battle Ck 4/",                      # was "Battle Ck"
  "Clear Ck",
  "Cottonwood Ck",
  "Antelope Ck",
  "Mill Ck",
  "Mill Ck Spawn 5/",                 # was "Mill Ck Spawn"
  "Deer Ck",
  "Deer Ck Spawn 5/",                 # was "Deer Ck Spawn"
  "Big Chico Ck",
  "Butte Ck 6/",                       # was "Butte Ck"
  "Butte Ck Spawn 5/",                # was "Butte Ck Spawn"
  "Feather In-R 2/",                  # was "Feather In-R"
  "Feather Hatch 3/",                 # was "Feather Hatch"
  "Feather TOTAL",
  "Yuba R",
  "TOTAL SPRING RUN 7/"               # was "TOTAL SPRING RUN"
)

# Table 5 (old, becoming 6): Sac System Hatch 1/, SJ System Hatch 2/
names(grandtab_detail[[5]][[2]]) <- c(
  "YEAR",
  "Sac System Hatch 1/",              # was "Sac System Hatch"
  "Sac System In-R",
  "Sac System Tribs In-R",
  "Sac System TOTAL",
  "SJ System Hatch 2/",               # was "SJ System Hatch"
  "SJ System Tribs In-R",
  "SJ System TOTAL",
  "Sac SJ System Hatch",
  "Sac SJ System In-R",
  "Sac SJ System TOTAL"
)

# Table 6 (old, becoming 7): RBDD_PFerry Tribs In-R 1/
names(grandtab_detail[[6]][[2]]) <- c(
  "YEAR",
  "Kesw_RBDD CNFH",
  "Kesw_RBDD Main In-R",
  "Kesw_RBDD Tribs In-R",
  "Kesw_RBDD TOTAL",
  "RBDD_PFerry TCFF",
  "RBDD_PFerry Main In-R",
  "RBDD_PFerry Tribs In-R 1/",        # was "RBDD_PFerry Tribs In-R"
  "RBDD_PFerry TOTAL",
  "PFerry_Sacr Feather_Nimbus Hatch",
  "PFerry_Sacr Tribs In-R",
  "PFerry_Sacr TOTAL"
)

# Table 7 (old, becoming 8): Sac Main In-R 1/, CNFH 2/, Battle5/,
# Up CNFH In-R 3/, Other 4/
names(grandtab_detail[[7]][[2]]) <- c(
  "YEAR",
  "Sac Main In-R 1/",                 # was "Sac Main In-R"
  "Sac Main trans CNFH 2/",           # was "Sac Main trans CNFH"
  "Sac Main TOTAL",
  "Battle5/ CNFH",                     # was "Battle CNFH"
  "Battle Dwn CNFH In-R",
  "Battle Up CNFH In-R 3/",           # was "Battle Up CNFH In-R"
  "Battle TOTAL",
  "Clear Ck",
  "Spring Gulch",
  "China Gulch",
  "Olney Ck",
  "Cottonwood Ck",
  "Paynes Ck",
  "Cow Ck",
  "Bear Ck",
  "Ash Ck",
  "Stillwater Ck",
  "Inks Ck",
  "Other 4/"                           # was "Other"
)

# Table 8 (old, becoming 9): Sac Main In-R 1/, TCFF 2/, Other 3/
names(grandtab_detail[[8]][[2]]) <- c(
  "YEAR",
  "Sac Main In-R 1/",                 # was "Sac Main In-R"
  "Sac Main trans TCFF 2/",           # was "Sac Main trans TCFF"
  "Sac Main TOTAL",
  "TCFF",
  "Salt Ck",
  "Antelope Ck",
  "Craig Ck",
  "Dye Ck",
  "Mill Ck",
  "Toomes Ck",
  "Thomes Ck",
  "Deer Ck",
  "Coyote Ck",
  "Stoney Ck",
  "Singer Ck",
  "Big Chico Ck",
  "Other 3/"                           # was "Other"
)

# Table 9 (old, becoming 10): Butte Ck 1/, Feather In-R 2/,
# Yuba R 3/, American In-R 4/5/
names(grandtab_detail[[9]][[2]]) <- c(
  "YEAR",
  "Butte Ck 1/",                       # was "Butte Ck"
  "Feather Hatch",
  "Feather In-R 2/",                  # was "Feather In-R"
  "Feather TOTAL",
  "Yuba R 3/",                         # was "Yuba R"
  "Bear R",
  "Dry Ck",
  "American Nimbus Hatch",
  "American In-R 4/5/",               # was "American In-R"
  "American TOTAL",
  "Natomas Drain"
)

# Table 10 (old, becoming 11): Merced Hatch 1/
names(grandtab_detail[[10]][[2]]) <- c(
  "YEAR",
  "Cosumnes R",
  "Mokelumne Hatch",
  "Mokelumne In-R",
  "Mokelumne TOTAL",
  "Stanislaus R",
  "Tuolumne R",
  "Merced Hatch 1/",                   # was "Merced Hatch"
  "Merced In-R",
  "Merced TOTAL"
)

# -- 3. Reassemble grandtab_detail as 11 elements ----------------------------

new_t4_entry <- list(
  "WINTER RUN: Sacramento Mainstem Winter (additional) Data 1/",
  t4_new
)

grandtab_detail <- c(
  grandtab_detail[1:2],               # Tables 1-2 unchanged
  list(list(grandtab_detail[[3]][[1]], t3_trimmed)),  # Table 3 trimmed
  list(new_t4_entry),                  # NEW Table 4
  grandtab_detail[4:10]               # Old 4-10 become 5-11
)

# -- 4. Split + shift grandtab_detail_short -----------------------------------

t3s_full <- grandtab_detail_short[[3]][[2]]

t3s_trimmed <- t3s_full[, 1:11]
t4s_new <- t3s_full[, c(1, 12:15)]

new_t4s_entry <- list(
  "CHINOOK SALMON ESCAPEMENT - WINTER RUN (additional) Data",
  t4s_new
)

grandtab_detail_short <- c(
  grandtab_detail_short[1:2],
  list(list(grandtab_detail_short[[3]][[1]], t3s_trimmed)),
  list(new_t4s_entry),
  grandtab_detail_short[4:10]
)

# -- 5. Rebuild derived datasets -----------------------------------------------

grandtab_sections <- build_sections(grandtab_detail)
grandtab_summary  <- build_summary(grandtab_detail)

# -- 6. Save all .rda files ---------------------------------------------------

save(grandtab_detail, file = "data/grandtab_detail.rda", compress = "xz")
save(grandtab_detail_short, file = "data/grandtab_detail_short.rda", compress = "xz")
save(grandtab_sections, file = "data/grandtab_sections.rda", compress = "xz")
save(grandtab_summary, file = "data/grandtab_summary.rda", compress = "xz")

message("All 4 .rda files rebuilt successfully.")
message("grandtab_detail: ", length(grandtab_detail), " tables")
message("grandtab_detail_short: ", length(grandtab_detail_short), " tables")
