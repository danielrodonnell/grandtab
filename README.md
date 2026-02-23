# grandtab

**grandtab** is an R package that provides programmatic access to California Department of Fish and Wildlife [GrandTab](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Escapement) Chinook salmon escapement data for all four Central Valley runs: fall, late-fall, winter, and spring.

NOTE: grandtab is a work in progress and may contain minor errors, especially in the documentation. If you discover an error, please open a GitHub issue and the author will correct it in short order.

## Installation

```r
# install.packages("remotes")
remotes::install_github("danielrodonnell/grandtab")
```

## Overview

GrandTab is the official California escapement database for Central Valley Chinook salmon, maintained by CDFW. The grandtab package exposes GrandTab's 11 data tables through a clean R interface with snake_case column names.

The package data is updated automatically each week via a GitHub Actions workflow that checks for new GrandTab PDF releases and uses the Claude API to extract updated rows.

## Usage

### Get escapement data

```r
library(grandtab)

# All runs, all tables
get_escapement()

# Single run
get_escapement(run = "lf")         # Late-fall run
get_escapement(run = "w")          # Winter run
get_escapement(run = "s")          # Spring run
get_escapement(run = "f")          # Fall run (prompts for section)

# Fall run by section
get_escapement(run = "f", section = 1)   # Keswick Dam to RBDD
get_escapement(run = "f", section = 2)   # RBDD to Princeton Ferry
get_escapement(run = "f", section = 3)   # Princeton Ferry to Sacramento
get_escapement(run = "f", section = 4)   # San Joaquin River system

# Summary (Central Valley totals, Table 1)
get_escapement(run = "s", summary = TRUE)

# Filter by location
get_escapement(run = "s", location = "battle")
get_escapement(run = "f", location = "feather")

# Filter by river system
get_escapement(run = "f", river_system = "san joaquin")

# Filter to specific years
get_escapement(run = "lf", run_years = 2018:2024)
```

### Column metadata

```r
# Get column metadata for any escapement query
get_column_metadata(run = "w")
get_column_metadata(run = "f", section = 3)

# Pipe from get_escapement()
get_escapement(run = "s") |> get_column_metadata()
```

### Raw GrandTab tables and footnotes

```r
# Raw tables as they appear in the GrandTab PDF
get_grandtab_raw()       # all tables
get_grandtab_raw(2)      # single table

# Footnotes
get_column_notes(2)         # all footnotes for Table 2
get_column_notes(2, 3)      # footnote 3 of Table 2

# Pipe
get_grandtab_raw(2) |> get_column_notes("1/")
```

### Winter run supplementary data

```r
# Table 4: fish passing RBDD, angler harvest, redd distribution
get_winter_extras()
get_winter_extras(run_years = 2018:2024)
```

### GrandTab documentation

```r
# Open the GrandTab introductory documentation in your system viewer
get_grandtab_info()
```

### Interactive map

```r
# Interactive leaflet map of all monitoring locations (requires leaflet, shiny, sf, ggplot2)
map_grandtab()
map_grandtab(location = "battle")
map_grandtab(section = 1)
map_grandtab(river_system = "sacramento", base_map = "Esri.WorldImagery")
```

### Access raw data objects

```r
# Named list of 11 tables with original GrandTab headers
grandtab_detail
```

## Data

The package includes data from the CDFW GrandTab database, covering Central Valley Chinook salmon escapement from 1952 to the present. Data are organized into 11 tables:

| Table | Description |
|-------|-------------|
| 1 | ALL RUNS: Central Valley summary |
| 2 | LATE-FALL RUN: Sacramento River System |
| 3 | WINTER RUN: Sacramento and San Joaquin River Systems |
| 4 | WINTER RUN supplementary data (fish passing RBDD, redd distribution) |
| 5 | SPRING RUN: Sacramento and San Joaquin River Systems |
| 6 | FALL RUN Summary I: Sacramento and San Joaquin River Systems |
| 7 | FALL RUN Summary II: Sacramento River System |
| 8 | FALL RUN Section 1: Keswick Dam to Red Bluff Diversion Dam |
| 9 | FALL RUN Section 2: Red Bluff Diversion Dam to Princeton Ferry |
| 10 | FALL RUN Section 3: Princeton Ferry to Sacramento |
| 11 | FALL RUN Section 4: San Joaquin River System |

Years marked with brackets (e.g. `[2024]`) in the source data are preliminary and subject to revision. The `provisional_data` column flags these rows with `1`.

## License

MIT © Danny O'Donnell
