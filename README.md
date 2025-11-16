# Swedish Pharmacy Map

An interactive web application visualizing pharmacy locations and accessibility metrics across Sweden. Built with R Shiny and Flexdashboard.

## Features

- **Interactive map** of all Swedish pharmacy locations
- **Vulnerability index** showing impact if a pharmacy closes
- **Population density** visualization by municipality
- **Filter by:**
  - County (län)
  - Municipality group
  - Pharmacy operator
  - Distance to nearest pharmacy
  - Vulnerability index
- **Download** filtered data as Excel

## Methodology

The vulnerability index measures the impact of a pharmacy closure using:

```
Vulnerability = Population served × Increased travel distance if pharmacy closes
```

Distances are calculated using the Vincenty ellipsoid formula (great-circle distance) for accuracy.

## Data Sources

- **Pharmacy Locations:** [Pipos](https://pipos.se/vara-tjanster/serviceanalys) (November 2025)
- **Population Data:** [Statistics Sweden (SCB)](https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/statistik-pa-rutor/) - 1km grid squares (2024)
- **Municipality Classifications:** Statistics Sweden (2023)
- **Geographic Boundaries:** swemaps2 R package

## Installation

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "tidyverse",
  "flexdashboard",
  "shiny",
  "leaflet",
  "sf",
  "geosphere",
  "shinyWidgets",
  "readxl",
  "janitor",
  "swemaps2",
  "writexl"
))
```

## Usage

1. **Prepare the data:**
   ```bash
   Rscript script/prepare_input_files.R
   ```

2. **Run the application:**
   - Open `app.Rmd` in RStudio
   - Click "Run Document" or press Ctrl+Shift+K

## Project Structure

```
pharmacy_map/
├── app.Rmd                          # Main Shiny application
├── script/
│   └── prepare_input_files.R       # Data processing script
├── data/
│   ├── raw/                        # Raw data files (not included)
│   └── processed/                  # Processed data for app
└── README.md
```

## Technical Details

- **Mapping:** Leaflet.js via R leaflet package
- **Geospatial Analysis:** sf, geosphere packages
- **Map Projection:** WGS84 (EPSG:4326)
- **UI Framework:** Flexdashboard with Shiny runtime

## License

This is a portfolio project. Data sources retain their original licenses.

---

*Last updated: November 2025*
