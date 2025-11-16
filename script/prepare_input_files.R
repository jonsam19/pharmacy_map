# =============================================================================
# Prepare Input Files - Swedish Pharmacy Map Application
# =============================================================================
# This script processes raw Pipos pharmacy data for use in the Shiny app.
#
# Data sources:
# - Pipos pharmacy locations (November 2025)
# - SCB population grid data (1km squares, 2024)
# - SCB municipality groupings (2023)
#
# Outputs:
# - data/processed/df_apotek.rds: Prepared pharmacy data for app.Rmd
# - data/processed/df_rutor.rds: Population grid data
# - data/processed/lan_map/: County boundaries shapefile
# - data/processed/kommun_map/: Municipality boundaries shapefile
# - data/processed/filter_choices.rds: Filter options for app widgets
#
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(geosphere)
library(swemaps2)

# Determine data directory paths (handle running from scripts/ or root)
data_raw_dir <- if (dir.exists("data/raw")) "data/raw" else "../data/raw"
data_processed_dir <- if (dir.exists("data/processed")) "data/processed" else "../data/processed"

# Create processed directory if it doesn't exist
if (!dir.exists(data_processed_dir)) {
  dir.create(data_processed_dir, recursive = TRUE)
}


# =============================================================================
# 1. Load raw data
# =============================================================================

# Swedish pharmacies and their coordinates downloaded from Pipos:
# https://pipos.se/vara-tjanster/serviceanalys
df_apotek_raw <- read_xlsx(file.path(data_raw_dir, "pipos_apoteksvaror_2025-11-12.xlsx"))

# Municipality groupings from SCB
df_kommungrupper <- read_xlsx(file.path(data_raw_dir, "Kommungruppsindelning-2023.xlsx"),
                              skip = 1) |>
  clean_names() |>
  select(kommunnamn, kommungrupp = huvudgrupp)

# Populated 1km squares downloaded from SCB:
# https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/statistik-pa-rutor/
df_rutor_raw <- st_read(file.path(data_raw_dir, "befolkning_1km_2024.gpkg"), quiet = TRUE)


# =============================================================================
# 2. Process population grid data
# =============================================================================

df_rutor <- df_rutor_raw |>
  clean_names() |>
  # Only include populated squares
  filter(beftotalt > 0) |>
  st_transform(crs = 4326) |>
  mutate(
    # Get centroids and extract coordinates
    centroid = st_centroid(sp_geometry),
    long = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) |>
  st_drop_geometry() |>
  select(id = rutid_scb, pop = beftotalt, long, lat)

# Save population grid data
df_rutor |>
  write_rds(file.path(data_processed_dir, "df_rutor.rds"))

message(sprintf("✓ Processed %s population grid squares",
                format(nrow(df_rutor), big.mark = ",")))


# =============================================================================
# 3. Process pharmacy data
# =============================================================================

# Function to clean municipality names
clean_kommun <- function(kommun) {
  # Remove " kommun" suffix
  kommun <- str_sub(kommun, end = -8)

  # Manual corrections for special case
  kommun <- case_when(
    kommun == "Falu" ~ "Falun",
    TRUE ~ kommun
  )

  # Remove genitive "s" (except for municipalities that naturally end in "s")
  kommun <- if_else(
    str_ends(kommun, "s") &
      !(kommun %in% c("Hofors", "Bollnäs", "Tranås", "Torsås",
                      "Mönsterås", "Höganäs", "Strängnäs", "Grums",
                      "Storfors", "Munkfors", "Hagfors", "Vännäs",
                      "Robertsfors", "Kramfors", "Västerås", "Borås",
                      "Alingsås", "Sotenäs", "Bengtsfors", "Degerfors", "Hällefors")),
    str_replace(kommun, "s$", ""),
    kommun
  )

  return(kommun)
}

clean_lan <- function(lan) {
  lan <- case_when(
    lan == "Dalarnas" ~ "Dalarna",
    lan == "Värmlands" ~ "Värmland",
    lan == "Västra Götalands" ~ "Västra Götaland",
    lan == "Västernorrlands" ~ "Västernorrland",
    lan == "Jönköpings" ~ "Jönköping",
    lan == "Norrbottens" ~ "Norrbotten",
    lan == "Södermanlands" ~ "Södermanland",
    lan == "Kronobergs" ~ "Kronoberg",
    lan == "Östergötlands" ~ "Östergötland",
    lan == "Västmanlands" ~ "Västmanland",
    lan == "Västerbottens" ~ "Västerbotten",
    lan == "Stockholms" ~ "Stockholm",
    lan == "Gävleborgs" ~ "Gävleborg",
    lan == "Jämtlands" ~ "Jämtland",
    lan == "Hallands" ~ "Halland",
    lan == "Gotlands" ~ "Gotland",
  TRUE ~ lan)
}

df_apotek <- df_apotek_raw |>
  filter(Serviceform == "Apotek") |>
  clean_names() |>
  mutate(across(c(x, y), as.numeric)) |>
  filter(!is.na(x)) |>
  st_as_sf(crs = 3006, coords = c("x", "y")) |>
  st_transform(crs = 4326) |>
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  as_tibble() |>
  # Clean municipality and region names to match kommun groups data
  mutate(kommun = clean_kommun(kommun),
        lan = clean_lan(lan)) |>
  # Standardise county names (remove " län" suffix)
  mutate(lan = str_remove(lan, " län$")) |>
  # Correct actor names
  mutate(aktor = case_when(
    huvudman == "LloydsApotek" ~ "Doz Apotek",
    huvudman == "Apoteksgruppen" ~ "Kronans Apotek",
    TRUE ~ huvudman)) |>
  # Create simple pharmacy ID
  mutate(pharmacy_id = row_number()) |>
  # Select and reorder columns
  select(
    pharmacy_id,
    aktor,
    namn,
    adress,
    postnummer,
    postort,
    kommun,
    lan,
    long,
    lat
  )

message(sprintf("✓ Loaded %d pharmacies from Pipos data", nrow(df_apotek)))


# =============================================================================
# 4. Join municipality groups
# =============================================================================

df_apotek <- df_apotek |>
  left_join(df_kommungrupper, by = c("kommun" = "kommunnamn"))


# =============================================================================
# 5. Calculate distances between pharmacies and population centres
# =============================================================================

# Function to calculate nearest and second nearest pharmacy distances
dist_fun <- function(sf_pop, df_pharmacies) {

  df_coords <- df_pharmacies |> select(pharmacy_id, long, lat)

  # Calculate Haversine distances using distm()
  dist_mat <- distm(sf_pop[, c("long", "lat")],
                    df_coords[, c("long", "lat")],
                    fun = distHaversine)

  # Find index of nearest and second nearest pharmacy for each population square
  min_index <- apply(dist_mat, 1, function(x) order(x)[1])
  second_min_index <- apply(dist_mat, 1, function(x) order(x)[2])

  # Add nearest and second nearest pharmacy ID columns
  sf_pop <- sf_pop %>%
    mutate(
      min_pharmacy_id = df_coords$pharmacy_id[min_index],
      second_min_pharmacy_id = df_coords$pharmacy_id[second_min_index]
    )

  # Join data for the nearest and second nearest pharmacies
  sf_pop <- sf_pop %>%
    left_join(df_coords, by = c("min_pharmacy_id" = "pharmacy_id"), suffix = c("", "_nearest"),
              relationship = "many-to-many") %>%
    left_join(df_coords, by = c("second_min_pharmacy_id" = "pharmacy_id"), suffix = c("", "_second_nearest"),
              relationship = "many-to-many")

  # Calculate exact distances to nearest and second nearest pharmacies
  sf_pop <- sf_pop %>%
    rowwise() %>%
    mutate(
      dist_nearest = distVincentyEllipsoid(c(long_nearest, lat_nearest), c(long, lat)) / 1e3,
      dist_second_nearest = distVincentyEllipsoid(c(long_second_nearest, lat_second_nearest), c(long, lat)) / 1e3
    ) %>%
    ungroup()

  return(sf_pop)
}

message("Calculating distances from population centres to nearest pharmacies...")
fagelvag_results <- dist_fun(df_rutor, df_apotek)

fagelvag_results |> 
  write_rds("data/processed/df_fagelvag.rds")


# =============================================================================
# 6. Calculate vulnerability metrics
# =============================================================================

# Calculate vulnerability for each pharmacy based on:
# - Population served
# - Mean distance to pharmacy
# - Impact if pharmacy closes (person-kilometres)
sarbarhet_apotek <- fagelvag_results |>
  group_by(pharmacy_id = min_pharmacy_id) |>
  summarise(
    pop = sum(pop),
    mean_dist_km = mean(dist_nearest),
    mean_dist_km2 = mean(dist_second_nearest),
    mean_diff_km = mean(dist_second_nearest - dist_nearest),
    max_diff_km = max(dist_second_nearest - dist_nearest)
  ) |>
  mutate(personkilometer_skillnad = pop * mean_diff_km) |>
  arrange(-personkilometer_skillnad) |>
  select(pharmacy_id, personkilometer_skillnad, mean_dist_km)

# Calculate vulnerability percentile (0-100 scale)
sarbarhet_apotek <- sarbarhet_apotek |>
  mutate(
    percentil_sarbarhet = ntile(personkilometer_skillnad, 100)
  )

# Join vulnerability metrics to pharmacy data
df_apotek <- df_apotek |>
  left_join(sarbarhet_apotek |> select(pharmacy_id, personkilometer_skillnad, percentil_sarbarhet),
            by = "pharmacy_id") |>
  # Use mean distance as proxy for road distance (until OpenRouteService calculation)
  left_join(sarbarhet_apotek |> select(pharmacy_id, dist_km_narmaste_gln_korvag = mean_dist_km),
            by = "pharmacy_id")

# Replace NA values with defaults for pharmacies with no nearby population
df_apotek <- df_apotek |>
  mutate(
    dist_km_narmaste_gln_korvag = replace_na(dist_km_narmaste_gln_korvag, 0),
    percentil_sarbarhet = replace_na(percentil_sarbarhet, 0L),
    personkilometer_skillnad = replace_na(personkilometer_skillnad, 0)
  )

message("✓ Calculated vulnerability metrics")


# =============================================================================
# 7. Create map boundary layers
# =============================================================================

# County (län) boundaries
lan_map <- swemaps2::county |>
  st_transform(crs = 4326) |>
  select(geometry, lan = ln_namn) |>
  mutate(lan = str_remove(lan, " län$"))

# Municipality (kommun) boundaries with population
# First get municipality boundaries
kommun_boundaries_pop <- swemaps2::municipality |>
  select(kommunkod = kn_kod)

# Spatial join to assign each population grid square to a municipality
rutor_with_kommun <- df_rutor_raw |>
  clean_names() |>
  st_transform(st_crs(kommun_boundaries_pop)) |>
  st_join(kommun_boundaries_pop, largest = TRUE)

# Aggregate population by municipality
kommun_pop <- rutor_with_kommun |>
  st_drop_geometry() |>
  group_by(kommunkod) |>
  summarise(pop = sum(beftotalt, na.rm = TRUE))

# Create municipality map with population data
kommun_map <- swemaps2::municipality |>
  # Fix municipality name discrepancy
  mutate(kn_namn = if_else(kn_namn == "Malung", "Malung-Sälen", kn_namn)) |>
  st_transform(crs = 4326) |>
  select(geometry, kommun = kn_namn, lan = ln_namn, kommunkod = kn_kod) |>
  mutate(lan = str_remove(lan, " län$")) |>
  left_join(kommun_pop, by = "kommunkod") |>
  select(-kommunkod)

# Save map layers as shapefiles
st_write(lan_map, file.path(data_processed_dir, "lan_map"),
         driver = "ESRI Shapefile", delete_layer = TRUE, quiet = TRUE)
st_write(kommun_map, file.path(data_processed_dir, "kommun_map"),
         driver = "ESRI Shapefile", delete_layer = TRUE, quiet = TRUE)

message("✓ Created and saved map boundary layers")


# =============================================================================
# 8. Create filter choice variables for app
# =============================================================================

# Extract unique values for dropdown filters
lan <- c(sort(unique(df_apotek$lan)))
kommungrupp <- c("All", sort(unique(df_apotek$kommungrupp)))
aktorer <- sort(unique(df_apotek$aktor))

# Save filter choices
filter_choices <- list(
  lan = lan,
  kommungrupp = kommungrupp,
  aktorer = aktorer
)

write_rds(filter_choices, file.path(data_processed_dir, "filter_choices.rds"))

message("✓ Created filter choice variables")


# =============================================================================
# 9. Save final pharmacy dataset
# =============================================================================

df_apotek |>
  write_rds(file.path(data_processed_dir, "df_apotek.rds"))

message(sprintf("✓ Saved %d pharmacies to df_apotek.rds", nrow(df_apotek)))


# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("Data preparation complete!\n")
cat("=============================================================================\n")
cat(sprintf("Pharmacies processed: %d\n", nrow(df_apotek)))
cat(sprintf("Counties: %d\n", length(lan)))
cat(sprintf("Pharmacy operators: %d\n", length(aktorer)))
cat(sprintf("Municipality groups: %d\n", length(kommungrupp)))
cat(sprintf("Population grid squares: %s\n", format(nrow(df_rutor), big.mark = ",")))
cat("\n")
cat("Output files:\n")
cat("  - data/processed/df_apotek.rds\n")
cat("  - data/processed/df_rutor.rds\n")
cat("  - data/processed/lan_map/ (shapefile)\n")
cat("  - data/processed/kommun_map/ (shapefile)\n")
cat("  - data/processed/filter_choices.rds\n")
