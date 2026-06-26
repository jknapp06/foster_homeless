library(tidyverse)
library(sf)
library(janitor)
library(openxlsx)
library(pins)
library(here)

# 1. Connect to Boards
cde_board <- board_folder("C:/Users/JKnapp/OneDrive - Solano County Office of Education/cde_data_pipeline/data/pins")

# Create a local board inside the app directory
dir.create(here("data", "pins"), recursive = TRUE, showWarnings = FALSE)
app_board <- board_folder(here("data", "pins"), versioned = FALSE)

# 2. Process and Pin Homeless Data
# Note: Reading a 'parquet' pin requires the {arrow} package to be installed.
homeless_raw <- pin_read(cde_board, "homeless_enrollment_clean")

homeless_total <- homeless_raw |>
  filter(
    county_name == "Solano",
    academic_year == "2023-24",
    dass == "All" | aggregate_level == "S",
    charter_school == "All" | aggregate_level == "S",
    reporting_group == "Total"
  ) |>
  mutate(
    across(
      any_of(c("temporarily_doubled_up", "temporary_shelters",
               "hotels_motels", "temporarily_unsheltered",
               "missing_unknown")),
      ~ if (is.character(.x)) parse_number(.x) else as.numeric(.x)
    )
  )

pin_write(app_board, homeless_total, "homeless_total", type = "rds")

# 3. Process and Pin District Geo
district_geo <- read_sf(here("data", "California_School_District_Areas_2022-23.geojson")) |>
  clean_names() |>
  filter(county_name == "Solano") |>
  rename(cds = cds_code) |>
  mutate(
    center_point = st_centroid(geometry),
    lng = st_coordinates(center_point)[,1],
    lat = st_coordinates(center_point)[,2]
  )
pin_write(app_board, district_geo, "district_geo", type = "rds")

# 4. Process and Pin School Geo
ca_schools <- pin_read(cde_board, "ca_schools_directory")

# Prepare homeless metrics for the join
school_homeless_metrics <- homeless_total |>
  filter(aggregate_level == "S") |>
  select(
    cds = school_code, # Use school_code to match the 7-digit code or cds if it's 14 digits. Check your data.
    enroll_total = cumulative_enrollment,
    ho_mcount = homeless_student_enrollment
  ) |>
  mutate(ho_mpct = round((ho_mcount / enroll_total) * 100, 1))

school_geo <- ca_schools |>
  filter(county_name == "Solano") |>
  mutate(
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    school_color = case_match(
      eil_name,
      "Elementary" ~ "lightblue",
      "Intermediate/Middle/Junior High" ~ "cadetblue",
      "High School" ~ "darkblue",
      "Elementary-High Combination" ~ "purple",
      .default = "gray"
    )
  ) |>
  drop_na(latitude, longitude) |>
  left_join(school_homeless_metrics, by = "cds") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

pin_write(app_board, school_geo, "school_geo", type = "rds")

# 5. Process and Pin Resources
resources_plottable <- read.xlsx(here("data", "resources_with_addresses.xlsx")) |>
  clean_names()
pin_write(app_board, resources_plottable, "resources_plottable", type = "rds")