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
# Assuming 'solano_upc_enrollment' or equivalent contains the homeless data
# Replace with actual pin name from your CDE pipeline if different
homeless_raw <- read_csv(here("data", "homeless.csv")) |> clean_names()

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
      c(temporarily_doubled_up, temporary_shelters,
        hotels_motels, temporarily_unsheltered,
        missing_unknown),
      ~ifelse(is.character(.), parse_number(.), .)
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
school_geo <- read_sf(here("data", "DistrictAreas2526.gpkg")) |>
  clean_names() |>
  filter(county_name == "Solano") |>
  mutate(
    school_color = case_match(
      school_type,
      "K-12" ~ "#00BFFF",
      "Juvenile Court" ~ "#483D8B",
      "Special Education" ~ "#DB7093",
      "County Community" ~ "#BA55D3",
      "Community Day" ~ "#DA70D6",
      "Continuation" ~ "#6A5ACD",
      "High" ~ "#7B68EE",
      "Middle" ~ "#1E90FF",
      "Elementary" ~ "#87CEFA",
      "Alternative Schools of Choice" ~ "#EE82EE",
      .default = "#808080"
    )
  ) |>
  rename(cds = cds_code)
pin_write(app_board, school_geo, "school_geo", type = "rds")

# 5. Process and Pin Resources
resources_plottable <- read.xlsx(here("data", "resources_with_addresses.xlsx")) |>
  clean_names()
pin_write(app_board, resources_plottable, "resources_plottable", type = "rds")