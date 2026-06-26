library(tidyverse)
library(pins)
library(here)

# 1. Establish robust project paths and load helpers
local_board <- board_folder(here("../cde_data_pipeline/data/pins"))

# 2. Read from Pins & Apply Universal Normalizer
dashboard <- pin_read(local_board, "solano_dashboard")

enrollment <- pin_read(local_board, "solano_dash_enrollment") |>
  select(-ends_with("code"))

# Load and fully prep homeless data immediately
homeless <- pin_read(local_board, "homeless_enrollment_clean") |>
  mutate(
    # FIX: Force district school codes to "0000000" so the CDS will match UPC
    school_code = if_else(
      is.na(school_code) | school_code == "",
      "0000000",
      school_code
    ),

    # (Assuming you create your cds column here like this:)
    cds = paste0(county_code, district_code, school_code),

    percent_homeless = homeless_student_enrollment /
      cumulative_enrollment *
      100,
    across(ends_with("(percent)"), parse_number),
    # Standardize naming to match Foster/Dashboard right away
    reporting_long = if_else(
      reporting_category == "TA",
      "Homeless Youth",
      reporting_long
    ),
    reporting_group = if_else(
      reporting_group == "Total",
      "Program",
      reporting_group
    )
  ) |>
  # Safely keep all non-school aggregates, and apply dashboard filters only to schools
  filter(
    aggregate_level != "S" | (charter_school == "All" & dass == "All")
  ) |>
  filter(reporting_group %in% c("Race/Ethnicity", "Program"))

# Load foster data (Upstream UPC script now handles aggregate_level, dass, and charter status!)
foster <- pin_read(local_board, "solano_upc") |>
  filter(program %in% c("foster", "homeless")) |>
  mutate(
    reporting_group = "Program",
    program = if_else(program == "foster", "Foster Youth", "Homeless Youth")
  )

public <- pin_read(local_board, "ca_schools_directory")

# 3. Define School Types & Helpers
doc_elementary <- c(52)
doc_high <- c(56)
doc_unified <- c(54)

soc_elementary <- c(60, 61)
soc_middle <- c(62, 64)
soc_high <- c(66, 67, 68)
soc_k_12 <- c(65)

round_away <- function(n) {
  if_else(n > 0, ceiling(n), floor(n))
}

# 4. Process Dashboard Targets
dashboard_with_levels <- dashboard |>
  filter(color == 1) |> # Filter for Red Dashboard indicators (1 = Red)
  left_join(select(public, cds, doc, soc), join_by(cds)) |>
  mutate(
    change_to_orange = case_when(
      indicator == "ELA" ~ 3,
      indicator == "Math" ~ 3,
      indicator == "ELPI" ~ 2,
      indicator == "absenteeism" ~ -0.005,
      indicator == "suspension" & doc %in% doc_high ~ -0.005,
      indicator == "suspension" ~ -0.003,
      indicator == "graduation" ~ 0.68,
      indicator == "college/career" ~ 0.10
    ),
    students_to_orange = round_away(case_match(
      indicator,
      "ELA" ~ (-1 * currdenom * change_to_orange) / currstatus,
      "Math" ~ (-1 * currdenom * change_to_orange) / currstatus,
      "graduation" ~ (currdenom * change_to_orange) - currnumer,
      "college/career" ~ (currdenom * change_to_orange) - currnumer,
      .default = change_to_orange * currdenom
    ))
  )

# 5. Combine and Output -------------------------------------------------------

foster_prepped <- foster |>
  select(
    academic_year,
    aggregate_level,
    cds,
    reporting_group,
    reporting_long = program,
    student_count,
    county_name,
    district_name,
    school_name
  )

# Diagnostic: Identify which join keys differ for Homeless Youth between datasets
key_mismatches <- foster_prepped |>
  filter(reporting_long == "Homeless Youth") |>
  # Dropped aggregate_level from the join keys
  anti_join(
    homeless,
    by = join_by(academic_year, cds, reporting_group, reporting_long)
  ) |>
  select(academic_year, cds, aggregate_level, reporting_group) |>
  left_join(
    homeless |>
      filter(reporting_long == "Homeless Youth") |>
      select(
        academic_year,
        cds,
        aggregate_level_homeless = aggregate_level,
        reporting_group_homeless = reporting_group
      ),
    by = join_by(academic_year, cds)
  )

print(head(key_mismatches, 10))

print("Homeless Years:")
print(unique(homeless$academic_year))

print("Foster Years:")
print(unique(foster_prepped$academic_year))

# Final Join
homeless_to_orange <- homeless |>
  # 1. Join STRICTLY on unique identifiers
  full_join(
    foster_prepped,
    by = join_by(
      academic_year,
      cds,
      reporting_group,
      reporting_long
    ),
    suffix = c("", "_foster")
  ) |>
  mutate(
    # Coalesce aggregate_level along with the names to resolve any disagreements safely
    aggregate_level = coalesce(aggregate_level, aggregate_level_foster),
    county_name = coalesce(county_name, county_name_foster),
    district_name = coalesce(district_name, district_name_foster),
    school_name = coalesce(school_name, school_name_foster)
  ) |>
  select(-ends_with("_foster")) |>

  # 2. Join the Dashboard data.
  left_join(
    dashboard_with_levels |>
      filter(academic_year != "2018-19") |>
      select(-c(county_name, district_name, school_name)),
    by = join_by(
      academic_year,
      cds,
      reporting_long == student_group_long
    ),
    relationship = "many-to-many"
  ) |>

  # 3. Final Selection & Sorting
  select(
    academic_year,
    aggregate_level,
    cds,
    county_name,
    district_name,
    school_name,
    charter_school,
    cumulative_enrollment,
    homeless_student_enrollment,
    reporting_group,
    reporting_long,
    percent_homeless,
    currdenom,
    currnumer,
    currstatus,
    indicator,
    priority,
    change,
    color,
    assistance_status,
    indicator_eligible,
    change_to_orange,
    students_to_orange,
    student_count
  ) |>
  arrange(
    academic_year,
    county_name,
    district_name,
    school_name,
    reporting_long
  )

# Save Solano subset
solano_to_orange <- homeless_to_orange |>
  filter(county_name == "Solano")

write_csv(homeless_to_orange, here("data", "homeless_to_orange.csv"))
write_csv(solano_to_orange, here("data", "solano_to_orange.csv"))
