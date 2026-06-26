# Clean homeless enrollment data files

library(tidyverse)

homeless_enrollment_24 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse2425.txt"
)
homeless_enrollment_23 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse2324.txt"
)
homeless_enrollment_22 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse2223.txt"
)
homeless_enrollment_21 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse2122.txt"
)
homeless_enrollment_20 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse2021.txt"
)
homeless_enrollment_19 <- read_tsv(
  "https://www3.cde.ca.gov/demo-downloads/homeless/hse1920.txt"
)

homeless_enrollment <-
  bind_rows(
    homeless_enrollment_19,
    homeless_enrollment_20,
    homeless_enrollment_21,
    homeless_enrollment_22,
    homeless_enrollment_23,
    homeless_enrollment_24
  ) |>
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
  mutate(
    county_name = if_else(
      aggregate_level == "T",
      "CA State Aggregate",
      county_name
    ),
    district_name = case_when(
      aggregate_level == "T" ~ "CA State Aggregagate",
      aggregate_level == "C" ~ "County Aggregate",
      .default = district_name
    ),
    school_name = case_when(
      aggregate_level == "T" ~ "CA State Aggregagate",
      aggregate_level == "C" ~ "County Aggregate",
      aggregate_level == "D" ~ "District Aggregate",
      .default = school_name
    ),
    reporting_group = case_when(
      startsWith(reporting_category, "R") ~ "Race/Ethnicity",
      startsWith(reporting_category, "S") ~ "Program",
      startsWith(reporting_category, "GR") ~ "Grade",
      startsWith(reporting_category, "G") ~ "Gender",
      startsWith(
        reporting_category,
        "HUY"
      ) ~ "Homeless Unaccompanied Youth Status",
      reporting_category == "TA" ~ "Total"
    ),
    reporting_long = case_match(
      reporting_category,
      "RB" ~ "Black/African American",
      "RI" ~ "American Indian or Alaska Native",
      "RA" ~ "Asian",
      "RF" ~ "Filipino",
      "RH" ~ "Hispanic or Latino",
      "RD" ~ "Did not Report",
      "RP" ~ "Pacific Islander",
      "RT" ~ "Two or More Races",
      "RW" ~ "White",
      "SE" ~ "English Learners",
      "SD" ~ "Students with Disabilities",
      "SM" ~ "Migrant",
      "GM" ~ "Male",
      "GF" ~ "Female",
      "GX" ~ "Non-Binary Gender",
      "GZ" ~ "Missing Gender",
      "GRKN" ~ "Kindergarten",
      "GR01" ~ "Grade 1",
      "GR02" ~ "Grade 2",
      "GR03" ~ "Grade 3",
      "GR04" ~ "Grade 4",
      "GR05" ~ "Grade 5",
      "GR06" ~ "Grade 6",
      "GR07" ~ "Grade 7",
      "GR08" ~ "Grade 8",
      "GR09" ~ "Grade 9",
      "GR10" ~ "Grade 10",
      "GR11" ~ "Grade 11",
      "GR12" ~ "Grade 12",
      "HUYN" ~ "Homeless Unaccompanied Youth (No)",
      "HUYY" ~ "Homeless Unaccompanied Youth (Yes)",
      "TA" ~ "Total Students"
    )
  )

write_csv(homeless_enrollment, "data/homeless.csv")
