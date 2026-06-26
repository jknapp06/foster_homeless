# Enrollment
# Solano County Office of Education
# Nov 2023

library(tidyverse)
library(vroom)

enrollment25_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2025.txt"
enrollment24_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2024.txt"
enrollment23_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2023.txt"
enrollment22_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2022.txt"
enrollment21_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2021.txt"
enrollment20_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2020.txt"
enrollment19_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2019.txt"
enrollment18_url <- "https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2018.txt"

enrollment25 <- vroom(enrollment25_url) |>
  rename_with(tolower)
enrollment24 <- vroom(enrollment24_url) |>
  rename_with(tolower)
enrollment23 <- vroom(enrollment23_url) |>
  rename_with(tolower)
enrollment22 <- vroom(enrollment22_url) |>
  rename_with(tolower)
enrollment21 <- vroom(enrollment21_url) |>
  rename_with(tolower)
enrollment20 <- vroom(enrollment20_url) |>
  rename_with(tolower)
enrollment19 <- vroom(enrollment19_url) |>
  rename_with(tolower)
enrollment18 <- vroom(enrollment18_url) |>
  rename_with(tolower)

enrollment <-
  bind_rows(
    enrollment25,
    enrollment24,
    enrollment23,
    enrollment22,
    enrollment21,
    enrollment20,
    enrollment19,
    enrollment18
  ) |>
  mutate(
    student_group_long = case_match(
      studentgroup,
      "ALL" ~ "All students",
      "AA" ~ "Black/African American",
      "AI" ~ "American Indian or Alaska Native",
      "AS" ~ "Asian",
      "FI" ~ "Filipino",
      "HI" ~ "Hispanic",
      "PI" ~ "Pacific Islander",
      "WH" ~ "White",
      "MR" ~ "Multiple Races/Two or more",
      "EL" ~ "English Learner",
      "ELO" ~ "English Learners Only",
      "RFP" ~ "RFEPs Only",
      "EO" ~ "English Only",
      "SBA" ~ "Smarter Balanced Assessment",
      "CAA" ~ "CA Alternative Assessment",
      "SED" ~ "Socioeconomically Disadvantaged",
      "SWD" ~ "Students with Disabilities",
      "FOS" ~ "Foster Youth",
      "HOM" ~ "Homeless Youth"
    ),
    countyname = case_match(
      rtype,
      "X" ~ "CA State Aggregate",
      .default = countyname
    ),
    districtname = case_match(
      rtype,
      "X" ~ "State of California",
      .default = districtname
    ),
    schoolname = case_when(
      rtype == "D" &
        (is.na(schoolname) |
          schoolname == "CHECK") ~ "District Aggregate",
      .default = schoolname
    )
  )

write_csv(enrollment, file = "data/enrollment.csv")

solano_enrollment <-
  enrollment |>
  filter(
    countyname == "Solano" |
      countyname == "CA State Aggregate"
  )

write_csv(
  solano_enrollment,
  file = "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/Data/enrollment.csv"
)
write_csv(solano_enrollment, file = "data/solano_enrollment.csv")
