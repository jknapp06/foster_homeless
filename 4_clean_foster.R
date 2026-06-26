# Foster data

library(tidyverse)
library(openxlsx)

upc2425 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc2425-k12.xlsx",
  sheet = 2,
  startRow = 2
)
upc2324 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc2324-k12.xlsx",
  sheet = 2,
  startRow = 2
)
upc2223 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc2223-k12.xlsx",
  sheet = 2,
  startRow = 2
)
upc2122 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc2122-k12.xlsx",
  sheet = 2,
  startRow = 2
)
upc2021 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc2021-k12.xlsx",
  sheet = 2,
  startRow = 2
)
upc1920 <- read.xlsx(
  "https://www.cde.ca.gov/ds/ad/documents/cupc1920-k12.xlsx",
  sheet = 2,
  startRow = 2
)

upc <-
  bind_rows(upc2425, upc2324, upc2223, upc2122, upc2021, upc1920) |>
  rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE))) |>
  mutate(
    school_name = if_else(
      school_name == "N/A",
      "District Aggregate",
      school_name
    )
  ) |>
  pivot_longer(
    cols = c(
      total_enrollment,
      `free_&_reduced_meal_program`,
      foster,
      tribal_foster_youth,
      homeless,
      migrant_program,
      direct_certification,
      unduplicated_frpm_eligible_count,
      `english_learner_(el)`,
      `calpads_unduplicated_pupil_count_(upc)`
    ),
    names_to = "program",
    values_to = "student_count"
  )

upc_solano <-
  upc |>
  filter(county_name == "Solano")

# fy22 <- read.xlsx("https://www.cde.ca.gov/ds/ad/documents/fyenrollbytype22.xlsx", sheet = 2, startRow = 2)
# fy21 <- read.xlsx("https://www3.cde.ca.gov/demo-downloads/foster/fyenrollbytype21.xlsx", sheet = 2, startRow = 2)
#
# fy_enroll <-
#   bind_rows(fy22, fy21)

write_csv(upc, file = "data/upc.csv")
write_csv(
  upc_solano,
  file = "C:/Users/jknapp/Solano County Office of Education/Assessment Research and Evaluation - A.R.E. Library/Data/upc_solano.csv"
)
