library(tidyverse)

doc_high <- c(56)

round_away <- function(n) {
  if_else(n > 0, ceiling(n), floor(n))
}

# 1. Mock Data Representing Red Dashboard Groups
test_df <- tibble(
  indicator = c(
    "ELA",
    "Math",
    "graduation",
    "absenteeism",
    "suspension",
    "suspension",
    "ELA_Zero_Error"
  ),
  DOC = c(54, 54, 56, 54, 56, 52, 54),
  currdenom = c(100, 100, 100, 200, 1000, 1000, 100),
  currnumer = c(NA, NA, 60, 50, 120, 80, NA),
  currstatus = c(-60, -45, 60, 25, 12, 8, 0) # Notice the 0 at the end
)

# 2. Validated Logic
test_output <- test_df |>
  mutate(
    # VALIDATION: Flag impossible scenarios to prevent division by zero or negative denominators
    flag_invalid = case_when(
      currdenom <= 0 | is.na(currdenom) ~ TRUE,
      indicator %in%
        c("ELA", "Math") &
        (currstatus >= 0 | is.na(currstatus)) ~ TRUE,
      .default = FALSE
    ),
    change_to_orange = case_when(
      indicator == "ELA" ~ 3,
      indicator == "Math" ~ 3,
      indicator == "ELPI" ~ 2,
      indicator == "absenteeism" ~ -0.005,
      indicator == "suspension" & DOC %in% doc_high ~ -0.005,
      indicator == "suspension" ~ -0.003,
      indicator == "graduation" ~ 0.68,
      indicator == "college/career" ~ 0.10
    ),
    # Execute math only on valid rows
    students_to_orange = if_else(
      flag_invalid,
      NA_real_,
      round_away(case_match(
        indicator,
        "ELA" ~ (-1 * currdenom * change_to_orange) / currstatus,
        "Math" ~ (-1 * currdenom * change_to_orange) / currstatus,
        "graduation" ~ (currdenom * change_to_orange) - currnumer,
        "college/career" ~ (currdenom * change_to_orange) - currnumer,
        .default = change_to_orange * currdenom
      ))
    )
  )

print(
  test_output |>
    select(indicator, currdenom, currstatus, flag_invalid, students_to_orange)
)
