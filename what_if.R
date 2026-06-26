library(tidyverse)
library(pins)
library(here)

# 1. Establish robust project paths and load helpers ---------------------------
pins_path <- Sys.getenv("CDE_PINS_PATH")

if (pins_path == "") {
  stop("CDE_PINS_PATH environment variable is not set. Please add it to .Renviron.")
}

local_board <- board_folder(pins_path)

# 2. Read from Pins ------------------------------------------------------------
dashboard <- pin_read(local_board, "ca_dashboard_full")
public <- pin_read(local_board, "ca_schools_directory")

# 3. Define School Types & Helpers ---------------------------------------------
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

# 4. Process Dashboard Targets for ALL Groups ----------------------------------
dashboard_targets <- dashboard |>
  filter(indicator != "science", academic_year != "2018-19") |> 
  left_join(select(public, cds, doc, soc), join_by(cds)) |>
  mutate(currnumer = if_else(indicator == "college/career", currdenom * (currstatus/100), currnumer)) |> 
  mutate(
    change_to_orange = if_else(
      color == 1,
      case_when(
        indicator == "ELA" ~ 3,
        indicator == "Math" ~ 3,
        indicator == "ELPI" ~ 2,
        indicator == "absenteeism" ~ -0.005,
        indicator == "suspension" & doc %in% doc_high ~ -0.005,
        indicator == "suspension" ~ -0.003,
        indicator == "graduation" ~ 0.68,
        indicator == "college/career" ~ 0.10
      ),
      NA_real_ 
    ),
    students_to_orange = if_else(
      color == 1,
      round_away(case_match(
        indicator,
        "ELA" ~ (-1 * currdenom * change_to_orange) / currstatus,
        "Math" ~ (-1 * currdenom * change_to_orange) / currstatus,
        "graduation" ~ (currdenom * change_to_orange) - currnumer,
        "college/career" ~ (currdenom * change_to_orange) - currnumer,
        .default = change_to_orange * currdenom
      )),
      NA_real_
    )
  )

# 5. Final Formatting and Output -----------------------------------------------

all_groups_to_orange <- dashboard_targets |>
  select(
    academic_year,
    rtype,
    cds,
    county_name,
    district_name,
    school_name,
    student_group_long,
    currdenom,
    currnumer,
    currstatus,
    indicator,
    color,
    change_to_orange,
    students_to_orange
  ) |>
  arrange(
    academic_year,
    county_name,
    district_name,
    school_name,
    student_group_long,
    indicator
  )

# Save Solano subset
solano_to_orange <- all_groups_to_orange |>
  filter(county_name == "Solano")

write_csv(all_groups_to_orange, here("data", "all_groups_to_orange.csv"))
write_csv(solano_to_orange, here("data", "solano_to_orange.csv"))