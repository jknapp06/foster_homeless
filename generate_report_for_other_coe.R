# Generate what-if reports for targeted student groups

library(tidyverse)
library(pins)

SELECTED_COUNTY <- "Shasta"
SELECTED_YEAR <- "2024-25" 
SELECTED_GROUP <- "Foster Youth" # Change this to target other groups

# 1. Establish pins connection for directory data
pins_path <- Sys.getenv("CDE_PINS_PATH")
if (pins_path == "") stop("CDE_PINS_PATH environment variable is not set.")
local_board <- board_folder(pins_path)

public <- pin_read(local_board, "ca_schools_directory") |> 
  select(cds, charter)

# 2. Load and filter targets
all_groups_to_orange <- read_csv("data/all_groups_to_orange.csv") |>
  filter(
    county_name == SELECTED_COUNTY,
    academic_year == SELECTED_YEAR,
    color == 1,
    student_group_long == SELECTED_GROUP
  ) |> 
  # Join directory to re-attach charter status for filtering
  left_join(public, by = join_by(cds))

# 3. Extract LEAs (Districts and Charter Schools)
districts <- all_groups_to_orange |>
  filter(rtype == "D") |>
  pull(district_name) |>
  unique()

charters <- all_groups_to_orange |>
  filter(rtype == "S", charter %in% c("Y", "Yes")) |>
  pull(school_name) |>
  unique()

leas_to_report <- unique(c(districts, charters))

# 4. Generate Reports
for (lea in leas_to_report) {
  tryCatch(
    {
      quarto::quarto_render(
        "external_what-if.qmd",
        output_file = paste0(
          str_replace_all(lea, ":", ""), "_", 
          str_replace_all(SELECTED_GROUP, " ", "_"), 
          "_what_if_one_pager.docx"
        ),
        execute_params = list(
          "lea" = lea, 
          "year" = SELECTED_YEAR, 
          "county" = SELECTED_COUNTY,
          "student_group" = SELECTED_GROUP
        ),
        quiet = FALSE
      )
    },
    error = function(e) {
      if (grepl("EMPTY_REPORT", e$message)) {
        message(e$message)
      } else {
        stop(e) 
      }
    }
  )
}