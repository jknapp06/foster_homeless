# Generate foster youth reports for each LEA in Solano County

library(tidyverse)

dashboard <- read_csv("../CADashboard/data/ca_dashboard.csv") %>%
  filter(countyname == "Solano")

districts <-
  dashboard %>%
  pull(districtname) %>%
  unique()

charters <-
  dashboard %>%
  filter(charter_flag == "Y") %>%
  pull(schoolname) %>%
  unique()

leas_to_report <- c(districts, charters)

for (lea in leas_to_report) {
  quarto::quarto_render(
    "foster_youth_report.qmd",
    output_file = paste(str_replace_all(lea, ":", ""), "foster one pager"),
    execute_params = list("lea" = lea)
  )
}
