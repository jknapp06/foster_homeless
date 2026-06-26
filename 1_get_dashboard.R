library(tidyverse)
library(here)

# move dashboard dataset from CADashboard folder to a local data folder
file.copy(
  from = "../CADashboard/data/dashboard_essa.csv",
  to = "data/dashboard_essa.csv",
  overwrite = TRUE
)
