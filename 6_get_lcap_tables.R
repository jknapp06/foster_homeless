library(tidyverse)
library(pdftools)
library(stringr)
library(tabulapdf)
library(here)

options(scipen = 999)

dashboard <- read_csv(here("data/dashboard_essa.csv"))
current_year = 2025
lcap_dir = "lcaps"

solano_lcap_links <-
  dashboard |>
  filter(
    countyname == "Solano",
    districtname == "Benicia Unified",
    rtype == "D" | (charter_flag == "Y" & rtype == "S")
  ) |>
  select(cds, countyname, districtname, schoolname, charter_flag) |>
  distinct() |>
  mutate(
    lcap_url = paste0(
      "https://api.mycdeconnect.org/reports/lcap?cdsCode=",
      cds,
      "&year=",
      current_year
    )
  )

extract_foster_youth_table <- function(pdf_path, lea) {
  # Try to extract tables from this section
  tryCatch(
    {
      # Use tabulapdf to extract tables from the specific section
      foster_youth_tables <- extract_tables(
        pdf_path,
        # pages = c(69, 100),
        pages = 71,
        output = "tibble"
      )

      # # Add LEA identifier
      # foster_youth_tables <- foster_youth_tables |>
      #   map(~ mutate(.x, lea = lea))

      return(foster_youth_tables)
    },
    error = function(e) {
      # If table extraction fails, return NULL
      message("Could not extract table for ", lea, ": ", e$message)
      return(NULL)
    }
  )
}

lcap_foster_youth_tables <- tibble()

for (i in 1:nrow(solano_lcap_links)) {
  lea <- if_else(
    is.na(solano_lcap_links$charter_flag[i]),
    solano_lcap_links$districtname[i],
    solano_lcap_links$schoolname[i]
  )
  lcap_link <- solano_lcap_links$lcap_url[i]
  file_name <- paste0(
    gsub("[^A-Za-z0-9]", "_", lea),
    ".pdf"
  )

  pdf_path <- here(lcap_dir, file_name)

  # Extract Foster Youth table
  foster_youth_table <- extract_foster_youth_table(pdf_path, lea)

  # Bind the tables
  if (!is.null(foster_youth_table)) {
    # If multiple tables are returned, unnest them
    if (length(foster_youth_table) > 1) {
      foster_youth_table <- tibble(
        table = foster_youth_table,
        lea = lea
      ) |>
        unnest(table)
    } else {
      foster_youth_table <- foster_youth_table[[1]] |>
        mutate(lea = lea)
    }

    lcap_foster_youth_tables <- bind_rows(
      lcap_foster_youth_tables,
      foster_youth_table
    )
  }
}

# Save the Foster Youth tables
# write_csv(lcap_foster_youth_tables, "data/lcap_foster_youth_tables.csv")
