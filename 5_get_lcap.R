library(tidyverse)
library(rvest)
library(googledrive)
library(pdftools)
library(tabulapdf)
library(polite)
library(here)

options(scipen = 999)

dashboard <- read_csv(here("data/dashboard_essa.csv"))

current_year = 2025

solano_lcap_links <-
  dashboard |>
  filter(
    countyname == "Solano",
    districtname == "Dixon Unified",
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

# lcaps_links <- list(
#   "Benicia Unified" = "https://api.mycdeconnect.org/reports/lcap?cdsCode=48705240000000&year=2025",
#   "Dixon Unified" = "https://api.mycdeconnect.org/reports/lcap?cdsCode=48705320000000&year=2025",
#   "Fairfield-Suisun Unified" = "https://api.mycdeconnect.org/reports/lcap?cdsCode=48705400000000&year=2025"

#   "Vallejo City Unified" = "https://api.mycdeconnect.org/reports/lcap?cdsCode=48705810000000&year=2025"
# )

# lcap_links <-
#   lcap_pages_url <- "https://www.cde.ca.gov/re/lc/lcaplinkscoesd2223.asp#accordionfaq"
# school_info <- read_tsv(
#   "https://www.cde.ca.gov/schooldirectory/report?rid=dl1&tp=txt"
# )
# districts <-
#   school_info |>
#   pull(District) |>
#   unique()

# # Get LCAP links from the CDE website
# lcap_links <- read_html(lcap_pages_url) |>
#   html_elements("li a") |>
#   {
#     tibble(name = html_text2(.), link = html_attr(., "href"))
#   }

# lcaps_clean <- lcap_links |>
#   mutate(district = str_trim(str_remove(name, "\\(PDF\\)"))) |>
#   filter(district %in% districts)

lcaps <- tibble()
lcap_errors <- tibble(
  lea = character(),
  schoolname = character(),
  link = character()
)

# Directory to save the PDFs
output_dir <- "lcaps"
dir.create(output_dir, showWarnings = FALSE)

lcap_tables <- list()

# Load text from LCAP PDFs and add them to lcaps tibble
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

  print(lea)

  tryCatch(
    {
      # Download the PDF
      if (str_detect(lcap_link, "google")) {
        drive_download(
          as_id(lcap_link),
          path = here(output_dir, file_name),
          overwrite = TRUE
        )
      } else {
        download.file(lcap_link, here(output_dir, file_name), mode = "wb")
      }

      # # Extract text from the PDF
      lcap <- pdf_text(here(output_dir, file_name)) |>
        as_tibble() |>
        mutate(lea = lea, link = lcap_link)

      # Add the extracted text to the LCAPS tibble
      lcaps <- bind_rows(lcaps, lcap)
    },
    error = function(e) {
      # On error, add the district and link to lcap_errors
      lcap_errors <- bind_rows(
        lcap_errors,
        tibble(lea = lea, link = lcap_link)
      )
    }
  )

  tryCatch(
    {
      # tabula_f <- system.file(output_dir, file_name, package = "tabulapdf")
      lcap_table <- extract_tables(
        here(output_dir, file_name),
        output = "tibble"
      )

      # lcap_table <-
      #   lcap_table |>
      #   mutate(lea = lea)

      lcap_tables <- list(lcap_tables, lcap_table)
    },
    error = function(e) {
      message("LCAP table error on ", file_name, ": ", e)
    }
  )
}

# # Used name instead of district, so need to clean it up again
# lcaps_and_links <-
#   lcaps |>
#   left_join(lcaps_clean, by = join_by(link, district))

lcaps <-
  lcaps |>
  rename(lcap_text = value)

lcap_tables_unnested <-
  lcap_tables |>
  as_tibble() |>
  unnest(names_repair = "unique")

# Save it
write_csv(lcaps, "data/lcaps.csv")
write_csv(lcap_errors, "data/lcap_link_errors.csv")
