# Download, clean, and combine dashboard files
# Solano County Office of Education
# Nov 2023

library(tidyverse)
library(openxlsx)

assistance_24_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus24.xlsx"
assistance_24_charter_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance24.xlsx"
assistance_23_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus23.xlsx"
assistance_23_charter_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/charterassistance23.xlsx"
assistance_22_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus22.xlsx"
assistance_19_url <- "https://www.cde.ca.gov/fg/aa/lc/documents/assistancestatus19-rev.xlsx"


assistance_24 <- read.xlsx(assistance_24_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, reportingyear = ReportYear,
         assistance_status = AssistanceStatus2024,
         ends_with("priorities"))
assistance_24_charter <- read.xlsx(assistance_24_charter_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, reportingyear = ReportingYear,
         assistance_status = AssistanceStatus2023, 
         -starts_with("Met"), 
         ends_with("current")) %>% 
  rename_with(~ gsub("current", "priorities", .x))
assistance_23 <- read.xlsx(assistance_23_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, reportingyear = ReportYear,
         assistance_status = AssistanceStatus2023,
         ends_with("priorities"))
assistance_23_charter <- read.xlsx(assistance_23_charter_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, reportingyear = ReportingYear,
         assistance_status = AssistanceStatus2023, 
         -starts_with("Met"), 
         ends_with("current")) %>% 
  rename_with(~ gsub("current", "priorities", .x))
assistance_22 <- read.xlsx(assistance_22_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, reportingyear = ReportYear,
         assistance_status = AssistanceStatus2022, 
         # met_method_1 = MetMethod1, 
         ends_with("priorities"))
assistance_19 <- read.xlsx(assistance_19_url, sheet = 4, startRow = 6) %>% 
  select(cds = CDS, grades_offered = Gsoffered, assistance_status = AssistanceStatus2019,
         # met_method_1 = MetMethod1, met_method_2 = MetMethod2,
         # met_method_3 = MetMethod3, 
         ends_with("priorities")) %>%
  mutate(reportingyear = "2019")

assistance <-
  bind_rows(assistance_23,
            assistance_23_charter,
            assistance_22,
            assistance_19) %>% 
  pivot_longer(cols = ends_with("priorities"), 
               names_to = "studentgroup", 
               values_to = "assistance",
               names_pattern = "(.*)priorities") %>% 
  mutate(studentgroup = if_else(studentgroup == "TOM", "MR", studentgroup))

write_csv(assistance, "data/assistance.csv")


