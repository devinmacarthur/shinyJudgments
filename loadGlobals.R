library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(highcharter)
library(htmlwidgets)
library(plotly)
library(magrittr)

#read primary input data
counties_or <- readRDS("data/counties_or.rds")
zcta_or <- readRDS("data/zcta_or.rds")
flat_file <- readRDS("data/flat_file.rds")
OR_Race <- readRDS("data/OR_Race.rds")
pctRace <- readRDS("data/pctRace.rds")
CountyDataACS <- readRDS("data/CountyDataACS.rds")
judgments <- readRDS("data/judgments.rds")

#Static figures for eviction filings since moratorium
since_mora_or <- flat_file %>% 
  filter(date > "2020-03-22") %>% 
  nrow()

since_mora_mult <- flat_file %>% 
  filter(location == "Multnomah") %>% 
  filter(date > "2020-03-17") %>% 
  nrow()

in_2020 <- flat_file %>% 
  filter(date >= "2020-1-1" & date < "2021-1-1") %>% 
  nrow()

in_2021 <- flat_file %>% 
  filter(date >= "2021-1-1" & date < "2022-1-1") %>% 
  nrow()

judgmentsEvict_2021 <- flat_file %>%
  filter(date >= "2021-01-01") %>%
  filter(Judgment_General == 1) %>% 
  nrow()

landlord_judgments <- flat_file %>%
  filter(date > "2020-03-22") %>%
  filter(Judgment_General == 1) %>% 
  nrow()

landlord_judgmentsIn2021 <- flat_file %>%
  filter(date >= "2021-01-01") %>%
  filter(Judgment_General == 1) %>% 
  nrow()

dismissed <- flat_file %>% 
  filter(date > "2020-03-22") %>%
  filter(Judgment_Dismissal == 1) %>% 
  nrow()

lawyers_tenant <- flat_file %>% 
  filter(date > "2020-03-22") %>%
  filter(tenant_has_lawyer == 1) %>% 
  nrow()

lawyers_landlord <- flat_file %>% 
  filter(date > "2020-03-22") %>%
  filter(landlord_has_lawyer == 1) %>% 
  nrow()

open_cases <- flat_file %>% 
  filter(status == "Open") %>% 
  nrow()

tables_county <- flat_file %>% 
  group_by(location)

fromDate <- format(min(flat_file$date, na.rm = TRUE), "%b %d %Y")
toDate <- format(max(flat_file$date, na.rm = TRUE), "%b %d %Y")

currentMonth <- format(max(flat_file$date, na.rm = TRUE), "%B %Y")

casesThisMonth <- flat_file %>% 
  filter(date >= max(flat_file$month, na.rm = TRUE)) %>% 
  nrow()

dismissedThisMonth <- judgments %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(date >= max(flat_file$month, na.rm = TRUE)) %>%
  filter(case_type == "Judgment - General Dismissal" |
           case_type == "Judgment - Limited Dismissal"|
           case_type == "Amended Judgment - General Dismissal"|
           case_type == "Amended Judgment - Limited Dismissal") %>% 
  group_by(case_code) %>% 
  summarise(n = n()) %>% 
  nrow()

evictedThisMonth <- judgments %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(date >= max(flat_file$month, na.rm = TRUE)) %>% 
  filter(case_type == "Judgment - General" |
           case_type == "Amended Judgment - General"|
           case_type == "Amended Judgment - Corrected General"|
           case_type == "Judgment - Corrected General"|
           case_type == "Judgment - General Creates Lien"|
           case_type == "Amended Judgment - General Creates Lien"|
           case_type == "Judgment - Corrected General Creates Lien") %>% 
  group_by(case_code) %>% 
  summarise(n = n()) %>% 
  nrow()
  
afterAprilCases <- flat_file %>% 
  filter(date >= "2020-04-01") %>% 
  nrow()
  

afterAprilEvicted <- judgments %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(date >= "2020-04-01") %>% 
  filter(case_type == "Judgment - General" |
           case_type == "Amended Judgment - General"|
           case_type == "Amended Judgment - Corrected General"|
           case_type == "Judgment - Corrected General"|
           case_type == "Judgment - General Creates Lien"|
           case_type == "Amended Judgment - General Creates Lien"|
           case_type == "Judgment - Corrected General Creates Lien") %>% 
  group_by(case_code) %>% 
  summarise(n = n()) %>% 
  nrow()


afterAprilDismissed <- judgments %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(date >= "2020-04-01") %>%
  filter(case_type == "Judgment - General Dismissal" |
           case_type == "Judgment - Limited Dismissal"|
           case_type == "Amended Judgment - General Dismissal"|
           case_type == "Amended Judgment - Limited Dismissal") %>% 
  group_by(case_code) %>% 
  summarise(n = n()) %>% 
  nrow()



afterAprilOpen <- flat_file %>% 
  filter(status == "Open" & date >= "2020-04-01") %>% 
  nrow()


legalRepORMult <- data.frame(place=c("Multnomah", "Multnomah", "Oregon", "Oregon"),
                             Tenure=c("Landlord", "Tenant", "Landlord", "Tenant"),
                             hasLawyer=c(flat_file %>% 
                                           filter(location == "Multnomah" & landlord_has_lawyer == 1) %>%
                                           nrow()/flat_file %>% filter(location == "Multnomah") %>% nrow(),
                                         flat_file %>% 
                                           filter(location == "Multnomah" & tenant_has_lawyer == 1) %>%
                                           nrow()/flat_file %>% filter(location == "Multnomah") %>% nrow(),
                                         flat_file %>% 
                                           filter(landlord_has_lawyer == 1) %>%
                                           nrow()/flat_file %>% nrow(),
                                         flat_file %>% 
                                           filter(tenant_has_lawyer == 1) %>%
                                           nrow()/flat_file %>% nrow()
                             ))

monthly <- flat_file %>%
  group_by(month) %>% 
  summarize(Filings = n()) 

  # ggplot(aes(x = month, y = Filings, fill = Filings)) +
  # geom_bar(stat = "identity") +
  # scale_fill_continuous(low = "lightskyblue1", high = "slateblue3") +
  # labs(title = "Oregon Evictions - Filings per Month since 01/01/2020",
  #      x = "", y = "Number of Cases", fill = "") +
  # scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

counties_input <- counties_or %>% 
  left_join(flat_file %>%
              group_by(location) %>% 
              summarize(filings = n()), by = "location")