library(dplyr)
library(jsonlite)
library(magrittr)
# using code from mdlibtoddh

# CHECK consistency between STG & PROD taxonomy
# check_taxonomy_services()
# Set root_url URL
# currently pulling from staging
# root_url <- ddhconnect:::production_root_url
root_url <- "https://newdatacatalogstg.worldbank.org/"

# STEP 1: Get data --------------------------------------------------------

# Metadata from flat files
httr::set_config(httr::config(ssl_verifypeer = 0L))

finance_lovs <- readr::read_csv("finance_lovs.csv")

# ddh_lovs <- readxl::read_excel("ddh_lovs.xlsx")
ddh_lovs <- ddhconnect::get_lovs(root_url = root_url)
#   %>%rename(ddh_machine_name = machine_name, field_lovs = list_value_name)
names(ddh_lovs)[names(ddh_lovs) == "ddh_machine_name"] <- "machine_name"
names(ddh_lovs)[names(ddh_lovs) == "field_lovs"] <- "list_value_name"
  
ddh_finance_map <- readxl::read_excel("ddh_finance_map.xlsx")
#names(ddh_finance_map)[names(ddh_finance_map) == "ddh_json_key"] <- "machine_name"

ddh_fields <- ddhconnect::get_fields(root_url = root_url) 
# %>% rename(ddh_machine_name = machine_name)
names(ddh_fields)[names(ddh_fields) == "ddh_machine_name"] <- "machine_name"

lookup <- ddh_fields

# STEP 2: Check data consistency ------------------------------------------
# lookup & finance
# mdlib_field_keys <- sort(unique($ddh_fields))
# lookup_field_keys <- sort(unique(lookup$field_key))
# assertthat::assert_that(all(mdlib_field_keys %in% lookup_field_keys),
#                         msg = 'Incomplete mdlib to ddh field_key mapping')


# STEP 3: Merge data ------------------------------------------------------
# keep fields look up?
#fin_lookup <- unique(subset(ddh_fields, select = c("machine_name"))) %>%
#          full_join(ddh_lovs, by = "machine_name") %>%
#          left_join(ddh_finance_map, by = "machine_name") %>%
#          left_join(finance_lovs, by = c("machine_name", "list_value_name"))

fin_lookup <- ddh_finance_map %>%
              full_join(ddh_lovs, by = "machine_name") %>%
              left_join(finance_lovs, by = c("machine_name", "list_value_name"))



# Save table -------------------------------------------------------
devtools::use_data(lookup,
                  finance_lovs,
                  ddh_lovs,
                  ddh_finance_map, overwrite = TRUE)
