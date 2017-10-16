library(dplyr)
library(jsonlite)
#library(magrittr)

# based on mdlibtoddh add_internal_data

# Set root_url URL
# currently pulling from staging
# root_url <- ddhconnect:::production_root_url
root_url <- "https://newdatacatalogstg.worldbank.org/"

# STEP 1: Get data --------------------------------------------------------
httr::set_config(httr::config(ssl_verifypeer = 0L))

# Metadata from flat files
finance_lovs <- readr::read_csv("./data-raw/finance_lovs.csv")

# ddh_lovs <- readxl::read_excel("ddh_lovs.xlsx")
ddh_lovs <- ddhconnect::get_lovs(root_url = root_url)


ddh_finance_map <- readxl::read_excel("./data-raw/ddh_finance_map.xlsx")

ddh_fields <- ddhconnect::get_fields(root_url = root_url)

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

lookup <- ddh_finance_map %>%
              full_join(ddh_lovs, by = "machine_name") %>%
              left_join(finance_lovs, by = c("machine_name", "list_value_name")) %>%
              select(machine_name, finance_json_key, list_value_name, finance_value, tid)


# Save table -------------------------------------------------------
devtools::use_data(lookup,
                   finance_lovs,
                   ddh_lovs,
                   ddh_finance_map,
                   overwrite = TRUE)
