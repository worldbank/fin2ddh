library(dplyr)
library(jsonlite)

# based on mdlibtoddh add_internal_data

# Set root_url URL
# currently pulling from staging
# root_url <- ddhconnect:::production_root_url
root_url <- "https://newdatacatalogstg.worldbank.org/"

# STEP 1: Get data --------------------------------------------------------
httr::set_config(httr::config(ssl_verifypeer = 0L))

# Metadata from flat files
finance_lovs <- readxl::read_excel("./data-raw/finance_lovs.xlsx")
ddh_lovs <- ddhconnect::get_lovs(root_url = root_url)
ddh_finance_map <- readxl::read_excel("./data-raw/ddh_finance_map.xlsx")
ddh_fields <- ddhconnect::get_fields(root_url = root_url)

# STEP 2: Merge data ------------------------------------------------------
lookup <- ddh_finance_map %>%
              full_join(ddh_lovs, by = "machine_name") %>%
              left_join(finance_lovs, by = c("machine_name", "list_value_name")) %>%
              select(machine_name, finance_json_key, list_value_name, finance_value, tid) %>%
              rename(ddh_machine_name = machine_name,
                     field_lovs = list_value_name)

# STEP 3: Save table -------------------------------------------------------
devtools::use_data(lookup,
                   finance_lovs,
                   ddh_lovs,
                   ddh_finance_map,
                   ddh_fields,
                   overwrite = TRUE)
