library(dplyr)
library(jsonlite)
library(magrittr)
# using code from mdlibtoddh

# STEP 1: Get data --------------------------------------------------------

# Metadata from flat files
httr::set_config(httr::config(ssl_verifypeer = 0L))

finance_lovs <- readr::read_csv("finance_lovs.csv")
ddh_lovs <- readxl::read_excel("ddh_lovs.xlsx")
# building off of this one
ddh_finance_map <- readxl::read_excel("ddh_finance_map.xlsx")

names(ddh_finance_map)[names(ddh_finance_map) == 'ddh_json_key'] <- 'machine_name'

lookup <- ddh_finance_map

# STEP 2: Check data consistency ------------------------------------------
# lookup & finance
# mdlib_field_keys <- sort(unique($ddh_fields))
# lookup_field_keys <- sort(unique(lookup$field_key))
# assertthat::assert_that(all(mdlib_field_keys %in% lookup_field_keys),
#                         msg = 'Incomplete mdlib to ddh field_key mapping')


# STEP 3: Merge data ------------------------------------------------------

# Merge lookup and mdlib
lookup <- lookup %>%
          full_join(ddh_lovs, by = "machine_name") %>%
          left_join(finance_lovs, by = c("machine_name", "list_value_name"))


# Save table -------------------------------------------------------
devtools::use_data(lookup,
                  finance_lovs,
                  ddh_lovs,
                  ddh_finance_map, overwrite = TRUE)
