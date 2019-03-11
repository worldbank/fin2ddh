library(dplyr)
library(jsonlite)

# based on mdlibtoddh add_internal_data

# Set root_url URL
# currently pulling from staging
# root_url <- ddhconnect:::production_root_url
root_url <- "http://ddh1stg.prod.acquia-sites.com/"

# STEP 1: Get data --------------------------------------------------------
httr::set_config(httr::config(ssl_verifypeer = 0L))

# Metadata from flat files
finance_lovs_df <- readxl::read_excel("./data-raw/finance_lovs.xlsx")
ddh_lovs_df <- ddhconnect::get_lovs(root_url = root_url)
ddh_finance_map <- readxl::read_excel("./data-raw/ddh_finance_map.xlsx")
# include fields when stable
#ddh_fields <- ddhconnect::get_fields(root_url = root_url)

ddh_schema_finance_dataset <- jsonlite::fromJSON("./data-raw/ddh_schema_finance_dataset.json")
ddh_schema_finance_resource <- jsonlite::fromJSON("./data-raw/ddh_schema_finance_resource.json")

# STEP 2: Merge data ------------------------------------------------------
lookup <- ddh_finance_map %>%
              full_join(ddh_lovs_df, by = "machine_name") %>%
              left_join(finance_lovs_df, by = c("machine_name", "list_value_name")) %>%
              select(machine_name, finance_json_key, list_value_name, finance_value, tid) %>%
              rename(ddh_machine_name = machine_name,
                     field_lovs = list_value_name)


# STEP 3: Restructure lookup vals and tids ---------------------------------
finance_lovs <- unique(finance_lovs_df$machine_name) %>%
  purrr::map(function(x){
    fin_vals <- finance_lovs_df[finance_lovs_df$machine_name==x,]$list_value_name
    names(fin_vals) <- finance_lovs_df[finance_lovs_df$machine_name==x,]$finance_value
    return(fin_vals)
  })
names(finance_lovs) <- unique(finance_lovs_df$machine_name)

ddh_lovs <- unique(ddh_lovs_df$machine_name) %>%
  purrr::map(function(x){
    fin_tids <- ddh_lovs_df[ddh_lovs_df$machine_name==x,]$tid
    names(fin_tids) <- ddh_lovs_df[ddh_lovs_df$machine_name==x,]$list_value_name
    return(fin_tids)
  })
names(ddh_lovs) <- unique(ddh_lovs_df$machine_name)


# STEP 4: Save table -------------------------------------------------------
devtools::use_data(lookup,
                   finance_lovs,
                   ddh_lovs,
                   ddh_finance_map,
                   overwrite = TRUE)

devtools::use_data(ddh_schema_finance_dataset,
                   ddh_schema_finance_resource,
                   overwrite = TRUE)
