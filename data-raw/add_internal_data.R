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

ddh_schema_finance_dataset  <- jsonlite::fromJSON("./data-raw/ddh_schema_finance_dataset.json")
ddh_schema_finance_resource <- jsonlite::fromJSON("./data-raw/ddh_schema_finance_resource.json")

# For unit tests
ddh_fin_datasets_test         <- jsonlite::fromJSON("./data-raw/ddh_fin_datasets.json", simplifyVector = FALSE)
fin_portal_datasets_test      <- jsonlite::fromJSON("./data-raw/fin_portal_datasets.json", simplifyVector = FALSE)

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

# STEP 5: Create fin_placeholder -------------------------------------------------------
# Used Same logic for creating md_placeholder in mdlibtoddh
# Get Taxonomy fields
taxonomy <- ddhconnect::get_lovs(root_url = root_url)%>%
  rename(ddh_machine_name = machine_name, field_lovs = list_value_name)

fields <- ddhconnect::get_fields(root_url = root_url) %>%
  filter(data_type == 'microdata') %>%
  rename(ddh_machine_name = machine_name)
# TODO: Report type to IT
fields$ddh_machine_name[fields$ddh_machine_name == "field__wbddh_depositor_notes"] <- "field_wbddh_depositor_notes"


# assert that all fields except for the specified ones are present in the lookup
# assert that all taxonomy fields except for the specified ones are present in the lookup
taxonomy_machine_names <- sort(unique(taxonomy$ddh_machine_name))
lookup_machine_names <- sort(unique(lookup$ddh_machine_name))
taxonomy_remove <-
  c(
    "field_format",
    "field_tags",
    "field_wbddh_economy_coverage",
    "field_wbddh_global_practices",
    "field_wbddh_spatial_data_type",
    "field_wbddh_region",
    "field_wbddh_periodicity",
    "field_wbddh_api_format",
    "field_wbddh_update_frequency",
    "field_frequency",
    "status",
    "field_granularity_list",
    "field_wbddh_working_unit_user"
  )

taxonomy_machine_names <- taxonomy_machine_names[!taxonomy_machine_names %in% taxonomy_remove]
assertthat::assert_that(length(taxonomy_machine_names[!taxonomy_machine_names %in% lookup_machine_names]) == 0,
                        msg = 'Incomplete list of taxonomy variables')
fields_machine_names <- sort(unique(c(fields$ddh_machine_name, "field_license_wbddh", "workflow_status"))) # TEMPORARY FIX (not returned by field service)
fields_remove <-
  c(
    "field_ddh_external_contact_email",
    "field_format",
    "field_granularity_list",
    "field_tags",
    "field_temporal_coverage",
    "field_wbddh_additional_publisher",
    "field_wbddh_aggregation_method",
    "field_wbddh_base_period",
    "field_wbddh_curator_notes",
    "field_wbddh_depositor_notes",
    "field_wbddh_ds_embargo_date",
    "field_wbddh_ds_source",
    "field_wbddh_dsttl_upi",
    "field_wbddh_economy_coverage",
    "field_wbddh_next_expected_update",
    "field_wbddh_no_of_economies",
    "field_wbddh_organization",
    "field_wbddh_other_producer",
    "field_wbddh_produced_by",
    "field_wbddh_related_indicators",
    "field_wbddh_search_tags",
    "field_wbddh_series_code",
    "field_wbddh_subscription_date",
    "field_wbddh_type_of_license",
    "field_wbddh_update_frequency",
    "field_best_bets",
    "field_creator_name",
    "field_date_of_creation",
    "field_external_metadata",
    "field_external_metadata",
    "field_link_api",
    "field_link_remote_file",
    "field_preview_image",
    "field_resource_weight",
    "field_upload",
    "field_wbddh_api_format",
    "field_wbddh_statistical_concept",
    "workflow_status"
  )

fields_machine_names <- fields_machine_names[!fields_machine_names %in% fields_remove]
assertthat::assert_that(length(fields_machine_names[!fields_machine_names %in% lookup_machine_names]) == 0,
                        msg = 'Incomplete list of taxonomy variables')
machine_names <- sort(unique(c(fields_machine_names, taxonomy_machine_names)))
fin_placeholder <- vector(mode = 'list', length = length(machine_names))
names(fin_placeholder) <- machine_names


# STEP 5: Save table -------------------------------------------------------
devtools::use_data(lookup,
                   finance_lovs,
                   ddh_lovs,
                   ddh_finance_map,
                   fin_placeholder,
                   overwrite = TRUE)

devtools::use_data(ddh_schema_finance_dataset,
                   ddh_schema_finance_resource,
                   ddh_fin_datasets_test,
                   fin_portal_datasets_test,
                   overwrite = TRUE)
