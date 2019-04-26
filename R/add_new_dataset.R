#' add_new_dataset
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_list list: list with one finance dataset's metadata, from get_fin_datasets_metadata()
#' @param ddh_fields dataframe: table of all the data catalog fields by node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#' @param is_unit_test_dataset boolean: check if unit test is being run for dataset
#' @param is_unit_test_resource boolean: check if unit test is being run for dataset
#'
#' @import jsonlite
#' @return list
#' @export
#'

add_new_dataset <- function(metadata_list,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                              token = dkanr::get_token()),
                            is_unit_test_dataset = FALSE,
                            is_unit_test_resource = FALSE) {

    # format raw metadata
    metadata_temp <- fin_to_ddh_keys(metadata_list)
    metadata_temp <- add_constant_metadata_dataset(metadata_temp)
    metadata_temp <- map_fin_metadata(metadata_temp)

    category <- metadata_list$view$category
    metadata_temp <- add_link_to_resources(metadata_temp, category)

    # create dataset
    metadata_temp_dataset <- filter_dataset_fields(metadata_temp, ddh_fields)

    # Return metadata_list if function is being used in unit test
    if(is_unit_test_dataset){

      return(metadata_temp_dataset)

    } else if(is_unit_test_resource){

      metadata_temp           <- add_constant_metadata_resource(metadata_temp)
      metadata_temp_resource  <- filter_resource_fields(metadata_temp, ddh_fields)
      return(metadata_temp_resource)
    }

    json_dat <- ddhconnect::create_json_dataset(values = metadata_temp_dataset,
                                                publication_status = "published",
                                                ddh_fields = ddh_fields,
                                                lovs = lovs,
                                                root_url = root_url)


    resp_dat <- ddhconnect::create_dataset(body = json_dat,
                                           root_url = root_url,
                                           credentials = credentials)

    tryCatch({
      # Create Resource
      metadata_temp           <- add_constant_metadata_resource(metadata_temp)
      metadata_temp_resource  <- filter_resource_fields(metadata_temp, ddh_fields)


      json_res <- ddhconnect::create_json_resource(values = metadata_temp_resource,
                                                   dataset_nid = resp_dat$nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)


      resp_res <- ddhconnect::create_resource(body = json_res,
                                              root_url = root_url,
                                              credentials = credentials)
      # Test Created Dataset
      metadata_dataset <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                                   root_url = root_url,
                                                   credentials = credentials)
      test_created_dataset(dataset_metadata = metadata_dataset,
                           metadata_list = metadata_temp_dataset,
                           root_url = root_url,
                           credentials = credentials)

      return(cat(resp_dat$uri))

    }, error = function(e){

      message <- paste("Error:",e,"; with creating resources for", resp_dat$uri)

      return(cat(message))
    })
  }
