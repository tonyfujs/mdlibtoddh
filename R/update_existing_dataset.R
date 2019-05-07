#' update_existing_dataset()
#'
#' Update a full microdata record in DDH (metadata + resources)
#'
#' @param md_internal_id character: Microdata internal ID of the dataset to be added
#' @param md_token character: Microdata API authentication token
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table
#' @param ddh_fields dataframe: table of all the data catalog fields by node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

update_existing_dataset <- function(md_internal_id, md_token, master,
                                    ddh_fields = ddhconnect::get_fields(),
                                    lovs = ddhconnect::get_lovs(),
                                    root_url = dkanr::get_url(),
                                    credentials = list(cookie = dkanr::get_cookie(),
                                                       token = dkanr::get_token())) {

  # STEP 1: Get raw values from microdata API
  survey_mtdt <- get_md_metadata(id = md_internal_id, token = md_token)

  # Add correct data classification information
  temp <- add_data_classification(metadata_list = survey_mtdt,
                                  md_internal_id,
                                  master = master)
  # STEP 2: format raw metadata
  temp <- map_md_to_ddh(temp)
  temp <- add_constant_metadata_dataset(temp)

  # Add resource link
  temp <- add_link_to_resources(metadata_list = temp,
                                md_internal_id = md_internal_id,
                                master = master)

  # STEP 3: Create dataset
  # Create JSON dataset
  temp_dataset <- filter_dataset_fields(temp, ddh_fields)

  #Filter out blank values
  non_blank     <- vapply(temp_dataset, function(x){!is_blank(x)}, FUN.VALUE = FALSE)

  temp_dataset  <- temp_dataset[non_blank]

  json_dat <- ddhconnect::create_json_dataset(values = temp_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)
  # Push dataset to DDH
  node_id <- master$ddh_nids[master$md_internal_id == md_internal_id]

  # For STG
  resp_dat <- ddhconnect::update_dataset(nid = node_id,
                                         body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)

  tryCatch({
    # STEP 4: Create resource
    # Create JSON resource
    temp <- add_constant_metadata_resource(temp)
    temp_resource <- filter_resource_fields(temp, ddh_fields)

    json_res <- ddhconnect::create_json_resource(values = temp_resource,
                                                 publication_status = "published",
                                                 dataset_nid = resp_dat$nid,
                                                 ddh_fields = ddh_fields,
                                                 lovs = lovs,
                                                 root_url = root_url)
    metadata_dataset <- ddhconnect::get_metadata(nid = node_id,
                                                 root_url = root_url,
                                                 credentials = credentials)
    nid_res <- unlist(ddhconnect::get_resource_nids(metadata_dataset))

    #Makesure resource is Microdata Landing Page
    if(length(nid_res) > 1){
      nid_res <- resource_check(as.list(nid_res))
    }

    resp_res <- ddhconnect::update_resource(nid = nid_res,
                                            body = json_res,
                                            root_url = root_url,
                                            credentials = credentials)

    # Account for blank values
    metadata_dataset <- pass_blank_values(node_id = node_id,
                                          dataset_metadata = metadata_dataset,
                                          metadata_list = temp_dataset,
                                          root_url = root_url,
                                          credentials = credentials)

    test_created_dataset(dataset_metadata = metadata_dataset,
                         metadata_list = temp_dataset,
                         lovs = lovs,
                         root_url = root_url,
                         credentials = credentials)

    return(resp_dat$uri)

  }, error = function(e){

    message <- paste("Error:",e,"; with creating resources for", resp_dat$uri)

    return(message)

  })
}
