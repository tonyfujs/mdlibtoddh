#' add_new_dataset()
#'
#' Add a full microdata record to DDH (metadata + resources)
#'
#' @param md_internal_id character: Microdata internal ID of the dataset to be added
#' @param md_token character: Microdata API authentication token
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table, output of mdlibtoddh::get_ddh_records_status()
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

add_new_dataset <- function(md_internal_id, md_token, master,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {

  # STEP 1: Get raw values from microdata API
  survey_mtdt <- get_md_metadata(id = md_internal_id, token = md_token)
  # STEP 2: format raw metadata
  # Add correct data classification information
  temp <- add_data_classification(metadata_list = survey_mtdt,
                                  md_internal_id = md_internal_id,
                                  master = master)
  # Format list
  temp <- map_md_to_ddh(temp)
  temp <- add_constant_metadata_dataset(temp)
  # Add search_tags
  temp <- add_search_tags(metadata_list = temp,
                          id = md_internal_id,
                          token = md_token)
  # Add resource link
  temp <- add_link_to_resources(metadata_list = temp,
                                md_internal_id = md_internal_id,
                                master = master)

  # STEP 3: Create dataset
  # Create JSON dataset
  temp_dataset <- filter_dataset_fields(temp, ddh_fields)
  json_dat <- ddhconnect::create_json_dataset(values = temp_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)
  # Push dataset to DDH
  resp_dat <- ddhconnect::create_dataset(body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)

  # STEP 4: Create resource
  # Create JSON resource
  temp <- add_constant_metadata_resource(temp)
  temp_resource <- filter_resource_fields(temp, ddh_fields)
  json_res <- ddhconnect::create_json_resource(values = temp_resource,
                                               publication_status = "published",
                                               ddh_fields = ddh_fields,
                                               lovs = lovs,
                                               root_url = root_url)
  resp_res <- ddhconnect::create_resource(credentials = credentials,
                                          body = json_res,
                                          root_url = root_url)

  # STEP 5: Attach resource
  # Attach resource to dataset
  json_attach <- ddhconnect::create_json_attach(resource_nids = c(resp_res$nid),
                                                root_url = root_url)
  resp_attach <- ddhconnect::attach_resources_to_dataset(dataset_nid = resp_dat$nid,
                                                         resource_nids = c(resp_res$nid),
                                                         root_url = root_url,
                                                         credentials = credentials)

  metadata_dataset <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                               root_url = root_url,
                                               credentials = credentials)
  test_created_dataset(dataset_metadata = metadata_dataset,
                       metadata_list = temp_dataset,
                       lovs = lovs,
                       root_url = root_url,
                       credentials = credentials)

  return(resp_dat$uri)
}
