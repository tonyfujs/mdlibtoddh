#' update_existing_dataset()
#'
#' Update a full microdata record in DDH (metadata + resources)
#'
#' @param md_internal_id character: Microdata internal ID of the dataset to be added
#' @param md_token character: Microdata API authentication token
#' @param ddh_credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

update_existing_dataset <- function(md_internal_id, md_token, ddh_credentials, master, root_url) {

  # STEP 1: Get raw values from microdata API
  survey_mtdt <- get_md_metadata(id = md_internal_id, token = md_token)
  # STEP 2: format raw metadata
  temp <- map_md_to_ddh(survey_mtdt)

  # Add correct data classification information
  temp <- add_data_classification(metadata_list = temp,
                                  md_internal_id,
                                  master = master)
  # Add resource link
  temp <- add_link_to_resources(metadata_list = temp,
                                md_internal_id = md_internal_id,
                                master = master)

  # TEMPORARY: REMOVE VERSION DATE
  temp$field_wbddh_version_date <- NULL
  # STEP 3: Create dataset
  # Create JSON dataset
  json_dat <- create_json_dataset(temp)
  # Push dataset to DDH
  node_id <- master$ddh_nids[master$md_internal_id == md_internal_id]
  resp_dat <- ddhconnect::update_dataset(credentials = ddh_credentials,
                                         nid = node_id,
                                         body = json_dat,
                                         root_url = root_url)

  # STEP 4: Create resource
  # Create JSON resource
  nid_res <- ddhconnect::get_resource_nid(credentials = ddh_credentials,
                                          nid = resp_dat$nid,
                                          root_url = root_url)
  json_res <- create_json_resource(temp)
  resp_res <- ddhconnect::update_dataset(credentials = ddh_credentials,
                                         nid = nid_res,
                                         body = json_res,
                                         root_url = root_url)


  return(resp_dat$uri)
}
