#' add_new_dataset()
#'
#' Add a full microdata record to DDH (metadata + resources)
#'
#' @param md_internal_id character: Microdata internal ID of the dataset to be added
#' @param existing_md_ids character: List of Microdata IDs already present in DDH
#' @param md_token character: Microdata API authentication token
#' @param ddh_credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

add_new_dataset <- function(md_internal_id, existing_md_ids, md_token, ddh_credentials, master, root_url) {

  # STEP 1: Get raw values from microdata API
  survey_mtdt <- get_md_metadata(id = md_internal_id, token = md_token)
  # STEP 2: format raw metadata
  temp <- map_md_to_ddh(survey_mtdt)
  # Add search_tags
  temp <- add_search_tags(metadata_list = temp, id = md_internal_id, token = md_token)

  # Add correct data classification information
  temp <- add_data_classification(metadata_list = temp,
                                  md_internal_id,
                                  master = master)
  # Add resource link
  temp <- add_link_to_resources(metadata_list = temp,
                                md_internal_id = md_internal_id,
                                master = master)

  # STEP 3: Create dataset
  # Create JSON dataset
  json_dat <- create_json_dataset(temp)
  # Push dataset to DDH
  resp_dat <- ddhconnect::create_dataset(credentials = ddh_credentials,
                                         body = json_dat,
                                         root_url = root_url)

  # STEP 4: Create resource
  # Create JSON resource
  json_res <- create_json_resource(temp)
  resp_res <- ddhconnect::create_resource(credentials = ddh_credentials,
                                          body = json_res,
                                          root_url = root_url)

  # STEP 5: Attach resource
  # Attach resource to dataset
  json_attach <- create_json_attach(resource_nid = resp_res$node_id)
  resp_attach <- attach_resource_to_dataset(credentials = ddh_credentials,
                                            dataset_nid = resp_dat$node_id,
                                            body = json_attach,
                                            root_url = root_url)

  return(resp_dat$uri)
}
