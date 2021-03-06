#' add_new_dataset()
#'
#' Add a full microdata record to DDH (metadata and resources)
#'
#' @param md_internal_id character: Microdata internal ID of the dataset to be
#' added
#' @param md_token character: Microdata API authentication token
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table, output of
#' mdlibtoddh::get_ddh_records_status()
#' @param ddh_fields dataframe: table of all the data catalog fields by
#' node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: Root URL to use for the API
#' (Staging or Production)
#' @param iso_3_df dataframe: Table of iso_3 codes and corresponding country
#' names
#'
#' @return character
#' @export
#'

add_new_dataset <- function(md_internal_id, md_token, master,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            iso_3_df = ddhconnect::get_iso3(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {

  # STEP 1: Get raw values from microdata API
  survey_mtdt <- get_md_metadata(id = md_internal_id, token = md_token)

  # STEP 2: format raw metadata
  # Format list
  temp <- map_md_to_ddh(survey_mtdt, iso3_lkup = iso_3_df)

  # Add correct data classification information
  temp <- add_data_classification(metadata_list = temp,
                                  md_internal_id = md_internal_id,
                                  master = master)

  # Get variables from MD API
  dico <- get_md_dictionary(id = md_internal_id, token = "")

  if(is_blank(temp[['field_wbddh_search_tags']])){
    ifelse(is_blank(dico), "", (temp[['field_wbddh_search_tags']] =  unique(dico)))
  }
  else{
    ifelse(is_blank(dico), "", (temp[['field_wbddh_search_tags']] =  unique(paste(temp[['field_wbddh_search_tags']], dico, sep = ';'))))
  }

  # Check if Microdata API returned enough information
  if(is.null(temp$title) & is.null(temp$body)){
    stop("Microdata API didn't return Title or Description")
  }

  temp <- add_constant_metadata_dataset(temp)

  # Add resource link
  temp <- add_link_to_resources(metadata_list = temp,
                                md_internal_id = md_internal_id,
                                master = master)

  # STEP 3: Create dataset
  # Create JSON dataset
  temp_dataset <- filter_dataset_fields(temp, ddh_fields)


  #Filter out blank values
  non_blank     <- sapply(temp_dataset, function(x){!is_blank(x)})
  temp_dataset  <- temp_dataset[non_blank]

  json_dat <- ddhconnect::create_json_dataset(values = temp_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)

  # Push dataset to DDH
  resp_dat <- ddhconnect::create_dataset(body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)


  tryCatch({

    # STEP 4: Create resource
    # Create JSON resource
    temp          <- add_constant_metadata_resource(temp)
    temp_resource <- filter_resource_fields(temp, ddh_fields)

    json_res      <- ddhconnect::create_json_resource(values = temp_resource,
                                                 publication_status = "published",
                                                 dataset_nid = resp_dat$nid,
                                                 ddh_fields = ddh_fields,
                                                 lovs = lovs,
                                                 root_url = root_url)
    resp_res <- ddhconnect::create_resource(credentials = credentials,
                                            body = json_res,
                                            root_url = root_url)

    metadata_dataset <- ddhconnect::get_metadata(nid = resp_dat$nid,
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
