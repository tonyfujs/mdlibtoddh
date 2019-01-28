#' get_ddh_datasets_list
#'
#' Return the full list of microdata datasets in DDH with the following information: node IDs, UUIDs, reference IDs (microdata), data classification, date of last update.
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return data.frame
#' @export
#'

get_ddh_datasets_list <- function(root_url = dkanr::get_url(),
                                  credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()))
{
  resp <- ddhconnect::search_catalog(
    fields = c(
      'nid',
      'field_wbddh_reference_id',
      'field_wbddh_data_class',
      'field_wbddh_modified_date',
      'field_ddh_harvest_sys_id',
      'created'
    ),
    filters = c(
      'field_wbddh_data_type' = 294,
      'type' = 'dataset'
    ),
    credentials = credentials,
    root_url = root_url
  )

  ddh_nids <- as.character(purrr::map(resp, 'nid'))
  ddh_nids <- sapply(ddh_nids, function(x) ifelse(x == "NULL", NA, x))
  
  md_refids <- as.character(purrr::map(resp, function(x) x[["field_wbddh_reference_id"]][["und"]][[1]][["value"]]))
  md_refids <- sapply(md_refids, function(x) ifelse(x == "NULL", NA, x))
  
  ddh_dataclass <- as.character(purrr::map(resp, function(x) x[["field_wbddh_data_class"]][["und"]][[1]][["tid"]]))
  ddh_dataclass <- sapply(ddh_dataclass, function(x) ifelse(x == "NULL", NA, x))
  
  ddh_created <- as.character(purrr::map(resp, "created"))
  ddh_created <- sapply(ddh_created, function(x) ifelse(x == "NULL", NA, x))
  
  ddh_updated <- as.character(purrr::map(resp, function(x) x[["field_wbddh_modified_date"]][["und"]][[1]][["value"]]))
  ddh_updated <- sapply(ddh_updated, function(x) ifelse(x == "NULL", NA, x))
  
  md_internal_id <- as.character(purrr::map(resp, function(x) x[["field_ddh_harvest_sys_id"]][["und"]][[1]][["value"]]))
  md_internal_id <- sapply(md_internal_id, function(x) ifelse(x == "NULL", NA, x))

  out <- data.frame(ddh_nids, md_refids, md_internal_id, ddh_dataclass, ddh_updated, ddh_created, stringsAsFactors = FALSE)

  return(out)
}
