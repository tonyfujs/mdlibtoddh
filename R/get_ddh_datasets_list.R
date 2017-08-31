#' get_ddh_datasets_list
#'
#' Return the full list of microdata datasets in DDH with the following information: node IDs, UUIDs, reference IDs (microdata), data classification, date of last update.
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return data.frame
#' @export
#'

get_ddh_datasets_list <- function(root_url, credentials)
{
  resp <- ddhconnect::search_catalog(
    fields = c(
      'nid',
      'field_wbddh_reference_id',
      'field_wbddh_data_class',
      'field_wbddh_modified_date',
      'field_ddh_harvest_sys_id'
    ),
    filters = c(
      'field_wbddh_data_type' = 294,
      'type' = 'dataset'
    ),
    credentials = credentials,
    root_url = root_url
  )

  ddh_nids <- purrr::map_chr(resp, 'nid')
  md_refids <- purrr::map_chr(resp, function(x) x[["field_wbddh_reference_id"]][["und"]][[1]][["value"]])
  ddh_dataclass <- purrr::map_chr(resp, function(x) x[["field_wbddh_data_class"]][["und"]][[1]][["tid"]])
  ddh_updated <- purrr::map_chr(resp, function(x) x[["field_wbddh_modified_date"]][["und"]][[1]][["value"]])
  md_internal_id <- purrr::map_chr(resp, function(x) x[["field_ddh_harvest_sys_id"]][["und"]][[1]][["value"]])

  out <- data.frame(ddh_nids, md_refids, md_internal_id, ddh_dataclass, ddh_updated, stringsAsFactors = FALSE)

  return(out)
}
