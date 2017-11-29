#' check_duplicates
#'
#' @param mdlib_token character: Microdatalib API authentication token
#' @param root_url character: API root URL
#' @param credentials list: named object with cooking and token
#'
#' @return list
#' @export
#'

check_duplicates <- function(mdlib_token, root_url = dkanr::get_url(), credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())){
  # get all microdata dataset
  all_microdata = get_ddh_records_status(mdlib_token, root_url, credentials)

  # return duplicate rows
  duplicate_datasets = all_microdata[duplicated(all_microdata$md_internal_id) | duplicated(all_microdata$md_internal_id, fromLast=TRUE),]
  return(duplicate_datasets[c("ddh_nids", "md_internal_id")])
}
