#' get_ddh_datasets_list
#'
#' Return the full list of microdata datasets in DDH with the following information: node IDs, UUIDs, reference IDs (microdata), data classification, date of last update.
#' @param root_url character: API root URL
#'
#' @return data.frame
#' @export
#'

get_ddh_datasets_list <- function (root_url)
{
  limit <- 500
  dtype <- 294

  url
  # Get count of datasets
  count_url <- paste0(root_url,
                      "/search-service/search_api/datasets?limit=1&fields=[nid,uuid]&filter[status]=1&filter[field_wbddh_data_type]=",
                      dtype)
  count <- httr::GET(url = count_url, httr::add_headers(.headers = c(charset = "utf-8")),
                     httr::accept_json())
  httr::warn_for_status(count)
  count <- httr::content(count)
  count <- as.numeric(count$count)
  iterations <- ceiling(count/limit)

  # Retrieve data
  nids <- vector(mode = "list", length = iterations)
  uuids <- vector(mode = "list", length = iterations)
  refids <- vector(mode = "list", length = iterations)
  dataclass <- vector(mode = "list", length = iterations)
  updated <- vector(mode = "list", length = iterations)
  for (i in 1:iterations) {
    temp_offset <- (i - 1) * 500
    temp_url <- paste0(root_url,
                       "/search-service/search_api/datasets?limit=500&fields=[nid,uuid,field_wbddh_reference_id,field_wbddh_data_class,field_wbddh_modified_date,]&filter[status]=1&filter[field_wbddh_data_type]=",
                       dtype, "&offset=", temp_offset)
    temp_resp <- httr::GET(url = temp_url, httr::add_headers(.headers = c(charset = "utf-8")),
                           httr::accept_json())
    httr::warn_for_status(temp_resp)
    temp_resp <- httr::content(temp_resp)
    temp_resp <- temp_resp$result
    temp_nids <- names(temp_resp)
    temp_uuids <- purrr::map_chr(temp_resp, 'uuid')
    temp_refids <- purrr::map_chr(temp_resp, function(x) x[['field_wbddh_reference_id']][[1]][[1]][[1]])
    temp_dataclass <- purrr::map_chr(temp_resp, function(x) x[['field_wbddh_data_class']][[1]][[1]][[1]])
    temp_updated <- purrr::map_chr(temp_resp, function(x) {
      date <- x[['field_wbddh_modified_date']][['und']][[1]][['value']]
      if(is.null(date)) {return("1970-01-01 00:01:01")} else {return(date)}
      })

    nids[[i]] <- temp_nids
    uuids[[i]] <- temp_uuids
    refids[[i]] <- temp_refids
    dataclass[[i]] <- temp_dataclass
    updated[[i]] <- temp_updated
  }

  nids <- unlist(nids)
  uuids <- unlist(uuids)
  uuids <- as.numeric(uuids)
  refids <- unlist(refids)
  dataclass <- unlist(dataclass)
  updated <- unlist(updated)
  out <- data.frame(nids, uuids, refids, dataclass, updated, stringsAsFactors = FALSE)

  return(out)
}
