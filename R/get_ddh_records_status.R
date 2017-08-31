#' get_ddh_records_status
#'
#' Compare DDH and Microdata library records
#'
#' @param mdlib_token character: Microdatalib API authentication token
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return data frame
#' @export
#'

get_ddh_records_status <- function(mdlib_token, root_url = ddhconnect:::production_root_url, credentials) {

  # ddh
  ddh_list <- get_ddh_datasets_list(root_url = root_url, credentials = credentials)
  ddh_list$ddh_updated <- as.numeric(lubridate::ymd_hms(ddh_list$ddh_updated))
  ddh_list$ddh <- 'ddh'

  # mdlib
  md_list <- get_md_datasets_list(token = Sys.getenv('mdlib_token'))
  md_list$md_internal <- 'md_internal'

  # md external
  md_list_public <- get_md_public_datasets_list(token = Sys.getenv('mdlib_token'))

  # Identidy Official / Public microdata records
  md_list$data_classification <- 'official'
  md_list$data_classification[md_list$md_internal_refid %in% md_list_public$md_external_refid] <- 'public'

  # Format

  # Combine datasets
  full_list <- dplyr::full_join(ddh_list, md_list, by = 'md_internal_id')
  full_list$status <- NA
  full_list$status[is.na(full_list$ddh_nids)] <- 'new'
  full_list$status[!is.na(full_list$ddh_nids) & !is.na(full_list$md_internal_id)] <- 'current'
  full_list$status[!is.na(full_list$ddh_nids) & is.na(full_list$md_internal_id)] <- 'old'

  # Compare last updated dates
  full_list$time_diff <- abs(full_list$md_internal_updated - full_list$ddh_updated)
  full_list$sync_status <- NA
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff <= 86400] <- 'in sync'
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff > 86400] <- 'out of sync'
  full_list$time_diff <- NULL

  return(full_list)
}


