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

get_ddh_records_status <- function(mdlib_token, root_url = dkanr::get_url(),
                                   credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())) {

  # ddh
  ddh_list <- get_ddh_datasets_list(root_url = root_url, credentials = credentials)
  ddh_list$ddh_updated <- as.numeric(lubridate::ymd_hms(ddh_list$ddh_updated))
  ddh_list$ddh <- 'ddh'

  # TODO: REVERT BACK WHEN MICRODATA API IS FIXED
  # TEMPORARY FIX
  md_list <- get_md_public_datasets_list(token = Sys.getenv('mdlib_token'))
  md_list$data_classification <- 'public'
  md_list$md_internal_updated <- as.numeric(lubridate::ymd_hms(md_list$md_internal_updated))
  # md_list$md_external_changed <- as.numeric(lubridate::ymd_hms(md_list$md_external_changed))

  # # mdlib
  # md_list <- get_md_datasets_list(token = Sys.getenv('mdlib_token'))
  # md_list$md_internal <- 'md_internal'

  # # md external
  # md_list_public <- get_md_public_datasets_list(token = Sys.getenv('mdlib_token'))
  #
  # # Identidy Official / Public microdata records
  # md_list$data_classification <- 'official'
  # md_list$data_classification[md_list$md_internal_refid %in% md_list_public$md_external_refid] <- 'public'

  # Format date
  # md_list$md_internal_updated <- as.numeric(lubridate::ymd_hms(md_list$md_internal_updated))

  # Combine datasets
  full_list <- dplyr::full_join(ddh_list, md_list, by = 'md_internal_id')
  full_list$status <- NA
  full_list$status[is.na(full_list$ddh_nids)] <- 'new'
  full_list$status[!is.na(full_list$ddh_nids) & !is.na(full_list$md_internal_updated)] <- 'current'
  full_list$status[!is.na(full_list$ddh_nids) & is.na(full_list$md_internal_updated)] <- 'old'
  # TODO: Have to revert back
  # full_list <- dplyr::left_join(full_list, md_list_public, by = c('md_internal_refid' = 'md_external_refid'))

  # Identify Current / New / Old datasets based on timestamps
  full_list$time_diff <- abs(full_list$md_internal_updated - full_list$ddh_updated) - 14400
  full_list$sync_status <- NA
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff <= 3600] <- 'in sync'
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff > 3600] <- 'out of sync'
  full_list$time_diff <- NULL

  # Identify change of access status on the public microdata catalog
  full_list$sync_status[full_list$data_classification == 'public' & full_list$ddh_dataclass != "358"] <- 'out of sync'
  full_list$sync_status[full_list$data_classification == 'official' & full_list$ddh_dataclass != "359"] <- 'out of sync'

  # Identify change of versions
  full_list$sync_status[full_list$md_refids != full_list$md_internal_refid] <- 'out of sync'

  # Identify duplicates
  full_list$oldest_timestamp <- stats::ave(full_list$ddh_created, full_list$md_internal_id, FUN = min)
  full_list$duplicate_status <- ifelse(full_list$ddh_created == full_list$oldest_timestamp, "original", "duplicate")
  full_list$oldest_timestamp <- NULL

  return(full_list)
}


