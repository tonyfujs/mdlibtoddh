#' flatten_md_metadata
#'
#' Flatten list
#'
#' @param metadata_in list: Output of mdlibtoddh::extract_md_metadata
#'
#' @return list
#' @export
#'

flatten_md_metadata <- function(metadata_in,
                                multiple_fields = c("field_wbddh_primary_investigator",
                                                    "field_wbddh_authority_name",
                                                    "field_wbddh_publisher_name"),
                                name_fields = c('field_wbddh_country',
                                                'field_wbddh_region')) {

  # Handle shallow indexes
  shallow <- purrr::map_int(metadata_in, purrr::depth) == 2
  metadata_in[shallow] <- purrr::flatten(metadata_in[shallow])

  # Handle variables pulling for multiples JSON keys
  multiple_fields <- multiple_fields[multiple_fields %in% names(metadata_in)]

  metadata_in[multiple_fields] <- purrr::map(metadata_in[multiple_fields], function(x) {
    paste(purrr::map(x, extract_from_list, keys = c('name', 'affiliation')), collapse = '; ')
  })

  # Handle extraction of value that follow "name" keys
  name_fields <- name_fields[name_fields %in% names(metadata_in)]

  metadata_in[name_fields] <- purrr::map(metadata_in[name_fields], function(x) x[[1]]$name)

  # Handle extraction of date values

  field <- "field_wbddh_start_date"
  if (!is.null(metadata_in[[field]])) {
    keep <- purrr::map_lgl(metadata_in[[field]], function(x) x$event %in% c("start", "single"))
    metadata_in[[field]]  <- metadata_in[[field]][keep]
    metadata_in[[field]] <- metadata_in[[field]][[1]]$date
  }

  field <- "field_wbddh_end_date"
  if (!is.null(metadata_in[[field]])) {
    keep <- purrr::map_lgl(metadata_in[[field]], function(x) x$event %in% c("end", "single"))
    metadata_in[[field]]  <- metadata_in[[field]][keep]
    metadata_in[[field]] <- metadata_in[[field]][[1]]$date
  }

  return(metadata_in)

}
