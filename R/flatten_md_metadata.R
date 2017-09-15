#' flatten_md_metadata
#'
#' Flatten list
#'
#' @param metadata_list list: Output of mdlibtoddh::extract_md_metadata
#' @param multiple_fields character: JSON array fields in Microdata
#' @param email_fields character: email fiels
#' @param name_fields character: Name fields
#'
#' @return list
#' @export
#'

flatten_md_metadata <- function(metadata_list,
                                multiple_fields = c("field_wbddh_primary_investigator",
                                                    "field_wbddh_authority_name",
                                                    "field_wbddh_publisher_name",
                                                    "field_wbddh_other_acknowledgment"),
                                email_fields = c("field_contact_email"),
                                name_fields = c('field_wbddh_country',
                                                'field_wbddh_region',
                                                'field_wbddh_data_collector')) {

  # Handle shallow indexes
  shallow <- purrr::map_int(metadata_list, purrr::vec_depth) == 2
  shallow <- shallow[shallow == TRUE]
  shallow <- names(shallow)[!names(shallow) %in% c(multiple_fields, email_fields, name_fields)]
  check_shallow(metadata_list[shallow])
  metadata_list[shallow] <- purrr::flatten(metadata_list[shallow])

  # Handle variables pulling for multiples JSON keys
  multiple_fields <- multiple_fields[multiple_fields %in% names(metadata_list)]

  metadata_list[multiple_fields] <- purrr::map(metadata_list[multiple_fields], function(x) {
    out <- paste(purrr::map(x, extract_from_list, keys = c('name', 'affiliation')), collapse = '; ')
    out <- stringr::str_replace_all(out, pattern = '^; ?|;$|; $', replacement = '')
  })


  # Handle variables pulling for multiples JSON keys
  email_fields <- email_fields[email_fields %in% names(metadata_list)]

  metadata_list[email_fields] <- purrr::map(metadata_list[email_fields], function(x) {
    out <- paste(purrr::map(x, extract_from_list, keys = c('email')), collapse = '; ')
    out <- stringr::str_replace_all(out, pattern = '^; ?|;$|; $', replacement = '')
    out <- stringr::str_replace_all(out, pattern = ' ', replacement = '')
    out <- stringr::str_trim(out, side = 'both') # Add str_trim to deal with non-standard space characters
    out <- stringr::str_replace_all(out, pattern = ';', replacement = '; ')
  })

  # Handle extraction of value that follow "name" keys
  name_fields <- name_fields[name_fields %in% names(metadata_list)]

  metadata_list[name_fields] <- purrr::map(metadata_list[name_fields], function(x) x[[1]]$name)

  # Handle extraction of date values

  field <- "field_wbddh_start_date"
  if (!is.null(metadata_list[[field]])) {
    keep <- purrr::map_lgl(metadata_list[[field]], function(x) x$event %in% c("start", "single"))
    metadata_list[[field]]  <- metadata_list[[field]][keep]
    metadata_list[[field]] <- metadata_list[[field]][[1]]$date
  }

  field <- "field_wbddh_end_date"
  if (!is.null(metadata_list[[field]])) {
    keep <- purrr::map_lgl(metadata_list[[field]], function(x) x$event %in% c("end", "single"))
    metadata_list[[field]]  <- metadata_list[[field]][keep]
    metadata_list[[field]] <- metadata_list[[field]][[1]]$date
  }

  # Handle field_wbddh_access_authority
  metadata_list$field_wbddh_access_authority <- paste(unlist(metadata_list$field_wbddh_access_authority), collapse = ', ')

  # Handle field_wbddh_funding_name
  metadata_list$field_wbddh_funding_name <- purrr::map_chr(metadata_list$field_wbddh_funding_name, 'agency')
  metadata_list$field_wbddh_funding_name <- paste(metadata_list$field_wbddh_funding_name, collapse = '; ')

  # CHECK: List has been correctly flattened
  assertthat::assert_that(all(purrr::map_int(metadata_list, purrr::vec_depth) == 1),
                            msg = "Some elements of the list have not been flattened correctly")

  return(metadata_list)

}


check_shallow <- function(x) {
  assertthat::assert_that(all(purrr::map_int(x, length) == 1),
                          msg = 'Non expected input to flatten_md_metadata()')
}
