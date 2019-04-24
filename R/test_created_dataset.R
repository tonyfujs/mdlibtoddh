#' test_created_dataset
#'
#' Test the metadata values of the dataset tranferred to DDH
#'
#' @param dataset_metadata character: The dataset metadata, returned from ddhconnect::get_metadata()
#' @param metadata_list list: List of metatadata values
#' @param lovs dataframe: lookup table of data catalog machine names and tids, return object of get_lovs()
#' @param root_url character: API root URL
#' @param credentials list: list with dkanr::get_cookie() and dkanr::get_token()
#'
#' @return character
#' @export
#'

test_created_dataset <- function(dataset_metadata, metadata_list,
                                 lovs = ddhconnect::get_lovs(),
                                 root_url = dkanr::get_url(),
                                 credentials = list(cookie = dkanr::get_cookie(),
                                                    token = dkanr::get_token())) {

  safe_see_if(dataset_metadata$workbench_moderation$current$published, "1", "status")
  safe_see_if(dataset_metadata$title, metadata_list$title, "title")

  # List of non-mandatory values to check
  machine_names_value <- c(
    "body",
    "field_ddh_harvest_sys_id",
    "field_license_wbddh",
    "field_wbddh_access_authority",
    "field_wbddh_acronym",
    "field_wbddh_citation_text",
    "field_wbddh_copyright",
    "field_wbddh_data_collector",
    "field_wbddh_data_editing",
    "field_wbddh_deviations_sample",
    "field_wbddh_disclaimer",
    "field_wbddh_dstranslated_title",
    "field_wbddh_estimate_sampling",
    "field_wbddh_funding_name",
    "field_wbddh_kind_of_data",
    "field_wbddh_mode_data_collection",
    "field_wbddh_note_data_collection",
    "field_wbddh_other_acknowledgment",
    "field_wbddh_other_forms_of_data",
    "field_wbddh_other_processing",
    "field_wbddh_primary_investigator",
    "field_wbddh_publisher_name",
    "field_wbddh_questionnaires",
    "field_wbddh_response_rates",
    "field_wbddh_sampling_procedure",
    "field_wbddh_series_information",
    "field_wbddh_study_type",
    "field_wbddh_subtitle",
    "field_wbddh_supervision",
    "field_wbddh_unit_of_analysis",
    "field_wbddh_universe",
    "field_wbddh_version_description",
    "field_wbddh_version_notes",
    "field_wbddh_weighting"
  )

  sapply(machine_names_value, check_value,
         dataset_metadata = dataset_metadata,
         input_metadata = metadata_list
         )

  machine_names_tid <- c(
    "field_ddh_harvest_src",
    "field_exception_s_",
    "field_frequency",
    "field_granularity_list",
    "field_license_wbddh",
    "field_topic",
    "field_wbddh_country",
    "field_wbddh_economy_coverage",
    "field_wbddh_data_class",
    "field_wbddh_data_type",
    "field_wbddh_languages_supported",
    "field_wbddh_update_frequency"
  )

  sapply(machine_names_tid, check_lov,
         dataset_metadata = dataset_metadata,
         input_metadata = metadata_list,
         lovs = lovs
         )
}

# use unlist instead of indexing, $field_frequency doesn't have $tid but uses a tid structure
check_lov <- function(dataset_metadata, input_metadata, machine_name, lovs) {

  if (!is_blank(dataset_metadata[[machine_name]])) {
    ddh_input <- unlist(c(dataset_metadata[[machine_name]]$und[[1]]$tid,
                          dataset_metadata[[machine_name]]$und[[1]]$value))
    ddh_value <- lovs$list_value_name[lovs$tid == ddh_input & lovs$machine_name == machine_name]
  } else {
    ddh_value <- ""
  }

  if (!is_blank(input_metadata[[machine_name]])) {
    input_value <- as.character(input_metadata[[machine_name]])
  } else {
    input_value <- ""
  }

  safe_see_if(ddh_value, input_value, machine_name)
}

check_value <- function(dataset_metadata, input_metadata, machine_name) {

  if (!is_blank(dataset_metadata[[machine_name]])) {
    ddh_value <- dataset_metadata[[machine_name]]$und[[1]]$value
  } else {
    ddh_value <- ""
  }

  if (!is_blank(input_metadata[[machine_name]])) {
    input_value <- as.character(input_metadata[[machine_name]])
  } else {
    input_value <- ""
  }

  safe_see_if(ddh_value, input_value, machine_name)
}
