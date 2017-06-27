#' create_json_dataset
#'
#' Create a json according to predefined template
#'
#' @param metadata_list list: List of metatadata values
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_dataset <- function(metadata_list, json_template = mdlibtoddh::json_template_dataset) {

  json_template$body$und$value <- metadata_list$body # body
  json_template$field_contact_email$und$value <- metadata_list$field_contact_email # email
  json_template$field_wbddh_acronym$und$value <- metadata_list$field_wbddh_acronym # acronym
  json_template$field_wbddh_data_type$und$tid <- metadata_list$field_wbddh_data_type # data type
  json_template$field_wbddh_disclaimer$und$value <- metadata_list$field_wbddh_disclaimer # disclaimer
  json_template$field_wbddh_end_date <- metadata_list$field_wbddh_end_date # end date
  # toJSON(json_template['field_wbddh_end_date'])
  json_template$field_wbddh_kind_of_data$und$tid <- metadata_list$field_wbddh_kind_of_data # kind of data
  # toJSON(json_template['field_wbddh_kind_of_data'])
  json_template$field_wbddh_languages_supported$und$tid <- metadata_list$field_wbddh_languages_supported # language
  json_template$field_wbddh_mode_data_collection$und$tid <- metadata_list$field_wbddh_mode_data_collection # mode data collection
  json_template$field_wbddh_other_processing$und$value <- metadata_list$field_wbddh_other_processing # other processing
  json_template$field_wbddh_primary_investigator$und$value <- metadata_list$field_wbddh_primary_investigator # Primary investigator
  json_template$field_wbddh_publisher_name <- metadata_list$field_wbddh_publisher_name # publisher name
  json_template$field_wbddh_study_type$und$value <- metadata_list$field_wbddh_study_type # study type
  json_template$field_wbddh_wbddh_universe <- metadata_list$field_wbddh_wbddh_universe # universe
  json_template$field_wbddh_version_description <- metadata_list$field_wbddh_version_description # version
  json_template$field_wbddh_weighting <- metadata_list$field_wbddh_weighting # weighting
  json_template$title <- metadata_list$title # title

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
