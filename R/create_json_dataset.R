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


  json_template$title <- safe_unbox(metadata_list$title) # title
  json_template$body$und$value <- if (length(metadata_list$body) > 0) {metadata_list$body} else {metadata_list$title} # body
  json_template$field_contact_email$und$value <- safe_assign(metadata_list$field_contact_email) # email
  json_template$field_topic$und$tid <- safe_unbox(metadata_list$field_topic) # topic
  json_template$field_wbddh_reference_id$und$value <- safe_unbox(metadata_list$field_wbddh_reference_id) # reference ID
  json_template$field_wbddh_data_class$und$tid <- safe_unbox(metadata_list$field_wbddh_data_class) # data classification
  json_template$field_wbddh_produced_by$und$value <- safe_assign(metadata_list$field_wbddh_produced_by)
  json_template$field_wbddh_data_collector$und$value <- safe_unbox(metadata_list$field_wbddh_data_collector)
  json_template$field_wbddh_data_type$und$tid <- safe_unbox(metadata_list$field_wbddh_data_type) # data type
  json_template$field_wbddh_disclaimer$und$value <- safe_unbox(metadata_list$field_wbddh_disclaimer) # disclaimer
  json_template$field_wbddh_kind_of_data$und$tid <- safe_unbox(metadata_list$field_wbddh_kind_of_data) # kind of data
  json_template$field_wbddh_languages_supported$und$tid <- safe_unbox(metadata_list$field_wbddh_languages_supported) # language
  json_template$field_wbddh_mode_data_collection$und$tid <- safe_unbox(metadata_list$field_wbddh_mode_data_collection) # mode data collection
  json_template$field_wbddh_primary_investigator$und$value <- safe_unbox(metadata_list$field_wbddh_primary_investigator) # Primary investig
  json_template$field_wbddh_study_type$und$value <- safe_unbox(metadata_list$field_wbddh_study_type) # study type
  json_template$field_wbddh_terms_of_use$und$tid <- safe_unbox(metadata_list$field_wbddh_terms_of_use)
  json_template$field_wbddh_unit_of_analysis$und$value <- safe_unbox(metadata_list$field_wbddh_unit_of_analysis)
  json_template$field_wbddh_wbddh_universe$und$value <- safe_unbox(metadata_list$field_wbddh_wbddh_universe) # universe
  json_template$field_wbddh_acronym$und$value <- safe_unbox(metadata_list$field_wbddh_acronym) # acronym
  json_template$field_wbddh_country$und$tid <- safe_unbox(metadata_list$field_wbddh_country)
  json_template$field_wbddh_search_tags$und$value <- safe_unbox(metadata_list$field_wbddh_search_tags)
  json_template$field_wbddh_copyright$und$value <- safe_unbox(metadata_list$field_wbddh_copyright)
  json_template$field_wbddh_access_authority$und$value <- safe_unbox(metadata_list$field_wbddh_access_authority)
  json_template$field_wbddh_data_editing$und$value <- safe_unbox(metadata_list$field_wbddh_data_editing)
  json_template$field_wbddh_funding_name$und$value <- safe_unbox(metadata_list$field_wbddh_funding_name)
  json_template$field_wbddh_subtitle$und$value <- safe_unbox(metadata_list$field_wbddh_subtitle)
  json_template$field_wbddh_version_notes$und$value <- safe_unbox(metadata_list$field_wbddh_version_notes)
  json_template$field_wbddh_response_rates$und$value <- safe_unbox(metadata_list$field_wbddh_response_rates)
  json_template$field_wbddh_estimate_sampling$und$value <- safe_unbox(metadata_list$field_wbddh_estimate_sampling)


  # # version date
  # json_template$field_wbddh_version_date$und$value <- safe_unbox(metadata_list$field_wbddh_version_date)

  # release date
  json_template$field_wbddh_release_date$und$value <- safe_unbox(metadata_list$field_wbddh_release_date)

  # modified date
  json_template$field_wbddh_modified_date$und$value <- safe_unbox(metadata_list$field_wbddh_modified_date)


  json_template$field_wbddh_sampling_procedure$und$value <- safe_unbox(metadata_list$field_wbddh_sampling_procedure)
  json_template$field_wbddh_deviations_sample <- safe_assign(metadata_list$field_wbddh_deviations_sample)
  json_template$field_wbddh_questionnaires$und$value <- safe_unbox(metadata_list$field_wbddh_questionnaires)
  json_template$field_wbddh_citation_text$und$value <- safe_unbox(metadata_list$field_wbddh_citation_text)
  json_template$field_ddh_harvest_src$und$tid <- safe_unbox(metadata_list$field_ddh_harvest_src)
  json_template$field_ddh_harvest_sys_id$und$value <- safe_unbox(metadata_list$field_ddh_harvest_sys_id)
  json_template$field_exception_s_$und$tid <- safe_unbox(metadata_list$field_exception_s_)
  json_template$field_wbddh_version_description$und$value <- safe_assign(metadata_list$field_wbddh_version_description) # version

  # To DOUBLE CHECK
  json_template$field_wbddh_end_date <- safe_assign(metadata_list$field_wbddh_end_date) # end date
  json_template$field_wbddh_other_processing$und$value <- safe_assign(metadata_list$field_wbddh_other_processing) # other processing
  json_template$field_wbddh_publisher_name <- safe_assign(metadata_list$field_wbddh_publisher_name) # publisher name
  json_template$field_wbddh_weighting <- safe_assign(metadata_list$field_wbddh_weighting) # weighting
  # # TO CHECK
  # json_template$field_wbddh_series_information <- metadata_list$field_wbddh_series_information
  # json_template$field_wbddh_supervision <- metadata_list$field_wbddh_supervision
  # json_template$field_exception_s_ <- metadata_list$field_exception_s_
  # json_template$field_wbddh_terms_of_use$und$tid <- metadata_list$field_wbddh_terms_of_use
  # json_template$field_wbddh_disclosure_risk <- metadata_list$field_wbddh_disclosure_risk
  # json_template$field_wbddh_economy_coverage <- metadata_list$field_wbddh_economy_coverage
  # json_template$field_wbddh_update_frequency$und$value <- metadata_list$field_wbddh_update_frequency
  # json_template$field_wbddh_ds_embargo_date$und$value <- metadata_list$field_wbddh_ds_embargo_date
  # json_template$field_wbddh_ds_source$und$value <- metadata_list$field_wbddh_ds_source
  # json_template$field_wbddh_dsttl_upi$und$value <- metadata_list$field_wbddh_dsttl_upi
  # json_template$field_ddh_external_contact_email$und$value <- metadata_list$field_ddh_external_contact_email
  # json_template$field_wbddh_embargo$und$value <- metadata_list$field_wbddh_embargo
  # json_template$field_wbddh_organization$und$value <- metadata_list$field_wbddh_organization
  # json_template$field_wbddh_statistical_concept$und$value <- metadata_list$field_wbddh_statistical_concept
  # json_template$field_wbddh_aggregation_method$und$value <- metadata_list$field_wbddh_aggregation_method

  # remove empty elements
  to_keep <- names(metadata_list[!purrr::map_int(metadata_list, length) == 0])
  to_keep <- unique(c(to_keep, "body"))
  json_template <- json_template[names(json_template) %in% to_keep]

  # Add required dataset elements
  json_template$type <- jsonlite::unbox("dataset")

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
