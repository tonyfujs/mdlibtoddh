#' test_created_dataset
#'
#' Test the metadata values of the dataset tranferred to DDH
#'
#' @param nid character: The dataset node id
#' @param metadata_list list: List of metatadata values
#' @param credentials list: object returned by the get_credentials() function
#' @param root_url character: API root URL
#'
#' @return character
#' @export
#'

test_created_dataset <- function(nid, metadata_list, credentials, root_url = production_root_url) {

  node_metadata = ddhconnect::get_metadata(nid, credentials, root_url)

  safe_assert(node_metadata$title, metadata_list$title) # title
  safe_assert(node_metadata$body$und$value, metadata_list$body) # body
  safe_assert(node_metadata$field_contact_email$und$value, metadata_list$field_contact_email) # email
  safe_assert(node_metadata$field_topic$und$tid, metadata_list$field_topic) # topic
  safe_assert(node_metadata$field_wbddh_reference_id$und$value, metadata_list$field_wbddh_reference_id) # reference ID
  safe_assert(node_metadata$field_wbddh_data_class$und$tid, metadata_list$field_wbddh_data_class) # data classification
  safe_assert(node_metadata$field_wbddh_produced_by$und$value, metadata_list$field_wbddh_produced_by)
  safe_assert(node_metadata$field_wbddh_data_collector$und$value, metadata_list$field_wbddh_data_collector)
  safe_assert(node_metadata$field_wbddh_data_type$und$tid, metadata_list$field_wbddh_data_type) # data type
  safe_assert(node_metadata$field_wbddh_disclaimer$und$value, metadata_list$field_wbddh_disclaimer) # disclaimer
  safe_assert(node_metadata$field_wbddh_kind_of_data$und$tid, metadata_list$field_wbddh_kind_of_data) # kind of data
  safe_assert(node_metadata$field_wbddh_languages_supported$und$tid, metadata_list$field_wbddh_languages_supported) # language
  safe_assert(node_metadata$field_wbddh_mode_data_collection$und$tid, metadata_list$field_wbddh_mode_data_collection) # mode data collection
  safe_assert(node_metadata$field_wbddh_primary_investigator$und$value, metadata_list$field_wbddh_primary_investigator) # Primary investig
  safe_assert(node_metadata$field_wbddh_study_type$und$value, metadata_list$field_wbddh_study_type) # study type
  # safe_assert(node_metadata$field_wbddh_terms_of_use$und$tid, metadata_list$field_wbddh_terms_of_use)
  safe_assert(node_metadata$field_wbddh_unit_of_analysis$und$value, metadata_list$field_wbddh_unit_of_analysis)
  safe_assert(node_metadata$field_wbddh_universe$und$value, metadata_list$field_wbddh_universe) # universe
  safe_assert(node_metadata$field_wbddh_acronym$und$value, metadata_list$field_wbddh_acronym) # acronym
  # to check for datasets with multiple countries
  safe_assert(node_metadata$field_wbddh_country$und$tid, unlist(stringr::str_split(metadata_list$field_wbddh_country, pattern = ';')))
  safe_assert(node_metadata$field_wbddh_search_tags$und$value, metadata_list$field_wbddh_search_tags)
  safe_assert(node_metadata$field_wbddh_copyright$und$value, metadata_list$field_wbddh_copyright)
  safe_assert(node_metadata$field_wbddh_access_authority$und$value, metadata_list$field_wbddh_access_authority)
  safe_assert(node_metadata$field_wbddh_data_editing$und$value, metadata_list$field_wbddh_data_editing)
  safe_assert(node_metadata$field_wbddh_funding_name$und$value, metadata_list$field_wbddh_funding_name)
  safe_assert(node_metadata$field_wbddh_version_notes$und$value, metadata_list$field_wbddh_version_notes)
  safe_assert(node_metadata$field_wbddh_response_rates$und$value, metadata_list$field_wbddh_response_rates)
  safe_assert(node_metadata$field_wbddh_estimate_sampling$und$value, metadata_list$field_wbddh_estimate_sampling)


  # # version date
  safe_assert(node_metadata$field_wbddh_version_date$und$value, metadata_list$field_wbddh_version_date)

  # release date
  safe_assert(node_metadata$field_wbddh_release_date$und$value, metadata_list$field_wbddh_release_date)

  # modified date
  safe_assert(node_metadata$field_wbddh_modified_date$und$value, metadata_list$field_wbddh_modified_date)


  safe_assert(node_metadata$field_wbddh_sampling_procedure$und$value, metadata_list$field_wbddh_sampling_procedure)
  safe_assert(node_metadata$field_wbddh_deviations_sample$und$value, metadata_list$field_wbddh_deviations_sample)
  safe_assert(node_metadata$field_wbddh_questionnaires$und$value, metadata_list$field_wbddh_questionnaires)
  safe_assert(node_metadata$field_wbddh_citation_text$und$value, metadata_list$field_wbddh_citation_text)
  safe_assert(node_metadata$field_ddh_harvest_src$und$tid, metadata_list$field_ddh_harvest_src)
  safe_assert(node_metadata$field_ddh_harvest_sys_id$und$value, metadata_list$field_ddh_harvest_sys_id)
  safe_assert(node_metadata$field_exception_s_$und$tid, metadata_list$field_exception_s_)
  safe_assert(node_metadata$field_wbddh_version_description$und$value, metadata_list$field_wbddh_version_description) # version

  safe_assert(node_metadata$field_wbddh_end_date$und$value, metadata_list$field_wbddh_end_date) # end date
  safe_assert(node_metadata$field_wbddh_other_processing$und$value, metadata_list$field_wbddh_other_processing) # other processing
  safe_assert(node_metadata$field_wbddh_publisher_name$und$value, metadata_list$field_wbddh_publisher_name) # publisher name
  safe_assert(node_metadata$field_wbddh_weighting$und$value, metadata_list$field_wbddh_weighting) # weighting
  safe_assert(node_metadata$field_wbddh_series_information$und$value, metadata_list$field_wbddh_series_information)
  # safe_assert(node_metadata$field_wbddh_supervision$und$value, metadata_list$field_wbddh_supervision)
  safe_assert(node_metadata$field_exception_s_$und$tid, metadata_list$field_exception_s_)
  safe_assert(node_metadata$field_wbddh_disclosure_risk$und$value, metadata_list$field_wbddh_disclosure_risk)
  safe_assert(node_metadata$field_wbddh_economy_coverage$und$value, metadata_list$field_wbddh_economy_coverage)
  safe_assert(node_metadata$field_wbddh_update_frequency$und$value, metadata_list$field_wbddh_update_frequency)
  safe_assert(node_metadata$field_wbddh_ds_embargo_date$und$value, metadata_list$field_wbddh_ds_embargo_date)
  safe_assert(node_metadata$field_wbddh_ds_source$und$value, metadata_list$field_wbddh_ds_source)
  safe_assert(node_metadata$field_wbddh_dsttl_upi$und$value, metadata_list$field_wbddh_dsttl_upi)
  safe_assert(node_metadata$field_ddh_external_contact_email$und$value, metadata_list$field_ddh_external_contact_email)
  safe_assert(node_metadata$field_wbddh_embargo$und$value, metadata_list$field_wbddh_embargo)
  safe_assert(node_metadata$field_wbddh_organization$und$value, metadata_list$field_wbddh_organization)
  safe_assert(node_metadata$field_wbddh_statistical_concept$und$value, metadata_list$field_wbddh_statistical_concept)
  safe_assert(node_metadata$field_wbddh_aggregation_method$und$value, metadata_list$field_wbddh_aggregation_method)

}