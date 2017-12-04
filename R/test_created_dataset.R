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

test_created_dataset <- function(nid, metadata_list, credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()),
                                 root_url = dkanr::get_url()) {

  node_metadata = ddhconnect::get_metadata(nid, credentials, root_url)

  safe_see_if(node_metadata$title, metadata_list$body, "title") # title
  safe_see_if(node_metadata$body$und[[1]]$value, metadata_list$body, "body") # body
  safe_see_if(node_metadata$field_contact_email$und[[1]]$value, metadata_list$field_contact_email, "field_contact_email") # email
  safe_see_if(node_metadata$field_topic$und[[1]]$tid, metadata_list$field_topic, "field_topic") # topic
  safe_see_if(node_metadata$field_wbddh_reference_id$und[[1]]$value, metadata_list$field_wbddh_reference_id, "field_wbddh_reference_id") # reference ID
  safe_see_if(node_metadata$field_wbddh_data_class$und[[1]]$tid, metadata_list$field_wbddh_data_class, "field_wbddh_data_class") # data classification
  safe_see_if(node_metadata$field_wbddh_produced_by$und[[1]]$value, metadata_list$field_wbddh_produced_by, "field_wbddh_produced_by")
  safe_see_if(node_metadata$field_wbddh_data_collector$und[[1]]$value, metadata_list$field_wbddh_data_collector, "field_wbddh_data_collector")
  safe_see_if(node_metadata$field_wbddh_data_type$und[[1]]$tid, metadata_list$field_wbddh_data_type, "field_wbddh_data_type") # data type
  safe_see_if(node_metadata$field_wbddh_disclaimer$und[[1]]$value, metadata_list$field_wbddh_disclaimer, "field_wbddh_disclaimer") # disclaimer
  safe_see_if(node_metadata$field_wbddh_kind_of_data$und[[1]]$tid, metadata_list$field_wbddh_kind_of_data, "field_wbddh_kind_of_data") # kind of data
  safe_see_if(node_metadata$field_wbddh_languages_supported$und[[1]]$tid, metadata_list$field_wbddh_languages_supported, "field_wbddh_languages_supported") # language
  safe_see_if(node_metadata$field_wbddh_mode_data_collection$und[[1]]$tid, metadata_list$field_wbddh_mode_data_collection, "field_wbddh_mode_data_collection") # mode data collection
  safe_see_if(node_metadata$field_wbddh_primary_investigator$und[[1]]$value, metadata_list$field_wbddh_primary_investigator, "field_wbddh_primary_investigator") # Primary investig
  safe_see_if(node_metadata$field_wbddh_study_type$und[[1]]$value, metadata_list$field_wbddh_study_type, "field_wbddh_study_type") # study type
  # safe_see_if(node_metadata$field_wbddh_terms_of_use$und[[1]]$tid, metadata_list$field_wbddh_terms_of_use)
  safe_see_if(node_metadata$field_wbddh_unit_of_analysis$und[[1]]$value, metadata_list$field_wbddh_unit_of_analysis, "field_wbddh_unit_of_analysis")
  safe_see_if(node_metadata$field_wbddh_universe$und[[1]]$value, metadata_list$field_wbddh_universe, "field_wbddh_universe") # universe
  safe_see_if(node_metadata$field_wbddh_acronym$und[[1]]$value, metadata_list$field_wbddh_acronym, "field_wbddh_acronym") # acronym
  # to check for datasets with multiple countries
  safe_see_if(node_metadata$field_wbddh_country$und[[1]]$tid, unlist(stringr::str_split(metadata_list$field_wbddh_country, pattern = ';')), "field_wbddh_country")
  safe_see_if(node_metadata$field_wbddh_search_tags$und[[1]]$value, metadata_list$field_wbddh_search_tags, "field_wbddh_search_tags")
  safe_see_if(node_metadata$field_wbddh_copyright$und[[1]]$value, metadata_list$field_wbddh_copyright, "field_wbddh_copyright")
  safe_see_if(node_metadata$field_wbddh_access_authority$und[[1]]$value, metadata_list$field_wbddh_access_authority, "field_wbddh_access_authority")
  safe_see_if(node_metadata$field_wbddh_data_editing$und[[1]]$value, metadata_list$field_wbddh_data_editing, "field_wbddh_data_editing")
  safe_see_if(node_metadata$field_wbddh_funding_name$und[[1]]$value, metadata_list$field_wbddh_funding_name, "field_wbddh_funding_name")
  safe_see_if(node_metadata$field_wbddh_version_notes$und[[1]]$value, metadata_list$field_wbddh_version_notes, "field_wbddh_version_notes")
  safe_see_if(node_metadata$field_wbddh_response_rates$und[[1]]$value, metadata_list$field_wbddh_response_rates, "field_wbddh_response_rates")
  safe_see_if(node_metadata$field_wbddh_estimate_sampling$und[[1]]$value, metadata_list$field_wbddh_estimate_sampling, "field_wbddh_estimate_sampling")


  # # version date
  safe_see_if(node_metadata$field_wbddh_version_date$und[[1]]$value, metadata_list$field_wbddh_version_date, "field_wbddh_version_date")

  # release date
  safe_see_if(node_metadata$field_wbddh_release_date$und[[1]]$value, metadata_list$field_wbddh_release_date, "field_wbddh_release_date")

  # modified date
  safe_see_if(node_metadata$field_wbddh_modified_date$und[[1]]$value, metadata_list$field_wbddh_modified_date, "field_wbddh_modified_date")


  safe_see_if(node_metadata$field_wbddh_sampling_procedure$und[[1]]$value, metadata_list$field_wbddh_sampling_procedure, "field_wbddh_sampling_procedure")
  safe_see_if(node_metadata$field_wbddh_deviations_sample$und[[1]]$value, metadata_list$field_wbddh_deviations_sample, "field_wbddh_deviations_sample")
  safe_see_if(node_metadata$field_wbddh_questionnaires$und[[1]]$value, metadata_list$field_wbddh_questionnaires, "field_wbddh_questionnaires")
  safe_see_if(node_metadata$field_wbddh_citation_text$und[[1]]$value, metadata_list$field_wbddh_citation_text, "field_wbddh_citation_text")
  safe_see_if(node_metadata$field_ddh_harvest_src$und[[1]]$tid, metadata_list$field_ddh_harvest_src, "field_ddh_harvest_src")
  safe_see_if(node_metadata$field_ddh_harvest_sys_id$und[[1]]$value, metadata_list$field_ddh_harvest_sys_id, "field_ddh_harvest_sys_id")
  safe_see_if(node_metadata$field_exception_s_$und[[1]]$tid, metadata_list$field_exception_s_, "field_exception_s_")
  safe_see_if(node_metadata$field_wbddh_version_description$und[[1]]$value, metadata_list$field_wbddh_version_description, "field_wbddh_version_description") # version

  safe_see_if(node_metadata$field_wbddh_end_date$und[[1]]$value, metadata_list$field_wbddh_end_date, "field_wbddh_end_date") # end date
  safe_see_if(node_metadata$field_wbddh_other_processing$und[[1]]$value, metadata_list$field_wbddh_other_processing, "field_wbddh_other_processing") # other processing
  safe_see_if(node_metadata$field_wbddh_publisher_name$und[[1]]$value, metadata_list$field_wbddh_publisher_name, "field_wbddh_publisher_name") # publisher name
  safe_see_if(node_metadata$field_wbddh_weighting$und[[1]]$value, metadata_list$field_wbddh_weighting, "field_wbddh_weighting") # weighting
  safe_see_if(node_metadata$field_wbddh_series_information$und[[1]]$value, metadata_list$field_wbddh_series_information, "field_wbddh_series_information")
  # safe_see_if(node_metadata$field_wbddh_supervision$und[[1]]$value, metadata_list$field_wbddh_supervision)
  safe_see_if(node_metadata$field_exception_s_$und[[1]]$tid, metadata_list$field_exception_s_, "field_exception_s_")
  safe_see_if(node_metadata$field_wbddh_disclosure_risk$und[[1]]$value, metadata_list$field_wbddh_disclosure_risk, "field_wbddh_disclosure_risk")
  safe_see_if(node_metadata$field_wbddh_economy_coverage$und[[1]]$value, metadata_list$field_wbddh_economy_coverage, "field_wbddh_economy_coverage")
  safe_see_if(node_metadata$field_wbddh_update_frequency$und[[1]]$value, metadata_list$field_wbddh_update_frequency, "field_wbddh_update_frequency")
  safe_see_if(node_metadata$field_wbddh_ds_embargo_date$und[[1]]$value, metadata_list$field_wbddh_ds_embargo_date, "field_wbddh_ds_embargo_date")
  safe_see_if(node_metadata$field_wbddh_ds_source$und[[1]]$value, metadata_list$field_wbddh_ds_source, "field_wbddh_ds_source")
  safe_see_if(node_metadata$field_wbddh_dsttl_upi$und[[1]]$value, metadata_list$field_wbddh_dsttl_upi, "field_wbddh_dsttl_upi")
  safe_see_if(node_metadata$field_ddh_external_contact_email$und[[1]]$value, metadata_list$field_ddh_external_contact_email, "field_ddh_external_contact_email")
  safe_see_if(node_metadata$field_wbddh_embargo$und[[1]]$value, metadata_list$field_wbddh_embargo, "field_wbddh_embargo")
  safe_see_if(node_metadata$field_wbddh_organization$und[[1]]$value, metadata_list$field_wbddh_organization, "field_wbddh_organization")
  safe_see_if(node_metadata$field_wbddh_statistical_concept$und[[1]]$value, metadata_list$field_wbddh_statistical_concept, "field_wbddh_statistical_concept")
  safe_see_if(node_metadata$field_wbddh_aggregation_method$und[[1]]$value, metadata_list$field_wbddh_aggregation_method, "field_wbddh_aggregation_method")

}
