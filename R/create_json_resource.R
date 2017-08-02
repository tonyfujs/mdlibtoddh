#' create_json_resource
#'
#' Create a json according to predefined template
#'
#' @param metadata_list list: List of metatadata values
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_resource <- function(metadata_list, json_template = mdlibtoddh::json_template_resource) {


  json_template$title <- safe_unbox("Related materials (Questionnaires, reports, tables, technical documents, and data files)")
  # json_template$body$und$value <- metadata_list$body # body
  json_template$field_wbddh_data_class$und$tid <- safe_unbox(metadata_list$field_wbddh_data_class) # data classification
  json_template$field_exception_s_$und$tid <- safe_unbox(metadata_list$field_exception_s_)
  json_template$field_link_api$und$url <- safe_unbox(metadata_list$field_link_api)
  json_template$field_wbddh_resource_type$und$tid <- safe_unbox('1192')

    # remove empty elements
  # to_keep <- names(metadata_list[!purrr::map_int(metadata_list, length) == 0])
  # json_template <- json_template[names(json_template) %in% to_keep]
  json_template$body <- NULL

  # Add required dataset elements
  json_template$type <- jsonlite::unbox("resource")

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
