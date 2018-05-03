#' create_json_attach
#'
#' Create a json according to predefined template
#'
#' @param resource_nid character: resource node id
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_attach <- function(resource_nid, json_template = mdlibtoddh::json_template_attach) {

  # temp <- purrr::map2_chr(resource_list, names(resource_list), function(value, key) {
  #   paste0(value, ' (', key, ')')
  # })

  json_template$workflow_status <- jsonlite::unbox("published")

  temp <- paste0("Related materials (Questionnaires, reports, tables, technical documents, and data files) (", resource_nid, ")")
  json_template$field_resources$und$target_id <- temp

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
