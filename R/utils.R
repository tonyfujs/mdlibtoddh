extract_from_list <- function(list, keys) {
  list <- list[keys]
  list <- list[!purrr::map_lgl(list, is.null)]
  list <- paste(list, collapse = ' - ')
  return(list)
}

create_lkup_vector <- function(lkup_table = mdlibtoddh::lookup,
                               vector_keys = 'microdata_category',
                               vector_values = 'ddh_category') {

  # CHECK: Input validity
  assertthat::assert_that(is.data.frame(lkup_table))
  assertthat::assert_that(is.character(vector_keys))
  assertthat::assert_that(is.character(vector_values))
  assertthat::assert_that(vector_values %in% names(lkup_table), msg = paste(vector_values, 'is not a valid column name'))
  assertthat::assert_that(vector_keys %in% names(lkup_table), msg = paste(vector_keys, 'is not a valid column name'))

  values <- lkup_table[[vector_values]]
  names <- lkup_table[[vector_keys]]
  df <- data.frame(names, values, stringsAsFactors = FALSE)
  df <- unique(df)
  out <- df$values
  names(out) <- df$names
  out <- out[!is.na(names(out))]

  return(out)
}

map_valid_lovs <- function(values, lkup_vector) {
  names(lkup_vector) <- tolower(names(lkup_vector))
  # Deal with multiple values
  values <- stringr::str_split(values, pattern = ';', simplify = FALSE)
  values <- unlist(values)
  values <- values[values != ""]
  values <- stringr::str_trim(values, side = 'both')
  out <- purrr::map_chr(values, function(x) unname(lkup_vector[tolower(x)]))

  out[is.na(out)] <- values[is.na(out)] # replace non-mapped values, by original values
  out <- unique(stringr::str_trim(out))

  purrr::map(out, function(x) assertthat::assert_that(are_valid_lovs(x, lkup_vector),
                                                      msg = paste0("No valid mapping found in lkup_vector for: ", x)))

  out <- paste(out, collapse = ';')

  return(out)
}


are_valid_lovs <- function(values, accepted_values) {
  all(values %in% accepted_values)
}

format_text <- function(text) {
  text <- stringr::str_replace_all(text, pattern = "\r?\n|\r", replacement = ' ')
  text <- stringr::str_replace_all(text, pattern = ' +', replacement = ' ')
  text <- stringr::str_trim(text)

  return(text)
}

clean_date <- function(dt) {
  if(is_year(dt)) {
    dt <- paste(dt, "01", "01", sep="-")
  }
  return(dt)
}

is_year <- function(dt) {
  return(suppressWarnings(!is.null(dt) && !is.na(as.numeric(dt)) && as.numeric(dt) >= 1900 && as.numeric(dt) <= 2100))
}

# extract_field_wbddh_reference_id
extract_field_wbddh_reference_id <- function(x) {
  x[["field_wbddh_reference_id"]]$und[[1]]$value
}
extract_field_wbddh_reference_id_safe <- purrr::possibly(extract_field_wbddh_reference_id, otherwise = '')
extract_field_wbddh_reference_id <- function(x) {
  temp <- extract_field_wbddh_reference_id_safe(x)
  if(is.null(temp)) {return("")} else {return(temp)}
}

# extract_field_wbddh_data_class
extract_field_wbddh_data_class <- function(x) {
  x[["field_wbddh_data_class"]]$und[[1]]$tid
}
extract_field_wbddh_data_class_safe <- purrr::possibly(extract_field_wbddh_data_class, otherwise = '')
extract_field_wbddh_data_class <- function(x) {
  temp <- extract_field_wbddh_data_class_safe(x)
  if(is.null(temp)) {return("")} else {return(temp)}
}

safe_unbox <- purrr::possibly(jsonlite::unbox, otherwise = '')
safe_assign <- function(x) {if (length(x) > 0) {x} else {""}}

is.same <- function(file_value, orig_value) {
  is.empty(file_value) && is.empty(orig_value) ||
    is.character(file_value) && is.character(orig_value) && (gsub("[\n]", "", file_value) == gsub("[\n]", "", orig_value))
}

is.empty <- function(s) {
  is.null(s) || s == ""
}

is_blank <- function(input) {
  return(gtools::invalid(input) || all(input == ""))
}

safe_see_if <- function(file_value, orig_value, field_name) {
  assert_result <- assertthat::see_if(is.same(file_value, orig_value))
  if(!assert_result){
    warning(paste0(field_name, ": The updated value is not equal to the passed value."))
  }
}

is.error <- function(x) inherits(x, "try-error")

already_exists <- function(id, existing_ids) {
  id %in% existing_ids
}

# expand the date value to yyyy-mm-dd format if the date field does not have a date or month value
expand_date <- function(date_value) {
  date_split <- unlist(strsplit(date_value, "-"))
  if (length(date_split) == 1) {
    expand_date_value <- paste(date_value, "01", "01", sep = "-")
  }
  else if (length(date_split) == 2) {
    expand_date_value <- paste(date_value, "01", sep = "-")
  }
  else {
    expand_date_value <- date_value
  }
  return(expand_date_value)
}

# subsets datasets fields
filter_dataset_fields <- function(metadata_temp,
                                  ddh_fields = ddhconnect::get_fields()) {
  dataset_fields <- ddh_fields$machine_name[ddh_fields$node_type == "dataset"]
  dataset_fields <- unique(dataset_fields)
  metadata_temp <- metadata_temp[names(metadata_temp) %in% dataset_fields]
  return(metadata_temp)
}

# subsets resource fields
filter_resource_fields <- function(metadata_temp,
                                   ddh_fields = ddhconnect::get_fields()) {
  resource_fields <- ddh_fields$machine_name[ddh_fields$node_type == "resource"]
  resource_fields <- unique(resource_fields)
  metadata_temp <- metadata_temp[names(metadata_temp) %in% resource_fields]
  return(metadata_temp)
}

#Makesure resource is Microdata Landing Page
resource_check <- function(nids,
                           resource_type = resource_meta$field_wbddh_resource_type$und[[1]]$tid,
                           resource_title = resource_meta$title) {
  nids <- unique(nids)
  for(i in seq_along(nids)){
    nid <- nids[[i]]
    resource_meta <- ddhconnect::get_metadata(nid)
    if((resource_type == 1192) & (resource_title == "Related materials (Questionnaires, reports, tables, technical documents, and data files)")){
      return(nid)
    }
  }
}


# Pass blank values for values which are not present in updated version of dataset
pass_blank_values <- function(node_id, dataset_metadata, metadata_list, root_url, credentials){

  # List of free-text values to check
  machine_names_value <- c(
    "body",
    "field_contact_email",
    "field_ddh_external_contact_email",
    "field_ddh_harvest_sys_id",
    "field_wbddh_access_authority",
    "field_wbddh_acronym",
    "field_wbddh_citation_text",
    "field_wbddh_disclaimer",
    "field_wbddh_ds_source",
    "field_wbddh_estimate_sampling",
    "field_wbddh_funding_name",
    "field_wbddh_modified_date",
    "field_wbddh_primary_investigator",
    "field_wbddh_questionnaires",
    "field_wbddh_reference_id",
    "field_wbddh_release_date",
    "field_wbddh_response_rates",
    "field_wbddh_sampling_procedure",
    "field_wbddh_series_information",
    "field_wbddh_study_type",
    "field_wbddh_supervision",
    "field_wbddh_unit_of_analysis",
    "field_wbddh_universe",
    "field_wbddh_version_description",
    "field_wbddh_kind_of_data",
    "field_wbddh_mode_data_collection",
    "field_wbddh_authority_name",
    "field_wbddh_data_editing",
    "field_wbddh_dstranslated_title",
    "field_wbddh_note_data_collection",
    "field_wbddh_other_acknowledgment",
    "field_wbddh_other_forms_of_data",
    "field_wbddh_other_processing",
    "field_wbddh_aggregation_method",
    "field_wbddh_version_notes"
  )

  # Filter out fields from DDH metadata
  dataset_metadata <- dataset_metadata[names(dataset_metadata) %in% machine_names_value]

  # Filter out non-blank fields
  to_pass <- c()
  for(i in seq_along(dataset_metadata)){
    if(!is_blank(dataset_metadata[i])){
      if(!names(dataset_metadata[i]) %in% names(metadata_list)){
        to_pass <- c(to_pass, names(dataset_metadata[i]))
      }
    }
  }

  # Create JSON with blank values
  json_blank <- ddhconnect::create_blank_json_body(values = to_pass,
                                                   publication_status = "published",
                                                   type = "dataset")

  # Update dataset with blank values
  update_dataset(nid = node_id, body = json_blank,
                 root_url = root_url,
                 credentials = credentials)
}
