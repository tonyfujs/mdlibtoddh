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

is.same <- function(file_value, orig_value, field_name) {
  is.empty(file_value) && is.empty(orig_value) ||
  is.character(file_value) && is.character(orig_value) && (gsub("[\n]", "", file_value) == gsub("[\n]", "", orig_value))

}

is.empty <- function(s) {
  is.null(s) || s == ""
}

assertthat::on_failure(is.same) <- function(call, env) {
  paste0(deparse(call$field_name), ": The updated value is not equal to the passed value.")
}

safe_see_if <- function(file_value, orig_value, field_name) {
  assert_result <- assertthat::see_if(is.same(file_value, orig_value, field_name))
  if(!assert_result){
    warning(attr(assert_result, "msg"))
  }
}

is.error <- function(x) inherits(x, "try-error")

already_exists <- function(id, existing_ids) {
  id %in% existing_ids
}
