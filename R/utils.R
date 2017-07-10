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
  out <- unname(lkup_vector[tolower(values)])
  out[is.na(out)] <- values[is.na(out)]
  out <- unique(stringr::str_trim(out))

  assertthat::assert_that(are_valid_lovs(out, lkup_vector),
                          msg = "No valid mapping found in lkup_vector")

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

is.error <- function(x) inherits(x, "try-error")
