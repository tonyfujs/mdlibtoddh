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
