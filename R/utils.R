extract_from_list <- function(list, keys) {
  list <- list[keys]
  list <- list[!purrr::map_lgl(list, is.null)]
  list <- paste(list, collapse = ' - ')
  return(list)
}
