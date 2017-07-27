#' add_search_tags
#'
#' Add search tags to the list of variable to be added to DDH
#'
#' @param metadata_list list: List of metadata pulled from Microdata API
#' @param id character: Microdata study internal ID
#' @param token character: Microdata API authentication token
#' @param ... ...: Extra paramter to be passed to add_search_tags (See get_md_dictionary() limit parameter)
#'
#' @return character
#' @export
#'

add_search_tags <- function(metadata_list, id, token, ...) {
  # Get metadata keywords
  keywords <- unlist(metadata_list$`codeBook/stdyDscr/stdyInfo/subject/keyword`)
  keywords <- stringr::str_replace_all(keywords, pattern = ' +', replacement = ' ')
  keywords <- stringr::str_replace_all(keywords, pattern = ', ?', replacement = ';')
  # Get codebook words
  dico <- get_md_dictionary(id = id, token = token, limit = limit)
  # Combine
  out <- paste(keywords, dico, sep = ';')
  out <- stringr::str_replace(out, pattern = '^;|;$', replacement = '')

  metadata_list$field_wbddh_search_tags <- out

  return(metadata_list)
}
