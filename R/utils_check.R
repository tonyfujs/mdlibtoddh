check_taxonomy_services <- function() {
  # STG taxonomy
  taxonomy_stg <- ddhconnect::get_lovs(root_url = ddhconnect:::stg_root_url)
  taxonomy_stg <- dplyr::rename_(taxonomy_stg, 'ddh_machine_name' = 'machine_name', 'field_lovs' = 'list_value_name')
  names(taxonomy_stg) <- paste0('stg_', names(taxonomy_stg))
  # PROD taxonomy
  taxonomy_prod <- ddhconnect::get_lovs(root_url = ddhconnect:::production_root_url)
  taxonomy_prod <- dplyr::rename_(taxonomy_prod,  'ddh_machine_name' = 'machine_name', 'field_lovs' = 'list_value_name')
  names(taxonomy_prod) <- paste0('prod_', names(taxonomy_prod))
  # Compare STG & PROD
  diff_taxonomy <- dplyr::full_join(taxonomy_prod, taxonomy_stg, by = c("prod_ddh_machine_name" = "stg_ddh_machine_name",
                                                                        "prod_field_lovs" = "stg_field_lovs"))
  diff_taxonomy$same_vocabulary_name <- diff_taxonomy$prod_vocabulary_name == diff_taxonomy$stg_vocabulary_name
  diff_taxonomy$same_tid <- diff_taxonomy$prod_tid == diff_taxonomy$stg_tid



  diff_taxonomy <- dplyr::select_(diff_taxonomy, 'prod_ddh_machine_name', 'prod_vocabulary_name', 'stg_vocabulary_name', 'prod_field_lovs', 'prod_tid', 'stg_tid', 'same_vocabulary_name', 'same_tid')
  diff_taxonomy <- dplyr::filter_(diff_taxonomy, ~prod_ddh_machine_name != 'field_tags')

  problematic_variables <- diff_taxonomy[is.na(diff_taxonomy$same_vocabulary_name), ]
  problematic_names <- diff_taxonomy[diff_taxonomy$same_vocabulary_name == FALSE, ]

  return(list(problematic_variables, problematic_names))
}

check_lkup_tid_variables <- function(lookup, ddh_tid_lovs) {
  lookup_machine_names <- unique(lookup$ddh_machine_name)
  assertthat::assert_that(all(names(ddh_tid_lovs) %in% lookup_machine_names),
                          msg = 'Some variables with controlled vocabulary are not documented in lookup')
}

check_lkup_tid_list <- function(lookup, machine_name) {
  lookup <- lookup[lookup$ddh_machine_name == machine_name, ]
  n_names <- length(lookup$ddh_machine_name[!is.na(lookup$ddh_machine_name)])
  n_tids <- length(lookup$tid[!is.na(lookup$tid)])
  assertthat::assert_that(n_names == n_tids,
                          msg = 'LOVs to tids mapping is incomplete')
}

check_lkup_tid_list <- purrr::safely(check_lkup_tid_list)
