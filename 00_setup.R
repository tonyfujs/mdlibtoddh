library(ddhconnect)
library(mdlibconnect)
library(dplyr)


# Set global variables ----------------------------------------------------

root_url <- ddhconnect:::stg_root_url

# my_credentials <- list(cookie = NA, token = NA)
# my_credentials$cookie <- "SSESSf70b342115438dbd206e1df9422509d2=KevKJ51dpF1ADPZjs30YcekzDUJqvApYX4AGYzuWN0I"
# my_credentials$token <- "YtaXBHgujmOPfHSeuQEajFq6IQmZf4PhOCRU1gzNR2g"

ddh_credentials <- get_credentials(username = Sys.getenv("ddh_username"),
                                   password = Sys.getenv("ddh_stg_password"),
                                   root_url = root_url)

get_user_details(credentials = ddh_credentials,
                 uid = 499754,
                 root_url = root_url)

# CHECK lookup table ------------------------------------------------------
# Check taxonomy
tax_check <- check_taxonomy_services()
assertthat::assert_that(sum(purrr::map_int(tax_check, nrow)) == 0,
                        msg = 'Discrepancy between STG & PROD taxonomy')
# Check lookup
data(lookup)
taxonomy <- get_lovs(root_url = root_url)
taxonomy_machine_names <- unique(taxonomy$machine_name)
lookup_machine_names <- unique(lookup$ddh_machine_name)
taxonomy_remove <-
  c(
    "field_format",
    "field_tags",
    "field_wbddh_economy_coverage",
    "field_wbddh_global_practices",
    "field_wbddh_spatial_data_type",
    "field_wbddh_region",
    "field_wbddh_periodicity",
    "field_wbddh_api_format",
    "field_wbddh_resource_type",
    "field_wbddh_update_frequency",
    "field_exception_s_",
    "field_frequency"
  )
taxonomy_machine_names <- taxonomy_machine_names[!taxonomy_machine_names %in% taxonomy_remove]
assertthat::assert_that(length(taxonomy_machine_names[!taxonomy_machine_names %in% lookup_machine_names]) == 0,
                        msg = 'Incomplete list of taxonomy variables')

test <- purrr::map(taxonomy_machine_names, check_lkup_tid_list, lookup = lookup)
names(test) <- taxonomy_machine_names
test <- purrr::map(test, function(x) {if (is.null(x$result)) {return(x$error)} else {return(x$result)}})


















