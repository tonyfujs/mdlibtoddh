library(dplyr)
library(jsonlite)
library(usethis)

# Set root URL
root_url <- ddhconnect:::production_root_url
dkanr::dkanr_setup(url = root_url,
                   username = Sys.getenv("ddh_username"),
                   password = Sys.getenv("ddh_prod_password"))

# STEP 1: Get data --------------------------------------------------------

# read the basic fields and fields specific to microdata
ddh_master_key <- readr::read_csv('./data-raw/ddh_metadata_master.csv')
lookup <- ddh_master_key %>%
          filter(form %in% c('Microdata', 'Basic'))

# lookup to map DDH fields to equivalent microdata fields
mdlib_api_mapping <- readr::read_csv('./data-raw/ddh_microdata_mapping.csv') %>%
  filter(!is.na(ddh_fields))

taxonomy <- ddhconnect::get_lovs(root_url = root_url)%>%
  rename(ddh_machine_name = machine_name, field_lovs = list_value_name)
#taxonomy <- readr::read_csv('./data-raw/taxonomy_cache.csv')

fields <- ddhconnect::get_fields(root_url = root_url) %>%
  dplyr::filter(data_type == 'microdata') %>%
  dplyr::rename(ddh_machine_name = machine_name)
# TODO: Report type to IT
fields$ddh_machine_name[fields$ddh_machine_name == "field__wbddh_depositor_notes"] <- "field_wbddh_depositor_notes"


# STEP 2: Check data consistency ------------------------------------------
# lookup & mdlib_api_mapping
mdlib_field_keys <- sort(unique(mdlib_api_mapping$ddh_fields))
lookup_field_keys <- sort(unique(lookup$field_key))
assertthat::assert_that(all(mdlib_field_keys %in% lookup_field_keys),
                        msg = 'Incomplete mdlib to ddh field_key mapping')
# assert that all taxonomy fields except for the specified ones are present in the lookup
taxonomy_machine_names <- sort(unique(taxonomy$ddh_machine_name))
lookup_machine_names <- sort(unique(lookup$ddh_machine_name))
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
    "field_wbddh_update_frequency",
    "field_frequency",
    "status",
    "field_granularity_list",
    "field_wbddh_working_unit_user",
    "field_collection_field",
    "field_wbddh_ds_source"
  )

taxonomy_machine_names <- taxonomy_machine_names[!taxonomy_machine_names %in% taxonomy_remove]
assertthat::assert_that(length(taxonomy_machine_names[!taxonomy_machine_names %in% lookup_machine_names]) == 0,
                        msg = 'Incomplete list of taxonomy variables')

# assert that all fields except for the specified ones are present in the lookup
fields_machine_names <- sort(unique(c(fields$ddh_machine_name, "field_license_wbddh", "workflow_status","field_wbddh_publisher_name",
                                      "field_wbddh_end_date", "field_wbddh_start_date"))) # TEMPORARY FIX (not returned by field service)
fields_remove <-
  c(
    "field_ddh_external_contact_email",
    "field_format",
    "field_granularity_list",
    "field_tags",
    "field_temporal_coverage",
    "field_wbddh_additional_publisher",
    "field_wbddh_aggregation_method",
    "field_wbddh_base_period",
    "field_wbddh_curator_notes",
    "field_wbddh_depositor_notes",
    "field_wbddh_ds_embargo_date",
    "field_wbddh_ds_source",
    "field_wbddh_dsttl_upi",
    "field_wbddh_economy_coverage",
    "field_wbddh_next_expected_update",
    "field_wbddh_no_of_economies",
    "field_wbddh_organization",
    "field_wbddh_other_producer",
    "field_wbddh_produced_by",
    "field_wbddh_related_indicators",
    "field_wbddh_series_code",
    "field_wbddh_subscription_date",
    "field_wbddh_type_of_license",
    "field_wbddh_update_frequency",
    "field_best_bets",
    "field_creator_name",
    "field_date_of_creation",
    "field_external_metadata",
    "field_external_metadata",
    "field_link_api",
    "field_link_remote_file",
    "field_preview_image",
    "field_resource_weight",
    "field_upload",
    "field_wbddh_api_format",
    "field_wbddh_statistical_concept",
    "workflow_status"
  )

fields_machine_names <- fields_machine_names[!fields_machine_names %in% fields_remove]
assertthat::assert_that(length(fields_machine_names[!fields_machine_names %in% lookup_machine_names]) == 0,
                        msg = 'Incomplete list of taxonomy variables')


# STEP 3: Merge data ------------------------------------------------------

# Merge lookup and mdlib
lookup <- lookup %>%
  full_join(mdlib_api_mapping, by = c('field_key' = 'ddh_fields')) %>%
  select(field_label:microdata_library,
         mdlib_section = microdatalib_section,
         mdlib_field = microdatalib_field,
         mdlib_json_field = json_fields,
         is_array = is_array,
         key_1 = Key_1,
         key_2 = Key_2)

lookup$field_lovs[lookup$field_lovs == 'PeopleSoft'] <- NA

# CHECK that merge is complete
mdlib_json <- sort(unique(mdlib_api_mapping$json_fields))
lookup_json <- sort(unique(lookup$mdlib_json_field[!is.na(lookup$ddh_machine_name)]))
assertthat::assert_that(length(setdiff(mdlib_json, lookup_json)) == 0,
                        msg = "Incomplete mapping between mdlib JSON keys and DDH field_keys")

# Merge taxonomy and lookup
taxonomy$field_lovs[taxonomy$field_lovs == 'Middle East & North Africa'] <- "Middle East and North Africa"
lookup <- taxonomy %>%
  filter(ddh_machine_name %in% taxonomy_machine_names) %>%
  dplyr::right_join(lookup, by = c('ddh_machine_name', 'field_lovs'))

# CHECK if all controlled vocabulary have a matching tid in lookup

check_tids <- lookup %>%
  filter(ddh_machine_name %in% taxonomy_machine_names) %>%
  filter(!is.na(field_lovs) & is.na(tid))

assertthat::assert_that(nrow(check_tids) == 0,
                        msg = 'Incomplete tid mapping')


# STEP 4: Generate microdata placeholder -----------------------------------

machine_names <- sort(unique(c(fields_machine_names, taxonomy_machine_names)))
md_placeholder <- vector(mode = 'list', length = length(machine_names))
names(md_placeholder) <- machine_names


# STEP 5: Generate a lkup table to map Microdata values to DDH LOVs -------

# map microdata field names to ddh field names
field_to_machine <- mdlibtoddh:::create_lkup_vector(lookup, vector_keys = 'field_key', vector_values = 'ddh_machine_name')
assertthat::assert_that(sum(is.na(field_to_machine)) == 0,
                        msg = 'Incomplete field_key to machine_name mapping')
field_to_machine_no_na <- field_to_machine[!is.na(field_to_machine)]

# map variations of a controlled vocabulary value to the controlled vocabulary value accepted by DDH
my_sheets <- readxl::excel_sheets('./data-raw/control_vocab_mapping.xlsx')
md_ddh_lovs <- purrr::map_df(my_sheets, function(x) {
  temp <- readxl::read_excel('./data-raw/control_vocab_mapping.xlsx', sheet = x)
  temp$ddh_machine_name <- field_to_machine_no_na[x]
  return(temp)
})

md_ddh_lovs <- bind_rows(md_ddh_lovs)

md_ddh_names <- sort(unique(md_ddh_lovs$ddh_machine_name))
md_ddh_lovs <- purrr::map(md_ddh_names, function(x){
  temp <- md_ddh_lovs[md_ddh_lovs$ddh_machine_name == x, ]
  out <- mdlibtoddh:::create_lkup_vector(temp, vector_keys = 'microdata_category' , vector_values = 'ddh_category')
  return(out)
})
names(md_ddh_lovs) <- md_ddh_names
md_ddh_lovs$field_wbddh_country <- NULL # Remove country mapping. No longer necessary: Using iso3 codes now

# STEP 6: Generate a lookup table to map DDH LOVs to tids -----------------

ddh_tid_lovs <- lookup %>%
  select(ddh_machine_name, field_lovs, tid) %>%
  filter(!is.na(tid))

ddh_tid_names <- sort(unique(ddh_tid_lovs$ddh_machine_name))
ddh_tid_lovs <- purrr::map(ddh_tid_names, function(x){
  temp <- ddh_tid_lovs[ddh_tid_lovs$ddh_machine_name == x, ]
  out <- mdlibtoddh:::create_lkup_vector(temp, vector_keys = 'field_lovs' , vector_values = 'tid')
  return(out)
})
names(ddh_tid_lovs) <- ddh_tid_names
ddh_tid_lovs <- ddh_tid_lovs[purrr::map_int(ddh_tid_lovs, length) > 0]


# Add JSON templates ------------------------------------------------------

json_template_dataset <- fromJSON('./data-raw/ddh_schema_microdata_dataset.json')
json_template_resource <- fromJSON('./data-raw/ddh_schema_microdata_resource.json')
json_template_attach <- fromJSON('./data-raw/ddh_schema_microdata_resource_attach.json')

dkanr::dkanr_logout()

# Save lookup table -------------------------------------------------------

lookup <- as.data.frame(lookup)
usethis::use_data(lookup,
                   md_placeholder,
                   md_ddh_lovs,
                   ddh_tid_lovs,
                   json_template_dataset,
                   json_template_resource,
                   json_template_attach,
                   overwrite = TRUE)

microdata_date_fields <- readLines("data-raw/microdata_date_fields.txt")
usethis::use_data(microdata_date_fields,
                   overwrite = TRUE,
                   internal = TRUE)

