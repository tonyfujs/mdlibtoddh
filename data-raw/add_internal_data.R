library(dplyr)

# STEP 1: Get data --------------------------------------------------------

# Matadata master
httr::set_config(httr::config(ssl_verifypeer = 0L))
googlesheets::gs_ls("ddh_metadata_master")
ddh_master_key <- googlesheets::gs_title("ddh_metadata_master")
lookup <- googlesheets::gs_read(ddh_master_key)

mdlib_api_mapping <- readr::read_csv('./data-raw/ddh_microdata_mapping.csv') %>%
  filter(!is.na(ddh_fields))

# taxonomy <- ddhconnect::get_lovs()%>%
#   rename(ddh_machine_name = machine_name, field_lovs = list_value_name)
taxonomy <- readr::read_csv('./data-raw/taxonomy_cache.csv')

fields <- ddhconnect::get_fields() %>%
  filter(data_type == 'microdata') %>%
  rename(ddh_machine_name = machine_name)

# Clean lookup table ------------------------------------------------------

# Format lookup
lookup <- lookup %>%
  filter(form %in% c('Microdata', 'Basic')) %>%
  full_join(mdlib_api_mapping, by = c('field_key'='ddh_fields')) %>%
  select(field_label:microdata_library,
         mdlib_section = microdatalib_section,
         mdlib_field = microdatalib_field,
         mdlib_json_field = json_fields)

lookup$field_lovs[lookup$field_lovs == 'PeopleSoft'] <- NA
lookup <- lookup %>% filter(!field_key == 'granularity')

# Format taxonomy
vocab_names <- sort(unique(taxonomy$vocabulary_name))
vocab_names <- vocab_names[vocab_names %in% unique(lookup$field_label)]
taxonomy <- taxonomy %>%
  filter(vocabulary_name %in% vocab_names) %>%
  rename(field_label = vocabulary_name, field_lovs = list_value_name) %>%
  left_join(lookup[, c('field_label', 'ddh_machine_name')]) %>%
  select(-field_label) %>%
  filter(!is.na(ddh_machine_name)) %>%
  distinct()

# join taxonomy
lookup <- lookup %>%
  dplyr::left_join(taxonomy, by = c('ddh_machine_name', 'field_lovs'))

# Save lookup table -------------------------------------------------------

lookup <- as.data.frame(lookup)
devtools::use_data(lookup,
                   overwrite = TRUE)




