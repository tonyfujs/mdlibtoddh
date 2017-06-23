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

# Generate microdata placeholder for DDH
machine_names <- sort(unique(fields$ddh_machine_name))
md_placeholder <- vector(mode = 'list', length = length(machine_names))
names(md_placeholder) <- machine_names


# Generate a lkup table to map Microdata values to DDH LOVs ---------------

field_to_machine <- create_lkup_vector(lookup, vector_keys = 'field_key', vector_values = 'ddh_machine_name')
my_sheets <- readxl::excel_sheets('./data-raw/control_vocab_mapping.xlsx')
md_ddh_lovs <- purrr::map_df(my_sheets, function(x) {
  temp <- readxl::read_excel('./data-raw/control_vocab_mapping.xlsx', sheet = x)
  temp$ddh_machine_name <- field_to_machine[x]
  return(temp)
})

md_ddh_lovs <- bind_rows(md_ddh_lovs)
md_ddh_lovs <- md_ddh_lovs %>%
  select(-ddh_category_multiple, field_lovs = ddh_category)

md_ddh_names <- sort(unique(md_ddh_lovs$ddh_machine_name))
md_ddh_lovs <- purrr::map(md_ddh_names, function(x){
  temp <- md_ddh_lovs[md_ddh_lovs$ddh_machine_name == x, ]
  out <- create_lkup_vector(temp, vector_keys = 'microdata_category' , vector_values = 'field_lovs')
  return(out)
})
names(md_ddh_lovs) <- md_ddh_names


# Generate a lookup table to map DDH LOVs to tids ---------------------------

ddh_tid_lovs <- lookup %>%
  select(ddh_machine_name, field_lovs, tid) %>%
  filter(!is.na(tid))

ddh_tid_names <- sort(unique(ddh_tid_lovs$ddh_machine_name))
ddh_tid_lovs <- purrr::map(ddh_tid_names, function(x){
  temp <- ddh_tid_lovs[ddh_tid_lovs$ddh_machine_name == x, ]
  out <- create_lkup_vector(temp, vector_keys = 'field_lovs' , vector_values = 'tid')
  return(out)
})
names(ddh_tid_lovs) <- ddh_tid_names
ddh_tid_lovs <- ddh_tid_lovs[purrr::map_int(ddh_tid_lovs, length) > 0]

# Save lookup table -------------------------------------------------------

lookup <- as.data.frame(lookup)
devtools::use_data(lookup,
                   md_placeholder,
                   md_ddh_lovs,
                   ddh_tid_lovs,
                   overwrite = TRUE)




