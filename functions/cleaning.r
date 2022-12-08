# Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

# Subset the data to only include Adelie penguins that are not NA for the culmen length and body mass
remove_empty_cul_bod_adelie <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_length_mm), !is.na(body_mass_g), species == "Adelie Penguin (Pygoscelis adeliae)") %>%
    select(species, culmen_length_mm, body_mass_g)
}
