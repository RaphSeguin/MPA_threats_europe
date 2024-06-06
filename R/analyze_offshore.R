analyze_offshore <- function(){
  
  load("output/mpa_offshore.Rdata")
  
  mpa_offshore_clean <- mpa_offshore %>%
    # left_join(mpa_eu_wdpa, by = "id") %>%
    # #Calcualte density
    # mutate(offshore_density = offshore_number/gis_m_area) %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) 
  
  #Clean structures
  #Function to coalescece columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  #Cleaning structures
  mpa_structures_clean <- mpa_offshore_clean %>%
    st_drop_geometry() %>%
    distinct(grid_id, oil, wind, other) %>%
    group_by(grid_id) %>%
    summarise_all(coalesce_by_column) %>%
    replace(is.na(.), 0) %>%
    ungroup()
  
  mpa_offshore_clean <- mpa_offshore_clean %>%
    dplyr::select(-c(oil, wind, other)) %>%
    left_join(mpa_structures_clean, by = "grid_id") %>%
    group_by(grid_id) %>%
    distinct(offshore_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_offshore_clean, file = "output/mpa_offshore_clean.Rdata")

  
  
}
