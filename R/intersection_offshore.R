intersection_offshore <- function(){

  #Intersecting with 
  mpa_offshore <- st_join(europe_grid, offshore) %>%
    dplyr::select(grid_id, id, protected, iucn_cat, structure_id, label) %>%
    st_drop_geometry() %>%
    distinct(grid_id,id,structure_id,label,.keep_all = T) %>%
    #adding number of structures
    group_by(grid_id) %>%
    mutate(offshore_number = ifelse(is.na(structure_id), 0, n()))%>%
    #transform to presence or absences
    mutate(offshore_presence = as.factor(ifelse(offshore_number >= 1, 1, 0))) %>%
    ungroup() %>%
    #also calculate number of structures by type
    group_by(grid_id, label) %>%
    mutate(specific_number = ifelse(is.na(structure_id), 0, n_distinct(structure_id))) %>%
    pivot_wider(names_from = label, values_from = specific_number) %>%
    dplyr::select(-`NA`) %>%
    # mutate(oil = ifelse(is.na(oil),0,oil),
    #        wind = ifelse(is.na(wind),0,wind),
    #        other = ifelse(is.na(other),0,other)) %>%
    ungroup()
  
  # Find the nearest marine protected area for each point
  # mpa_centers <- st_centroid(mpa_eu_wdpa)
  # nearest_offshore <- st_nearest_feature(mpa_centers, offshore)
  
  # Calculate the distances to the nearest marine protected areas
  # offshore_distance <- st_distance(mpa_centers, offshore[nearest_offshore,], by_element = TRUE)/1000
  
  # #Creating df of distance only
  # mpa_offshore_distance <- mpa_eu_wdpa %>%
  #   # st_drop_geometry() %>%
  #   dplyr::select(id) %>%
  #   cbind(offshore_distance = as.numeric(offshore_distance))
  
  #Joining with offshore data
  # mpa_offshore <- mpa_offshore %>%
  #   left_join(mpa_offshore_distance, by = "id") %>%
    #Set distance to 0 if offshore is inside MPA
    # mutate(offshore_distance = ifelse(offshore_presence == 1, 0, offshore_distance)) 
  
  save(mpa_offshore, file = "output/mpa_offshore.Rdata")
  return(mpa_offshore)
  
}