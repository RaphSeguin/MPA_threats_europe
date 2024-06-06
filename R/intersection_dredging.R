intersection_dredging <- function(){
  
  #Intersecting with
  mpa_dredge <- st_join(europe_grid, dredge) %>%
    dplyr::select(grid_id, id,protected,iucn_cat, dredge_id, material,purpose,end_use) %>%
    st_drop_geometry() %>%
    #adding number of structures
    group_by(grid_id) %>%
    mutate(dredge_number = ifelse(is.na(dredge_id), 0, n_distinct(dredge_id)))%>%
    mutate(dredge_presence = as.factor(ifelse(dredge_number >= 1, 1, 0))) %>%
    ungroup()
  
  # # Find the nearest marine protected area for each point
  # mpa_centers <- st_centroid(mpa_eu_wdpa)
  # nearest_dredge <- st_nearest_feature(mpa_centers, dredge)
  # 
  # # Calculate the distances to the nearest marine protected areas
  # dredge_distance <- st_distance(mpa_centers, dredge[nearest_dredge,], by_element = TRUE)/1000
  # 
  # #Creating df of distance only
  # mpa_dredge_distance <- mpa_eu_wdpa %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(id) %>%
  #   cbind(dredge_distance = as.numeric(dredge_distance))
  # 
  # #Joining with offshore data
  # mpa_dredge <- mpa_dredging %>%
  #   left_join(mpa_dredge_distance, by = "id") %>%
  #   #Set distance to 0 if offshore is inside MPA
  #   mutate(dredge_distance = ifelse(dredge_presence == 1, 0, dredge_distance)) 
  
  save(mpa_dredge, file = "output/mpa_dredge.Rdata")
  return(mpa_dredge)

}