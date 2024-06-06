intersection_desalination <- function(){
  
  #Intersecting with
  mpa_desalination <- st_join(europe_grid, desalination) %>%
    dplyr::select(grid_id,id,protected, iucn_cat,desalination_id, feedwater, customer_t, industry_t) %>% 
    st_drop_geometry() %>%
    #adding number of structures
    group_by(grid_id) %>%
    mutate(desalination_number = ifelse(is.na(desalination_id), 0, n()))%>%
    mutate(desalination_presence = as.factor(ifelse(desalination_number >= 1, 1, 0))) %>%
    ungroup()
  # 
  # # Find the nearest marine protected area for each point
  # mpa_centers <- st_centroid(mpa_eu_wdpa)
  # nearest_desalination <- st_nearest_feature(mpa_centers, desalination)
  # 
  # # Calculate the distances to the nearest marine protected areas
  # desalination_distance <- st_distance(mpa_centers, desalination[nearest_desalination,], by_element = TRUE)/1000
  # 
  # #Creating df of distance only
  # mpa_desalination_distance <- mpa_eu_wdpa %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(id) %>%
  #   cbind(desalination_distance = as.numeric(desalination_distance))
  # 
  #Joining with activity data
  # mpa_desalination <- mpa_desalination %>%
  #   left_join(mpa_desalination_distance, by = "id") %>%
  #   #Set distance to 0 if activity is inside MPA
  #   mutate(desalination_distance = ifelse(desalination_presence == 1, 0, desalination_distance)) 
  
  save(mpa_desalination, file = "output/mpa_desalination.Rdata")
  return(mpa_desalination)
  
}