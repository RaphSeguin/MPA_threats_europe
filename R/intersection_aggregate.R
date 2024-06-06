intersection_aggregate <- function(){
  
  #Intersecting with
  mpa_aggregate <- st_join(europe_grid, aggregate) %>%
    dplyr::select(grid_id, id,protected,iucn_cat, status_yr, gid, dateyear, material_aggregate, purpose, end_use_aggregate) %>%
    mutate(year_aggregate = as.numeric(year(dateyear))) %>%
    st_drop_geometry() %>%
    #adding number of structures
    group_by(grid_id, year_aggregate) %>%
    mutate(aggregate_number = ifelse(is.na(gid), 0, 
                                     ifelse(year_aggregate < status_yr, 0, n()))) %>%
    ungroup() %>%
    group_by(grid_id) %>%
    mutate(aggregate_number = sum(aggregate_number),
           aggregate_presence = as.factor(ifelse(aggregate_number >= 1, 1, 0))) %>%
    ungroup()
  
  # 
  # # Find the nearest marine protected area for each point
  # mpa_centers <- st_centroid(mpa_eu_wdpa)
  # nearest_aggregate <- st_nearest_feature(mpa_centers, aggregate)
  # 
  # # Calculate the distances to the nearest marine protected areas
  # aggregate_distance <- st_distance(mpa_centers, aggregate[nearest_aggregate,], by_element = TRUE)/1000
  # 
  # #Creating df of distance only
  # mpa_aggregate_distance <- mpa_eu_wdpa %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(id) %>%
  #   cbind(aggregate_distance = as.numeric(aggregate_distance))
  # 
  # #Joining with offshore data
  # mpa_aggregate <- mpa_aggregate %>%
  #   left_join(mpa_aggregate_distance, by = "id") %>%
  #   #Set distance to 0 if offshore is inside MPA
  #   mutate(aggregate_distance = ifelse(aggregate_presence == 1, 0, aggregate_distance)) 
  
  save(mpa_aggregate, file = "output/mpa_aggregate.Rdata")
  return(mpa_dredge)
  
}