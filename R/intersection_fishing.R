intersection_fishing <- function(){
  
  fishing_effort_clean_sf <- fishing_effort_clean %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  mpa_fishing <- st_join(europe_grid, fishing_effort_clean_sf,left = T)
  
  mpa_fishing <- mpa_fishing %>% 
    st_drop_geometry() %>%
    mutate(fishing_presence = as.factor(ifelse(is.na(name_fishing),0,1)))
  
  save(mpa_fishing, file = "output/mpa_fishing.Rdata")
  
  return(mpa_fishing)
  
}