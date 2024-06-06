intersection_gravity <- function(){
  
  europe_grid_gravity <- europe_grid %>%
    st_transform(crs = st_crs(gravity)) %>%
    st_centroid()
    
  #Getting number of shipping boats in MPA
  gravity <- raster::extract(gravity, europe_grid_gravity)
  
  # #mean gravity for each cell
  # mpa_gravity_sum <- lapply(1:length(mpa_gravity), function(i){
  #   
  #   temp <- mpa_gravity[[i]]
  #   mean <- mean(temp, na.rm = T)
  #   
  # })
  
  # mpa_gravity_sum <- do.call(rbind, mpa_gravity_sum) %>% as.data.frame()
  
  #Gravity
  mpa_gravity <- europe_grid %>%
    st_drop_geometry() %>%
    dplyr::select(grid_id, id,protected, iucn_cat) %>%
    cbind(gravity) 
  
  save(mpa_gravity, file = "output/mpa_gravity.Rdata")
  return(mpa_gravity)
  
}