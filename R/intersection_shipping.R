intersection_shipping <- function(){
  
  europe_grid_shipping <- europe_grid %>%
    st_transform(crs = st_crs(shipping)) %>%
    st_centroid()
  
  #Getting number of shipping boats in MPA
  mpa_shipping <- raster::extract(shipping, europe_grid_shipping)
  
  # #sum of boats for each mpa
  # mpa_shipping_sum <- lapply(1:length(mpa_shipping), function(i){
  #   
  #   temp <- mpa_shipping[[i]]
  #   sum <- sum(temp)
  #   
  # })
  # 
  # mpa_shipping_sum <- do.call(rbind, mpa_shipping_sum) %>% as.data.frame()
  
  mpa_shipping <- europe_grid %>%
    st_drop_geometry() %>%
    dplyr::select(grid_id, id,protected, iucn_cat) %>%
    cbind(mpa_shipping) %>%
    dplyr::rename(shipping_boats = "mpa_shipping") %>%
    dplyr::mutate(shipping_presence = as.factor(ifelse(shipping_boats > 0, 1,0)))

  save(mpa_shipping, file = "output/mpa_shipping.Rdata")
  return(mpa_shipping)
  
}