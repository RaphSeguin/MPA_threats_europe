load_desalination <- function(){
  
  desalination <- st_read("data/shapefiles/desalination/desalinationPoint.shp") %>%
    #Keeping only online desalination
    dplyr::filter(plant_stat == "Online") %>%
    dplyr::select(desalination_id = id, feedwater, customer_t, industry_t) %>%
    #Filter only seawater
    filter(str_detect(feedwater, "Seawater|seawater")) %>%
    #Keep only distinct points
    distinct(geometry, .keep_all = T)
  
  #Calculate distance of desalination to coast
  dist_to_shore_raster <- raster("data/shapefiles/distance-from-shore.tif")
  dist_to_shore <- raster::extract(dist_to_shore_raster, desalination)
  
  desalination$dist_to_shore <- dist_to_shore

  #Only keep desalination plants which are less than 1 km from coast
  desalination <- desalination %>%
    filter(dist_to_shore < 1)
  
  save(desalination, file = "output/desalination.Rdata")
  
  return(desalination)
  
}
