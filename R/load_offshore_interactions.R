load_offshore_interactions <- function(){
  
  offshore_interactions_wind <- read.csv("data/shapefiles/offshore_interactions/infra_vessel_activity_wind_100th_degree_v20230222.csv") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  return(offshore) 
  
  
}