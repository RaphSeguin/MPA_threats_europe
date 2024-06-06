load_offshore <- function(){
  
  offshore <- read.csv("data/shapefiles/offshore_infrastructures/offshore_infrastructure_v20231106.csv") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    filter(!label %in% c("possible_oil","possible_wind","lake_maracaibo")) %>%
    mutate(label = ifelse(label %in% c("oil","probable_oil"),"oil",
                          ifelse(label %in% c("wind","probable_wind"), "wind", "other")))
  
  return(offshore) 
  

  }