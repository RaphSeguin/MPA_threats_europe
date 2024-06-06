loading_ports <- function(){
  
  #Loading ports shapefile 
  #From https://data.humdata.org/dataset/world-port-index?
  ports <- st_read("data/shapefiles/Ports/WPI.shp") %>%
    clean_names() %>%
    dplyr::select(index_no, port_name, country, harborsize)
    
  return(ports)
  
}