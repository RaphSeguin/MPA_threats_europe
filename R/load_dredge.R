load_dredge <- function(){
  
  dredge <- st_read("data/shapefiles/dredging/dredgingPoint.shp") %>%
    dplyr::rename(dredge_id = "id") %>%
    dplyr::select(dredge_id, dateyear, country, material,purpose,end_use) %>%
    filter(purpose == "Maintenance dredging") %>%
    distinct(dredge_id, dateyear, .keep_all = T)
  
  return(dredge)
  
}