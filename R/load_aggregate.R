load_aggregate <- function(){
  
  #load aggregate data
  aggregate <- st_read("data/shapefiles/dredging/aggregatesPoint.shp") %>%
    dplyr::select(gid, dateyear, material_aggregate = extractio0, purpose, end_use_aggregate = end_use) %>%
    distinct(gid, dateyear, .keep_all = T)
  
  return(aggregate)

}