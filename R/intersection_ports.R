intersection_ports <- function(){
  
  #Calculating intersections with ports
  mpa_ports <- st_join(europe_grid, ports) %>%
    dplyr::select(grid_id, id, protected, iucn_cat, index_no, port_name, country, harborsize) %>%
    st_drop_geometry() %>%
    #adding number of ports
    group_by(grid_id) %>%
    mutate(ports_number = ifelse(is.na(index_no), 0, n()),
           #transform to presence or absence
           port_presence = as.factor(ifelse(ports_number >= 1, 1, 0))) %>%
    ungroup()
  
  # #Calculate distance to nearest port from GFW
  # dist_to_port_raster <- raster("data/shapefiles/Ports/distance-from-port-v20201104.tiff")
  # mpa_centers <- st_centroid(mpa_eu_wdpa)
  # distance_to_port <- raster::extract(dist_to_port_raster,mpa_centers, df =T)
  # rm(dist_to_port_raster)
  # 
  # #Creating df of distance only
  # mpa_port_distance <- mpa_eu_wdpa %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(id) %>%
  #   cbind(port_distance = as.numeric(distance_to_port$distance.from.port.v20201104))
  # 
  #Joining with offshore data
  # mpa_ports <- mpa_intersection_ports %>%
    # left_join(mpa_port_distance, by = "id") %>%
    #Set distance to 0 if offshore is inside MPA
    # mutate(port_distance = ifelse(port_presence == 1, 0, port_distance))

  save(mpa_ports, file = "output/mpa_ports.Rdata")
  return(mpa_ports)
  
}