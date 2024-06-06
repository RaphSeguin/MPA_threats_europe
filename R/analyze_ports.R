analyze_ports <- function(){
  
  load("output/mpa_ports.Rdata")
  
  #Joining with mpa characteristics
  mpa_ports_clean <- mpa_ports %>%
    # left_join(mpa_eu_wdpa, by = "id") %>%
    #Calculate density
    # mutate(port_density = ports_number/gis_m_area) %>% 
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(ports_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_ports_clean, file = "output/mpa_ports_clean.Rdata")
  
}
