combine_pressures <- function(){
  
  load("output/mpa_ports_clean.Rdata")
  load("output/mpa_offshore_clean.Rdata")
  load("output/mpa_shipping_clean.Rdata")
  load("output/mpa_fishing_clean.Rdata")
  load("output/mpa_dredge_clean.Rdata")
  load("output/mpa_desalination_clean.Rdata")
  load("output/mpa_aggregate_clean.Rdata")
  load("output/mpa_SAR_clean.Rdata")
  load("output/mpa_gravity_clean.Rdata")

  #Combining all pressures together in one dataframe
  mpa_pressures <- europe_grid %>%
    #Keep only necessary features
    st_drop_geometry() %>%
    dplyr::select(grid_id, LME_NAME, SOVEREIGN1, protected, id, iucn_cat, cell_area,match_status) %>%
    #Join with ports data
    dplyr::left_join(mpa_ports_clean %>% dplyr::select(grid_id, ports_number, port_presence), by = "grid_id") %>%
    #Join with offshore data
    dplyr::left_join(mpa_offshore_clean %>% dplyr::select(grid_id, offshore_number, offshore_presence, oil, other, wind), by = "grid_id") %>%
    #Join with shipping data
    dplyr::left_join(mpa_shipping_clean %>% dplyr::select(grid_id, shipping_boats,shipping_presence), by = "grid_id") %>%
    #Join with fishing data
    dplyr::left_join(mpa_fishing_clean %>% dplyr::select(grid_id, sum_fishing, fishing_presence, mean_length, fishing_density), by = "grid_id") %>%
    #Join with dredge data
    dplyr::left_join(mpa_dredge_clean %>% dplyr::select(grid_id,  dredge_number ,dredge_presence), by = "grid_id") %>%
    #Join with aggregate data
    dplyr::left_join(mpa_aggregate_clean %>% dplyr::select(grid_id, aggregate_number, aggregate_presence), by = "grid_id") %>%
    #Join with desalination data
    dplyr::left_join(mpa_desalination_clean %>% dplyr::select(grid_id, desalination_number, desalination_presence), by = "grid_id") %>%
    #Join with SAR data
    dplyr::left_join(mpa_SAR_clean %>% dplyr::select(grid_id, unmatched_number, unmatched_presence), by = "grid_id") %>%
    #Join with gravity data
    dplyr::left_join(mpa_gravity_clean %>% dplyr::select(grid_id, gravity), by = "grid_id") %>%
    #Combine dredging and aggregate
    mutate(dredge_presence = ifelse(dredge_presence == 1 | aggregate_presence == 1, 1, 0),
           dredge_number = dredge_number + aggregate_number) %>%
    dplyr::select(-c(aggregate_presence, aggregate_number)) %>%
    #Sum of pressures
    rowwise() %>%
    mutate(sum_pressures = sum(as.numeric(desalination_presence), 
                             as.numeric(offshore_presence),
                             as.numeric(port_presence),
                             as.numeric(fishing_presence),
                             as.numeric(dredge_presence),
                             as.numeric(unmatched_presence)-5)) %>%
    #Add a column with the number of protected and unprotected cells
    group_by(match_status) %>%
    mutate(cell_number = n()) %>%
    ungroup() %>%
    #Add a column with the number of protected cells by IUCN category
    group_by(iucn_cat) %>%
    mutate(cell_number_iucn = n()) %>%
    ungroup() %>%
    #Remove slovenia because only unmatched ??
    filter(SOVEREIGN1 != "Slovenia")
  
  save(mpa_pressures, file = "output/mpa_pressures.Rdata")
  
  return(mpa_pressures)
  
}
