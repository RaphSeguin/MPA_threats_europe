analyze_fishing <- function() {
    
    load("output/mpa_fishing.Rdata")
    
    #Joining with mpa characteristics
    mpa_fishing_clean <- mpa_fishing %>%
      mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
      mutate(apparent_fishing_hours = as.numeric(apparent_fishing_hours)) %>%
      #Group by MPA
      group_by(grid_id,iucn_cat,cell_area) %>%
      reframe(sum_fishing = sum(apparent_fishing_hours),
              sum_fishing = ifelse(is.na(sum_fishing), 0, sum_fishing), 
              fishing_presence = as.factor(ifelse(sum_fishing > 0,1,0)),
              mean_length = mean(length,na.rm=T),
              mean_length = ifelse(is.na(mean_length),0,mean_length),
              number_of_boats = n_distinct(mmsi)) %>%
      ungroup() %>%
      #add density
      mutate(fishing_density = sum_fishing/cell_area) 
    
    save(mpa_fishing_clean, file = "output/mpa_fishing_clean.Rdata")

  
}