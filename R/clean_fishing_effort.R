clean_fishing_effort <- function(fishing_effort_2023){
  
  fishing_effort_clean <- fishing_effort_2023 %>%
    left_join(vessel_registry, by = "mmsi") %>%
    mutate(mmsi = as.factor(mmsi),
           country = as.factor(country),
           gear = as.factor(gear),
           gear_cat = as.factor(gear_cat)) %>%
    na.omit()
  
  save(fishing_effort_clean, file = "output/fishing_effort_clean.Rdata")
  
  return(fishing_effort_clean)
    
  
}