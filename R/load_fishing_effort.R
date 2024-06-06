load_fishing_effort <- function(){
  
  # Loading fishing effort
  fishing_effort <- list.files(path = "data/shapefiles/fishing_effort/",
                               pattern="*.csv",
                               full.names = T) %>%
    map_df(~read_csv(., id = "name_fishing", col_types = cols(.default = "c"))) %>%
    mutate(name_fishing = sub('.*/', '', name_fishing)) %>%
    mutate(name_fishing = sub("\\..*", "", name_fishing)) %>%
    clean_names() %>%
    mutate(mmsi = as.factor(mmsi)) %>%
    mutate(unique_id = paste0(name_fishing,time_range,mmsi,entry_timestamp, exit_timestamp,apparent_fishing_hours,lat,lon)) 
  

  return(fishing_effort)
  
}