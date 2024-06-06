load_registry <- function(){
  
  fleet_registry <- read_rds("data/fleet_registry/clean_fleet_register_20230626.rds") %>%
    clean_names() %>%
    dplyr::select(country, mmsi, gear, gear_cat, length) %>%
    distinct(mmsi, .keep_all = T)
  
}
