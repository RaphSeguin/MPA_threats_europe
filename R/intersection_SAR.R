intersection_SAR <- function(){
  
  #Load SAR study area
  #downloaded from nature paper
  study_area <- read.csv("data/study_area.csv") %>%
    dplyr::select(study_area) %>%
    # pivot_longer(cols = c(study_area,study_area_02,study_area_05)) %>%
    dplyr::rename(geometry = "study_area") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    head(18713) 
  
  #Now intersect with SAR data
  SAR_data_sf <- SAR_data %>%
    #Keeping only year 2021
    filter(year == 2021) %>%
    #Keeping only unmatched fishing vessels
    #If 80% sure that boat is fishing, it is unmatched and fishing
    mutate(matched_category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
    filter(matched_category == "unmatched_fishing") %>%
    #Also if unmatched_fishing and length higher than 80% quantile then delete it 
    filter(length_m < 145) %>%
    filter(length_m >= 15) %>%
    #Transform to sf
    st_as_sf(coords = c("lon","lat"), crs = 4326) 
  
  #Intersecting with SAR_data
  mpa_SAR <- st_join(europe_grid, SAR_data_sf)
  
  mpa_SAR <- mpa_SAR %>% 
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    mutate(unmatched_presence = as.factor(ifelse(SOVEREIGN1 == "Finland",0,
                                     ifelse(is.na(unique_id),0,1))),
           unmatched_number = ifelse(SOVEREIGN1 == "Finland",NA,
                                     ifelse(unmatched_presence ==  1,n(),0))) %>%
    ungroup()
  
  save(mpa_SAR, file = "output/mpa_SAR.Rdata")
  
}