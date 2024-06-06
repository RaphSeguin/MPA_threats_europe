prep_mpa_data <- function(){
  
  ### Load MPAs from World Database on Protected Areas (WDPA)
  #Download data source here 
  #https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
  mpa_eu_wdpa <- bind_rows(st_read(dsn = "data/shapefiles/MPA/WDPA_Oct2023_Public_shp_0",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "data/shapefiles/MPA/WDPA_Oct2023_Public_shp_1",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "data/shapefiles/MPA/WDPA_Oct2023_Public_shp_2",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE)) %>%
    clean_names() %>%
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    filter(!status %in% c("Proposed","Established","Not Reported")) %>%
    filter(status_yr < 2023) %>%
    #No MPAs which are outside EEZs
    filter(iso3 != "ABNJ") %>%
    dplyr::select(id= wdpaid, name, desig_eng, iucn_cat, status_yr, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine,status) %>%
    filter(iso3 %in% c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","FRA;ITA;MCO","GRC","HUN","IRL","ITA",
                       "LVA","LTU","LUX","MLT","NLD","POL","PRT","CZE","ROU","SVK","SVN","SWE")) 
  
  # #Keeping only MPAs which are in SAR study range
  # 
  # #Now keeping only MPAs which fall withing range of study area
  # #downloaded from nature paper
  # study_area <- read.csv("data/study_area.csv") %>%
  #   dplyr::select(study_area) %>%
  #   # pivot_longer(cols = c(study_area,study_area_02,study_area_05)) %>%
  #   dplyr::rename(geometry = "study_area") %>%
  #   st_as_sf(wkt = "geometry", crs = 4326) %>%
  #   head(18713) 
  # 
  # mpa_wdpa_study_area <- st_intersection(mpa_eu_wdpa, study_area) 
  # 
  # mpa_eu_wdpa <- mpa_wdpa_study_area %>%
  #   #Only keeping MPAs with more than 1km^2 surface
  #   #Calculating new area
  #   mutate(area_correct = set_units(st_area(mpa_wdpa_study_area),km^2),
  #          area_correct = as.numeric(area_correct)) %>%
  #   #Bigger than 1km^2
  #   filter(area_correct > 1) %>%
  #   distinct(id, .keep_all = T) %>%
  #   st_make_valid()
  # 
  save(mpa_eu_wdpa, file = "output/mpa_eu_wdpa.Rdata")
  
  return(mpa_eu_wdpa)
  
}