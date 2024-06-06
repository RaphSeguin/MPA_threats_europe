create_grid <- function(){
  
    #make grid
    eez_merge <- eez %>% st_as_sf() %>% st_make_valid() %>% st_transform(crs = "+proj=utm +zone=32 +ellps=WGS84")
    
    # Define grid cell dimensions
    cell_size <- 5000 # 5km in meters
    
    # find which grid points intersect `polygons` (countries) 
    # and create an index to subset from
    grd <- st_make_grid(eez_merge, cellsize = cell_size, square = T)
    index <- which(lengths(st_intersects(grd, eez_merge)) > 0)
    
    # subset the grid to make a fishnet
    grid <- grd[index]
    
    europe_grid <- grid %>%
      st_as_sf() %>%
      st_transform(crs = 4326) %>%
      mutate(grid_id = seq_along(1:length(grid))) 
    
    # save(europe_grid_5km, file = "output/europe_grid_5km.Rdata")

  # Perform spatial join
  # grid_with_mpa <- st_join(europe_grid, mpa_eu_wdpa %>% dplyr::select(id, iucn_cat), join = st_intersects)
  
  intersection <- st_intersection(europe_grid, mpa_eu_wdpa)

  # Calculate area of each intersection in km^2
  grid_mpas_intersect <- intersection %>%
    mutate(intersect_area_km2 = as.numeric(set_units(st_area(.),km^2))) %>%
    st_drop_geometry()# Convert m^2 to km^2
  
  #determine protection status in each cell
  grid_protection <- europe_grid %>%
    left_join(
      grid_mpas_intersect %>%
        group_by(grid_id) %>%
        summarise(total_intersect_area_km2 = sum(intersect_area_km2)),
      by = "grid_id"
    ) %>%
    mutate(protected = if_else(total_intersect_area_km2 > 12.5, TRUE, FALSE, missing = FALSE))
  
  #Assign the highest IUCN category for overlapping MPAs:
  grid_mpas_iucn <- grid_mpas_intersect %>%
    group_by(grid_id) %>%
    slice(which.min(match(iucn_cat, c("Ia", "Ib", "II", "III", "IV", "V", "VI","Not Assigned","Not Reported","Not Applicable")))) %>%
    ungroup()

  final_grid <- grid_protection %>%
    left_join(
      grid_mpas_iucn %>%
        dplyr::select(grid_id, id, iucn_cat,status_yr),
      by = "grid_id"
    ) 
  
  # #If more than one MPA then keep strictest one 
  # grid_with_mpa <- grid_with_mpa 
  # 
  # # Add a column indicating if each square is protected or not
  # europe_grid$protected <- FALSE
  # europe_grid$protected[europe_grid$grid_id %in% grid_with_mpa$grid_id] <- TRUE
  # 
  # # Add MPA ID and IUCN category for protected squares
  # europe_grid$id <- NA
  # europe_grid$iucn_cat <- NA
  # europe_grid$id[europe_grid$grid_id %in% grid_with_mpa$grid_id] <- grid_with_mpa$id
  # europe_grid$iucn_cat[europe_grid$grid_id %in% grid_with_mpa$grid_id] <- grid_with_mpa$iucn_cat
  
  #Cleaning
  europe_grid <- final_grid %>%
    mutate(iucn_cat = as.factor(as.character(ifelse(is.na(iucn_cat), "Unprotected",
                             ifelse(iucn_cat %in% c("Not Assigned","Not Reported","Not Applicable"),"No_IUCN_cat",
                                    ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat))))))
  
  #Add area
  cell_area <- set_units(st_area(europe_grid %>% st_transform(crs = 3035)),km^2)
  europe_grid$cell_area = cell_area
  
  #Grid with country
  grid_with_country <- st_join(europe_grid, eez %>% dplyr::select(SOVEREIGN1), join = st_intersects) 
  
  europe_grid <- grid_with_country %>% 
    distinct(grid_id, .keep_all = T) %>%
    filter(!is.na(SOVEREIGN1))
  
  #Associate with LMEs
  europe_grid <- europe_grid %>%
    st_join(LMEs, join = st_nearest_feature) %>%
    filter(LME_NAME != "Black Sea")
  
  save(europe_grid, file = "output/europe_grid.Rdata")
  return(europe_grid)
  
}