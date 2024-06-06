port_results <- function(){
  
  mpa_pressures_sf <- mpa_pressures %>%
    left_join(europe_grid %>% dplyr::select(grid_id), by = "grid_id") %>%
    st_as_sf()
  
  world <- ne_countries(type = "countries", scale = "large") %>%
    dplyr::mutate(EU = ifelse(sovereignt %in% mpa_pressures$SOVEREIGN1, "Study","Not study"))
  
  #Mapping ports
  test_plot <- ggplot() + 
    geom_sf(data = world, aes(fill = EU)) + 
    scale_fill_manual(values = c("Study"="darkgrey","Not study"="lightgrey")) +
    guides(fill = "none") +
    ggnewscale::new_scale_fill() + 
    geom_sf(data = mpa_pressures_sf, aes(fill = match_status),lwd=0.01) +
    scale_fill_manual(values = legend_match, name = "Cell type") +
    geom_sf(data = mpa_eu_wdpa, fill = "lightblue", color = "red", alpha = 0.1) +
    theme(legend.position = "bottom") + 
    xlim(-20,35) + 
    ylim(33,66) +
    theme_map()
  
  ggsave("figures/map_ports.png", width = 297, height = 210, units = "mm", dpi = 600)
  
  
  
}
