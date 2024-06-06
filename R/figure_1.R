figure_1 <- function(){
  
  europe_sf <- ne_countries(scale = "large") %>%
    st_crop(xmin = -21, xmax = 40, ymin = 32, ymax = 68)
  
  mpa_sf <- mpa_eu_wdpa %>%
    st_crop(xmin = -20, xmax = 40, ymin = 35, ymax = 68)
  
  mpa_pressures_sf <- mpa_pressures %>%
    left_join(europe_grid %>% dplyr::select(grid_id), by = "grid_id") %>%
    st_as_sf()
  
  color_gradient <- c("#c4e6c3","#6dbc90","#4da284","#36877a","#266b6e","#1d4f60")
  
  #Figure 1 
  figure_1_A <- ggplot() +
    geom_sf(data = mpa_pressures_sf %>% filter(!is.na(sum_pressures)), aes(fill = sum_pressures),lwd = 0.01) + 
    scale_fill_gradientn(colours = colours,
                         guide = guide_legend(
                           title.position = "top"
                         )) +
    geom_sf(data = europe_sf, fill = "darkgrey") +
    geom_sf(data = mpa_sf, color = "red", alpha = 0, lwd = 0.1) +
    theme_map(base_size = 17,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(fill = "Number of industrial activities in cell",
         title = "A") +
    coord_sf()
  
  ggsave("figures/figure_1_A.jpg", width = 350, height = 320, units = "mm", dpi = 600)
  
  #Wilcoxon test
  stat_data <- mpa_pressures %>%
    dplyr::select(grid_id, match_status, sum_pressures,
                  sum_fishing, unmatched_number,offshore_number,
                  oil, wind, other,
                  dredge_number, desalination_number, ports_number) %>%
    dplyr::filter(match_status != "Unmatched") %>%
    mutate(match_status = factor(match_status, levels = c("Protected","Matched"))) %>%
    mutate(match_status = droplevels(match_status)) %>%
    na.omit()
  
  stat_test <- stat_data %>%
    wilcox_test(sum_pressures ~ match_status, alternative = "greater")
  
  #Fig 2A
  (figure_1_B <- ggplot() + 
    geom_jitter(data = mpa_pressures, aes(match_status, sum_pressures),alpha = 0.1, shape = ".") +
    geom_boxplot(data = mpa_pressures, aes(match_status, sum_pressures, fill = match_status), alpha = 0.8) +
      scale_fill_manual(values = legend_match,
                        guide = guide_legend(
                          title.position = "top"
                        )) +
      theme_minimal(base_size = 17,
                base_family = "Times New Roman") +
      theme(legend.position = "bottom") + 
      labs(x = " ",
           y = "Number of industrial activities in cell",
           subtitle = get_test_label(stat_test, detailed = TRUE),
           fill = "Cell type",
           title = "B")) 
  
  ggsave("figures/figure_1_B.jpg", width = 297, height = 200, units = "mm", dpi = 600)
  
  
}
