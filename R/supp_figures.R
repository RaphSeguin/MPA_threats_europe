supp_figures <- function(){
  
  #Figure S1
  #Map of MPAs included in this study
  europe_sf <- ne_countries(scale = "large") %>%
    st_crop(xmin = -21, xmax = 40, ymin = 32, ymax = 68)
  
  mpa_sf <- mpa_eu_wdpa %>%
    st_crop(xmin = -20, xmax = 40, ymin = 35, ymax = 68)
  
  mpa_pressures_sf <- mpa_pressures %>%
    left_join(europe_grid %>% dplyr::select(grid_id), by = "grid_id") %>%
    st_as_sf()
  
  mpa_pressures_points <- mpa_pressures_sf %>%
    st_centroid()
  
  fig_S1 <- ggplot() + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    geom_sf(data = mpa_sf, fill = "lightblue") +
    annotation_scale() +
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  ggsave(fig_S1, file = "figures/fig_S1.jpg", width = 297, height = 210, units = "mm",dpi= 300)
  
  #Figure S2
  #Map of grids and their matching type
  
  fig_S2 <- ggplot() + 
    geom_sf(data = europe_grid, aes(fill = match_status), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_manual(values = legend_match, name = "Cell type") + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S2, file = "figures/fig_S2.jpg", width = 297, height = 210, units = "mm")
  
  #Figure S3
  #Example of protected versus unportected cells
  
  fig_S3 <- ggplot() + 
    geom_sf(data = europe_grid %>% st_crop(xmin = -5, xmax = 0, ymin = 46, ymax = 50), 
            aes(fill = match_status), lwd = 0.02) + 
    geom_sf(data = europe_sf  %>% st_crop(xmin = -5, xmax = 0, ymin = 46, ymax = 50), 
            fill = "lightgrey") +
    geom_sf(data = mpa_eu_wdpa  %>% st_crop(xmin = -5, xmax = 0, ymin = 46, ymax = 50), 
            fill = "lightblue", alpha = 0.2, color = "red") + 
    scale_fill_manual(values = legend_match, name = "Cell type") + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale() +
    xlim(-5, 0) +
    ylim(46,50)
  
  ggsave(fig_S3, file = "figures/fig_S3.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Figure S4
  #Europe grid according to LMEs
  fig_S4 <- ggplot() + 
    geom_sf(data = europe_grid, aes(fill = LME_NAME), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_viridis_d(name = "Large marine ecosystem") + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S4, file = "figures/fig_S4.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Table S1
  #Reporting results of Wilcoxon test for all things
  
  #Prep data for stat test
  stat_data <- mpa_pressures %>%
    dplyr::select(grid_id, match_status, sum_pressures,
                  sum_fishing, unmatched_number,offshore_number,
                  oil, wind, other,
                  dredge_number, desalination_number, ports_number) %>%
    dplyr::filter(match_status != "Unmatched") %>%
    mutate(match_status = factor(match_status, levels = c("Protected","Matched"))) %>%
    mutate(match_status = droplevels(match_status)) %>%
    na.omit()
  
  #Stat test - sum of industrial pressures
  stat_test <- stat_data %>%
    wilcox_test(sum_pressures ~ match_status, paired = F, conf.level = 0.95,
                detailed=T,alternative = "less") %>%
    add_significance() 
  
  sum_pressures_test = as.data.frame(stat_test) %>%
    mutate(Test = "Sum of industrial pressures", .before = estimate)
  
  #Stat test - industrial fishing
  stat_test_fishing <- stat_data %>%
    filter(sum_fishing > 0) %>%
    wilcox_test(sum_fishing ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_fishing_test = as.data.frame(stat_test_fishing) %>%
    mutate(Test = "Industrial fishing", .before = estimate)
  
  #Stat test - untracked fishing
  stat_test_unmatched <- stat_data %>%
    filter(unmatched_number > 0) %>%
    wilcox_test(unmatched_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_untracked_test = as.data.frame(stat_test_unmatched) %>%
    mutate(Test = "Untracked fishing vessels", .before = estimate)
  
  #Stat test - offshore structures
  stat_test_MAS <- stat_data %>%
    filter(offshore_number > 0) %>%
    wilcox_test(offshore_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_MAS_test = as.data.frame(stat_test_MAS) %>%
    mutate(Test = "Marine artificial structures", .before = estimate)
  
  #Stat test - OIL
  stat_test_oil <- stat_data %>%
    filter(oil > 0) %>%
    wilcox_test(oil ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_oil_test = as.data.frame(stat_test_oil) %>%
    mutate(Test = "Oil and gas infrastructures", .before = estimate)
  
  #Stat test - wind
  stat_test_wind <- stat_data %>%
    filter(wind > 0) %>%
    wilcox_test(wind ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_wind_test = as.data.frame(stat_test_wind) %>%
    mutate(Test = "Offshore wind infrastructures", .before = estimate)
  
  #Stat test - other
  stat_test_other <- stat_data %>%
    filter(other > 0) %>%
    wilcox_test(other ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_other_test = as.data.frame(stat_test_other) %>%
    mutate(Test = "Other marine infrastructures", .before = estimate)
  
  #Stat test - dredge
  stat_test_dredge <- stat_data %>%
    filter(dredge_number > 0) %>%
    wilcox_test(dredge_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_dredge_test = as.data.frame(stat_test_dredge) %>%
    mutate(Test = "Dredging and aggregate extraction", .before = estimate)
  
  #Stat test - desalination
  stat_test_desalination <- stat_data %>%
    filter(desalination_number > 0) %>%
    wilcox_test(desalination_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_desalination_test = as.data.frame(stat_test_desalination) %>%
    mutate(Test = "Desalination plants", .before = estimate)
  
  #Stat test - ports
  stat_test_ports <- stat_data %>%
    filter(ports_number > 0) %>%
    wilcox_test(ports_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "less") %>%
    add_significance() 
  
  sum_ports_test = as.data.frame(stat_test_ports) %>%
    mutate(Test = "Maritime ports", .before = estimate)
  
  table_S1 <- rbind(sum_pressures_test,
                    sum_fishing_test,
                    sum_untracked_test,
                    sum_MAS_test,
                    sum_oil_test,
                    sum_wind_test,
                    sum_other_test,
                    sum_dredge_test,
                    sum_desalination_test,
                    sum_ports_test)%>%
    mutate_if(is.numeric, round, 4)
  
  write.csv(table_S1, file = "figures/table_S1_less.csv")
  
  #GREATER
  
  #Stat test - sum of industrial pressures
  stat_test <- stat_data %>%
    wilcox_test(sum_pressures ~ match_status, paired = F, conf.level = 0.95,
                detailed=T,alternative = "greater") %>%
    add_significance() 
  
  sum_pressures_test = as.data.frame(stat_test) %>%
    mutate(Test = "Sum of industrial pressures", .before = estimate)
  
  #Stat test - industrial fishing
  stat_test_fishing <- stat_data %>%
    filter(sum_fishing > 0) %>%
    wilcox_test(sum_fishing ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_fishing_test = as.data.frame(stat_test_fishing) %>%
    mutate(Test = "Industrial fishing", .before = estimate)
  
  #Stat test - untracked fishing
  stat_test_unmatched <- stat_data %>%
    filter(unmatched_number > 0) %>%
    wilcox_test(unmatched_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_untracked_test = as.data.frame(stat_test_unmatched) %>%
    mutate(Test = "Untracked fishing vessels", .before = estimate)
  
  #Stat test - offshore structures
  stat_test_MAS <- stat_data %>%
    filter(offshore_number > 0) %>%
    wilcox_test(offshore_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_MAS_test = as.data.frame(stat_test_MAS) %>%
    mutate(Test = "Marine artificial structures", .before = estimate)
  
  #Stat test - OIL
  stat_test_oil <- stat_data %>%
    filter(oil > 0) %>%
    wilcox_test(oil ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_oil_test = as.data.frame(stat_test_oil) %>%
    mutate(Test = "Oil and gas infrastructures", .before = estimate)
  
  #Stat test - wind
  stat_test_wind <- stat_data %>%
    filter(wind > 0) %>%
    wilcox_test(wind ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_wind_test = as.data.frame(stat_test_wind) %>%
    mutate(Test = "Offshore wind infrastructures", .before = estimate)
  
  #Stat test - other
  stat_test_other <- stat_data %>%
    filter(other > 0) %>%
    wilcox_test(other ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_other_test = as.data.frame(stat_test_other) %>%
    mutate(Test = "Other marine infrastructures", .before = estimate)
  
  #Stat test - dredge
  stat_test_dredge <- stat_data %>%
    filter(dredge_number > 0) %>%
    wilcox_test(dredge_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_dredge_test = as.data.frame(stat_test_dredge) %>%
    mutate(Test = "Dredging and aggregate extraction", .before = estimate)
  
  #Stat test - desalination
  stat_test_desalination <- stat_data %>%
    filter(desalination_number > 0) %>%
    wilcox_test(desalination_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_desalination_test = as.data.frame(stat_test_desalination) %>%
    mutate(Test = "Desalination plants", .before = estimate)
  
  #Stat test - ports
  stat_test_ports <- stat_data %>%
    filter(ports_number > 0) %>%
    wilcox_test(ports_number ~ match_status, paired = F, conf.level = 0.95,detailed=T,
                alternative = "greater") %>%
    add_significance() 
  
  sum_ports_test = as.data.frame(stat_test_ports) %>%
    mutate(Test = "Maritime ports", .before = estimate)
  
  table_S2 <- rbind(sum_pressures_test,
                    sum_fishing_test,
                    sum_untracked_test,
                    sum_MAS_test,
                    sum_oil_test,
                    sum_wind_test,
                    sum_other_test,
                    sum_dredge_test,
                    sum_desalination_test,
                    sum_ports_test) %>%
    mutate_if(is.numeric, round, 4)
  
  write.csv(table_S2, file = "figures/table_S2_greater.csv")

  #GREATER 
  
  #Figure S5
  #Distribution of the number of industrial activities across cells
  
  fig_S5 <-ggplot(mpa_pressures,aes(sum_pressures, fill = match_status)) +
    geom_histogram(binwidth = 0.5) +
    facet_wrap(~ match_status) +
    scale_fill_manual(values = legend_match, name = "Cell type") + 
    ggthemes::theme_clean(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") +
    labs(y = "Number of cells",
         x = "Number of industrial activities")
  
  ggsave(fig_S5, file = "figures/fig_S5.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Figure S6
  #Number of industrial activities per country
  
  fig_S6 <- mpa_pressures %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(average_pressure = mean(sum_pressures, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(average_pressure_LME = mean(sum_pressures, na.rm =T )) %>%
    ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(reorder(SOVEREIGN1,average_pressure_LME), average_pressure, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of industrial activities in cell",
         x = " ")
  
  ggsave(fig_S6, file = "figures/fig_S6.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Figure S7
  
  #Map of industrial fishing presence across europe

  fig_S7 <- ggplot() + 
    geom_sf(data = mpa_pressures_sf, aes(fill = fishing_presence), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_manual(values = c(`0` = "#629AD5", 
                                 `1` = "#C4672D"),
                      name = "Presence of industrial fishing",
                      labels=c("No fishing","Fishing"))+ 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S7, file = "figures/fig_S7.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S8
  
  #Map of industrial fishing pressure across europe
  fig_S8 <- ggplot() + 
    geom_sf(data = mpa_pressures_sf, aes(fill = log10(sum_fishing+1)), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_gradientn(colours = colours, 
                      name = "Fishing hours (log10+1)") + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S8, file = "figures/fig_S8.jpg", width = 297*1.5, height = 210*1.5, units = "mm", dpi= 300)
  
  #Figure S9
  
  #Distribution of industrial fishing across countries
  fig_S9 <- mpa_pressures %>%
    filter(sum_fishing > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(fishing_pressure = mean(sum_fishing, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(average_fishing_pressure = mean(sum_fishing, na.rm =T )) %>%
    ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(reorder(SOVEREIGN1,average_fishing_pressure), fishing_pressure, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of fishing hours per cell",
         x = " ")
  
  ggsave(fig_S9, file = "figures/fig_S9.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S10
  
  #Map of untracked fishing vessels across europe

  fig_S10 <- ggplot() + 
    geom_sf(data = mpa_pressures_sf %>% filter(SOVEREIGN1 != "Finland"), aes(fill = unmatched_presence), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_manual(values = c(`0` = "#629AD5", 
                                 `1` = "#C4672D"),
                      name = "Presence of untracked fishing vessels",
                      labels=c("No untracked vessels","Untracked vessels"))+ 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S10, file = "figures/fig_S10.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S11
  
  #Map of unmatched vessel number across europe
  fig_S11 <- ggplot() + 
    geom_sf(data = mpa_pressures_sf  %>% filter(SOVEREIGN1 != "Finland"), 
            aes(fill = log10(unmatched_number+1)), lwd = 0.02) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_fill_gradientn(colours = colours, 
                         name = "Sum of untracked fishing vessels (log10+1)") + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S11, file = "figures/fig_S11.jpg", width = 297*1.5, height = 210*1.5, units = "mm", dpi= 300)
  
  #Figure S12
  
  #Distribution of unmatched vessel number across europe
  fig_S12 <- mpa_pressures %>%
    filter(unmatched_number > 0) %>%
    filter(SOVEREIGN1 != "Finland") %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(untracked_average = mean(unmatched_number, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(untracked_average_country = mean(unmatched_number, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(reorder(SOVEREIGN1,untracked_average_country), untracked_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of untracked fishing vessels per cell",
         x = " ")
  
  ggsave(fig_S12, file = "figures/fig_S12.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Fig S13
  
  #Distribution of offshore structures
  mybreaks <- c(0.5,1,1.5,2)
  
  fig_S13 <-  ggplot() + 
    geom_sf(data = mpa_pressures_points %>%
              filter(offshore_number > 0), aes(color = log10(offshore_number+1),
                                             size = log10(offshore_number+1)), stroke = F,
            shape = 20) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_size_continuous(
      name = "Number of MAS", 
      range = c(0.5, 9),
      breaks = mybreaks
    ) +
    scale_color_viridis_c(
      option = "magma", 
      breaks = mybreaks,
      name = "Number of MAS"
    ) +
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S13, file = "figures/fig_S13.jpg", width = 297*1.5, height = 210*1.5, units = "mm", dpi= 300)
  
  #Figure S14
  
  #Distribution of MAS number across europe
  fig_S14 <- mpa_pressures %>%
    filter(offshore_number > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(MAS_average = mean(offshore_number, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(MAS_average_country = mean(offshore_number, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(reorder(SOVEREIGN1,MAS_average_country), MAS_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number marine artificial structures per cell",
         x = " ")
  
  ggsave(fig_S14, file = "figures/fig_S14.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Distribution of MAS structures across europe
  fig_S15_1 <- mpa_pressures %>%
    filter(oil > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(MAS_average = mean(oil, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(MAS_average_country = mean(oil, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(SOVEREIGN1, MAS_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 13,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of oil and gas infrastructures per cell",
         x = " ")
  
  fig_S15_2 <- mpa_pressures %>%
    filter(wind > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(MAS_average = mean(wind, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(MAS_average_country = mean(wind, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(SOVEREIGN1, MAS_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 13,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Offshore wind infrastructures",
         x = " ") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  fig_S15_3 <- mpa_pressures %>%
    filter(other > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(MAS_average = mean(other, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(MAS_average_country = mean(other, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(SOVEREIGN1, MAS_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 13,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Other infrastructures",
         x = " ") +
    theme(axis.title.y =element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  fig_S15 <- ggarrange(fig_S15_1,
            fig_S15_2,
            fig_S15_3, nrow = 1, ncol = 3, common.legend = T)
  
  ggsave(fig_S15, file = "figures/fig_S15.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Fig S16
  
  #Map of dredging across europe
  

  mybreaks = c(0.5, 1, 1.5,2)
  
  fig_S16 <- ggplot() + 
    geom_sf(data = mpa_pressures_points %>%
              filter(dredge_number > 0), aes(color = log10(dredge_number+1),
                                             size = log10(dredge_number+1)), stroke = F,
            shape = 20) + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    scale_size_continuous(
      name = "Number of dredging points", 
      range = c(0.5, 10),
      breaks = mybreaks
    ) +
    scale_color_viridis_c(
      option = "magma", 
      breaks = mybreaks,
      name = "Number of dredging points"
    ) +
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S16, file = "figures/fig_S16.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S17
  
  #Distribution of unmatched vessel number across europe
  fig_S17 <- mpa_pressures %>%
    filter(dredge_number > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(dredge_average = mean(dredge_number, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(dredge_average_country = mean(dredge_number, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(reorder(SOVEREIGN1,dredge_average_country), dredge_average, fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of dredging/aggregate extraction per cell",
         x = " ")
  
  ggsave(fig_S17, file = "figures/fig_S17.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S18
  
  #Map of desalination plants across europe
  
  fig_S18 <- ggplot() + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    geom_sf(data = mpa_pressures_points %>%
              filter(desalination_number > 0), size = 3,
            shape = 20) + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S18, file = "figures/fig_S18.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S19
  #Distribution of desalination plants number across europe
  fig_S19 <- mpa_pressures %>%
    filter(desalination_number > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(desalination_average = mean(desalination_number, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(desalination_average_country = mean(desalination_number, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(reorder(SOVEREIGN1,desalination_average_country), desalination_average, 
               fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of desalination plants per cell",
         x = " ")
  
  ggsave(fig_S19, file = "figures/fig_S19.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S20
  
  #Map of maritime ports across europe
  fig_S20 <- ggplot() + 
    geom_sf(data = europe_sf, fill = "lightgrey") + 
    geom_sf(data = mpa_pressures_points %>%
              filter(ports_number > 0), size = 2,
            shape = 20) + 
    ggthemes::theme_map(base_size = 14,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    annotation_scale()
  
  ggsave(fig_S20, file = "figures/fig_S20.jpg", width = 297, height = 210, units = "mm", dpi= 300)
  
  #Figure S21
  #Distribution of maritime ports across europe
  
  fig_S21 <- mpa_pressures %>%
    filter(ports_number > 0) %>%
    #Group by match status
    group_by(SOVEREIGN1,match_status) %>%
    mutate(port_average = mean(ports_number, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(SOVEREIGN1) %>%
    mutate(port_average_country = mean(ports_number, na.rm =T )) %>%
    ungroup() %>% 
    ggplot(aes(reorder(SOVEREIGN1,port_average_country), port_average, 
               fill = match_status, group = match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of maritime ports per cell",
         x = " ")
  
  ggsave(fig_S21, file = "figures/fig_S21.jpg", width = 297, height = 210, units = "mm", dpi= 300)

}

