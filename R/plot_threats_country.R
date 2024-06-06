plot_threats_LME <- function(mpa_pressures){
  
  #Plot average threats by LME/matched
  mpa_pressures %>%
    #Group by match status
    group_by(LME_NAME,match_status) %>%
    mutate(average_pressure = mean(sum_pressures, na.rm =T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(LME_NAME) %>%
    mutate(average_pressure_LME = mean(sum_pressures, na.rm =T )) %>%
    ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(LME_NAME, average_pressure, fill = match_status, group = match_status)) + 
      geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
      scale_fill_manual(values = legend_match,name = "Cell type") +
      coord_flip() +
      theme_minimal() + 
      theme(legend.position = "bottom") +
      labs(y = "Average number of industrial activities in cell",
           x = " ",
           title = "A")
  
   #Plot average threats by LME/IUCN
  mpa_pressures %>%
    #Group by match status
    group_by(LME_NAME,iucn_cat) %>%
    mutate(average_pressure = mean(sum_pressures, na.rm = T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    group_by(LME_NAME) %>%
    mutate(average_pressure_LME = mean(sum_pressures, na.rm = T )) %>%
    ungroup() %>% 
    mutate(iucn_cat = factor(iucn_cat, levels = level_order)) %>%
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(reorder(LME_NAME,average_pressure_LME), average_pressure, fill = iucn_cat,group=iucn_cat)) + 
    geom_col(stat="identity",position = "dodge" ,color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend,name = "Cell type") +
    # scale_x_discrete(labels = labels) + 
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of industrial activities in cell",
         x = " ",
         title = "A")
  
}