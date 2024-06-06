figure_2 <- function(){
  
  #FIGURE_2_A 
  #Plot average threats by LME/matched
  figure_2_A <- mpa_pressures %>%
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
    theme_minimal(base_size = 17,
              base_family = "Times New Roman") +
    theme(legend.position = "bottom") + 
    labs(y = "Average number of industrial activities in cell",
         x = " ")
  
  ggsave(figure_2_A, file = "figures/figure_2_A.jpg", width = 297, height = 210, units = "mm")
  
  #FIGURE_2_B
  
   #For each activity, percentage of cells with no activity
  mpa_percentage <- mpa_pressures %>%
    #Group by activity and protection status
    #For ports
    group_by(match_status, port_presence) %>%
    mutate(cells_with_ports = n(),
           cells_with_ports_percentage = cells_with_ports/cell_number*100) %>%
    ungroup() %>%
    #For shipping
    # group_by(match_status, shipping_presence) %>%
    # mutate(cells_with_shipping = n(),
    #        cells_with_shipping_percentage = cells_with_shipping/cell_number*100) %>%
    ungroup() %>%
    #For desalination
    group_by(match_status, desalination_presence) %>%
    mutate(cells_with_desalination = n(),
           cells_with_desalination_percentage = cells_with_desalination/cell_number*100) %>%
    ungroup() %>%
    #For offshore
    group_by(match_status, offshore_presence) %>%
    mutate(cells_with_offshore = n(),
           cells_with_offshore_percentage = cells_with_offshore/cell_number*100) %>%
    ungroup() %>%
    #For fishing
    group_by(match_status, fishing_presence) %>%
    mutate(cells_with_fishing = n(),
           cells_with_fishing_percentage = cells_with_fishing/cell_number*100) %>%
    ungroup() %>%
    #For dredge
    group_by(match_status, dredge_presence) %>%
    mutate(cells_with_dredge = n(),
           cells_with_dredge_percentage = cells_with_dredge/cell_number*100) %>%
    ungroup() %>%
    # #For aggregate
    # group_by(match_status, aggregate_presence) %>%
    # mutate(cells_with_aggregate = n(),
    #        cells_with_aggregate_percentage = cells_with_aggregate/cell_number*100) %>%
    # ungroup() %>%
    #For unmatched
    group_by(match_status, unmatched_presence) %>%
    mutate(cells_with_unmatched = n(),
           cells_with_unmatched_percentage = cells_with_unmatched/cell_number*100) %>%
    ungroup() 
  
  #Percentage of cells with each activity
  
  #ports
  (plot_ports_percentage <- mpa_percentage %>%
    filter(port_presence == 1) %>%
    mutate(activity = "ports") %>%
    distinct(match_status,cells_with_ports_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_ports_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_histogram(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Maritime ports",
         x = " ",
         y = "Percentage of cells with activity") +
    guides(fill = "none"))
  
  #shipping
  # (plot_shipping_percentage <- mpa_percentage %>%
  #   filter(shipping_presence == 1) %>%
  #   mutate(activity = "shipping") %>%
  #   distinct(match_status,cells_with_shipping_percentage,activity) %>%
  #   ggplot(aes(match_status, cells_with_shipping_percentage,fill=activity)) +
  #   # scale_x_discrete(labels = c("Unprotected","Protected")) + 
  #   scale_fill_manual(values = legend_activities) + 
  #   geom_bar(stat = "identity") +
  #   theme_minimal() +
  #   labs(title = "Shipping",
  #        x = " ",
  #        y = "Percentage of cells with activity") +
  #   guides(fill = "none"))
  
  #Desalination
  (plot_desalination_percentage <- mpa_percentage %>%
    filter(desalination_presence == 1) %>%
    mutate(activity = "desalination") %>%
    distinct(match_status,cells_with_desalination_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_desalination_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Desalination plants",
         x = " ",
         y = "Percentage of cells with activity") +
    guides(fill = "none"))
  
  #Offshore
  (plot_offshore_percentage <- mpa_percentage %>%
    filter(offshore_presence == 1) %>%
    mutate(activity = "offshore") %>%
    distinct(match_status,cells_with_offshore_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_offshore_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) +
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Marine artificial structures",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none"))
  
  #Fishing
  (plot_fishing_percentage <- mpa_percentage %>%
    filter(fishing_presence == 1) %>%
    mutate(activity = "fishing") %>%
    distinct(match_status,cells_with_fishing_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_fishing_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Industrial fishing",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none"))
    
  #Dredge
  (plot_dredge_percentage <- mpa_percentage %>%
    filter(dredge_presence == 1) %>%
    mutate(activity = "dredge") %>%
    distinct(match_status,cells_with_dredge_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_dredge_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Dredge and aggregate",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none"))
  
  #Unmatched
  (plot_unmatched_percentage <- mpa_percentage %>%
    filter(unmatched_presence == 1) %>%
    mutate(activity = "unmatched") %>%
    distinct(match_status,cells_with_unmatched_percentage,activity) %>%
    ggplot(aes(match_status, cells_with_unmatched_percentage,fill=activity)) +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
      theme_minimal(base_size = 17,
                    base_family = "Times New Roman") +
    labs(title = "Untracked fishing vessels",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none"))
  
  #Aggregate
  # (plot_aggregate_percentage <- mpa_percentage %>%
  #   filter(aggregate_presence == 1) %>%
  #   mutate(activity = "aggregate") %>%
  #   distinct(match_status,cells_with_aggregate_percentage,activity) %>%
  #   ggplot(aes(match_status, cells_with_aggregate_percentage,fill=activity)) +
  #   # scale_x_discrete(labels = c("Unprotected","Protected")) + 
  #   scale_fill_manual(values = legend_activities) + 
  #   geom_bar(stat = "identity") +
  #   theme_minimal() +
  #   labs(title = "Aggregate",
  #        x = " ",
  #        y = "Percentage of cells with activity")+
  #   guides(fill = "none"))
  
  (figure_2_B <- (plot_ports_percentage + plot_desalination_percentage + plot_offshore_percentage) /
    (plot_fishing_percentage + plot_dredge_percentage + plot_unmatched_percentage) +
    plot_layout(guides = "collect", widths = c(1,25)) &
    theme(axis.title.y = element_blank()))
  
  #Y axis
  (y_axis <- ggplot(data.frame(l = "Percentage of cells with activity", x = 1, y = 1)) +
    geom_text(aes(x, y, label = l), angle = 90, family = "Times New Roman",size = 7) + 
    theme_void() +
    coord_cartesian(clip = "off"))
  
  figure_2_B <- y_axis + figure_2_B + plot_layout(widths = c(1, 25)) 
  
  ggsave(figure_2_B, file = "figures/figure_2_B.jpg", width = 297, height = 210, units = "mm")
  

}