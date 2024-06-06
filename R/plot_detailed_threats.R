plot_detailed_threats <- function(){
  
  #Plot detailed number of ports
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_ports = mean(ports_number)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_ports_LME = mean(ports_number)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_ports,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of ports in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of offshore
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_offshore = mean(offshore_number)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_offshore_LME = mean(offshore_number)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_offshore,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of offshore structures in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of shipping
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_shipping = mean(shipping_boats)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_shipping_LME = mean(shipping_boats)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_shipping,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of shipping boats in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of shipping
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_fishing = mean(sum_fishing)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_fishing_LME = mean(sum_fishing)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_fishing,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of fishing hours in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of dredging
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_dredge = mean(dredge_number,na.rm=T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_dredge_LME = mean(dredge_number)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_dredge,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of dredge in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of aggregate
  # mpa_pressures %>%
  #   #Group by match status
  #   group_by(match_status) %>%
  #   mutate(average_number_of_aggregate = mean(aggregate_number)) %>%
  #   ungroup() %>% 
  #   #Get ecossystem average presure
  #   # group_by(LME_NAME) %>%
  #   # mutate(average_number_of_aggregate_LME = mean(aggregate_number)) %>%
  #   # ungroup() %>% 
  #   # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
  #   ggplot(aes(match_status, average_number_of_aggregate,
  #              fill = match_status,group=match_status)) + 
  #   geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
  #   scale_fill_manual(values = legend_match,name = "Cell type") +
  #   coord_flip() +
  #   theme_minimal() + 
  #   theme(legend.position = "bottom") +
  #   labs(y = "Average number of aggregate in cell",
  #        x = " ",
  #        title = "A")
  
  #Plot detailed number of desalination
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_desalination = mean(desalination_number)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_desalination_LME = mean(desalination_number)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_desalination,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of desalination in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of unmatched
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_number_of_unmatched = mean(unmatched_number,na.rm=T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_unmatched_LME = mean(unmatched_number)) %>%
    # ungroup() %>% 
    dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_number_of_unmatched,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average number of unmatched fishing vessels in cell",
         x = " ",
         title = "A")
  
  #Plot detailed number of gravity
  mpa_pressures %>%
    #Group by match status
    group_by(match_status) %>%
    mutate(average_gravity = mean(gravity,na.rm=T)) %>%
    ungroup() %>% 
    #Get ecossystem average presure
    # group_by(LME_NAME) %>%
    # mutate(average_number_of_unmatched_LME = mean(unmatched_number)) %>%
    # ungroup() %>% 
    # dplyr::filter(SOVEREIGN1 != "Slovenia") %>%
    ggplot(aes(match_status, average_gravity,
               fill = match_status,group=match_status)) + 
    geom_col(stat="identity",position = "dodge",color="black",width = 0.7,lwd = 0.1) +
    scale_fill_manual(values = legend_match,name = "Cell type") +
    coord_flip() +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    labs(y = "Average human gravity in cell",
         x = " ",
         title = "A")
  
}
