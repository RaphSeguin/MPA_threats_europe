plot_number_threats <- function(mpa_pressures){
  
  #Number of industrial activities by protection or not protection
  ggplot(mpa_pressures,aes(match_status, sum_pressures)) + 
    geom_jitter(alpha = 0.2, shape = ".") + 
    geom_boxplot() +
    theme_minimal() +
    # scale_x_discrete(labels = c("Unprotected","Protected")) + 
    labs(y = "Number of industrial activities ",
         x = " ")
  
  ggsave("figures/industrial_by_protection.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
    #Number of industrial activities by IUCN category
    ggplot(mpa_pressures,aes(factor(iucn_cat,level_order), sum_pressures, fill = iucn_cat)) + 
      scale_fill_manual(values = legend, labels = labels) +
      scale_x_discrete(labels = labels) +
      geom_jitter(alpha = 0.2, shape = ".",aes(color = iucn_cat)) + 
      scale_color_manual(values = legend, labels = labels) +
      geom_boxplot(alpha = 0.8) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "bottom") +
      # scale_x_discrete(labels = c("Unprotected","Protected")) + 
      labs(title = "Sum of industrial activities in cells by IUCN category",
           y = "Number of industrial activities inside cell",
           x = " ",
           fill = "IUCN category") +
      guides(color = "none")
  
  ggsave("figures/industrial_by_iucn_category.jpg", width = 297, height = 210, units = "mm", dpi = 300)
    
}