analyze_desalination <- function(){

  load("output/mpa_desalination.Rdata")
  
  mpa_desalination_clean <- mpa_desalination %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(desalination_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_desalination_clean, file = "output/mpa_desalination_clean.Rdata")
  
}
