analyze_dredging <- function(){
  
  load("output/mpa_dredge.Rdata")
  
  mpa_dredge_clean <- mpa_dredge %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(dredge_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_dredge_clean, file = "output/mpa_dredge_clean.Rdata")
  
}
