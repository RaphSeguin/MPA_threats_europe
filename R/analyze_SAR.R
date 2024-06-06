analyze_SAR <- function(){
  
  load("output/mpa_SAR.Rdata")
  
  mpa_SAR_clean <- mpa_SAR %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(unmatched_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_SAR_clean, file = "output/mpa_SAR_clean.Rdata")
}