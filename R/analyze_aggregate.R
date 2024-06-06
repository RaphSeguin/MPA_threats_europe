analyze_aggregate <- function(){
  
  load("output/mpa_aggregate.Rdata")
  
  mpa_aggregate_clean <- mpa_aggregate %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(aggregate_number, .keep_all = T) %>%
    ungroup()

  save(mpa_aggregate_clean, file = "output/mpa_aggregate_clean.Rdata")

}
