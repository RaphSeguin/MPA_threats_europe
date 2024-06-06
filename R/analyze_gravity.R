analyze_gravity <- function(){
  
  load("output/mpa_gravity.Rdata")
  
  mpa_gravity_clean <- mpa_gravity %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order))
  
  save(mpa_gravity_clean, file = "output/mpa_gravity_clean.Rdata")

}
