analyze_shipping <- function(){
  
  load("output/mpa_shipping.Rdata")
  
  mpa_shipping_clean <- mpa_shipping %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order))
  
  save(mpa_shipping_clean, file = "output/mpa_shipping_clean.Rdata")
  
}
