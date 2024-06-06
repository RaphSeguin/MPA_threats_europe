#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","ggspatial","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr","sfarrow","corrplot","DHARMa",
          "harrypotter","wesanderson","ranger","missForest","rgdal","countrycode","ggpubr","data.table","randomForestExplainer","spatialRF","spaMM","DHARMa","glmmTMB","performance","spdep","rstatix","formatdown","ggrepel",
          "units","xml2","XML","rnaturalearth","ggExtra","raster","MatchIt","cobalt","knitr","broom","papaja","extrafont","patchwork",
          "exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","imputeTS","rgeos","visreg","piecewiseSEM")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# key <- gfw_auth()
sf_use_s2(FALSE)
#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)
setwd(here())

#-----------------Loading all data---------------------

#Workflow
#EEZ
#Download from here
#https://marineregions.org/downloads.php
eez <- loading_eez()

#Download from here
#https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics
europe <- st_read("data/shapefiles/Europe/NUTS_RG_20M_2016_4326.shp") %>%
  filter(LEVL_CODE == 0)

#LMEs 
#Download from here
#https://www.sciencebase.gov/catalog/item/55c77722e4b08400b1fd8244
LMEs <- st_read("data/shapefiles/LMEs/LMEs66.shp") %>%
  dplyr::select(LME_NAME, LME_NUMBER)

#MPA WDPA
#Download from here
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
mpa_eu_wdpa <- prep_mpa_data()

#-------Loading threats data----------

setwd(here())

#ports
ports <- loading_ports()

#Fishing effort 2023
vessel_registry <- load_registry()
fishing_effort_2023 <- load_fishing_effort()
fishing_effort_clean <- clean_fishing_effort(fishing_effort_2023)

#Fishing vessels matched/unmatched
load("data/SAR_data.Rdata")

#Offshore infrastructures
offshore <- load_offshore() 

#Shipping lanes
shipping <- load_shipping() 

#Load dredge and aggregate
dredge <- load_dredge()
aggregate <- load_aggregate() 

#load desalination
desalination <- load_desalination()

#load gravity
gravity <- load_gravity()

#-------Intersecting threats data------

#Create grid to intersect with, if protected or not
europe_grid <- create_grid() 

#trying out matching
matched_grid <- match_grid() 

#loading outputs
load("output/europe_grid.Rdata")
load("output/matched_grid.Rdata")
# 
europe_grid <- europe_grid %>%
    #Join with matching
    left_join(matched_grid %>% st_drop_geometry(), by = "grid_id") %>%
    #Recode iucn category
    mutate(iucn_cat = as.factor(ifelse(iucn_cat == "Unprotected" & match_status == "Matched", "Matched_outside",
                             ifelse(iucn_cat == "Unprotected" & match_status == "Unmatched", "Unmatched_outside",as.character(iucn_cat))))) %>%
    mutate(match_status = factor(match_status, levels = c("Unmatched","Matched","Protected")))

save(europe_grid, file = "output/europe_grid.Rdata")

#Intersect with ports
mpa_ports <- intersection_ports() 

#Intersect with offshore structures
mpa_offshore <- intersection_offshore() 

#Intersect with shipping lane
mpa_shipping <- intersection_shipping()

#Intersecting with fishing effort
mpa_fishing <- intersection_fishing() 

#Presence of industrial fishing fleets
mpa_SAR <- intersection_SAR()

#Presence of dredge
mpa_dredge <- intersection_dredging()

#Presence of aggregation
mpa_aggregate <- intersection_aggregate()

#Presence of desalination
mpa_desalination <- intersection_desalination()

#Estimate gravity
mpa_gravity <- intersection_gravity()

#-----Analyzing data-------

#Ordering iucn cats
level_order <- c('I','II', 'III',"IV","V","VI","No_IUCN_cat","Matched_outside","Unmatched_outside")
labels = c("I","II","III","IV","V","VI","No declared category","Matched outside","Unmatched outside")

#Legend
legend = c("I" = "#051D41",
           "II" = "#092C63",
           "III" = "#0E58C6",
           "IV" = "#2F79EE",
           "V" = "#5090EF",
           "VI"= "#93BAF8",
           "No_IUCN_cat" = "#D87458",
           "Matched_outside" = "#511717",
           "Unmatched_outside" = "#4C4F4E")

#Legend match status
#Legend
legend_match = c("Protected" = "#F2B47E",
           "Matched" = "#3B4B59",
           "Unmatched" = "#D9CCC5")

#Legend of activities
legend_activities = c("ports" = "#0B1309",
                      "shipping" = "#F5ECCD",
                      "desalination" = "#7C9A74",
                      "offshore" = "#6F767D",
                      "fishing" = "#2B4016",
                      "dredge" = "#305282",
                      "unmatched" = "#273A51",
                      "aggregate" = "#CFB293")

#Plot gradien
colours <- c("#f6d2a9","#f5b78e","#f19c7c","#ea8171","#dd686c","#ca5268","#b13f64")

#number of cells with protection
# n_unprotected <- nrow(filter(europe_grid, iucn_cat == "Unprotected"))
# n_protected <- nrow(filter(europe_grid, iucn_cat != "Unprotected"))

#Analzying port
analyze_ports() 

#Analyze offshore
analyze_offshore() 

#Analyze shipping lanes
analyze_shipping() 

#Analyze fishing
analyze_fishing()

#Analyze SAR
analyze_SAR()

#Analyze dredging
analyze_dredging()

#Analyaze aggregate
analyze_aggregate()

#Analyze desalination
analyze_desalination()

#Analyze gravity
analyze_gravity()

#---Estimating fishing pressure in MPAs------

mpa_pressures <- combine_pressures()

load("output/mpa_pressures.Rdata")

#Results on ports
# port_results() 

#Plot number of threats by status and IUCN category
plot_number_threats(mpa_pressures)

#Plot percentage of cells with industrial activity inside
plot_percentage_threats(mpa_pressures)

#Plot cumulated number of threats in country
plot_threats_LME()

#plot ddetailed activities
plot_detailed_threats()
plot_detailed_threats_LME()

#Figures
figure_1()

#Supplementary figure
supp_figures()

