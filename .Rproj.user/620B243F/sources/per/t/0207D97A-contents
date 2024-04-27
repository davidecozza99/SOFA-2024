# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(writexl)
library(RColorBrewer)
library(openxlsx)
library(tidyr)

conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)

here()


aus_data <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_affor", "GS_live_rumdensity"))
  
bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_agrexp", "GS_diet", "GS_crop"))

col_data <- read_xlsx(here("data", "report_COL_20240325_15H07.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_crop", "GS_pop_urban"))
  
eth_data <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_pop", "GS_agrexp", "GS_crop"))

gbr_data <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Indicators")  %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_foodwaste", "GS_crop"))



all_data <- aus_data %>%
  bind_rows(bra_data) %>%
  bind_rows(col_data) %>%
  bind_rows(eth_data) %>%
  bind_rows(gbr_data)

all_data <- all_data %>% 
  select(Pathway, Location, Population, Year, kcal_feas, kcal_PoU,
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue,
         NewForestChange, LNPPNewForest, LNPPNewOtherLand, AgroecoSh) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>%
  mutate(LNPPNewOtherLand_change = LNPPNewOtherLand - lag(LNPPNewOtherLand)) %>% 
  mutate(LNPPNewForest_change = LNPPNewForest - lag(LNPPNewForest)) %>% 
  # filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)



# Commodities -----------------------------
aus_comm <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Commodities")
bra_comm <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Commodities") 
col_comm <- read_xlsx(here("data", "report_COL_20240325_15H07.xlsx"), sheet = "Commodities")  
eth_comm <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Commodities")  
gbr_comm <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Commodities")  



all_comm <- aus_comm %>%
  bind_rows(bra_comm) %>%
  bind_rows(col_comm) %>%
  bind_rows(eth_comm) %>%
  bind_rows(gbr_comm) %>% 
  rename(Pathway = `Current Trend`) %>% 
  # filter(Year %in% c("2030", "2050"))%>% 
  select(Location, Pathway, Year, Product, kcalfeasprod)


mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

all_kcal <- all_comm %>% 
  inner_join(mapping, by ="Product") %>% 
  unique %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product)


all_kcal_final <- all_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)


all_data <- left_join(all_data, all_kcal_final %>% select(Pathway, Year, kcal_plant, Location, kcal_anim), by = c("Pathway", "Location", "Year")) %>%
  unique() 

# 
# #Creation of EAT_LANCET Kcal variables

mapping_eat<- read_excel(here("data", "mapping_product_group_EAT.xlsx")) %>%
  rename(Product = PRODUCT)

all_kcal_eat <- all_comm %>%
  inner_join(mapping_eat, by ="Product") %>%
  unique %>%
  group_by(Pathway, Location, Year, EAT_foodgroup) %>%
  mutate(kcal_eat = sum(kcalfeasprod)) %>%
  select(-kcalfeasprod, -PROD_GROUP, -Product)


all_kcal_eat_final <- all_kcal_eat %>%
  pivot_wider(names_from = EAT_foodgroup, values_from = kcal_eat, values_fn = list(kcal_eat = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_NotIdentifiedinEATLancet = "NA") %>%
  replace(is.na(.), 0)


all_kcal_eat_final <- all_kcal_eat %>%
  pivot_wider(names_from = EAT_foodgroup, values_from = kcal_eat, values_fn = list(kcal_eat = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_NotIdentifiedinEATLancet = "NA") %>%
  rename_with(
    ~ if_else(. %in% c("Location", "Pathway", "Year", "kcal_NotIdentified_EATLancet"), ., paste0("kcal_", .,"_EATLancet")),
    -c(Location, Pathway, Year, kcal_NotIdentifiedinEATLancet)
  )

#Final Database ---------------------------------------

all_data <- left_join(all_data, all_kcal_eat_final, by = c("Pathway", "Location", "Year")) %>%
  unique() %>%
  mutate(Pathway = recode(Pathway, "Current Trend_Yes" = "Current Trend"))


#Save
write.xlsx(all_data, file = here("data", "Decomposition", "database_decomposition_withETHnew.xlsx"))






mapping_fao_fbs <- read_excel(here("data", "FAO_product_maps_2023_ML.xlsx")) %>%
  rename(Product = Fable_frpoduct) %>% 
  mutate(Product = tolower(Product)) %>% 
  select(-FAO_ItemCode, -`SLORD GROUP Matched`) 

all_kcal_fao_fbs <- all_comm %>% 
  inner_join(mapping_fao_fbs, by ="Product", relationship = "many-to-many") %>% 
  unique %>% 
  group_by(Pathway, Location, Year, `FAO FBS`) %>% 
  mutate(kcal_fao_fbs = sum(kcalfeasprod)) %>%
  select(-kcalfeasprod, -Product)


all_kcal_fao_fbs_final <- all_kcal_fao_fbs %>%
  pivot_wider(names_from = `FAO FBS`, values_from = kcal_fao_fbs, values_fn = list(kcal_fao_fbs = function(x) x[which.min(!is.na(x))])) %>%
  select(-"NA") %>% 
  replace(is.na(.), 0) %>% 
rename_with(
    ~ if_else(. %in% c("Location", "Pathway", "Year"), ., paste0("kcal_", .,"_fao_fbs")),
    -c(Location, Pathway, Year)
  )



#Final Database ---------------------------------------

all_data_fao_fbs <- left_join(all_data, all_kcal_fao_fbs_final, by = c("Pathway", "Location", "Year")) %>%
  unique() %>%
  mutate(Pathway = recode(Pathway, "Current Trend_Yes" = "Current Trend"))


#Save
write.xlsx(all_data_fao_fbs, file = here("data", "Decomposition", "database_decomposition_newvariables_asked_withETH.xlsx"))









#Differences

# all_data <- all_data %>%
#   group_by(Year, Location) %>%
#   mutate(
#     Pathway_code = ifelse(str_starts(Pathway, "NC"), "NC", ifelse(str_starts(Pathway, "GS"), "GS", "CT")),
#     diff_kcal_feas = ifelse(Pathway != "Current Trend_Yes", kcal_feas - first(kcal_feas[Pathway == "Current Trend_Yes"]), NA),
#     diff_kcal_plant = ifelse(Pathway != "Current Trend_Yes", kcal_plant - first(kcal_plant[Pathway == "Current Trend_Yes"]), NA),
#     diff_kcal_anim = ifelse(Pathway != "Current Trend_Yes", kcal_anim - first(kcal_anim[Pathway == "Current Trend_Yes"]), NA),
#     diff_ForestChange = ifelse(Pathway != "Current Trend_Yes", ForestChange - first(ForestChange[Pathway == "Current Trend_Yes"]), NA),
#     diff_Cropland_change = ifelse(Pathway != "Current Trend_Yes", Cropland_change - first(Cropland_change[Pathway == "Current Trend_Yes"]), NA),
#     diff_Pasture_change = ifelse(Pathway != "Current Trend_Yes", Pasture_change - first(Pasture_change[Pathway == "Current Trend_Yes"]), NA),
#     diff_OtherLand_change = ifelse(Pathway != "Current Trend_Yes", OtherLand_change - first(OtherLand_change[Pathway == "Current Trend_Yes"]), NA),
#     diff_CalcFarmLabourFTE = ifelse(Pathway != "Current Trend_Yes", CalcFarmLabourFTE - first(CalcFarmLabourFTE[Pathway == "Current Trend_Yes"]), NA),
#     diff_CO2 = ifelse(Pathway != "Current Trend_Yes", CO2 - first(CO2[Pathway == "Current Trend_Yes"]), NA),
#     diff_CH4 = ifelse(Pathway != "Current Trend_Yes", CH4 - first(CH4[Pathway == "Current Trend_Yes"]), NA),
#     diff_N2O = ifelse(Pathway != "Current Trend_Yes", N2O - first(N2O[Pathway == "Current Trend_Yes"]), NA),
#     diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
#     diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
#     diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
#     diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
#     diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
#   )  


# write.xlsx(all_data, file = here("data", "Decomposition", "database_decomposition.xlsx"))



# #### Product database
# 
# 
# aus_comm <- read_xlsx(here("data", "report_AUS_20240228_15H16.xlsx"), sheet = "Commodities")
# bra_comm <- read_xlsx(here("data", "report_BRA_20240228_15H42.xlsx"), sheet = "Commodities") 
# col_comm <- read_xlsx(here("data", "report_COL_20240306_10H35.xlsx"), sheet = "Commodities")  
# eth_comm <- read_xlsx(here("data", "report_ETH_20240228_15H45.xlsx"), sheet = "Commodities")  
# gbr_comm <- read_xlsx(here("data", "report_GBR_20240305_8H06.xlsx"), sheet = "Commodities")  
# 
# mapping<- read_excel(here("data", "mapping_product_group_EAT.xlsx")) %>% 
#   rename(Product = PRODUCT) 
# 
# 
# all_comm <- aus_comm %>%
#   bind_rows(bra_comm) %>%
#   bind_rows(col_comm) %>%
#   bind_rows(eth_comm) %>%
#   bind_rows(gbr_comm) %>% 
#   rename(Pathway = `Current Trend`) 
#   
# 
# all_comm <- all_comm %>% 
#   left_join(mapping, by ="Product") 
#   
# 
# write.xlsx(all_comm, file = here("data", "Decomposition", "database_decomposition_products.xlsx"))
















# #For India, just Current Trend
ind_comm <- read.csv(here("data", "FullProductDataBase.csv")) %>%
  filter(country=="IND") %>%
  filter(tradeadjusment == "Yes")


ind_comm <- ind_comm %>%
  select(country, pathway_id, year, product, kcalfeasprod)


# mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
#   rename(product = PRODUCT)
# 
# all_kcal_ind <- ind_comm %>% 
#   inner_join(mapping, by ="product") %>% 
#   unique %>% 
#   mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>% 
#   group_by(pathway_id, country, year, Anim_Plant) %>% 
#   mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
#   select(-kcalfeasprod, -PROD_GROUP, -product)
# 
# 
# all_kcal_ind_final <- all_kcal_ind %>%
#   pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
#   rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
#   replace(is.na(.), 0)

#Creation of EAT_LANCET Kcal variables

mapping_eat<- read_excel(here("data", "mapping_product_group_EAT.xlsx")) %>% 
  rename(product = PRODUCT) 

all_kcal_ind_eat <- ind_comm %>% 
  inner_join(mapping_eat, by ="product") %>% 
  unique %>% 
  group_by(pathway_id, country, year, EAT_foodgroup) %>% 
  mutate(kcal_eat = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -product)


all_kcal_ind_eat_final <- all_kcal_ind_eat %>%
  pivot_wider(names_from = EAT_foodgroup, values_from = kcal_eat, values_fn = list(kcal_eat = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_NotIdentifiedinEATLancet = "NA") %>%
  replace(is.na(.), 0)


all_kcal_ind_eat_final <- all_kcal_ind_eat %>%
  pivot_wider(names_from = EAT_foodgroup, values_from = kcal_eat, values_fn = list(kcal_eat = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_NotIdentifiedinEATLancet = "NA") %>%
  rename_with(
    ~ if_else(. %in% c("country", "pathway_id", "year", "kcal_NotIdentified_EATLancet"), ., paste0("kcal_", .,"_EATLancet")),
    -c(pathway_id, year, kcal_NotIdentifiedinEATLancet)
  ) 

#Save
write.xlsx(all_kcal_ind_eat_final, file = here("data", "Decomposition", "India_EAT.xlsx"))

