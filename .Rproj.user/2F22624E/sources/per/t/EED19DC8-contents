## Scenario selection

# libraries ---------------------------------------------------------------
library(here)
#library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(reshape2)
library(ggplot2)
library(stringr)
library(conflicted)
library(writexl)
library(openxlsx)
library(stats)
library(zoo)
library(cluster) 
library(factoextra)

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
here()
# file --------------------------------------------------------------------

file <- list.files(path = here("data", "SOFA extraction"))

file# data --------------------------------------------------------------------
df <- read.csv(here("data", "240418_FullDataBase.csv"), sep = "") %>% 
  dplyr::filter(tradeadjustment == "No")
product <- read.csv(here("data",  "240418_FullProductDataBase.csv"),sep = "")  %>% 
  dplyr::filter(tradeadjustment == "No")
mapping_F6 <- read_excel(here("data",  "DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map")

db_scenarios <- read.csv(here("data", "240418_scenarios.csv")) %>%
  select(pathway, country, afforestation,agricultural_land_expansion)%>% 
  rename(ALPHA3 = country,
         Pathway = pathway) %>% 
  mutate(Pathway = recode(Pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  unique()

fao_prod <- read.csv(here("data", "FAO_FoodBalance.csv"))

# mapping_animalprod <- read.csv(here("data", "FAOSTAT_mapping_animalprod.csv"))

# fao_prod <- fao_prod %>% 
#   inner_join(mapping_animalprod, by = "Item")

mapping <- read_excel(here("data",  "mapping_GAMS_FAO_products.xlsx"))
mapping[which(mapping$FAO == "Groundnuts (Shelled Eq)"), "FAO"] <- "Groundnuts"

mapping_country <- read_excel(here("data", "mapping_country_FAO_FABLE.xlsx")) %>% 
  mutate(iso3c = countrycode::countrycode(sourcevar = Country_FAO, origin = "country.name", destination = "iso3c"))
mapping_ALPHA3 <- read_excel(here("data",  "mapping_alpha3_Country.xlsx")) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3))


fao_prod$Area <- as.character(fao_prod$Area)
fao_prod[which(fao_prod$Area == "United Kingdom of Great Britain and Northern Ireland"), "Area"] <- "United Kingdom"


product_dt <- left_join(product,
                        # rename(alpha3 = country) %>% 
                        # mutate(alpha3 = as.character(alpha3)) %>% 
                        # select(#-id, 
                        #   -scenathon_id), 
                        df,
                        # rename(alpha3 = country) %>% 
                        # select(#-id, 
                        #   -scenathon_id),
                        by = c("country", "pathway", "year")) %>% 
  rename(ALPHA3 = country) %>% 
  mutate(pathway = recode(pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  left_join(mapping_ALPHA3) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  unique()


##Compute kcal/kg data from FAO 2020
df_fao <- fao_prod %>%
  left_join(mapping %>% select(FPRODUCT, FAO) %>% unique(), by = c("Item" = "FAO")) %>%
  mutate(iso3c = countrycode::countrycode(sourcevar = Area, origin = "country.name", destination = "iso3c")) %>%
  left_join(mapping_country, by = c("iso3c")) %>%
  left_join(mapping_ALPHA3, by = c("Country_FABLE" = "Country")) %>%
  mutate(ALPHA3 = ifelse(Country_FABLE == "NPL", "NPL", 
                         ifelse(Country_FABLE == "GRC", "GRC", ALPHA3))) %>% 
  select(ALPHA3, FPRODUCT, Element, Value) 

df <- df %>% 
  rename(ALPHA3 = country) %>% 
  rename(Pathway = pathway)

product <- product %>% 
  rename(ALPHA3 = country)


#Computing Kcal content per Kg
df_fao <- (df_fao %>%
             group_by(ALPHA3, FPRODUCT, Element) %>% 
             dplyr::summarise_at(vars(Value),
                                 sum,
                                 na.rm = T) %>% 
             pivot_wider(names_from = Element,
                         values_from = Value) %>% 
             dplyr::rename(kg = "Food supply quantity (kg/capita/yr)",
                           kcal = "Food supply (kcal/capita/day)") %>% 
             mutate(kcal.kg = ifelse(kg!=0,
                                     kcal/kg,
                                     NA)) %>% 
             # mutate(ALPHA3 = ifelse(ALPHA3 == "NMC", "RMECAS", ALPHA3)) %>% 
             data.frame()) 



# pdty_livestock ----------------------------------------------------------
# 
# db_full <- data.frame()
# 
# for (cur_file in file){
#   #extract the righ sheet in Calculator
#   data <- read_excel(here("data", "SOFA extraction", cur_file),
#                      sheet = "2_calc_livestock",
#                      range = "A30:Z173")
# 
#   #Colnames are lower in the SWE calculator
#   if(grepl("SWE", cur_file)){
#   data <- read_excel(here("data", "Calcs", cur_file),
#                        sheet = "2_calc_livestock",
#                        range = "A31:Z174")
#   }
# 
#   data <- data %>%
#     slice(which(herdcount == 1)) %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, ANIMAL, FPRODUCT, herd, pdtyanim)  %>%
#     mutate(Pathway = ifelse(grepl("affor", cur_file),
#                             "GS_affor",
#                             ifelse(grepl("diet", cur_file),
#                                    "GS_diet", 
#                                    ifelse(grepl("liverum", cur_file),
#                                                      "GS_live_rum", 
#                                           ifelse(grepl("agrexp", cur_file),
#                                                                        "GS_agrexp",
#                                                  ifelse(grepl("crop", cur_file),
#                                                         "GS_crop", 
#                                                                ifelse(grepl("popurban", cur_file),
#                                                                       "GS_pop_urban",
#                                                                       ifelse(grepl("pop", cur_file),
#                                                                              "GS_pop",
#                                    "GS_foodwaste")))))))) %>% 
#   mutate(ALPHA3 = ifelse(grepl("AUS", cur_file), "AUS",
#                          ifelse(grepl("BRA", cur_file), "BRA",
#                                 ifelse(grepl("COL", cur_file), "COL",
#                                        ifelse(grepl("ETH", cur_file), "ETH",
#                                               ifelse(grepl("GBR", cur_file), "GBR", ALPHA3))))))
#     
# 
#   db_full <- db_full %>%
#     rbind.data.frame(data)
# 
# 
# }
# write.xlsx(db_full %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_SOFAPathways_live.xlsx")), row.names = F)
db_full <- readxl::read_excel(here("data", "extracted", "20240423_SOFAPathways_live.xlsx"))

#Computing Livestock productivity in t/TLU
db_full_agg <- db_full %>%
  # use the herd in TLU to later compute a weighted average
  mutate(weight_pdty = herd*pdtyanim) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_pdty = sum(weight_pdty, na.rm = T),
                   herd = sum(herd, na.rm = T)) %>%
  #weighted average
  mutate(pdty = weight_pdty/herd) 
# %>% 
#   mutate(ALPHA3 = ifelse(ALPHA3 == "NMC", "RMECAS", ALPHA3))  
#   



#here the productivity can also increase or decrease if a country changes the proportion each animal represents
#Computing livestock productivity relative change 2020-2050

db_change_Live_Prod <- db_full_agg %>% 
  mutate(var_pivot = paste0("pdty_", YEAR)) %>% 
  select(var_pivot, Pathway, ALPHA3, pdty) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(pdty)) %>% 
  #relative change between 2050 and 2020
  mutate(pdty_live_change = round(pdty_2050/pdty_2020, 2)) %>% 
  select(ALPHA3, Pathway, pdty_live_change) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "RMECAS", "R_NMC", ALPHA3))  

write.xlsx(db_change_Live_Prod, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "pdtylive_change_otherpath.xlsx")), row.names = F)







# Crops productivity ------------------------------------------------------
# 
# db_full_crop <- data.frame()
# 
# for (cur_file in file){
#   #???print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "SOFA extraction", cur_file),
#                      sheet = "3_calc_crops",
#                      range = "G28:AE798")
#   # if(grepl("SWE", cur_file)){
#   # data <- read_excel(here("data", "Calcs", cur_file),
#   #                      sheet = "2_calc_livestock",
#   #                      range = "BH31:BU75")
#   # }
# 
#   data <- data %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, CROP, Harvarea, Pdty) %>%
#     mutate(Pathway = ifelse(grepl("affor", cur_file),
#                             "GS_affor",
#                             ifelse(grepl("diet", cur_file),
#                                    "GS_diet", 
#                                    ifelse(grepl("liverum", cur_file),
#                                           "GS_live_rum", 
#                                           ifelse(grepl("agrexp", cur_file),
#                                                  "GS_agrexp",
#                                                  ifelse(grepl("crop", cur_file),
#                                                         "GS_crop", 
#                                                         ifelse(grepl("popurban", cur_file),
#                                                                "GS_pop_urban",
#                                                                ifelse(grepl("pop", cur_file),
#                                                                       "GS_pop",
#                                                                       "GS_foodwaste")))))))) %>% 
#     mutate(ALPHA3 = ifelse(grepl("AUS", cur_file), "AUS",
#                            ifelse(grepl("BRA", cur_file), "BRA",
#                                   ifelse(grepl("COL", cur_file), "COL",
#                                          ifelse(grepl("ETH", cur_file), "ETH",
#                                                 ifelse(grepl("GBR", cur_file), "GBR", ALPHA3)))))) %>% 
#     unique()
# 
#   db_full_crop <- db_full_crop %>%
#     rbind.data.frame(data) %>%
#     data.frame()
# }
# 
# write.xlsx(db_full_crop, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_SOFAPathways_crop.xlsx")), row.names = F)
db_full_crop <- readxl::read_excel(here("data", "extracted", "20240423_SOFAPathways_crop.xlsx"))

db_full_crop_agg <- db_full_crop %>%
  #Use harvested area as weight
  mutate(weight_pdty = Harvarea*Pdty) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_pdty = sum(weight_pdty, na.rm = T),
                   Harvarea = sum(Harvarea, na.rm = T)) %>%
  #weighted average
  mutate(pdty = weight_pdty/Harvarea)  

db_change_crop <- db_full_crop_agg %>% 
  mutate(var_pivot = paste0("pdty_", YEAR)) %>% 
  select(var_pivot, Pathway, ALPHA3, pdty) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(pdty)) %>% 
  #relative change betwwen 2050 and 2015
  mutate(pdty_crop_change = round(pdty_2050/pdty_2020, 2)) %>% 
  select(ALPHA3, Pathway, pdty_crop_change)


write.xlsx(db_change_crop, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "pdtycrop_change_otherpath.xlsx")), row.names = F)



# 
# #Reading extracted data
# db_full_crop <- readxl::read_excel(here("data", "extracted", "20240229_ExtractedPdtyCrop.xlsx")) 
# 
# #Computing Crop productivity in t/ha using the harvested area to aggregate 
# db_full_crop_agg <- db_full_crop %>%
#   #Use harvested area as weight
#   mutate(weight_pdty = Harvarea*Pdty) %>%
#   group_by(ALPHA3, Pathway, YEAR) %>%
#   dplyr::summarise(weight_pdty = sum(weight_pdty, na.rm = T),
#                    Harvarea = sum(Harvarea, na.rm = T)) %>%
#   #weighted average
#   mutate(pdty = weight_pdty/Harvarea)
# 
# #Computing Crop productivity relative change 2020 - -2050
# db_change_crop <- db_full_crop_agg %>% 
#   mutate(var_pivot = paste0("pdty_", YEAR)) %>% 
#   select(var_pivot, Pathway, ALPHA3, pdty) %>% 
#   pivot_wider(names_from = var_pivot,
#               values_from = c(pdty)) %>% 
#   #relative change between 2050 and 2015
#   mutate(pdty_crop_change = round(pdty_2050/pdty_2020, 2)) %>% 
#   select(ALPHA3, Pathway, pdty_crop_change) %>% 
#   data.frame() 
# 
# #Save
# write.xlsx(db_change_crop, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_pdtycrop.xlsx")), row.names = F)











# Food Waste --------------------------------------------------------

db_full_waste <- data.frame()

for (cur_file in file){
  #???Extract the right sheet from calculators
  data <- read_excel(here("data", "SOFA extraction", cur_file),
                     sheet = "1_calc_human_demand",
                     range = "A27:AP1116")

  if(grepl("DNK|GRC|NPL|R_ASP|R_CSA|R_NEU|R_OEU|R_SSA|RMECAS|TUR", cur_file)){
  data <- read_excel(here("data", "Calcs", cur_file),
                       sheet = "1_calc_human_demand",
                       range = "A31:AQ1116")
  }





  data <- data %>%
    select(year, food_waste, LOSS_SCEN, fproduct, prodgroup) %>%
    mutate(Pathway = ifelse(grepl("affor", cur_file),
                            "GS_affor",
                            ifelse(grepl("diet", cur_file),
                                   "GS_diet",
                                   ifelse(grepl("liverum", cur_file),
                                          "GS_live_rum",
                                          ifelse(grepl("agrexp", cur_file),
                                                 "GS_agrexp",
                                                 ifelse(grepl("crop", cur_file),
                                                        "GS_crop",
                                                        ifelse(grepl("popurban", cur_file),
                                                               "GS_pop_urban",
                                                               ifelse(grepl("pop", cur_file),
                                                                      "GS_pop",
                                                                      ifelse(grepl("foodwaste", cur_file),
                                                                             "GS_foodwaste",
                                                                      ifelse(grepl("Current", cur_file),
                                                                                              "CurrentTrend",
                                                                                              ifelse(grepl("National", cur_file),
                                                                                                     "NationalCommitments",
                                                                                                     "GlobalSustainability"))))))))))) %>%
    mutate(ALPHA3 = ifelse(grepl("AUS", cur_file), "AUS",
                           ifelse(grepl("BRA", cur_file), "BRA",
                                  ifelse(grepl("COL", cur_file), "COL",
                                         ifelse(grepl("ETH", cur_file), "ETH",
                                                ifelse(grepl("GBR", cur_file), "GBR",
                                                       ifelse(grepl("Current", cur_file),
                                                                  str_sub(cur_file, 40, 42),
                                                                             ifelse(grepl("National", cur_file),
                                                                                    str_sub(cur_file, 46, 48),
                                                                                    str_sub(cur_file, 47, 49))))))))) %>%
    unique()

  db_full_waste <- db_full_waste %>%
    rbind.data.frame(data)
}

write.xlsx(db_full_waste, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_SOFAPathways_foodwaste.xlsx")), row.names = F)

db_full_waste <- readxl::read_excel(here("data", "extracted", "20240514_SOFAPathways_foodwaste.xlsx")) %>% 
  # mutate(ALPHA3 = ifelse(nchar(ALPHA3) == 4, stringr::str_sub(ALPHA3, 2, 4), ALPHA3)) %>% 
  rename(FPRODUCT = fproduct) %>%
  mutate(FPRODUCT = ifelse(FPRODUCT == "MILK", "milk", FPRODUCT))


mapping_fao_fbs <- read_excel(here("data", "FAO_product_maps_2023_ML.xlsx")) %>%
  rename(FPRODUCT = Fable_frpoduct) %>% 
  mutate(FPRODUCT = tolower(FPRODUCT)) %>% 
  select(-FAO_ItemCode, -`SLORD GROUP Matched`) 



db_full_waste_final <- db_full_waste %>% 
  mutate(FPRODUCT = tolower(FPRODUCT)) %>%
  left_join(mapping_fao_fbs, 
            relationship = "many-to-many") %>% 
  select(-prodgroup, -LOSS_SCEN) %>% 
  unique() %>% 
  dplyr::filter(!year %in% c(2000,2005, 2010, 2015)) %>% 
  select(ALPHA3, Pathway, year, FPRODUCT, `FAO FBS`, food_waste)



write.xlsx(db_full_waste_final, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_SOFAPathways_foodwaste_final.xlsx")), row.names = F)








