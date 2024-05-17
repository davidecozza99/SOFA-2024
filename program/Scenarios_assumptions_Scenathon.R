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

file <- list.files(path = here("data", "Calcs_new"))

# data --------------------------------------------------------------------
df <- read.csv(here("data", "240424_FullDataBase.csv"), sep = "") %>% 
  dplyr::filter(tradeadjustment == "No")
product <- read.csv(here("data",  "240424_FullProductDataBase.csv"),sep = "")  %>% 
  dplyr::filter(tradeadjustment == "No")
mapping_F6 <- read_excel(here("data",  "DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map")

db_scenarios <- read.csv(here("data", "240424_scenarios.csv")) %>%
  select(pathway, country, afforestation,agricultural_land_expansion)%>% 
  rename(ALPHA3 = country,
         Pathway = pathway) %>% 
  mutate(Pathway = recode(Pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  unique()

fao_prod <- read.csv(here("data", "FAO_FoodBalance.csv"))


fao_prod$Item <- ifelse(fao_prod$Item == "Rice and products", "Rice (Milled Equivalent)", 
                        ifelse(fao_prod$Item == "Groundnuts", "Groundnuts (in Shell Eq)",
                               ifelse(fao_prod$Item == "Vegetables, other", "Vegetables, Other",
                                      ifelse(fao_prod$Item == "Fruits, other", "Fruits, Other",
                                             ifelse(fao_prod$Item == "Freshwater Fish", "Fish Seafood + (Total)",
                                                    ifelse(fao_prod$Item == "Meat, Other", "Meat Other",
                                                           ifelse(fao_prod$Item == "Offals, Edible", "Offals Edible",
                                                            
                                                                fao_prod$Item)))))))
 


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

product_df <- left_join(product_dt, df_fao, by = c("ALPHA3" = "ALPHA3", "product" = "FPRODUCT")) %>%
  rename(Pathway = pathway)




#####################  
#Computing Population and Kcal target relative change 2020-2050

df_change <- df %>% 
  slice(which(year %in% c(2020, 2050))) %>% 
  select(ALPHA3, Pathway, year, population, kcal_targ) %>% 
  unique() %>% 
  pivot_wider(names_from = year,
              values_from = c(population, kcal_targ)) %>% 
  mutate(Population_change = round(population_2050/population_2020, 2)) %>% 
  mutate(kcal_targ_change = round(kcal_targ_2050/kcal_targ_2020,2)) %>% 
  select(ALPHA3, Pathway, Population_change, kcal_targ_change) %>% 
  # mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  mutate(Pathway = recode(Pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  data.frame() %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "RMECAS", "R_NMC", ALPHA3))  

  



#Computing imports and exports quantity relative changes 2020-2050, using Kcal per Kg to aggregate all products
product_tot <- product_df %>% 
  slice(which(year %in% c(2020, 2050))) %>%
  mutate(Pathway = recode(Pathway, "CurrentTrends" = "CurrentTrend")) %>% 
  mutate(Export_quantity = export_quantity * kcal.kg) %>% 
  mutate(Import_quantity = import_quantity * kcal.kg) %>% 
  group_by(ALPHA3, Pathway, year) %>%
  dplyr::summarise(Export_quantity = sum(Export_quantity, na.rm = T),
                   Import_quantity = sum(Import_quantity, na.rm = T)) %>%
  select(ALPHA3, Pathway, year,
         Export_quantity, Import_quantity) %>% 
  pivot_wider(names_from = year,
              values_from = c(Export_quantity, Import_quantity)) %>% 
  mutate(Export_quantity_change = round(Export_quantity_2050/Export_quantity_2020, 2)) %>% 
  mutate(Import_quantity_change = round(Import_quantity_2050/Import_quantity_2020, 2)) %>% 
  select(ALPHA3, Pathway, Export_quantity_change, Import_quantity_change) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  data.frame() %>% 
  ## manually put lower for NPL because way too high and can't see the others
  mutate(Export_quantity_change = ifelse(ALPHA3 == "NPL", 4, Export_quantity_change)) %>%
  # mutate(Import_quantity_change = ifelse(ALPHA3 == "ETH", 4, Import_quantity_change)) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "ASP", "R_ASP",
                         ifelse(ALPHA3 == "CSA", "R_CSA",
                                ifelse(ALPHA3 == "NEU", "R_NEU",
                                       ifelse(ALPHA3 == "OEU", "R_OEU",
                                              ifelse(ALPHA3 == "SSA", "R_SSA", 
                                                     ifelse(ALPHA3 == "NMC", "R_NMC", ALPHA3)))))))





# pdty_livestock ----------------------------------------------------------
# 
# db_full <- data.frame()

# for (cur_file in file){
#   #extract the righ sheet in Calculator
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                      sheet = "2_calc_livestock",
#                      range = "A30:Z173")
# 
#   #Colnames are lower in the SWE calculator
#   if(grepl("SWE", cur_file)){
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                        sheet = "2_calc_livestock",
#                        range = "A31:Z174")
#   }
# 
#   data <- data %>%
#     slice(which(herdcount == 1)) %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, ANIMAL, FPRODUCT, herd, pdtyanim) %>%
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                            str_sub(cur_file, 40, 42),
#                            ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                   str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                            ifelse(ALPHA3 == "R_C", "R_CSA",
#                                   ifelse(ALPHA3 == "R_N", "R_NEU",
#                                          ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                 ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                        ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability")))
# 
# 
#   db_full <- db_full %>%
#     rbind.data.frame(data)
# 
# 
# }
# write.xlsx(db_full %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedPdtyLivestock.xlsx")), row.names = F)
db_full <- readxl::read_excel(here("data", "extracted_scenathon", "20240425_ExtractedPdtyLivestock.xlsx"))

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
  

# Ruminent density --------------------------------------------------------
# 
# db_full2 <- data.frame()
# 
# for (cur_file in file){
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                      sheet = "2_calc_livestock",
#                      range = "BH30:BV74")
#   #colnames is lower in SWE calculator
#   if(grepl("SWE", cur_file)){
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                        sheet = "2_calc_livestock",
#                        range = "BT31:CT75")
#   }
# 
#   data <- data %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, ANIMAL, Pasture, RumDensity) %>%
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                           str_sub(cur_file, 40, 42),
#                           ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                  str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                            ifelse(ALPHA3 == "R_C", "R_CSA",
#                                   ifelse(ALPHA3 == "R_N", "R_NEU",
#                                          ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                 ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                        ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# 
#   db_full2 <- db_full2 %>%
#     rbind.data.frame(data)
# }
# 
# write.xlsx(db_full2 %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedRumDensity.xlsx")), row.names = F)
db_full2 <- readxl::read_excel(here("data", "extracted_scenathon", "20240425_ExtractedRumDensity.xlsx")) 


db_full2_agg <- db_full2 %>%
  # use the pasture surface in ha to later compute a weighted average
  mutate(weight_dens = Pasture*RumDensity) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_dens = sum(weight_dens, na.rm = T),
                   Pasture = sum(Pasture, na.rm = T)) %>%
  #weighted average
  mutate(density = weight_dens/Pasture)



db_change_RumDensity <- db_full2_agg %>% 
  mutate(var_pivot = paste0("density_", YEAR)) %>% 
  select(var_pivot, Pathway, ALPHA3, density) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(density)) %>% 
  #relative change between 2050 and 2015
  mutate(density_change = round(density_2050/density_2020, 2)) %>% 
  select(ALPHA3, Pathway, density_change)

# db_change_RumDensity <- db_change_RumDensity %>% 
#   mutate(ALPHA3 = ifelse(str_sub(ALPHA3, 1, 2) == "R_", str_sub(ALPHA3, 3, 5), ALPHA3)) #%>% 
# dplyr::mutate(ALPHA3 = ifelse(ALPHA3 %in% c("NOC", "NOS"), "NOR", ALPHA3)) %>%
# mutate(ALPHA3 = ifelse(ALPHA3 == "OEU", "ROEU", ALPHA3))


# Crops productivity ------------------------------------------------------
# 
# db_full_crop <- data.frame()
# 
# for (cur_file in file){
#   #???print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
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
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                            str_sub(cur_file, 40, 42),
#                            ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                   str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                            ifelse(ALPHA3 == "R_C", "R_CSA",
#                                   ifelse(ALPHA3 == "R_N", "R_NEU",
#                                          ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                 ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                        ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# 
#   db_full_crop <- db_full_crop %>%
#     rbind.data.frame(data) %>%
#     data.frame()
# }
# 
# write.xlsx(db_full_crop %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedPdtyCrop.xlsx")), row.names = F)
db_full_crop <- readxl::read_excel(here("data", "extracted_scenathon", "20240425_ExtractedPdtyCrop.xlsx"))

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





# #FAO PRODUCTIVITY 
# db_change_productivity <- db_change_crop %>%
#   left_join(db_change_Live_Prod) %>%
#   mutate(
#     annual_growth_crop_perc = round(((pdty_crop_change^(1/(2050-2020))) - 1) * 100, 3),
#     annual_growth_live_perc = round(((pdty_live_change^(1/(2050-2020))) - 1) * 100, 3)
#   )
# 
# # 
# # 
# write.xlsx(db_change_productivity, file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_Annual_prod.ty_change.xlsx")), row.names = F)
# # 
# 






# Expansion ----------------------------------------------------------------
# 
# db_full_expansion <- data.frame()
# 
# for (cur_file in file){
#   print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                      sheet = "4_calc_land",
#                      range = "A1:AZ100")
#   index <- which(data == "TABLE: calc_land_cor" | data == "TABLE:calc_land_cor", arr.ind = T)
# 
#   if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
#     #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
#     data <- data[c((index[1,1]+7):nrow(data)), c(index[1,2]:(index[1,2]+50))]
#     colnames(data) <- data[1,]
#     data <- data[-1,]
#     data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
# 
# 
#     data <- data %>%
#       #slice(which(Year %in% c(2015, 2050))) %>%
#       select(Year, MaxExpansion) %>%
#       mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                              str_sub(cur_file, 40, 42),
#                              ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                     str_sub(cur_file, 47, 49)))) %>%
#       mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                              ifelse(ALPHA3 == "R_C", "R_CSA",
#                                     ifelse(ALPHA3 == "R_N", "R_NEU",
#                                            ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                   ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                          ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#       mutate(Pathway = ifelse(grepl("Current", cur_file),
#                               "CurrentTrend",
#                               ifelse(grepl("National", cur_file),
#                                      "NationalCommitments",
#                                      "GlobalSustainability"))) %>%
#       unique() %>%
#       dplyr::mutate_at(vars(Year, MaxExpansion), as.numeric) %>%
#       plyr::arrange(Year) %>%
#       group_by(Pathway) %>%
#       dplyr::mutate(MaxExpansion = cumsum(MaxExpansion)) %>%
#       dcast(Pathway + ALPHA3 ~ Year, value.var = "MaxExpansion") %>%
#       mutate(MaxExpansion =`2050`-`2020`) %>%
#       select(Pathway, ALPHA3, MaxExpansion)
#     #slice(which(Year ==2050))
#   }
# 
# 
#   db_full_expansion <- db_full_expansion %>%
#     rbind.data.frame(data) %>%
#     dplyr::mutate(ALPHA3 = gsub("_", "", ALPHA3))
# }
# 
# write.xlsx(db_full_expansion %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedExpansion.xlsx")), row.names = F)
db_full_expansion <- readxl::read_excel(here("data", "extracted_scenathon", "20240425_ExtractedExpansion.xlsx")) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "RMECAS", "NMC", ALPHA3)) 
# %>% 
#   mutate(ALPHA3 = ifelse(nchar(ALPHA3) == 4, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))

#Productive land expansion constraint in Million ha 
db_change_Expansion <- db_scenarios %>% 
  left_join(db_full_expansion) %>% 
  select(-MaxExpansion) %>% 
  mutate(Expansion_change = as.numeric(ifelse(agricultural_land_expansion == "FreeExpansion", 2,
                                              ifelse(agricultural_land_expansion == "NoDefor", 0.5,
                                                     ifelse(agricultural_land_expansion == "NoDefor2030", 0.5, 0))))) %>% 
  mutate(Pathway = recode(Pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "RMECAS", "R_NMC", ALPHA3))  



# Afforestation -----------------------------------------------------------
# 
# db_full_affor <- data.frame()
# 
# for (cur_file in file){
# 
#     ##print(cur_file)
#     #???Extract the right sheet from calculators
#     data <- read_excel(here("data", "Calcs_new", cur_file),
#                        sheet = "SCENARIOS definition",
#                        range = "A1:JY1272")
#     index <- which(data == "TABLE: AfforScenDef", arr.ind = TRUE)
#     if (plyr::empty(index)) {index <- which(data == "Table: AfforScenDef", arr.ind = TRUE)}
#     print(index)
# 
#     if (!plyr::empty(index)) {  # Don't know if the table is in the calculator; we check before digging in
#       # If it is in the calc then we only want a certain amount of columns after "Biofuel_scen" cell
#       data <- data[c(index[1,1]:nrow(data)) + ifelse(grepl("CAN", cur_file), 8, 9),
#                    c(index[1,2]:(index[1,2]+10))]
#       colnames(data) <- data[1,]
#       data <- data[-1,]
#       data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
#     }
# 
#     data <- data %>%
#       slice(which(Year %in% c(2020, 2050))) %>%
#       rename_all(.funs = tolower) %>%
#       select(year, afforscen, newforest) %>%
#       mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                              str_sub(cur_file, 40, 42),
#                              ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                     str_sub(cur_file, 47, 49)))) %>%
#       mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                              ifelse(ALPHA3 == "R_C", "R_CSA",
#                                     ifelse(ALPHA3 == "R_N", "R_NEU",
#                                            ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                   ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                          ifelse(ALPHA3 == "RME", "NMC", ALPHA3))))))) %>%
#       mutate(Pathway = ifelse(grepl("Current", cur_file),
#                               "CurrentTrend",
#                               ifelse(grepl("National", cur_file),
#                                      "NationalCommitments",
#                                      "GlobalSustainability"))) %>%
#       unique()
# 
#     db_full_affor <- db_full_affor %>%
#       rbind.data.frame(data) %>%
#       mutate(ALPHA3 = gsub("_", "", ALPHA3))
#   }
# 
# 
# 
# db_full_affor_temp <- db_full_affor %>%
# mutate(ALPHA3 = ifelse(grepl("\\.", ALPHA3), stringr::str_sub(ALPHA3, 1, 3), ALPHA3))
# 
# write.xlsx(db_full_affor %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_1ExtractedAfforestation.xlsx")), row.names = F)


db_full_affor <- read_excel(here("data", "extracted_scenathon", "20240426_1ExtractedAfforestation.xlsx")) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "RASP", "R_ASP",
                         ifelse(ALPHA3 == "RCSA", "R_CSA",
                                ifelse(ALPHA3 == "RNEU", "R_NEU",
                                       ifelse(ALPHA3 == "ROEU", "R_OEU",
                                              ifelse(ALPHA3 == "RSSA", "R_SSA",
                                                     ifelse(ALPHA3 == "RMECAS", "NMC", ALPHA3))))))) %>%
  # Convert newforest column to numeric
  mutate(newforest = as.numeric(newforest)) %>%
  group_by(year, afforscen, ALPHA3, Pathway ) %>% 
  mutate(newforest = sum(newforest, na.rm = TRUE)) %>% 
  unique()



#Extract the afforestation target
db_full_afforestation_agg <- db_scenarios %>% 
  mutate(ALPHA3 =ifelse(ALPHA3 == "RMECAS", "NMC", 
                        ifelse(ALPHA3 == "TUR", "TUR", ALPHA3))) %>% 
  unique() %>% 
  select(-agricultural_land_expansion) %>% 
  mutate(Pathway = if_else(Pathway == "NationalCommitment", "NationalCommitments", Pathway)) %>%
  left_join(db_full_affor %>% 
              rename(Year = year, afforestation = afforscen)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(NewForest = as.numeric(newforest)) %>% 
  select(ALPHA3, Pathway, Year, NewForest)




#Computing afforestation absolute difference 2020 - 2050 in Million ha (Mha)   
db_change_afforestation <- db_full_afforestation_agg %>% 
  mutate(var_pivot = paste0("affor_", Year)) %>% 
  select(var_pivot, Pathway, ALPHA3, NewForest) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = NewForest) %>% 
  select(-affor_NA) %>% 
  mutate(Affor = (affor_2050-affor_2020)) %>%
  mutate(across(starts_with("affor_"), as.numeric)) %>%
  select(ALPHA3, Pathway, Affor) %>% 
  data.frame() %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "NMC", "R_NMC", ALPHA3))
# %>% 
  # mutate(Affor = ifelse(affor > 20000 , 20000, affor))


# db_change_afforestation <- df %>% 
#   filter(year == "2020") %>% 
#   mutate(Pathway = if_else(Pathway == "CurrentTrends", "CurrentTrend", Pathway)) %>%
#   left_join(db_change_afforestation) %>% 
#   select(ALPHA3, Pathway, Affor, calcforest) %>% 
#   mutate(Affor = (Affor/calcforest)) %>% 
#   select(-calcforest)




# %>% 
#   mutate(Affor = ifelse(Affor > 10 , 10, Affor))
# 



# Food Waste --------------------------------------------------------
# 
# db_full_waste <- data.frame()
# 
# for (cur_file in file){
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                      sheet = "1_calc_human_demand",
#                      range = "A27:AP1116")
# 
#   if(grepl("DNK|GRC|NPL|R_ASP|R_CSA|R_NEU|R_OEU|R_SSA|RMECAS|TUR", cur_file)){
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                        sheet = "1_calc_human_demand",
#                        range = "A31:AQ1116")
#   }
# 
# 
# 
#   data <- data %>%
#     slice(which(year %in% c(2020, 2050))) %>%
#     select(year, food_waste, LOSS_SCEN, fproduct, prodgroup) %>%
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                            str_sub(cur_file, 40, 42),
#                            ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                   str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "ASP",
#                            ifelse(ALPHA3 == "R_C", "CSA",
#                                   ifelse(ALPHA3 == "R_N", "NEU",
#                                          ifelse(ALPHA3 == "R_O", "OEU",
#                                                 ifelse(ALPHA3 == "R_S", "SSA",
#                                                        ifelse(ALPHA3 == "RME", "NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# 
#   db_full_waste <- db_full_waste %>%
#     rbind.data.frame(data)
# }
# 
# write.xlsx(db_full_waste %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedFoodWaste.xlsx")), row.names = F)
# 

db_full_waste <- readxl::read_excel(here("data", "extracted_scenathon", "20240425_ExtractedFoodWaste.xlsx")) %>% 
  # mutate(ALPHA3 = ifelse(nchar(ALPHA3) == 4, stringr::str_sub(ALPHA3, 2, 4), ALPHA3)) %>% 
  rename(FPRODUCT = fproduct) %>% 
  mutate(FPRODUCT = ifelse(FPRODUCT == "MILK", "milk", FPRODUCT))


#Computing weighted Food waste: share of product kcal content to total Kcal content
db_change_foodwaste <- db_full_waste %>%
  dplyr::filter(year==2050) %>% 
  mutate(FPRODUCT = tolower(FPRODUCT)) %>%  
  left_join(df_fao) %>%
  dplyr::filter(!is.na(kcal)) %>%
  group_by(year, ALPHA3, Pathway) %>% 
  mutate(total_kcal = sum(kcal, na.rm = TRUE)) %>% ##from here added new lines (See if the results makes sense)
  ungroup() %>% 
  mutate(kcal_without_fw = kcal/(1-food_waste)) %>% 
  mutate(food_waste_kcal = kcal_without_fw - kcal) %>% 
  group_by(year, ALPHA3, Pathway) %>% 
  mutate(sum_food_waste_kcal = sum(food_waste_kcal)) %>% 
  mutate(food_waste2050 = sum_food_waste_kcal/total_kcal) %>% 
  ungroup() %>% 
  select(ALPHA3, Pathway, food_waste2050) %>% 
  rename(Foodwaste_change=food_waste2050) %>% 
  unique() %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "ASP", "R_ASP",
                         ifelse(ALPHA3 == "CSA", "R_CSA",
                                ifelse(ALPHA3 == "NEU", "R_NEU",
                                       ifelse(ALPHA3 == "OEU", "R_OEU",
                                              ifelse(ALPHA3 == "SSA", "R_SSA",
                                                     ifelse(ALPHA3 == "NMC", "R_NMC", ALPHA3))))))) ################ SWhould be final (*do not run next food waste code ) To verify if works when merging the data


# To re add back above, if the results above make no sense
# %>%
#   mutate(weight =  100 * kcal / total_kcal) %>%
#   ungroup() %>%
#   group_by(ALPHA3, Pathway, FPRODUCT) %>% 
#   mutate(food_waste_weighted = food_waste * weight)
# 
# options(scipen=999)

#Computing share of food waste relative change 2020 - 2050 
# db_change_foodwaste <- db_change_foodwaste %>%
#   select(year, ALPHA3, Pathway, FPRODUCT, food_waste_weighted) %>% 
#   pivot_wider(names_from = year, names_glue = "Foodwaste_{year}", values_from = food_waste_weighted) %>% 
#   mutate(Foodwaste_change = Foodwaste_2050 / Foodwaste_2020) %>% 
#   group_by(ALPHA3, Pathway) %>% 
#   mutate(Foodwaste_change = mean(Foodwaste_change, na.rm = TRUE)) %>%
#   select(-FPRODUCT, -Foodwaste_2020, -Foodwaste_2050) %>% 
#   ungroup() %>% 
#   distinct() %>% 
#   mutate(ALPHA3 = ifelse(ALPHA3 == "ASP", "R_ASP",
#                       ifelse(ALPHA3 == "CSA", "R_CSA",
#                              ifelse(ALPHA3 == "NEU", "R_NEU",
#                                     ifelse(ALPHA3 == "OEU", "R_OEU",
#                                            ifelse(ALPHA3 == "SSA", "R_SSA",
#                                                   ifelse(ALPHA3 == "NMC", "R_NMC", ALPHA3)))))))

# Protected Areas -----------------------------------------------------------
#Extracting Total Land

total_land <- df %>% 
  select(ALPHA3, totalland, year) %>% 
  dplyr::filter (year==2050)


#Extracting data from the Calculators - only run when needed

# 
# db_pa <- data.frame()
# 
# for (cur_file in file){
#   data <- read_excel(here("data", "Calcs_new", cur_file),
#                      sheet = "SCENARIOS definition",
#                      range = "AA1:ZZ1000")
#   index <- which(data == "TABLE: Patarget_def", arr.ind = TRUE)
# 
#   if (plyr::empty(index)) {
#     index <- which(data == "TABLE: PATarget_def", arr.ind = TRUE)
#   }
# 
# 
#   if(!plyr::empty(index)){
#     data <- data[c(index[1,1]:nrow(data)) + ifelse(grepl("ETH", cur_file), 8,
#                                                   ifelse(grepl("CAN", cur_file), 8, 9)),
#                  c(index[1,2]:(index[1,2]+8))]
#     colnames(data) <- data[1,]
#     data <- data[-1,]
#     data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
# 
#     data <- data %>%
#       slice(which(Year %in% c(2020, 2050))) %>%
#       rename_all(.funs = tolower) %>%
#       select(lcagg, year, paareatarget) %>%
#       mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                              str_sub(cur_file, 40, 42),
#                              ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                     str_sub(cur_file, 47, 49)))) %>%
#       mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                              ifelse(ALPHA3 == "R_C", "R_CSA",
#                                     ifelse(ALPHA3 == "R_N", "R_NEU",
#                                            ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                   ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                          ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#       mutate(Pathway = ifelse(grepl("Current", cur_file),
#                               "CurrentTrend",
#                               ifelse(grepl("National", cur_file),
#                                      "NationalCommitments",
#                                      "GlobalSustainability"))) %>%
#       unique()
#   }
#   db_pa <- db_pa %>%
#     rbind.data.frame(data)
#   # %>%
#   #   mutate(ALPHA3 = gsub("_", "", ALPHA3))
# 
# }
# 
# write.xlsx(db_pa %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedPA.xlsx")), row.names = F)

### Manually added values for MEX and SWE
db_pa <- readxl::read_excel(here("data", "extracted_scenathon", "20240427_ExtractedPA.xlsx"))

db_change_pa <- db_pa %>%
  mutate(paareatarget = as.numeric(paareatarget)) %>%
  dplyr::filter(year==2050) %>% 
  group_by(year, ALPHA3, Pathway) %>%
  mutate(Total_PA = sum(paareatarget, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-lcagg, -paareatarget, -year) %>%
  distinct() %>% 
  left_join(total_land, relationship = "many-to-many") %>% 
  mutate(pa = round(as.numeric(Total_PA) / as.numeric(totalland), 3)) %>% 
  select(-Total_PA, -totalland, -year ) %>% 
  unique() 
  

         
         
# db_change_pa <- db_pa %>%
#   group_by(ALPHA3, Pathway) %>%
#   mutate(pa = round((as.numeric(Total_PA[year == "2050"]) - as.numeric(Total_PA[year == "2020"])) / as.numeric(Total_PA[year == "2020"]), digits = 2)) %>%
#   select(-year, -Total_PA) %>%
#   unique() %>% 
#   mutate(pa = ifelse(pa > 2.5 , 2.5, pa))





#Extracting data from the Calculators - only run when needed

# db_irr <- data.frame()
# 
# for (cur_file in file){
#   #???print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
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
#     select(YEAR, CROP, Harvarea, IrrHarvArea) %>%
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                            str_sub(cur_file, 40, 42),
#                            ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                   str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                            ifelse(ALPHA3 == "R_C", "R_CSA",
#                                   ifelse(ALPHA3 == "R_N", "R_NEU",
#                                          ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                 ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                        ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
#   
#   db_irr <- db_irr %>%
#     rbind.data.frame(data) %>%
#     data.frame()
# }
# 
# write.xlsx(db_irr %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedIrr.xlsx")), row.names = F)



db_irr <- readxl::read_excel(here("data", "extracted_scenathon", "20240507_ExtractedIrr.xlsx")) %>% 
  dplyr::filter(YEAR == 2050)

options(scipen = 999)

db_change_irr <- db_irr %>%
  group_by(ALPHA3, Pathway) %>%
  mutate(total_harvarea = sum(Harvarea, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ALPHA3, Pathway, CROP) %>%
  mutate(Harvarea_per_crop = Harvarea / total_harvarea) %>% 
  ungroup() %>% 
  group_by(ALPHA3, Pathway) %>%
  mutate(share_irr = (IrrHarvArea / Harvarea) *Harvarea_per_crop) %>% 
  mutate(share_irr_final = round(sum(share_irr, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  select(ALPHA3, YEAR, Pathway, share_irr_final) %>% 
  unique() %>%  
  select(-YEAR)





# db_agrprac <- data.frame()
# 
# for (cur_file in file){
#   #???print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calcs_new", cur_file),
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
#     select(YEAR, CROP, SPAMgroup, Harvarea, AreaAgroeco) %>%
#     mutate(ALPHA3 = ifelse(grepl("Current", cur_file),
#                            str_sub(cur_file, 40, 42),
#                            ifelse(grepl("National", cur_file), str_sub(cur_file, 46, 48),
#                                   str_sub(cur_file, 47, 49)))) %>%
#     mutate(ALPHA3 = ifelse(ALPHA3 == "R_A", "R_ASP",
#                            ifelse(ALPHA3 == "R_C", "R_CSA",
#                                   ifelse(ALPHA3 == "R_N", "R_NEU",
#                                          ifelse(ALPHA3 == "R_O", "R_OEU",
#                                                 ifelse(ALPHA3 == "R_S", "R_SSA",
#                                                        ifelse(ALPHA3 == "RME", "R_NMC", ALPHA3))))))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# 
#   db_agrprac <- db_agrprac %>%
#     rbind.data.frame(data) %>%
#     data.frame()
# }
# 
# write.xlsx(db_agrprac %>% data.frame(), file = here("data", "extracted_scenathon", paste0(gsub("-", "",Sys.Date()), "_ExtractedAgrprac.xlsx")), row.names = F)




db_agrprac <- readxl::read_excel(here("data", "extracted_scenathon", "20240514_ExtractedAgrprac.xlsx")) %>% 
  dplyr::filter(YEAR == 2050)

options(scipen = 999)

db_change_agrprac <- db_agrprac %>%
  group_by(ALPHA3, Pathway) %>%
  mutate(total_harvarea = sum(Harvarea, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ALPHA3, Pathway, CROP) %>%
  mutate(Harvarea_per_crop = Harvarea / total_harvarea) %>% 
  ungroup() %>% 
  group_by(ALPHA3, Pathway) %>%
  mutate(share_agrprac = (AreaAgroeco / Harvarea) *Harvarea_per_crop) %>% 
  mutate(share_agrprac_final = round(sum(share_agrprac, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  select(ALPHA3, YEAR, Pathway, share_agrprac_final) %>% 
  unique() %>%  
  select(-YEAR)



# data final --------------------------------------------------------------

data_final <- df_change %>% 
  left_join(db_change_crop) %>% 
  left_join(db_change_Live_Prod) %>% 
  left_join(db_change_RumDensity) %>% 
  left_join(product_tot) %>% 
  left_join(db_change_Expansion) %>% 
  left_join(db_change_afforestation) %>%
  left_join(db_change_foodwaste) %>% 
  left_join(db_change_pa) %>% 
  # left_join(db_change_irr) %>% 
  # left_join(db_change_agrprac) %>% 
  dplyr::filter(ALPHA3 != "WORLD")
# %>% 
  # mutate(ALPHA3 = ifelse(ALPHA3 == "RMECAS", "NMC", ALPHA3)) 
# %>%
#   mutate(ALPHA3 = ifelse(nchar(ALPHA3) == 5, str_sub(ALPHA3, 3, 5), ALPHA3))

data_final_FABLE <- data_final %>% 
  #slice(which(!(ALPHA3 %in% c("ASP", "NEU", "NMC", "ROEU", "SSA", "CSA")))) %>% 
  data.frame()
# 
# 
# 
# 
# 
# # Select variables for normalization
# selected_vars <- c("Population_change", "kcal_targ_change", "pdty_crop_change", "pdty_live_change",
#                    "density_change", "Expansion_change", "Export_quantity_change", "Import_quantity_change",
#                    "Foodwaste_change"
#                    # , "Affor", "pa"
#                    )
# 
# data_final_FABLE_k <- data_final_FABLE %>%
#   group_by(ALPHA3) %>%
#   mutate(across(all_of(selected_vars), ~ mean(., na.rm = TRUE))) %>%
#   ungroup()
# 
# # Normalize variables
# normalized_data <- scale(data_final_FABLE_k[selected_vars])
# 
# # Replace missing values with the mean of each variable
# normalized_data <- na.aggregate(normalized_data, FUN = mean)
# 
# #Veryfying which is the best number of cluster
# # fviz_nbclust(normalized_data, kmeans, method = "wss")
# # fviz_nbclust(normalized_data, kmeans, method = "silhouette")
# 
# 
# 
# # number of clusters
# k <- 5
# 
# set.seed(234)  
# 
# # Perform k-means clustering
# cluster_results <- kmeans(normalized_data, centers = k)

# fviz_cluster(cluster_results, data = normalized_data)


# Compute cluster centers
# cluster_centers <- cluster_results$centers

# # Calculate variability of each variable across clusters
# variability <- apply(cluster_centers, 2, sd)
# 
# # Rank variables by variability
# variable_importance <- sort(variability, decreasing = TRUE)
# 
# # Print variable importance
# print(variable_importance)


# Add cluster labels to the original data
# data_final_FABLE_k$cluster <- cluster_results$cluster
# 
# 
# ggplot(data_final_FABLE_k, aes(x = cluster, y =ALPHA3 )) +
#   geom_point() +
#   labs(title = "Cluster - Country CT")

# #Without taking into consideratio the weight of each country
# cluster_summary <- data_final_FABLE_k %>%
#  
# 
# clusters <- data_final_FABLE_k %>% 
#   select(ALPHA3, cluster) %>% 
#   unique()
# 
# 
# data_final_FABLE <- data_final_FABLE %>% 
#   left_join(clusters, by="ALPHA3") %>% 
#   group_by(cluster, Pathway) %>%
#   mutate(across(all_of(selected_vars), mean, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   select(-cluster) 
#   
# # 
# #BOX PLOTS
# data_final_FABLE_box <- data_final_FABLE %>%
#   left_join(clusters, by="ALPHA3") %>% 
#   select(-Pathway, -ALPHA3)
# 
# # Define the variables of interest
# variables <- c("Population_change", "kcal_targ_change", "pdty_crop_change", "pdty_live_change",
#                "density_change", "Expansion_change", "Export_quantity_change", "Import_quantity_change",
#                "Foodwaste_change")
# 
# # Reshape the data from wide to long format
# data_long <- tidyr::pivot_longer(data_final_FABLE_box, cols = variables, names_to = "variable", values_to = "value")
# 
# # Create boxplots for each variable, faceted by cluster
# ggplot(data_long, aes(x = factor(cluster), y = value)) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales = "free_y", ncol = 3) +
#   labs(title = "Boxplots of variables by clluster") +
#   theme_minimal()
# 

melted <- melt(data_final_FABLE, id.vars = c("ALPHA3", "Pathway", "afforestation", "agricultural_land_expansion")) 
melted$value <- ifelse(melted$variable == "pa", melted$value,
                       ifelse(melted$variable == "share_irr_final", melted$value,
                              ifelse(melted$variable == "share_agrprac_final", melted$value,
                       ifelse(melted$variable == "Foodwaste_change", melted$value,
                       ifelse(
                         melted$value!= "NaN" & melted$variable != "Affor",
                         melted$value-1,
                         ifelse(melted$variable == "Affor",
                                melted$value,
                                NA))))))
melted$sign <- ifelse(melted$value < 0,
                      0,
                      1)

complete_data <- melted %>% 
  select(-afforestation, -agricultural_land_expansion)






### All info in one graph

var.labs <- c(
  Population_change = "Population",
  kcal_targ_change = "Calories \nper Capita",
  pdty_crop_change = "Crops \nProductivity",
  pdty_live_change = "Livestock \nProductivity",
  density_change = "Ruminant \nDensity ",
  Expansion_change ="Agricultural \nexpansion \n(i)",
  Export_quantity_change = "Exports \n(kcal)",
  Import_quantity_change = "Imports \n(kcal)",
  Foodwaste_change = "Share of \n Food Waste \n(iii)",
  Affor = "Afforestation \n(1000ha) \n(ii)",
  pa= "Protected \n Areas \n(iv)",
  share_irr_final= "Irrigated \nArea\n(v)",
  share_agrprac_final= "Area under \nAgricological \nPractises\n(v)"
)


complete_data$Pathway <- factor(as.character(complete_data$Pathway), levels = c("GlobalSustainability", "NationalCommitments", "CurrentTrend"))

complete_data <- complete_data %>%
  mutate(ALPHA3 = ifelse(ALPHA3 == "GBR",
                         "UK",
                         ALPHA3))



complete_data$ALPHA3 <- gsub("^R_", "", complete_data$ALPHA3)



complete_data$ALPHA3 <- factor(as.character(complete_data$ALPHA3), levels = c("ARG", "AUS", "BRA", "CAN",
                                                                              "CHN", "COL", "DEU", "DNK","ETH",
                                                                              "FIN", "GRC","IDN", 'IND',
                                                                              "MEX", "NPL", "NOR", "RUS",
                                                                              "RWA", "SWE", "TUR", "UK", "USA",
                                                                              "ASP", "CSA", "NEU", "NMC",
                                                                              "OEU", "SSA"))




### Merge cluster data with complete_data
# complete_data <- complete_data %>% 
#   left_join(clusters, by="ALPHA3")
# 
# 


# Define the order of Pathway levels
pathway_order <- c("CurrentTrend", "NationalCommitments", "GlobalSustainability")

# cluster_labels <- c(`1` = "NOR, DNK, DEU", `2` = "ARG, AUS, GRC, \n MEX, R_NEU, R_OEU,\nRUS, TUR, USA", `3` = "AFRICA", 
#                     `4` = "COL, IDN,\n NPL, R_ASP", `5` = "BRA, CAN, CHN,\n FIN, GBR, IND,\n R_CSA, RMECAS, SWE")

# complete_data_country <- complete_data %>% 
#   dplyr::filter(!ALPHA3 %in% c("R_ASP", "R_CSA", "R_NEU", "R_NMC",
#          "R_OEU", "R_SSA"))
# complete_data_region <- complete_data %>% 
#   dplyr::filter(ALPHA3 %in% c("R_ASP", "R_CSA", "R_NEU", "R_NMC",
#                         "R_OEU", "R_SSA"))


# complete_data <- complete_data %>%
#   mutate(ALPHA3 = gsub("^R_", "", ALPHA3))  
# 


# # Reorder ALPHA3 with new levels
# complete_data$ALPHA3 <- factor(complete_data$ALPHA3, levels = c("ARG", "AUS", "BRA", "CAN", "CHN", "COL", "DEU", "DNK", "ETH", "FIN", "GRC", "IDN", "IND", 
#                                                                 "MEX", "NPL", "NOR", "RUS", "RWA", "SWE", "TUR", "UK", "USA", "ASP", "CSA", "NEU", "NMC", 
#                                                                 "OEU", "SSA"))
# 



p_final <- ggplot(complete_data, aes(y = value, x = reorder(Pathway, -as.numeric(factor(Pathway, levels = pathway_order))), group = ALPHA3, fill = sign)) +
  geom_col(position = "dodge", show.legend = FALSE)+
  ylab("Relative change between 2020 and 2050 (2020=0)")+
  coord_flip()+
  scale_x_discrete(labels = c(GlobalSustainability = "GS",
                              NationalCommitments = "NC",
                              CurrentTrend ="CT"))+
  scale_y_continuous(n.breaks = 3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_grid(ALPHA3~variable,
             switch = "y",
             labeller = labeller(variable = var.labs),
             drop = T,
             space = "fixed",
             scale = "free")+
  theme(
    panel.background = element_rect(fill = '#F2F2F2'),
    panel.grid  = element_blank(),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background=element_blank(),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 12),
      strip.text = element_text(size = 12, face = "bold"),
      panel.spacing.x = unit(0.75, "lines"),
      plot.caption = element_text(size = 14),
      strip.text.y.left = element_text(angle = 0),
      axis.text = element_text(size = 10),
      axis.title.x = element_text(size = 20),
      axis.line.x = element_line()
    )
# +
#   labs(caption = "(i) Results are expressed in code, taking the value 1 for 'Free expansion scenario', -1 for 'No deforestation' and -2 for 'No Agricultural expansion'.
#   \n(ii) Results are expressed in net increase rather than relative change.
#   \n(iii) Results are expressed % of consumption which is wasted.
#   \n(iv) Results are expressed in % of total land in 2050.
#   \n(v) Results are expressed in % of harvest area in 2050.
# ")
  #  




width = 14
height = 30
print(p_final)


tiff(here("output", "figures", paste0(gsub("-", "",Sys.Date()), "_", "ScenarioAssumptionScenathon.tiff")),
     units = "in", height = 14, width = 18, res = 300)
plot(p_final)
dev.off()



