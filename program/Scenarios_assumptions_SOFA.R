## Scenario selection


#install.packages("reshape2")

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

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")

# file --------------------------------------------------------------------

file_SOFA <- list.files(path = here("data", "Calc_UP46"))

# data --------------------------------------------------------------------

df <- read.csv(here("data", "FullDataBase.csv")) 
# %>%  dplyr::filter(TradeAdjustment == "No")
product <- read.csv(here("data",  "FullProductDataBase.csv")) 
# %>% dplyr::filter(TradeAdjusment == "No")
mapping_F6 <- read_excel(here("data",  "DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map")

##### Problem with this #####
db_scenarios <- read_excel(here("data", "scenarios2023.xlsx")) %>%
  select(pathway, country, `Scenario on Afforestation`)%>% 
  rename(ALPHA3 = country,
         Pathway = pathway)



fao_prod <- read.csv(here("data", "FAO_FoodBalance.csv"))
mapping_animalprod <- read.csv(here("data", "FAOSTAT_mapping_animalprod.csv"))

fao_prod <- fao_prod %>% 
  inner_join(mapping_animalprod, by = "Item")


mapping <- read_excel(here("data",  "mapping_GAMS_FAO_products.xlsx"))
mapping[which(mapping$FAO == "Groundnuts (Shelled Eq)"), "FAO"] <- "Groundnuts"

mapping_country <- read_excel(here("data", "mapping_country_FAO_FABLE.xlsx")) %>% 
  mutate(iso3c = countrycode::countrycode(sourcevar = Country_FAO, origin = "country.name", destination = "iso3c"))
mapping_ALPHA3 <- read_excel(here("data",  "mapping_alpha3_Country.xlsx")) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3))

fao_prod$Area <- as.character(fao_prod$Area)
fao_prod[which(fao_prod$Area == "United Kingdom of Great Britain and Northern Ireland"), "Area"] <- "United Kingdom"


product_dt <- left_join(product %>% 
                          rename(alpha3 = country_id) %>% 
                          mutate(alpha3 = as.character(alpha3)) %>% 
                          select(#-id, 
                                 -iteration, 
                                 -scenathon_id), 
                        df %>% 
                          rename(alpha3 = country) %>% 
                          select(#-id, 
                                 -iteration,
                                 -scenathon_id),
                        by = c("alpha3", "pathway", "Year")) %>% 
  rename(ALPHA3 = alpha3) %>% 
  left_join(mapping_ALPHA3) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3))


##Compute kcal/kg data from FAO 2020
df_fao <- fao_prod %>% 
  left_join(mapping %>% select(FPRODUCT, FAO) %>% unique(), by = c("Item" = "FAO")) %>% 
  mutate(iso3c = countrycode::countrycode(sourcevar = Area, origin = "country.name", destination = "iso3c")) %>% 
  left_join(mapping_country, by = c("iso3c")) %>% 
  left_join(mapping_ALPHA3, by = c("Country_FABLE" = "Country")) %>% 
  select(ALPHA3, FPRODUCT, Element, Value) 

df <- df %>% 
  rename(ALPHA3 = country) %>% 
  rename(Pathway = pathway)

product <- product %>% 
  rename(ALPHA3 = country_id)


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
             data.frame())

product_dt <- left_join(product_dt, df_fao, by = c("ALPHA3" = "ALPHA3", "Product" = "FPRODUCT")) %>% 
  rename(Pathway = pathway)

#Computing Population and Kcal target relative change 2020-2050

#####################   PROBLEM WITH THIS !!!

df_change <- df %>% 
  slice(which(Year %in% c(2020, 2050))) %>% 
  select(ALPHA3, Pathway, Year, Population, kcal_targ) %>% 
  pivot_wider(names_from = Year,
              values_from = c(Population, kcal_targ)) %>% 
  mutate(Population_change = round(Population_2050/Population_2020, 2)) %>% 
  mutate(kcal_targ_change = round(kcal_targ_2050/kcal_targ_2020,2)) %>% 
  select(ALPHA3, Pathway, Population_change, kcal_targ_change) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  data.frame()

#Computing imports and exports quantity relatve changes 2020-2050, using Kcal per Kg to aggregate all products
product_tot <- product_dt %>% 
  slice(which(Year %in% c(2020, 2050))) %>%
  mutate(Export_quantity = Export_quantity * kcal.kg) %>% 
  mutate(Import_quantity = Import_quantity * kcal.kg) %>% 
  group_by(ALPHA3, Pathway, Year) %>%
  dplyr::summarise(Export_quantity = sum(Export_quantity, na.rm = T),
                   Import_quantity = sum(Import_quantity, na.rm = T)) %>%
  select(ALPHA3, Pathway, Year,
         Export_quantity, Import_quantity) %>% 
  pivot_wider(names_from = Year,
              values_from = c(Export_quantity, Import_quantity)) %>% 
  mutate(Export_quantity_change = round(Export_quantity_2050/Export_quantity_2020, 2)) %>% 
  mutate(Import_quantity_change = round(Import_quantity_2050/Import_quantity_2020, 2)) %>% 
  select(ALPHA3, Pathway, Export_quantity_change, Import_quantity_change) %>% 
  mutate(ALPHA3 = gsub("R_", "", ALPHA3)) %>% 
  data.frame() %>% 
  ## manually put lower for NPL because way too high and can't see the others
  ## Extreme value to flag to Shyam
  mutate(Export_quantity_change = ifelse(ALPHA3 == "NPL", 4, Export_quantity_change)) %>% 
  mutate(Import_quantity_change = ifelse(ALPHA3 == "ETH", 4, Import_quantity_change))


# pdty_livestock ---------------------------------------------------------- OKAY


# Extracting data from the Calculators - only run when needed
# db_full <- data.frame()
# 
# for (cur_file in file_SOFA){
#   #extract the righ sheet in Calculator
#   data <- read_excel(here("data", "Calc_UP46", cur_file),
#                      sheet = "2_calc_livestock",
#                      range = "A30:Z173")
# 
# 
#   data <- data %>%
#     slice(which(herdcount == 1)) %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, ANIMAL, FPRODUCT, herd, pdtyanim) %>%
#     #get right name for regions
#     mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability")))
# 
#   db_full <- db_full %>%
#     rbind.data.frame(data)
# 
# 
# }
# write.xlsx(db_full %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedPdtyLivestock.xlsx")), row.names = F)

#Reading extracted data
db_full <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedPdtyLivestock.xlsx")) 

#Computing Livestock productivity in t/TLU
db_full_agg <- db_full %>%
  # use the herd in TLU to later compute a weighted average
  mutate(weight_pdty = herd*pdtyanim) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_pdty = sum(weight_pdty, na.rm = T),
                   herd = sum(herd, na.rm = T)) %>%
  #weighted average
  mutate(pdty = weight_pdty/herd)
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
  select(ALPHA3, Pathway, pdty_live_change)



# Ruminent density -------------------------------------------------------- OKAY

#Extracting data from the Calculators - only run when needed 

# db_full2 <- data.frame()
# 
# for (cur_file in file_SOFA){
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calc_UP46", cur_file),
#                      sheet = "2_calc_livestock",
#                      range = "BH30:BV74")
# 
#   data <- data %>%
#     slice(which(YEAR %in% c(2020, 2050))) %>%
#     select(YEAR, ANIMAL, Pasture, RumDensity) %>%
#     mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
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
# write.xlsx(db_full2 %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedRumDensity.xlsx")), row.names = F)

#Reading extracted data
db_full2 <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedRumDensity.xlsx")) 

#Computing Ruminant density in TLU/ha
db_full2_agg <- db_full2 %>%
  # use the pasture surface in ha to later compute a weighted average
  mutate(weight_dens = Pasture*RumDensity) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_dens = sum(weight_dens, na.rm = T),
                   Pasture = sum(Pasture, na.rm = T)) %>%
  #weighted average
  mutate(density = weight_dens/Pasture)

#Computing Ruminant density relative change 2020 - 2050
db_change_RumDensity <- db_full2_agg %>% 
  mutate(var_pivot = paste0("density_", YEAR)) %>% 
  select(var_pivot, Pathway, ALPHA3, density) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(density)) %>% 
  #relative change between 2050 and 2015
  mutate(density_change = round(density_2050/density_2020, 2)) %>% 
  select(ALPHA3, Pathway, density_change)

# Crops productivity ------------------------------------------------------  OKAY

#Extracting data from the Calculators - only run when needed
db_full_crop <- data.frame()

for (cur_file in file_SOFA){
  #???Extract the right sheet from calculators
  data <- read_excel(here("data", "Calc_UP46", cur_file),
                     sheet = "3_calc_crops",
                     range = "G28:AE798")


  data <- data %>%
    slice(which(YEAR %in% c(2020, 2050))) %>%
    select(YEAR, CROP, Harvarea, Pdty) %>%
    mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
    mutate(Pathway = ifelse(grepl("Current", cur_file),
                            "CurrentTrend",
                            ifelse(grepl("National", cur_file),
                                   "NationalCommitments",
                                   "GlobalSustainability"))) %>%
    unique()

  db_full_crop <- db_full_crop %>%
    rbind.data.frame(data) %>%
    data.frame()
}

 write.xlsx(db_full_crop %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedPdtyCrop.xlsx")), row.names = F)

#Reading extracted data
db_full_crop <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedPdtyCrop.xlsx")) 

#Computing Crop productivity in t/ha using the harvested area to aggregate 
db_full_crop_agg <- db_full_crop %>%
  #Use harvested area as weight
  mutate(weight_pdty = Harvarea*Pdty) %>%
  group_by(ALPHA3, Pathway, YEAR) %>%
  dplyr::summarise(weight_pdty = sum(weight_pdty, na.rm = T),
                   Harvarea = sum(Harvarea, na.rm = T)) %>%
  #weighted average
  mutate(pdty = weight_pdty/Harvarea)

#Computing Crop productivity relative change 2020 - -2050
db_change_crop <- db_full_crop_agg %>% 
  mutate(var_pivot = paste0("pdty_", YEAR)) %>% 
  select(var_pivot, Pathway, ALPHA3, pdty) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(pdty)) %>% 
  #relative change betwwen 2050 and 2015
  mutate(pdty_crop_change = round(pdty_2050/pdty_2020, 2)) %>% 
  select(ALPHA3, Pathway, pdty_crop_change) %>% 
  data.frame() 

# Expansion ----------------------------------------------------------------  OKAY

#Extracting data from the Calculators - only run when needed
db_full_expansion <- data.frame()

for (cur_file in file_SOFA){
  print(cur_file)
  #???Extract the right sheet from calculators
  data <- read_excel(here("data", "Calc_UP46", cur_file),
                     sheet = "4_calc_land",
                     range = "A1:AZ100")
  index <- which(data == "TABLE: calc_land_cor" | data == "TABLE:calc_land_cor", arr.ind = T)

  if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
    #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
    data <- data[c((index[1,1]+7):nrow(data)), c(index[1,2]:(index[1,2]+50))]
    colnames(data) <- data[1,]
    data <- data[-1,]
    data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])


    data <- data %>%
      #slice(which(Year %in% c(2015, 2050))) %>%
      select(Year, MaxExpansion) %>%
      mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
      mutate(Pathway = ifelse(grepl("Current", cur_file),
                              "CurrentTrend",
                              ifelse(grepl("National", cur_file),
                                     "NationalCommitments",
                                     "GlobalSustainability"))) %>%
      unique() %>%
      dplyr::mutate_at(vars(Year, MaxExpansion), as.numeric) %>%
      plyr::arrange(Year) %>%
      group_by(Pathway) %>%
      dplyr::mutate(MaxExpansion = cumsum(MaxExpansion)) %>%
      dcast(Pathway + ALPHA3 ~ Year, value.var = "MaxExpansion") %>%
      mutate(MaxExpansion =`2050`-`2020`) %>%
      select(Pathway, ALPHA3, MaxExpansion)
    #slice(which(Year ==2050))
  
  }

  db_full_expansion <- db_full_expansion %>%
    rbind.data.frame(data) %>%
    dplyr::mutate(ALPHA3 = gsub("_", "", ALPHA3))
}

write.xlsx(db_full_expansion %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedExpansion.xlsx")), row.names = F)

#Reading extracted data
db_full_expansion <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedExpansion.xlsx"))

#Productive land expansion constraint in Million ha 
db_change_Expansion <- db_full_expansion %>% 
  mutate(Expansion_change = MaxExpansion/1000) %>% 
  select(ALPHA3, Pathway, Expansion_change)

# Afforestation -----------------------------------------------------------  SHOULD BE OKAY

#Extracting data from the Calculators - only run when needed   ####To revise
# db_full_affor <- data.frame()
#
# for (cur_file in file_SOFA){
#   ##print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calc_UP46", cur_file),
#                      sheet = "SCENARIOS definition",
#                      range = "A1:JY1272")
#   index <- which(data == "TABLE: AfforScenDef", arr.ind = T)
#   if(plyr::empty(index)){index <- which(data == "Table: AfforScenDef", arr.ind = T)}
#   print(index)
# 
#   if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
#     #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
#     data <- data[c(index[1,1]:nrow(data))+ifelse(grepl("CAN", cur_file), 8, 9),
#                  c(index[1,2]:(index[1,2]+6))]
#     colnames(data) <- data[1,]
#     data <- data[-1,]
#     data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
#   
#   data <- data %>%
#     slice(which(Year %in% c(2020, 2050))) %>%
#     rename_all(.funs = tolower) %>%
#     select(year, newforest, afforscen) %>%
#     mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# }
# 
#   db_full_affor <- db_full_affor %>%
#     rbind.data.frame(data)%>%
#     mutate(ALPHA3 = gsub("_", "", ALPHA3))
# }
# 
# write.xlsx(db_full_affor %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedAfforestation.xlsx")), row.names = F)

#Reading extracted data
db_full_affor <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedAfforestation.xlsx"))


#Extract the afforestation target
db_full_afforestation_agg <- db_scenarios %>% 
  unique() %>% 
  left_join(db_full_affor %>% rename(Year = year, Afforestation = afforscen)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(NewForest = as.numeric(newforest)) %>% 
  select(ALPHA3, Pathway, Year, NewForest)

#Computing afforestation absolute difference 2020 - 2050 in Million ha (Mha)    ############## PROBLEM WITH THIS ################
db_change_afforestation <- db_full_afforestation_agg %>% 
  mutate(var_pivot = paste0("affor_", Year)) %>% 
  select(var_pivot, Pathway, ALPHA3, NewForest) %>% 
  pivot_wider(names_from = var_pivot,
              values_from = c(NewForest)) %>% 
  mutate(Affor = round((affor_2050-affor_2020)/1000, 2)) %>% 
  select(ALPHA3, Pathway, Affor) %>% 
  data.frame()


# Food Waste --------------------------------------------------------
#Extracting data from the Calculators - only run when needed
# db_full_waste <- data.frame()
# 
# for (cur_file in file_SOFA){
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "Calc_UP46", cur_file),
#                      sheet = "1_calc_human_demand",
#                      range = "A27:AP1116")
# 
#   data <- data %>%
#     slice(which(year %in% c(2020, 2050))) %>%
#     select(year, food_waste, LOSS_SCEN, fproduct, prodgroup) %>%
#     mutate(ALPHA3 = str_sub(cur_file, 15, 17)) %>%
#     # dplyr::mutate(ALPHA3 = ifelse(ALPHA3 %in% c("NOC", "NOS"), "NOR", ALPHA3)) %>%
# mutate(Pathway = ifelse(grepl("Current", cur_file),
#                         "CurrentTrend",
#                         ifelse(grepl("National", cur_file),
#                                "NationalCommitments",
#                                "GlobalSustainability"))) %>%
# unique()
# 
#   db_full_waste <- db_full_waste %>%
#     rbind.data.frame(data)
# }
# 
# write.xlsx(db_full_waste %>% data.frame(), file = here("data", "extracted", paste0(gsub("-", "",Sys.Date()), "_ExtractedFoodWaste.xlsx")), row.names = F)
# 
# 

#Reading extracted data
db_full_waste <- readxl::read_excel(here("data", "extracted", "20240221_ExtractedFoodWaste.xlsx")) %>% 
  mutate(ALPHA3 = ifelse(nchar(ALPHA3) == 4, stringr::str_sub(ALPHA3, 2, 4), ALPHA3)) %>% 
  rename(FPRODUCT=fproduct) %>% 
  mutate(FPRODUCT = ifelse(FPRODUCT == "MILK", "milk", FPRODUCT))


#Computing weighted Food waste: share of product kcal content to total Kcal content
db_change_foodwaste <- db_full_waste %>%
  left_join(df_fao) %>%
  dplyr::filter(!is.na(kcal)) %>%
  group_by(year, ALPHA3, Pathway) %>% 
  mutate(total_kcal = sum(kcal, na.rm = TRUE)) %>%
  mutate(weight =  100 * kcal / total_kcal) %>%
  ungroup() %>%
  group_by(ALPHA3, Pathway, FPRODUCT) %>% 
  mutate(food_waste_weighted = food_waste * weight)


#Computing share of food waste relative change 2020 - 2050 
db_change_foodwaste <- db_change_foodwaste %>%
  select(year, ALPHA3, Pathway, FPRODUCT, food_waste_weighted) %>% 
  pivot_wider(names_from = year, names_glue = "Foodwaste_{year}", values_from = food_waste_weighted) %>% 
  mutate(Foodwaste_change = Foodwaste_2050 / Foodwaste_2020) %>% 
  group_by(ALPHA3, Pathway) %>% 
  mutate(Foodwaste_change = mean(Foodwaste_change, na.rm = TRUE)) %>%
  select(-FPRODUCT, -Foodwaste_2020, -Foodwaste_2050) %>% 
  ungroup() %>% 
  distinct()




# Protected Areas -----------------------------------------------------------
#Extracting data from the Calculators - only run when needed
# db_pa <- data.frame()
# 
# for (cur_file in file_SOFA){
#   data <- read_excel(here("data", "scenathon_2023", "Calc_UP44", cur_file),
#                      sheet = "SCENARIOS definition",
#                      range = "JA1:KF150")
#   index <- which(data == "TABLE: PATarget_def", arr.ind = T)
#   if(plyr::empty(index)){index <- which(data == "Table: PATarget_def", arr.ind = T)}
#   print(index)
# 
#   if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
#     #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
#     data <- data[c(index[1,1]:nrow(data))+ifelse(grepl("CAN", cur_file), 8, 9),
#                  c(index[1,2]:(index[1,2]+6))]
#     colnames(data) <- data[1,]
#     data <- data[-1,]
#     data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
#   }
# 
#   data <- data %>%
#     slice(which(Year %in% c(2020, 2050))) %>%
#     rename_all(.funs = tolower) %>%
#     select(year, LCAgg, PAareatarget, PAcalc) %>%
#     mutate(ALPHA3 = ifelse(grepl("_R_", cur_file),
#                            str_sub(cur_file, 26, 30),
#                            str_sub(cur_file, 26, 28))) %>%
#     mutate(Pathway = ifelse(grepl("Current", cur_file),
#                             "CurrentTrend",
#                             ifelse(grepl("National", cur_file),
#                                    "NationalCommitments",
#                                    "GlobalSustainability"))) %>%
#     unique()
# 
#   db_pa <- db_pa %>%
#     rbind.data.frame(data)%>%
#     mutate(ALPHA3 = gsub("_", "", ALPHA3))
# }
# 
# # db_full_affor_temp <- db_full_affor %>% 
# #   mutate(ALPHA3 = ifelse(grepl("\\.", ALPHA3), stringr::str_sub(ALPHA3, 1, 3), ALPHA3))
# 
# xlsx::write.xlsx(db_full_affor %>% data.frame(), file = here("data", "scenathon_2023", paste0(gsub("-", "",Sys.Date()), "_ExtractedAfforestation.xlsx")), row.names = F)




# data final --------------------------------------------------------------

data_final_FABLE <- df_change %>% 
  left_join(db_change_crop) %>% 
  left_join(db_change_Live_Prod) %>% 
  left_join(db_change_RumDensity) %>% 
  left_join(product_tot) %>% 
  # left_join(db_change_Expansion) %>% 
  left_join(db_change_afforestation) %>% 
  left_join(db_change_foodwaste) %>% 
  data.frame()


# plot --------------------------------------------------------------------

melted <- melt(data_final_FABLE)
melted$value <- ifelse(melted$value!= "NaN" & melted$variable != "Affor",
                       melted$value-1,
                       ifelse(melted$variable == "Affor",
                              melted$value,
                              NA))
melted$sign <- ifelse(melted$value < 0,
                      0,
                      1)

complete_data <- melted


### All info in one graph

var.labs <- c(Population_change = "Population \n(million)",
              kcal_targ_change = "Calories \n per Capita \n(kcal/cap)",
              pdty_crop_change = "Crops \nProductivity \n(t/ha)",
              pdty_live_change = "Livestock \nProductivity \n(t/TLU)",
              density_change = "Ruminant \nDensity \n(TLU/ha)",
              Export_quantity_change = "Exports \n(kcal)",
              Import_quantity_change = "Imports \n(kcal)",
              Foodwaste_change = "Share of \n Food Waste \n(kcal)",
              Affor = "Afforestation \n(Mha)"
)

complete_data$Pathway <- factor(as.character(complete_data$Pathway), levels = c("GlobalSustainability", "NationalCommitments", "CurrentTrend"))

complete_data <- complete_data %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "GBR",
                         "UK",
                         ALPHA3))
complete_data$ALPHA3 <- factor(as.character(complete_data$ALPHA3), levels = c("ARG", "AUS", "BRA", "CAN",
                                                                              "CHN", "COL", "DEU", "DNK","ETH",
                                                                              "FIN", "GRC","IDN", 'IND',
                                                                              "MEX", "NPL", "NOR", "RUS",
                                                                              "RWA", "SWE", "TUR", "UK", "USA", 
                                                                              "ASP", "CSA", "NEU", "NMC",
                                                                              "OEU", "SSA"))


data_SOFA <- complete_data %>% 
  dplyr::filter(ALPHA3 %in% c("AUS", "BRA", "COL", "ETH", "IND", 
                              "UK"))

p_final_SOFA <-  ggplot() + 
  geom_col(aes(y=value,
               x = Pathway,
               group = ALPHA3,
               fill = sign),
           data = data_SOFA,
           position = "dodge",
           show.legend = F)+
  ylab("Relative change between 2020 and 2050 (2020=0)")+
  coord_flip()+
  scale_x_discrete(labels = c(GlobalSustainability = "GS",
                              NationalCommitments = "NC",
                              CurrentTrend = "CT"))+
  scale_y_continuous(n.breaks = 3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_grid(ALPHA3~variable,
             switch = "y",
             labeller = labeller(variable = var.labs),
             drop = T,
             space = "fixed",
             scale = "free")+
  #theme_light()+
  theme(
    panel.background = element_rect(fill = '#F2F2F2'),
    panel.grid  = element_blank(),
    strip.placement = "outside",
    #axis.title.x = element_text("2050 compared to 2015 (2015=1)"),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background=element_blank(),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing.x = unit(0.75, "lines"),
    strip.text.y.left = element_text(angle = 0),
    axis.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),
    axis.line.x = element_line()) +
  labs(caption = " \nFor Ethiopia, Imports (Kcal) values are in fact equal to 64.14 for the Current Trend pathway, and 54.42 for National Commitment and Global Sustainability Pathways. Results have been manually set to 3 to better visualization., 
       \n About Afforestation (Mha), results are expressed in net increase rather than relative change"
  )

width = 14
height = 14
print(p_final_SOFA)


# tiff(here("output", "figures", "scenario-assumptions", paste0(gsub("-", "",Sys.Date()), "_", "ScenarioAssumptionSOFACalculators.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_final_SOFA)
# dev.off()
# 
# png(here("output", "figures", "scenario-assumptions", paste0(gsub("-", "",Sys.Date()), "_", "ScenarioAssumptionSOFACalculators.png")),
#     units = "in", height = 5, width = 14, res = 300)
# plot(p_final_SOFA)
# dev.off()
# 

# 
# tiff(here("output", "figures", "scenario-assumptions", paste0(gsub("-", "",Sys.Date()), "_", "ScenarioAssumptionSOFACalculators_eth.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_final_SOFA)
# dev.off()
# 
# png(here("output", "figures", "scenario-assumptions", paste0(gsub("-", "",Sys.Date()), "_", "ScenarioAssumptionSOFACalculators_eth.png")),
#     units = "in", height = 5, width = 14, res = 300)
# plot(p_final_SOFA)
# dev.off()

