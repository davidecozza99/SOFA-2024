# N

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

here()

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflicts_prefer(dplyr::filter)




aus_data <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) 

bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) 

col_data <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) 

eth_data <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) 

gbr_data <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Indicators")  %>% 
  rename(Pathway = `Current Trend`)


all_data <- aus_data %>%
  bind_rows(bra_data) %>%
  bind_rows(col_data) %>%
  bind_rows(eth_data) %>%
  bind_rows(gbr_data) %>% 
  rename(country = Location, year = Year, pathway = Pathway, calcn_org = CalcN_org, calcn_synth = CalcN_synth) %>% 
  mutate(pathway = if_else(pathway == "Current Trend_Yes", "CurrentTrend", pathway)) %>% 
  mutate(country = ifelse(country == "Australia", "AUS",
                          ifelse(country == "Brazil", "BRA",
                                 ifelse(country == "Ethiopia", "ETH",
                                        ifelse(country == "UK", "GBR",
                                               ifelse(country == "Colombia", "COL", country))))))


### Compute N per TLU by Livestock type (N cattle is ....): this coefficient should be constant more or less across pathways


aus_comm <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) 

bra_comm <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) 

col_comm <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  mutate(Anim_feas = ifelse(Product == "pigs", Anim_feas/13, 
                            ifelse(Product == "sheep_goats", Anim_feas/26,
                                   ifelse(Product == "chicken", Anim_feas/39,
                                          ifelse(Product == "cattle", Anim_feas/26,
                                                 Anim_feas)))))

eth_comm <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) 

gbr_comm <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Commodities")  %>% 
  rename(Pathway = `Current Trend`)

all_comm <- aus_comm %>%
  bind_rows(bra_comm) %>%
  bind_rows(col_comm) %>%
  bind_rows(eth_comm) %>%
  bind_rows(gbr_comm) %>% 
  rename(ALPHA3 = Location, YEAR = Year, ANIMAL= Product) %>% 
  mutate(Pathway = if_else(Pathway == "Current Trend_Yes", "CurrentTrend", Pathway)) %>% 
  mutate(ALPHA3 = ifelse(ALPHA3 == "Australia", "AUS",
                         ifelse(ALPHA3 == "Brazil", "BRA",
                                ifelse(ALPHA3 == "Ethiopia", "ETH",
                                       ifelse(ALPHA3 == "UK", "GBR",
                                              ifelse(ALPHA3 == "Colombia", "COL", ALPHA3)))))) %>% 
  select(Pathway, ALPHA3, YEAR, ANIMAL, Anim_feas) %>% 
  group_by(Pathway, ALPHA3, YEAR, ANIMAL) %>% 
  mutate(TLU = sum(Anim_feas)) 

# merge with GAMS manure data ----------------------------------------------

db_manure <- read_excel(here("data", "Manure", "Nmanure_GAMS.xlsx"),
                        sheet = "All_Nmanure_TLU") %>% 
  filter(!(is.na(ALPHA3))) %>% 
  pivot_longer(!ALPHA3, values_to = "Nmanure_TLU", names_to = "ANIMAL_GLOBIOM")


#### Mapping Globiom (BOVO, BOVD) with ANIMAL (cattle)
Mapping_Globiom_Animal <- read_excel(here("data",  "Mapping_Globiom_Animal.xlsx"))


db_manure_animal <- db_manure %>% 
  left_join(Mapping_Globiom_Animal) %>% 
  unique() 


db_manure_animal2 <- db_manure_animal %>% 
  left_join(all_comm, relationship = "many-to-many") %>% 
  filter(ALPHA3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  filter(Pathway != "Current Trend") %>% 
  select(-Anim_feas)
#Filtering out Current Trend with no tradeadj pathway



  db_NperTLU_live <- db_manure_animal2 %>% 
  group_by(ALPHA3, YEAR, Pathway, ANIMAL) %>% 
  mutate(Nmanure_Anim = Nmanure_TLU*TLU/1000) %>% 
  mutate(NManure = sum(Nmanure_Anim, na.rm = T)) %>% 
  mutate(Nmanure_Anim_final = (Nmanure_TLU*Nmanure_Anim/NManure)) %>% 
  mutate(NperTLU_live = sum(Nmanure_Anim_final)) %>% 
  select(ALPHA3, YEAR, Pathway, ANIMAL,TLU, NperTLU_live) %>% 
  unique() %>% 
  mutate(Nmanure = (TLU * NperTLU_live)/1000) %>% 
  group_by(ALPHA3, YEAR, Pathway) %>% 
    mutate(Nmanure=sum(Nmanure)) %>% 
    select(-ANIMAL, -TLU, -NperTLU_live) %>% 
    unique()
    

write.xlsx(db_NperTLU_live, file = here("data",  "Manure", paste0(format(Sys.Date(),format = "%y%m%d"), "_db_Nmanure_live.xlsx")), row.names = F)
  


# file --------------------------------------------------------------------

# file <- list.files(path = here("data", "SOFA extraction"))


# Extracting data from the Calculators - only run when needed
# <<<<<<< HEAD

# db_full <- data.frame()
# 
# for (cur_file in file){
#   print(cur_file)
#   #???Extract the right sheet from calculators
#    data <- read_excel(here("data", "SOFA extraction", cur_file),
#                      sheet = "5_feas_livestock",
#                      range = "AA20:BZ175")
#   index <- which(data == "TABLE: Calc_FeasProdLivestock" | data == "TABLE: Calc_FeasProdLivestok", arr.ind = T)
# 
#   if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
#     #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
#     data <- data[c((index[1,1]+7):nrow(data)), c(index[1,2]:(index[1,2]+30))]
#     colnames(data) <- data[1,]
#     data <- data[-1,]
#     data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
# 
# 
#     data <- data %>%
#       #slice(which(Year %in% c(2015, 2050))) %>%
#       select(ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
#       mutate(Pathway = ifelse(grepl("affor", cur_file),
#                               "GS_affor",
#                               ifelse(grepl("diet", cur_file),
#                                      "GS_diet",
#                                      ifelse(grepl("liverum", cur_file),
#                                             "GS_live_rum",
#                                             ifelse(grepl("agrexp", cur_file),
#                                                    "GS_agrexp",
#                                                    ifelse(grepl("crop", cur_file),
#                                                           "GS_crop",
#                                                           ifelse(grepl("popurban", cur_file),
#                                                                  "GS_pop_urban",
#                                                                  ifelse(grepl("pop", cur_file),
#                                                                         "GS_pop",
#                                                                         ifelse(grepl("foodwaste", cur_file),
#                                                                                "GS_foodwaste",
#                                                                                ifelse(grepl("Current", cur_file),
#                                                                                       "CurrentTrend",
#                                                                                       ifelse(grepl("National", cur_file),
#                                                                                              "NationalCommitments",
#                                                                                              "GlobalSustainability"))))))))))) %>%
#       mutate(ALPHA3 = ifelse(grepl("AUS", cur_file), "AUS",
#                              ifelse(grepl("BRA", cur_file), "BRA",
#                                     ifelse(grepl("COL", cur_file), "COL",
#                                            ifelse(grepl("ETH", cur_file), "ETH",
#                                                   ifelse(grepl("GBR", cur_file), "GBR",
#                                                          ifelse(grepl("Current", cur_file),
#                                                                 str_sub(cur_file, 40, 42),
#                                                                 ifelse(grepl("National", cur_file),
#                                                                        str_sub(cur_file, 46, 48),
#                                                                        str_sub(cur_file, 47, 49))))))))) %>%
#       unique()
#   }
# 
# 
#   db_full <- db_full %>%
#     rbind.data.frame(data) %>%
#     dplyr::mutate(ALPHA3 = gsub("_", "", ALPHA3))
# }
# 
# db_herd <- db_full %>%
#   select(Pathway, ALPHA3, ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
#   mutate(country = ifelse(nchar(ALPHA3) == 4 , substr(ALPHA3, 2, 4),
#                           ifelse(ALPHA3 == "RME", "NMC", ALPHA3))) %>%
#   data.frame()
# 
# write.xlsx(db_herd, file = here("data",  "Manure", paste0(format(Sys.Date(),format = "%y%m%d"), "_Extracted_herdsize.xlsx")), row.names = F)
# 
# 
# 
# db_herd <- read_excel(here("data",  "Manure", "240516_Extracted_herdsize.xlsx")) %>% 
#   # mutate(ALPHA3 = ifelse(ALPHA3 == "RME", "NMC",
#   #                        ifelse(nchar(ALPHA3)>3, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))) %>% 
#   select(-ALPHA3) %>% 
#   rename(ALPHA3 = country) %>% 
#   mutate(TLU = as.numeric(FinFeasHerd)) %>% 
#   select(-FinFeasHerd) %>% 
#   filter(Pathway != "GS_live_rum") %>% 
#   filter(Pathway != "GS_pop_urban")





### Multiply the N per TLU by Livestock type by the number of animal in TLU (to be find in the country decomposition reports)

db_NperTLU_live <- db_manure_herd %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  left_join(all_comm, by = join_by(Pathway, YEAR, ALPHA3, ANIMAL)) %>% 
  mutate(NperTLU_live = Nmanure_Anim/Anim_feas) %>% 
  select(-TLU, -Nmanure_TLU, -ANIMAL_GLOBIOM) %>%
  unique() %>% 
  filter(ANIMAL != "other") %>% 
  mutate(CalcNleftManure = NperTLU_live*Anim_feas/1000) %>% 
  group_by(ALPHA3, YEAR, Pathway, ANIMAL) %>% 
  mutate(avg_CalcNleftManure = sum(CalcNleftManure, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Pathway = if_else(Pathway == "CurrentTrend", "Current Trend_Yes", Pathway)) %>% 
  rename(Location = ALPHA3) %>% 
  mutate(Location = ifelse(Location == "AUS", "Australia",
                           ifelse(Location == "BRA", "Brazil",
                                  ifelse(Location == "ETH", "Ethiopia",
                                         ifelse(Location == "GBR", "UK",
                                                ifelse(Location == "COL", "Colombia", Location)))))) 

  





