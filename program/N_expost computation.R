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
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_affor", "GS_live_rumdensity"))

bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_agrexp", "GS_diet", "GS_crop"))

col_data <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Indicators") %>% 
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
  bind_rows(gbr_data) %>% 
  rename(country = Location, year = Year, pathway = Pathway, calcn_org = CalcN_org, calcn_synth = CalcN_synth) %>% 
  mutate(pathway = if_else(pathway == "Current Trend_Yes", "CurrentTrend", pathway)) %>% 
  mutate(country = ifelse(country == "Australia", "AUS",
                          ifelse(country == "Brazil", "BRA",
                                 ifelse(country == "Ethiopia", "ETH",
                                        ifelse(country == "UK", "GBR",
                                               ifelse(country == "Colombia", "COL", country))))))








# file --------------------------------------------------------------------

file <- list.files(path = here("data", "SOFA extraction"))


# Extracting data from the Calculators - only run when needed
# <<<<<<< HEAD
# 
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



db_herd <- read_excel(here("data",  "Manure", "240516_Extracted_herdsize.xlsx")) %>% 
  # mutate(ALPHA3 = ifelse(ALPHA3 == "RME", "NMC",
  #                        ifelse(nchar(ALPHA3)>3, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))) %>% 
  select(-ALPHA3) %>% 
  rename(ALPHA3 = country) %>% 
  mutate(TLU = as.numeric(FinFeasHerd)) %>% 
  select(-FinFeasHerd)



# merge with GAMS manure data ----------------------------------------------

db_manure <- read_excel(here("data", "Manure", "Nmanure_GAMS.xlsx"),
                        sheet = "All_Nmanure_TLU") %>% 
  filter(!(is.na(ALPHA3))) %>% 
  pivot_longer(!ALPHA3, values_to = "Nmanure_TLU", names_to = "ANIMAL_GLOBIOM")

#Total N inputs from manure in 1000 t N for each year
db_manure_herd <- db_herd %>% 
  mutate(ANIMAL_GLOBIOM = ifelse(ANIMAL_GLOBIOM == "SGTD", "SGTO", ANIMAL_GLOBIOM)) %>% 
  left_join(db_manure) %>% 
  mutate(Nmanure_Anim = Nmanure_TLU*TLU/1000) %>%
  group_by(Pathway, ALPHA3, YEAR) %>% 
  reframe(NManure = sum(Nmanure_Anim, na.rm = T)) %>% 
  rename_all(tolower) %>% 
  rename(country = alpha3) %>% 
  filter(!is.na(year))

# ggplot(db_manure_herd) +
#   geom_line(aes(x = as.numeric(year), y = nmanure, linetype = pathway, colour = pathway),
#             linewidth = 1)+
#   facet_wrap(~country)


# CalcNLeftpasture in 1000 t N
db_N <- all_data %>% 
  select(pathway, country, year, calcn_org, calcn_synth) %>% 
  ######################### CAREFUL ONLY HAVE EXTRACTED WITH TRADE ADJUSTMENT ###############################
#filter(tradeadjustment == "Yes") %>%
################################################################################
mutate(year = as.character(year)) %>% 
  mutate(pathway = if_else(pathway == "GS_live_rumdensity", "GS_live_rum", pathway))

  
  # mutate(country = gsub("_", "", country)) %>% 
  # mutate(country = ifelse(country == "RMECAS", "NMC", 
  #                         ifelse(nchar(country)>3, stringr::str_sub(country, 2, 4), country)))

db_NleftPasture <- db_manure_herd %>%
  left_join(db_N) %>% 
  mutate(NPasture_beforeadj = nmanure - calcn_org,
         CalcNLeftPasture = pmax(0, nmanure - calcn_org))

# ggplot(db_NleftPasture) +
#   geom_line(aes(x = as.numeric(year), y = CalcNLeftPasture, linetype = pathway, colour = pathway),
#             linewidth = 1)+
#   facet_wrap(~country)


write.xlsx(db_NleftPasture, file = here("output", "N", paste0(format(Sys.Date(), format = "%y%m%d"), "_ExpostNComputations_SOFA.xlsx")))




