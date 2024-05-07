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



# Extracting Anim_feas from decompsoition reports


aus_data <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_affor", "GS_live_rumdensity"))

bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_agrexp", "GS_diet", "GS_crop"))

col_data <- read_xlsx(here("data", "report_COL_20240325_15H07.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_crop", "GS_pop_urban"))

eth_data <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_pop", "GS_agrexp", "GS_crop"))

gbr_data <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Commodities")  %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway %in% c("Current Trend_Yes", "NationalCommitments", "GlobalSustainability", "GS_diet", "GS_foodwaste", "GS_crop"))

Map_animals <- read_xlsx(here("data", "Manure", "Map_animals.xlsx"))

all_data <- aus_data %>%
  bind_rows(bra_data) %>%
  bind_rows(col_data) %>%
  bind_rows(eth_data) %>%
  bind_rows(gbr_data)



db_herd <- all_data %>% 
  select(Pathway, Location, Product, Year, Anim_feas) %>% 
  rename(ANIMAL = Product) %>% 
  left_join(Map_animals, relationship = "many-to-many") %>% 
  group_by(Pathway, Location, Year) %>% 
  rename(TLU = Anim_feas, ALPHA3 = Location) %>% 
  ungroup() %>% 
  filter(!is.na(ANIMAL_GLOBIOM)) %>%
  mutate(ALPHA3 = ifelse(ALPHA3 == "Australia", "AUS",
                         ifelse(ALPHA3 == "Brazil", "BRA",
                                ifelse(ALPHA3 == "Colombia", "COL",
                                       ifelse(ALPHA3 == "Ethiopia", "ETH",
                                              ifelse(ALPHA3 == "UK", "GBR", ALPHA3))))))




# db_herd <- read_excel(here("data", "scenathon_2023", "manure", "240501_Extracted_herdsize.xlsx")) %>% 
#   # mutate(ALPHA3 = ifelse(ALPHA3 == "RME", "NMC",
#   #                        ifelse(nchar(ALPHA3)>3, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))) %>% 
#   select(-ALPHA3) %>% 
#   rename(ALPHA3 = country) %>% 
#   mutate(TLU = as.numeric(FinFeasHerd)) %>% 
#   select(-FinFeasHerd)

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
  group_by(Pathway, ALPHA3, Year) %>% 
  reframe(NManure = sum(Nmanure_Anim, na.rm = T)) %>% 
  rename_all(tolower) %>% 
  rename(country = alpha3) %>% 
  filter(!is.na(year)) %>% 
  rename(ISO3 = country, Year=year, Pathway=pathway) %>% 
  mutate(Pathway = ifelse(Pathway == "Current Trend_Yes", "Current Trend", Pathway))


# ggplot(db_manure_herd) +
#   geom_line(aes(x = as.numeric(year), y = nmanure, linetype = pathway, colour = pathway),
#             linewidth = 1)+
#   facet_wrap(~country)


# CalcNLeftpasture in 1000 t N
dec_data <- read_xlsx(here("data", "Decomposition", "database_decomposition_complete_20240311_DC_SL_DC.xlsx"), 
                      sheet = "Decomposition Analysis_aggregg") %>%  
            select(Pathway, ISO3, Year, CalcN_org, CalcN_synth) %>% 
mutate(Year = as.character(Year)) 


db_NleftPasture <- db_manure_herd %>% 
  mutate(Year = as.character(Year)) %>%
  left_join(dec_data) %>% 
  mutate(CalcNLeftPasture = pmax(0, nmanure - CalcN_org))


write.xlsx(db_NleftPasture, file = here("output", "N", paste0(format(Sys.Date(), format = "%y%m%d"), "_ExpostNComputations_SOFA.xlsx")))

