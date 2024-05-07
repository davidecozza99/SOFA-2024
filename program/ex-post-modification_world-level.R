#Creating database at the world level
#this needs to be run through the setup

# libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(readxl)
library(xlsx)
library(conflicted)
library(readxl)
library(tidyr)
conflict_prefer("filter", "dplyr")

# DB used -----------------------------------------------------------------

#To run easily with setup R.file
update_DB_date <- "240424"
date_DB = ifelse(exists("date_DB"), date_DB, update_DB_date)

update_PoU_date <- "240424"
date_PoU = ifelse(exists("date_PoU"), date_PoU, update_PoU_date)

# data --------------------------------------------------------------------

db_PoU <- read_excel(here("data", "scenathon_2023", paste0(date_PoU, "_ExPost_PoU_Computations.xlsx"))) %>% 
  rename_all(tolower)

db_full <- read_csv(here("data", "scenathon_2023", paste0(date_DB, "_indicators.csv")))  %>%
  mutate(country = ifelse(country == "RMECAS", "R_NMC", country)) %>% 
  unique() %>% 
  #LNPPmatureotherland fix for Australia
  mutate(lnppmatureotherland = ifelse((country == "AUS" & between(year, 2000, 2020)), calcotherland, lnppmatureotherland)) %>% 
  mutate(year = as.factor(year)) %>% 
  left_join(db_PoU %>% mutate(year = as.factor(year)) %>% rename(country = iso3),
            relationship = "many-to-many")%>% 
  rename(calcpeatco2 = calcsequestco3) %>% 
  mutate(calcpeatco2 = as.numeric(calcpeatco2)) 


db_fullProduct <- read.csv(here("data", "scenathon_2023", paste0("240430", "_commodities.csv"))) %>% 
  mutate(country = ifelse(country == "RMECAS", "R_NMC", country)) %>% 
  unique() %>% 
  mutate(totalnsynth = as.numeric(totalnsynth)) %>% 
  mutate(year = as.factor(year))

db_herd <- read_excel(here("data", "scenathon_2023", "manure", "240501_Extracted_herdsize.xlsx")) %>% 
  # mutate(ALPHA3 = ifelse(ALPHA3 == "RME", "NMC",
  #                        ifelse(nchar(ALPHA3)>3, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))) %>% 
  select(-ALPHA3) %>% 
  rename(ALPHA3 = country) %>% 
  mutate(TLU = as.numeric(FinFeasHerd)) %>% 
  select(-FinFeasHerd)

db_manure <- read_excel(here("data", "scenathon_2023", "manure", "Nmanure_GAMS.xlsx"),
                        sheet = "All_Nmanure_TLU") %>% 
  filter(!(is.na(ALPHA3))) %>% 
  pivot_longer(!ALPHA3, values_to = "Nmanure_TLU", names_to = "ANIMAL_GLOBIOM")

db_water <- read_excel(here("output", "240429_ExpostWaterAverageReq.xlsx"))

LNPPshare_FAO <- read_excel(here("data", "scenathon_2023", "240429_LNPPshare.xlsx"),
                            sheet = "All_LNPPshare_FAO") %>% 
  mutate(country = countrycode::countrycode(CountryName, origin = "country.name", destination = "iso3c")) %>% 
  mutate(country = ifelse(CountryName %in% c("R_ASP", "R_CSA", "R_NEU", "R_NMC", "R_OEU", "R_REF", "R_SSA"), 
                          CountryName,
                          country))

LNPPshare_ESA <- read_excel(here("data", "scenathon_2023", "240429_LNPPshare.xlsx"),
                            sheet = "All_LNPPshare_ESA") %>% 
  mutate(country = countrycode::countrycode(CountryName, origin = "country.name", destination = "iso3c")) %>% 
  mutate(country = ifelse(CountryName %in% c("R_ASP", "R_CSA", "R_NEU", "R_NMC", "R_OEU", "R_REF", "R_SSA"), 
                          CountryName,
                          country))

# expost manure computations ----------------------------------------------

#Total N inputs from manure in 1000 t N for each year
db_manure_herd <- db_herd %>% 
  mutate(ANIMAL_GLOBIOM = ifelse(ANIMAL_GLOBIOM == "SGTD", "SGTO", ANIMAL_GLOBIOM)) %>% 
  left_join(db_manure) %>% 
  mutate(Nmanure_Anim = Nmanure_TLU*TLU/1000) %>%
  group_by(Pathway, TradeAdjustment, ALPHA3, YEAR) %>% 
  reframe(NManure = sum(Nmanure_Anim, na.rm = T)) %>% 
  rename_all(tolower) %>% 
  rename(country = alpha3) %>% 
  filter(!is.na(year))


# CalcNLeftpasture in 1000 t N
db_NleftPasture <- db_manure_herd %>% 
  left_join(
    db_full %>% 
      select(tradeadjustment, pathway, country, year, calcn_org) %>% 
      mutate(year = as.character(year)) %>% 
      mutate(country = gsub("_", "", country)) %>% 
      mutate(country = ifelse(country == "RMECAS", "NMC", 
                              ifelse(nchar(country)>3, stringr::str_sub(country, 2, 4), country)))
  ) %>% 
  mutate(#NPasture_beforeadj = nmanure - calcn_org,
         calcn_agsoils = calcn_org,
         calcn_leftpasture = pmax(0, nmanure - calcn_org)) %>% 
  select(pathway, tradeadjustment, country, year, calcn_agsoils, calcn_leftpasture)

db_full <- db_full %>% 
  select(-c(calcn_org)) %>% 
  left_join(db_NleftPasture)

# expost water ------------------------------------------------------------

df_water <- db_fullProduct %>% 
  select(pathway, country, product, tradeadjustment, year, feasharvarea, irrharvarea, rfharvarea, feasplantarea) %>% 
  # rename(real_irrharvarea = rfharvarea,
  #        real_feasplantarea = irrharvarea,
  #        real_rfharvarea = feasplantarea) %>% 
  # rename(irrharvarea = real_irrharvarea,
  #        feasplantarea = real_feasplantarea,
  #        rfharvarea = real_rfharvarea) %>% 
  left_join(db_water, by = c(country = "ALPHA3")) %>% 
  mutate(requirement = irrharvarea * Requirement_ha/1000) %>% 
  group_by(pathway, country, tradeadjustment, year,) %>% 
  reframe(
    water_requirement = sum(requirement, na.rm = T)
  )

db_full <- db_full %>% 
  left_join(df_water)


# modification LNPP ----------------------------------------------------------

df_LNPP <- db_full %>% 
  select(pathway, country, tradeadjustment, year, 
         calcforest, calcotherland, protectedareasforest, protectedareasothernat, lnppmatureforest, lnppmatureotherland, lnppnewforest, lnppnewotherland) %>% 
  left_join(LNPPshare_FAO %>% 
              rename(year = Year) %>% 
              filter(LandCover %in% c("Forest", "Otherland")) %>% 
              select(-CountryName) %>% 
              pivot_wider(names_from = LandCover,
                          values_from = c("ShLNPPinPA", "ShLNPPoutsidePA", "AreaPA", "AreanotPA")), 
            relationship = "many-to-many")

df_LNPP_fill <- df_LNPP %>% 
  filter(year < 2015) %>% 
  arrange(pathway, tradeadjustment, country, year) %>% 
  fill(ShLNPPinPA_Forest, .direction = "up") %>% 
  fill(ShLNPPinPA_Otherland, .direction = "up") %>% 
  fill(ShLNPPoutsidePA_Forest, .direction = "up") %>% 
  fill(ShLNPPoutsidePA_Otherland, .direction = "up") %>% 
  rbind(
    df_LNPP %>% 
      filter(year > 2010) %>% 
      arrange(pathway, tradeadjustment, country, year) %>% 
      fill(ShLNPPinPA_Forest) %>% 
      fill(ShLNPPinPA_Otherland) %>% 
      fill(ShLNPPoutsidePA_Forest) %>% 
      fill(ShLNPPoutsidePA_Otherland)
  ) %>% 
  mutate(lnppmatureforestPA = ifelse(year < 2025,
                                     ShLNPPinPA_Forest * protectedareasforest,
                                     NA),
         lnppmatureotherlandPA = ifelse(year < 2025,
                                     ShLNPPinPA_Otherland * protectedareasothernat,
                                     NA)) %>% 
  mutate(protectedareasforest = ifelse(year < 2025,
                                       protectedareasforest,
                                       NA),
         protectedareasothernat = ifelse(year < 2025,
                                        protectedareasothernat,
                                        NA)) %>% 
  arrange(pathway, tradeadjustment, country, year) %>% 
  fill(lnppmatureforestPA) %>% 
  fill(lnppmatureotherlandPA) %>% 
  fill(protectedareasforest) %>% 
  fill(protectedareasothernat) %>%   
  mutate(lnppmatureforest_expost =  ifelse(year <2025,
                                           (ShLNPPinPA_Forest * protectedareasforest) + ShLNPPoutsidePA_Forest *(calcforest - protectedareasforest),
                                           lnppmatureforestPA + ShLNPPoutsidePA_Forest *(calcforest - protectedareasforest)),
         lnppmatureotherland_expost =  ifelse(year < 2025,
                                              (ShLNPPinPA_Otherland * protectedareasothernat) + ShLNPPoutsidePA_Otherland *(calcotherland - protectedareasothernat),
                                              lnppmatureotherlandPA +ShLNPPoutsidePA_Otherland *(calcotherland - protectedareasothernat)))
  
# xlsx::write.xlsx(df_LNPP_fill %>% data.frame(), 
#                  file = here("output", "figures", paste0(format(Sys.Date(),format = "%y%m%d"), "_lnppexpostdetails.xlsx")), row.names = F)
# 
# 
# 
# ggplot2::ggplot(df_LNPP_fill %>% 
#          filter(tradeadjustment == "Yes") %>% 
#          filter(pathway == "GlobalSustainability"))+
#   geom_line(aes(x = as.numeric(year), y = lnppmatureotherland_expost))+
#   facet_wrap(~country, scales = "free")+
#   scale_y_continuous(limits = c(0, NA))+
#   ggtitle("GS")
# 
# ggplot2::ggplot(df_LNPP_fill %>% 
#                   filter(tradeadjustment == "Yes") %>% 
#                   filter(pathway == "NationalCommitments") %>% 
#                   arrange(tradeadjustment, pathway, country, year) %>% 
#                   mutate(calcotherland_change = calcotherland - dplyr::lag(calcotherland)) %>% 
#                   filter(year > 2010))+
#   geom_line(aes(x = as.numeric(year), y = calcotherland_change))+
#   facet_wrap(~country, scales = "free")+
#   scale_y_continuous(limits = c(0, NA))+
#   ggtitle("NC")

db_full <- db_full %>% 
  left_join(df_LNPP_fill %>% 
              select(pathway, country, tradeadjustment, year, lnppmatureforest_expost, lnppmatureotherland_expost))


# Global World level database -----------------------------------

#These variables have to been scale at per capita level
scale_pop <- function(x) (x*db_full$population)
scale_land <- function(x) (x*db_full$totalland)
scale_agro <- function(x) (x*db_full$calccropland)


#Scale the per capita and land variables
data_scale <- db_full %>%
  mutate_at(vars("kcal_hist", "kcal_targ", "kcal_feas",
                 "fat_feas", "prot_feas", "kcal_mder", 
                 "kcal_pou", "pou_calc", "pou_computed", "mder_pou"),
            scale_pop) %>%
  mutate_at(vars("calcbioscore"),
            scale_land) %>%
  mutate_at(vars("agroecosh"),
            scale_agro) %>%
  data.frame()

data_world <- data_scale %>% select(-c(scenathon_id, country_id, submission_id, pathway_id, iteration, country))
data_world <- aggregate(x = data.frame(data_world[,colnames(select_if(data_world, is.numeric))]),
                        by = list(year = data_world$year, 
                                  pathway =  data_world$pathway, 
                                  tradeadjustment = data_world$tradeadjustment),
                        FUN = sum,
                        na.rm = T,
                        na.action = NULL)


scale_pop3 <- function(x) (x/data_world$population)
scale_land3 <- function(x) (x/data_world$totalland)
scale_agro3 <- function(x) (x/data_world$calccropland)
data_world <- data_world %>% 
  mutate_at(vars("kcal_hist", "kcal_targ", "kcal_feas", 
                 "fat_feas", "prot_feas", "kcal_mder",
                 "kcal_pou", "pou_calc", "pou_computed", "mder_pou"),
            scale_pop3) %>% 
  mutate_at(vars("calcbioscore"),
            scale_land3) %>% 
  mutate_at(vars("agroecosh"),
            scale_agro3) %>% 
    #select(-submission_id, -country_id, -iteration, -it, -scenathon, -scenathon_id) %>% 
  data.frame()

trade_world <- aggregate(x = data.frame(db_fullProduct[,colnames(select_if(db_fullProduct, is.numeric))]),
                        by = list(year = db_fullProduct$year,
                                  pathway =  db_fullProduct$pathway,
                                  tradeadjustment = db_fullProduct$tradeadjustment,
                                  product = db_fullProduct$product),
                        FUN = sum,
                        na.rm = T,
                        na.action = NULL)



# add it to fulldb --------------------------------------------------------

db_countrylevel <- db_full %>% 
  select(-c(scenathon_id, country_id, submission_id, pathway_id, iteration))

db_full_w_world <- db_countrylevel %>% 
  rbind(data_world %>% 
          mutate(country = "WORLD") %>% 
          select(colnames(db_countrylevel))) %>% 
  mutate(pathway_id = ifelse(pathway == "CurrentTrends", "CT",
                             ifelse(pathway == "NationalCommitments", "NC",
                                    "GS")))
# 
db_fullProduct_w_world <- db_fullProduct %>%
  rbind(trade_world %>%
          mutate(country = "WORLD") %>%
          select(colnames(db_fullProduct))) %>% 
  select(-c(scenathon_id, country_id, submission_id, pathway_id, iteration)) %>% 
  mutate(pathway_id = ifelse(pathway == "CurrentTrends", "CT",
                             ifelse(pathway == "NationalCommitments", "NC",
                                    "GS")))


# save global data --------------------------------------------------------


write.table(db_full_w_world, file = here("data", "scenathon_2023", paste0(format(Sys.Date(),format = "%y%m%d"), "_FullDataBase.csv")), row.names = F)
# write.xlsx(db_full_w_world, file = here("data", "scenathon_2023", paste0(gsub("-", "",Sys.Date()), "_FullDataBase.xlsx")), row.names = F)


write.table(db_fullProduct_w_world %>% data.frame(), file = here("data", "scenathon_2023", paste0(format(Sys.Date(),format = "%y%m%d"), "_FullProductDataBase.csv")), row.names = F)
# write.xlsx(db_fullProduct_w_world %>% data.frame(), file = here("data", "scenathon_2023", paste0(gsub("-", "",Sys.Date()), "_FullProductDataBase.xlsx")), row.names = F)

rm(list=setdiff(ls(), c("date_DB", "date_PoU")))
