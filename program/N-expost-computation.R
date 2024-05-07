# N

################# DB to use #########
date_DB = "240424"


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
here()

conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflicts_prefer(dplyr::filter)

# file --------------------------------------------------------------------

#<<<<<< HEAD
file <- list.files(path = here("data", "scenathon_2023", "Calculators Manure"))
# =======
# file <- list.files(path = here("data", "scenathon_2023", "Calculators BT"))
# >>>>>>> edc16e7e537126dc8656a34888ba11ed7865bae0

#  Extraction -------------------------------------------------------------


#Extracting data from the Calculators - only run when needed 
#<<<<<<< HEAD

db_full <- data.frame()

for (cur_file in file){
  print(cur_file)
  #???Extract the right sheet from calculators
  data <- read_excel(here("data", "scenathon_2023", "Calculators Manure", cur_file),
                     sheet = "5_feas_livestock",
                     range = "AA20:BZ175")
  index <- which(data == "TABLE: Calc_FeasProdLivestock" | data == "TABLE: Calc_FeasProdLivestok", arr.ind = T)

    if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
      #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
      data <- data[c((index[1,1]+7):nrow(data)), c(index[1,2]:(index[1,2]+30))]
      colnames(data) <- data[1,]
      data <- data[-1,]
      data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])


      data <- data %>%
        #slice(which(Year %in% c(2015, 2050))) %>%
        select(ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
        mutate(ALPHA3 = ifelse(grepl("_R_", cur_file),
                               str_sub(cur_file, 26, 30),
                               str_sub(cur_file, 26, 28))) %>%
        mutate(TradeAdjustment = ifelse(grepl("_4", cur_file),
                                        "No",
                                        "Yes")) %>% 
        mutate(Pathway = ifelse(grepl("scenathon-16", cur_file),
                                "CurrentTrends",
                                ifelse(grepl("scenathon-17", cur_file),
                                       "NationalCommitments",
                                       "GlobalSustainability"))) %>%
        unique()
      #slice(which(Year ==2050))
    }


    db_full <- db_full %>%
      rbind.data.frame(data) %>%
      dplyr::mutate(ALPHA3 = gsub("_", "", ALPHA3))
}

db_herd <- db_full %>%
  select(Pathway, TradeAdjustment, ALPHA3, ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
  mutate(country = ifelse(nchar(ALPHA3) == 4 , substr(ALPHA3, 2, 4),
                          ifelse(ALPHA3 == "RME", "NMC", ALPHA3))) %>%
  data.frame()

xlsx::write.xlsx(db_herd, file = here("data", "scenathon_2023", "manure", "240501_Extracted_herdsize.xlsx"),
                 row.names = F)
#=======
# 
# db_full <- data.frame()
# 
# for (cur_file in file){
#   print(cur_file)
#   #???Extract the right sheet from calculators
#   data <- read_excel(here("data", "scenathon_2023", "Calculators BT", cur_file),
#                      sheet = "5_feas_livestock",
#                      range = "AA20:BZ175")
#   index <- which(data == "TABLE: Calc_FeasProdLivestock" | data == "TABLE: Calc_FeasProdLivestok", arr.ind = T)
# 
#     if(!plyr::empty(index)){#Don't know if the table is in the calculator; we check before digging in
#       #if it is in the calc than we only want a certain amount of columns after "Biofuel_scen" cell
#       data <- data[c((index[1,1]+7):nrow(data)), c(index[1,2]:(index[1,2]+30))]
#       colnames(data) <- data[1,]
#       data <- data[-1,]
#       data <- data.frame(data[rowSums(is.na(data)) != ncol(data), ])
# 
# 
#       data <- data %>%
#         #slice(which(Year %in% c(2015, 2050))) %>%
#         select(ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
#         mutate(ALPHA3 = ifelse(grepl("_R_", cur_file),
#                                str_sub(cur_file, 26, 30),
#                                str_sub(cur_file, 26, 28))) %>%
#         mutate(Pathway = ifelse(grepl("scenathon-16", cur_file),
#                                 "CurrentTrends",
#                                 ifelse(grepl("scenathon-17", cur_file),
#                                        "NationalCommitments",
#                                        "GlobalSustainability"))) %>%
#         unique()
#       #slice(which(Year ==2050))
#     }
# 
# 
#     db_full <- db_full %>%
#       rbind.data.frame(data) %>%
#       dplyr::mutate(ALPHA3 = gsub("_", "", ALPHA3))
# }
# 
# db_herd <- db_full %>%
#   select(Pathway, ALPHA3, ANIMAL_GLOBIOM, YEAR, FinFeasHerd) %>%
#   mutate(country = ifelse(nchar(ALPHA3) == 4 , substr(ALPHA3, 2, 4),
#                           ifelse(ALPHA3 == "RME", "NMC", ALPHA3))) %>%
#   data.frame()
# 
# xlsx::write.xlsx(db_herd, file = here("data", "scenathon_2023", "manure", "240424_Extracted_herdsize.xlsx"),
#                  row.names = F)
#>>>>>>> edc16e7e537126dc8656a34888ba11ed7865bae0


db_herd <- read_excel(here("data", "scenathon_2023", "manure", "240501_Extracted_herdsize.xlsx")) %>% 
  # mutate(ALPHA3 = ifelse(ALPHA3 == "RME", "NMC",
  #                        ifelse(nchar(ALPHA3)>3, stringr::str_sub(ALPHA3, 2, 4), ALPHA3))) %>% 
  select(-ALPHA3) %>% 
  rename(ALPHA3 = country) %>% 
  mutate(TLU = as.numeric(FinFeasHerd)) %>% 
  select(-FinFeasHerd)



# merge with GAMS manure data ----------------------------------------------

db_manure <- read_excel(here("data", "scenathon_2023", "manure", "Nmanure_GAMS.xlsx"),
                     sheet = "All_Nmanure_TLU") %>% 
  filter(!(is.na(ALPHA3))) %>% 
  pivot_longer(!ALPHA3, values_to = "Nmanure_TLU", names_to = "ANIMAL_GLOBIOM")

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

ggplot(db_manure_herd) +
  geom_line(aes(x = as.numeric(year), y = nmanure, linetype = pathway, colour = pathway),
            linewidth = 1)+
  facet_wrap(~country)


# CalcNLeftpasture in 1000 t N
db_N <- read.csv(here("data", "scenathon_2023", paste0(date_DB, "_FullDataBase.csv")), sep = "") %>% 
  select(tradeadjustment, pathway, country, year, calcn_org, calcn_synth) %>% 
  ######################### CAREFUL ONLY HAVE EXTRACTED WITH TRADE ADJUSTMENT ###############################
  #filter(tradeadjustment == "Yes") %>%
  ################################################################################
  mutate(year = as.character(year)) %>% 
  mutate(country = gsub("_", "", country)) %>% 
  mutate(country = ifelse(country == "RMECAS", "NMC", 
                          ifelse(nchar(country)>3, stringr::str_sub(country, 2, 4), country)))

db_NleftPasture <- db_manure_herd %>% 
  left_join(db_N) %>% 
  mutate(NPasture_beforeadj = nmanure - calcn_org,
         CalcNLeftPasture = pmax(0, nmanure - calcn_org))

ggplot(db_NleftPasture) +
  geom_line(aes(x = as.numeric(year), y = CalcNLeftPasture, linetype = pathway, colour = pathway),
            linewidth = 1)+
  facet_wrap(~country)

xlsx::write.xlsx(db_NleftPasture %>% data.frame(), 
                 file = here("output", "figures", "N", paste0(format(Sys.Date(),format = "%y%m%d"), "_ExpostNComputations.xlsx")), row.names = F)

# plots -------------------------------------------------------------------

p <- ggplot(db_NleftPasture %>% mutate(year = as.numeric(year)))+
  geom_line(aes(x = year, y = CalcNLeftPasture, colour = pathway))+
  scale_x_continuous(breaks = c(2000, 2025, 2050))+
  facet_wrap(~country,
             scales = "free")
p


# N targets ---------------------------------------------------------------

#FAO historical data for manure
db_FAO_Manure <- read_csv(here("data","scenathon_2023", "manure", "240415_FAOSTAT_Manure.csv")) %>% 
  mutate(Element = ifelse(`Element Code` == 72538, "FAOExcretedManure",
                          ifelse(`Element Code` == 72380, "FAONLeftPasture",
                                 "FAONorg"))) %>% 
  select(Year, Element, Value) %>% 
  pivot_wider(names_from = Element, values_from = Value) %>% 
  rename(year = Year)
#FAOhistorical data for synthetic N
db_FAO_Nutrient <- read_csv(here("data","scenathon_2023", "manure", "240415_FAOSTAT_NutrientNitrogen.csv")) %>% 
  rename(FAONsynth = Value,
         year = Year) %>% 
  select(year, FAONsynth)

#Fao total N use (Org and synth)
db_FAO_totn <- db_FAO_Manure %>% 
  left_join(db_FAO_Nutrient) %>% 
  mutate(FAOTotalN = FAOExcretedManure/1000000 + FAONsynth/1000)

#Compute scenathon 2023 total N
db_Ntarget <- db_NleftPasture %>% 
  select(tradeadjustment, pathway, country, year, CalcNLeftPasture, calcn_org, calcn_synth) %>% 
  mutate(totalN = CalcNLeftPasture + calcn_org + calcn_synth) %>% 
  group_by(tradeadjustment, pathway, year) %>% 
  reframe(totalN = sum(totalN),
          totNleftPasture = sum(CalcNLeftPasture),
          calcn_org = sum(calcn_org),
          calcn_synth = sum(calcn_synth))


p_tot <- ggplot(db_Ntarget%>% mutate(year = as.numeric(year)))+
  geom_line(aes(x = year, y = totalN/1000, colour = pathway))+
  geom_point(aes(x = year, y = 68, shape = "Target"))+
  geom_point(aes(x = year, y = FAOTotalN/1000, shape = "FAO"), data = db_FAO_totn)+
  scale_x_continuous(breaks = c(2000, 2025, 2050))+
  ggtitle("Total N input")
p_tot
# png(here("output", "figures", "N", paste0("TotalNInput", "_", gsub("-", "",Sys.Date()),".png")), bg = "white", height = 7, width = 10, unit = "in", res = 600)
# plot(p_tot)
# dev.off()

p_manureleftpasture <- ggplot(db_Ntarget %>% mutate(year = as.numeric(year)))+
  geom_line(aes(x = year, y = totNleftPasture/1000, colour = pathway))+
  geom_point(aes(x = year, y = FAONLeftPasture/1000000000, shape = "FAO"), data = db_FAO_Manure)+
  ggtitle("Manure Left on Pasture - N content")
p_manureleftpasture
# png(here("output", "figures", "N", paste0("ManureLeftPasture", "_", gsub("-", "",Sys.Date()),".png")), bg = "white", height = 7, width = 10, unit = "in", res = 600)
# plot(p_manureleftpasture)
# dev.off()

p_manuresoil <- ggplot(db_Ntarget %>% mutate(year = as.numeric(year)))+
  geom_line(aes(x = year, y = calcn_org/1000, colour = pathway))+
  geom_point(aes(x = year, y = FAONorg/1000000000, shape = "FAO"), data = db_FAO_Manure)+
  ggtitle("Manure applied to soils/ organic fertilisers - N content")
p_manuresoil

# png(here("output", "figures", "N", paste0("ManureSoil", "_", gsub("-", "",Sys.Date()),".png")), bg = "white", height = 7, width = 10, unit = "in", res = 600)
# plot(p_manuresoil)
# dev.off()

p_synth <- ggplot(db_Ntarget %>% mutate(year = as.numeric(year)))+
  geom_line(aes(x = year, y = calcn_synth/1000, colour = pathway))+
  geom_point(aes(x = year, y = FAONsynth/1000000, shape = "FAO"), data = db_FAO_Nutrient)+
  ggtitle("synthetic fertilisers - N content")
p_synth

# png(here("output", "figures", "N", paste0("NSynthetic", "_", gsub("-", "",Sys.Date()),".png")), bg = "white", height = 7, width = 10, unit = "in", res = 600)
# plot(p_synth)
# dev.off()

xlsx::write.xlsx(db_Ntarget %>% data.frame(), 
           file = here("output", "figures", "N", paste0(gsub("-", "",Sys.Date()), "_TotalNInput.xlsx")), row.names = F)


# Country level comparison ------------------------------------------------

mapping <- read_excel("data/scenathon_2023/mapping_country_FAO_FABLE.xlsx", 
                      sheet = "Sheet2") %>% 
  mutate(iso3 = countrycode::countrycode(Country_FAO, origin = 'country.name', destination = "iso3c"))

db_FAO_Manure_c <- read_csv(here("data","scenathon_2023", "manure", "240415_FAOSTAT_ManureAllCountries.csv")) %>% 
  filter(Area != "China, mainland") %>% 
  mutate(Element = ifelse(`Element Code` == 72538, "FAOExcretedManure",
                          ifelse(`Element Code` == 72380, "FAONLeftPasture",
                                 "FAONorg"))) %>% 
  select(Area, Year, Element, Value) %>% 
  data.frame() %>% 
  mutate(iso3 = countrycode::countrycode(Area, origin = 'country.name', destination = "iso3c")) %>% 
  rename(year = Year) %>% 
  left_join(mapping) %>% 
  group_by(country, Element, year) %>%
  reframe(Value = sum(Value)) %>% 
  pivot_wider(names_from = Element, values_from = Value) 
  
   

db_FAO_Nutrient_c <- read_csv(here("data","scenathon_2023", "manure", "240415_FAOSTAT_NutrientNitrogenAllCountries.csv")) %>% 
  filter(Area != "China, mainland") %>% 
  mutate(iso3 = countrycode::countrycode(Area, origin = 'country.name', destination = "iso3c")) %>% 
  rename(year = Year) %>% 
  left_join(mapping) %>% 
  group_by(country, year) %>%
  reframe(FAONsynth = sum(Value))


db_calc_c <-db_NleftPasture %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(db_FAO_Nutrient_c %>% mutate(FAONsynth = FAONsynth/1000)) %>% 
  left_join(db_FAO_Manure_c %>% mutate(FAONLeftPasture = FAONLeftPasture/1000000,
                                       FAONorg = FAONorg/1000000,
                                       FAOExcreted = FAOExcretedManure/1000000)) %>% 
  mutate(rel_diff_synth = (calcn_synth -FAONsynth)/FAONsynth,
         rel_diff_Npasture = (CalcNLeftPasture - FAONLeftPasture)/FAONLeftPasture,
         rel_diff_Nsoils = (calcn_org - FAONorg)/FAONorg,
         rel_diff_NExcreted = (nmanure - FAOExcreted)/FAOExcreted,
         diff_synth = (calcn_synth -FAONsynth),
         diff_Npasture = (CalcNLeftPasture - FAONLeftPasture),
         diff_Nsoils = (calcn_org - FAONorg),
         diff_NExcreted = (nmanure - FAOExcreted)) %>% 
  group_by(pathway, year) %>% 
  mutate(tot_diff_synth = sum(abs(diff_synth), na.rm = T),
         tot_diff_Npasture = sum(abs(diff_Npasture), na.rm = T),
         tot_diff_Nsoils = sum(abs(diff_Nsoils), na.rm = T),
         tot_diff_NExcreted = sum(abs(diff_NExcreted), na.rm = T)) %>% 
  mutate(sh_diff_synth = diff_synth/tot_diff_synth,
         sh_diff_Npasture = diff_Npasture/tot_diff_Npasture,
         sh_diff_Nsoils = diff_Nsoils/tot_diff_Nsoils,
         sh_diff_NExcreted = diff_NExcreted/tot_diff_NExcreted) %>% 
  mutate(weight = (abs(sh_diff_synth)+ abs(sh_diff_NExcreted))/2)
  
df_synth <- db_calc_c %>% 
  filter(tradeadjustment == "No") %>% 
  filter(pathway == "CurrentTrends") %>% 
  select(country, year, calcn_synth, FAONsynth, rel_diff_synth, diff_synth, tot_diff_synth, sh_diff_synth) %>% 
  filter(year %in% seq(2000, 2020, 5))

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown", "orange", "forestgreen"
)

ggplot(db_calc_c %>% filter(country != "RWA"))+
  geom_abline(intercept = 0)+
  geom_point(aes(x = rel_diff_synth + 1, 
                 y = rel_diff_NExcreted + 1, 
                 size = (abs(sh_diff_synth)+ abs(sh_diff_NExcreted))/2,
                 colour = country),
             show.legend = F)+
  scale_colour_manual(values = c25)+
  geom_label(aes(x = rel_diff_synth + 1, 
                y = rel_diff_NExcreted + 1,
                label = country,
                group = country,
                colour = country,
                alpha = 0.005),
             show.legend = F,
            data= db_calc_c %>% filter(country != "RWA") %>% filter(year == 2010))+
  xlab("synth")+ ylab("N Excreted")+
  theme(legend.position = "bottom")

df_org <- db_calc_c %>% 
  filter(tradeadjustment == "No") %>% 
  filter(pathway == "CurrentTrends") %>% 
  select(country, year, calcn_org, FAONorg, rel_diff_Nsoils, diff_Nsoils, tot_diff_Nsoils, sh_diff_Nsoils) %>% 
  filter(year %in% seq(2000, 2020, 5))

df_pasture <- db_calc_c %>% 
  filter(tradeadjustment == "No") %>% 
  filter(pathway == "CurrentTrends") %>% 
  select(country, year, CalcNLeftPasture, FAONLeftPasture, rel_diff_Npasture, diff_Npasture, tot_diff_Npasture, sh_diff_Npasture) %>% 
  filter(year %in% seq(2000, 2020, 5))

df_excreted <- db_calc_c %>% 
  filter(tradeadjustment == "No") %>% 
  filter(pathway == "CurrentTrends") %>% 
  select(country, year, nmanure, FAOExcreted, rel_diff_NExcreted, diff_NExcreted, tot_diff_NExcreted, sh_diff_NExcreted) %>% 
  filter(year %in% seq(2000, 2020, 5))

p_synth_diff <- ggplot(db_calc_c)+
  geom_point(aes(x = rel_diff_synth, y = rel_diff_synth, size = diff_synth, colour = country))
