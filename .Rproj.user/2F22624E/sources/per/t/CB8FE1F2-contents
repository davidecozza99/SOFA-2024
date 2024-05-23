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
library(tidyr)
library(cowplot)

conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)

here()





#Data -------------------------------------------------------------------

db_manure <- read_excel("data/Manure/240517_db_Nmanure_live.xlsx") %>% 
  rename(Year = YEAR) %>%
  mutate(Year = as.double(Year)) %>%
  mutate(
    ALPHA3 = case_when(
      ALPHA3 == "AUS" ~ "Australia",
      ALPHA3 == "BRA" ~ "Brazil",
      ALPHA3 == "COL" ~ "Colombia",
      ALPHA3 == "ETH" ~ "Ethiopia",
      ALPHA3 == "GBR" ~ "UK",
      TRUE ~ ALPHA3  # Keeps the original value if it doesn't match any of the specified cases
    )
  ) %>%
  rename(Location = ALPHA3) %>%
  mutate(Pathway = if_else(Pathway == "CurrentTrend", "Current Trend_Yes", Pathway))

# Split the database into separate data frames based on the value of ALPHA3
db_manure <- db_manure %>% filter(Location == "Australia")



aus_data <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth, Nmanure,
         CalcWFblue) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)
  # left_join(N) %>% 
  # mutate(TotalN = CalcN_org + CalcN_synth + CalcNLeftPasture, na.rm =TRUE)


aus_data$Pathway[aus_data$Pathway == "NationalCommitments"] <- "NC_complete"
aus_data$Pathway[aus_data$Pathway == "GlobalSustainability"] <- "GS_complete"

aus_data$Pathway[aus_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
aus_data$Pathway[aus_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"



# Commodities -----------------------------
aus_comm <- read_xlsx(here("data", "report_AUS_20240306_9H01.xlsx"), sheet = "Commodities") %>%
  rename(Pathway = `Current Trend`) %>%
  filter(Year %in% c("2030", "2050"))%>%
  select(Location, Pathway, Year, Product, kcalfeasprod) %>%
  unique()

# write.xlsx(aus_comm, file = here("data", "Decomposition", "aus_tool.xlsx"))


aus_comm$Pathway[aus_comm$Pathway == "NationalCommitments"] <- "NC_complete"
aus_comm$Pathway[aus_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

aus_comm$Pathway[aus_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
aus_comm$Pathway[aus_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

aus_kcal <- aus_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()

# 
# filter(Pathway %in% c("Current Trend_Yes", "GS_popactivity")) %>%
#   filter(Year==2030) %>%
#   distinct()


aus_kcal_final <- aus_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

#Final Database ---------------------------------------
aus_data <- left_join(aus_data, aus_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() %>% 
  filter(Pathway != "GS_live_rumdensity") 




aus <- aus_data %>%
  group_by(Year) %>%
  mutate(
    Pathway_code = ifelse(str_starts(Pathway, "NC"), "NC", ifelse(str_starts(Pathway, "GS"), "GS", NA)),
    diff_kcal_feas = ifelse(Pathway != "Current Trend_Yes", kcal_feas - first(kcal_feas[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_plant = ifelse(Pathway != "Current Trend_Yes", kcal_plant - first(kcal_plant[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_anim = ifelse(Pathway != "Current Trend_Yes", kcal_anim - first(kcal_anim[Pathway == "Current Trend_Yes"]), NA),
    diff_ForestChange = ifelse(Pathway != "Current Trend_Yes", ForestChange - first(ForestChange[Pathway == "Current Trend_Yes"]), NA),
    diff_Cropland_change = ifelse(Pathway != "Current Trend_Yes", Cropland_change - first(Cropland_change[Pathway == "Current Trend_Yes"]), NA),
    diff_Pasture_change = ifelse(Pathway != "Current Trend_Yes", Pasture_change - first(Pasture_change[Pathway == "Current Trend_Yes"]), NA),
    diff_OtherLand_change = ifelse(Pathway != "Current Trend_Yes", OtherLand_change - first(OtherLand_change[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcFarmLabourFTE = ifelse(Pathway != "Current Trend_Yes", CalcFarmLabourFTE - first(CalcFarmLabourFTE[Pathway == "Current Trend_Yes"]), NA),
    diff_CO2 = ifelse(Pathway != "Current Trend_Yes", CO2 - first(CO2[Pathway == "Current Trend_Yes"]), NA),
    diff_CH4 = ifelse(Pathway != "Current Trend_Yes", CH4 - first(CH4[Pathway == "Current Trend_Yes"]), NA),
    diff_N2O = ifelse(Pathway != "Current Trend_Yes", N2O - first(N2O[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) %>% 
  filter(Pathway != "GS_live_rumdensity") 
  


aus$scenarios <- substring(aus_data$Pathway, 4)




#Labelling --------------------------
pathway_labels <- c(
  "GDP" = "GDP",
  "pop" = "Population",
  "diet" = "Diet",
  "import" = "Import",
  "export" = "Export",
  "postharvloss" = "Post Harvest Loss",
  "foodwaste" = "Food Waste",
  "live" = "Livestock Productivity",
  "crop" = "Crop Productivity",
  "popactivity" = "Population Activity",
  "agrexp" = "Deforestation Control",
  "affor" = "Afforestation",
  "urban" = "Urbanization",
  "rumdensity" = "Ruminant Density",
  "pa" = "Protected Areas",
  "biofuel" = "Biofuel",
  "agropra" = "Agroecological Practices",
  "irri" = "Irrigation",
  "final" = "Final",
  "agroforestry" = "Agroforestry",
  "grassland" = "Intensive/ Extensive\n grassland share",
  "peatland" = "Peatland",
  # "live_rumdensity" = "Livestock productivity and Ruminant Density",
  "tradeeffect" = "International Demand")

pathway_colors <- c(  
  "GDP" = "yellow",  
  "pop" = "grey",  
  "diet" = "#00BFFF",  
  "import" = "#98FB98",
  "export" = "darkorange",  
  "postharvloss" = "red",
  "foodwaste" = "#4250A5",    
  "live" = "#2F4F4F",  
  "crop" = "#985AE0",  
  "popactivity" = "#897E0B",
  "agrexp" = "#D2691E",   
  "affor" = "#228B22",  
  "urban" = "#FF00FF",    
  "rumdensity" = "green",         
  "pa" = "#000080",        
  "biofuel" = "#A52A2A",  
  "agropra" = "#FFA07A",
  "irri" = "#FFD700",  
  "agroforestry" = "black",  
  "grassland" = "#FF4500",  
  # "peatland" = "#FF4500",
  "live_rumdensity" = "#8B008B",
  "tradeeffect" = "pink"
)


element_labels <- c(
  "GAS" = "GHG",
  "CO2" = "CO2 Emissions", 
  "CH4" = "Methane (CH4) Emissions", 
  "N2O" = "Nitrous Oxide (N2O) Emissions", 
  "kcal_feas" = "Feasible Kcal", 
  "kcal_plant" = "Feasible Kcal from Plant-based products",
  "kcal_anim" = "Feasible Kcal from Animal-based products",
  "kcal_mder" = "MDER Kcal", 
  "ForestChange" = "Forest Change", 
  "Cropland_change" = "Cropland Change", 
  "Pasture_change" = "Pasture Change", 
  "OtherLand_change" = "Other Land Change", 
  "CalcFarmLabourFTE" = "Farm Labour FTE",
  "LNPPMatureForest" = "LNPP Mature Forest", 
  "LNPPMatureOtherLand" = "LNPP Mature Other Land", 
  "TotalN" = "Total Nitrogen", 
  "CalcWFblue" = "Water Irrigation Requirements"
)

units_labels <- c(
  "GAS" = "Mt CO2e per year",
  "CO2" = "Mt CO2e per year", 
  "CH4" = "Mt CO2e per year", 
  "N2O" = "Mt CO2e per year", 
  "kcal_feas" = "kcal per capita per day", 
  "kcal_plant" = "kcal per capita per day", 
  "kcal_anim" = "kcal per capita per day",
  "kcal_mder" = "kcal per capita per day", 
  "ForestChange" = "1000 ha per year", 
  "Cropland_change" = "1000 ha per year", 
  "Pasture_change" = "1000 ha per year", 
  "OtherLand_change" = "1000 ha per year", 
  "CalcFarmLabourFTE" = "1000 FTE workers",
  "LNPPMatureForest" = "1000 ha", 
  "LNPPMatureOtherLand" = "1000 ha", 
  "TotalN" = "1000 tonnes", 
  "CalcWFblue" = "1000 tonnes"
)


# List of elements for decomposition analysis
elements <- c("CH4","CO2", "N2O",
              "kcal_feas","kcal_anim", "kcal_plant",
              "ForestChange",  "Cropland_change", "Pasture_change", "OtherLand_change",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)

# write.xlsx(aus, file = here("data", "Decomposition", "aus.xlsx"))

#Reordering pathwyas + erasing CT
aus$Pathway_code <- factor(aus$Pathway_code, levels = c("NC", "GS"))
aus <- aus[complete.cases(aus$Pathway_code), ]


aus<- aus %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>% 
  mutate(Year = as.factor(Year))


# write.xlsx(aus, file = here("data", "Decomposition", "aus.xlsx"))

#Plot -------------------------------------------------------------------

# folder to store the plots
figure_directory <- here("output", "decomposition", "AUS", paste0(gsub("-", "", Sys.Date())))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

 
#Loop
plots_list <- list()

for (element in elements) {
  # Create the plot
  current_plot <- aus %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(aus, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 2, byrow = T))+
    geom_point(data = filter(aus, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    # geom_point(data = filter(aus, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
    #            aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
    #            size = 3, shape = 16, alpha =0.7) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = paste("Difference in", units_labels[element], "compared to CT")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(aus$Year[!is.na(aus[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 14, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 14),
      axis.text.x = element_text(color = "black", size = 13),
      axis.text.y = element_text(color = "black", size = 13),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(1, 'mm'),
      legend.spacing.y = unit(0.5, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(5, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    )
  
  filename <- paste0(gsub("-", "", Sys.Date()), "_" ,element, ".tiff")
  
  # # # # Save the current plot as TIFF
  # tiff(
  #   filename = here(figure_directory, filename),
  #   units = "in", height = 10, width = 18, res = 600
  # )
  # print(current_plot)
  # dev.off()

  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

plots_list
