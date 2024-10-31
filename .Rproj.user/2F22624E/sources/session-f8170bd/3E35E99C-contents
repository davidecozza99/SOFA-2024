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


conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)

here()

#Data -------------------------------------------------------------------
# old: report_BRA_20240224_12H29

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
db_manure <- db_manure %>% filter(Location == "Brazil")

brazil_data <- read_xlsx(here("data", "report_BRA_20240424_10H17_cumulative.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, NewForestChange, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth, Nmanure,
         CalcWFblue)%>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GHG = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)

brazil_data$scenarios <- substring(brazil_data$Pathway, 4)


# write_xlsx(brazil_data, here("data", "Decomposition", "brazil_deco2.xlsx"))
### On excel, added between each pathway the reference CT with Tradeadj, so when computing for the lag, we take in consideration again CT when passing from NC_xxx to GS_xxx


brazil_data <- read_xlsx(here("data","Decomposition", "brazil_deco2.xlsx"))

brazil_data$Pathway[brazil_data$Pathway == "NationalCommitments"] <- "NC_complete"
brazil_data$Pathway[brazil_data$Pathway == "GlobalSustainability"] <- "GS_complete"

brazil_data$Pathway[brazil_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
brazil_data$Pathway[brazil_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

brazil_data$scenarios <- substring(brazil_data$Pathway, 4)

brazil_data <- brazil_data %>% 
  filter(!Pathway %in% c("NC_tradeeffect","GS_tradeeffect" )) %>% 
  mutate(Year = as.factor(Year))


brazil <- brazil_data %>% 
  group_by(Year) %>% 
  mutate(
    Pathway_code = ifelse(str_starts(Pathway, "NC"), "NC", ifelse(str_starts(Pathway, "GS"), "GS", NA)),
    diff_kcal_feas = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, kcal_feas - lag(kcal_feas)),
    diff_ForestChange = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, ForestChange - lag(ForestChange)),
    diff_NewForestChange = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, NewForestChange - lag(NewForestChange)),
    diff_CalcFarmLabourFTE = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CalcFarmLabourFTE - lag(CalcFarmLabourFTE)),
    diff_CO2 = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CO2 - lag(CO2)),
    diff_CH4 = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CH4 - lag(CH4)),
    diff_N2O = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, N2O - lag(N2O)),
    diff_GHG = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, GHG - lag(GHG)),
    diff_kcal_feas = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, kcal_feas - lag(kcal_feas)),
    diff_kcal_mder = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, kcal_mder - lag(kcal_mder)),
    diff_LNPPMatureForest = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, LNPPMatureForest - lag(LNPPMatureForest)),
    diff_LNPPMatureOtherLand = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, LNPPMatureOtherLand - lag(LNPPMatureOtherLand)),
    diff_TotalN = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, TotalN - lag(TotalN)),
    diff_CalcWFblue = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CalcWFblue - lag(CalcWFblue))
  ) %>% 
  unique() 



#Labelling --------------------------
pathway_labels <- c(
  "GDP" = "GDP",
  "pop" = "Population",
  "diet" = "Diet",
  "import" = "Import",
  "export" = "Export",
  "postharvloss" = "Post harvest loss",
  "foodwaste" = "Food waste",
  "live" = "Livestock productivity",
  "crop" = "Crop productivity",
  "popactivity" = "Population activity",
  "agrexp" = "Deforestation control",
  "affor" = "Afforestation",
  "urban" = "Urbanization",
  "rumdensity" = "Ruminant density",
  "pa" = "Protected areas",
  "biofuel" = "Biofuel",
  "agropra" = "Agroecological practices",
  "irri" = "Irrigation",
  "final" = "Final",
  "agroforestry" = "Agroforestry",
  "grassland" = "Intensive/ Extensive\n grassland share",
  "peatland" = "Peatland",
  # "live_rumdensity" = "Livestock productivity and Ruminant Density",
  "tradeeffect" = "International demand")


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
  "GHG" = "AFOLU GHG (CO2 + CH4 + N2O)",
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
  "GHG" = "Mt CO2e per year",
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
elements <- c("CH4"
              ,"CO2", "N2O", "kcal_feas",
              "kcal_mder", "ForestChange",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)



# # # Remove NA values
brazil$Pathway_code <- factor(brazil$Pathway_code, levels = c("NC", "GS"))
brazil <- brazil[complete.cases(brazil$Pathway_code), ]


# 
bra <- brazil
# %>% 
#   filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
#                          "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>% 
#   mutate(Year = as.factor(Year))

# write_xlsx(bra, here("data", "Decomposition", "brazil_prova.xlsx"))


# folder to store the plots
figure_directory <- here("output", "decomposition", "BRA_othermethod", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)




plots_list <- list()

for (element in elements) {
  
  # Create custom label depending on the element
  if (element == "GAS" || element == "CO2" || element == "CH4" || element == "N2O") {
    y_label <- expression(Difference~"in"~Mt~CO[2]*e~per~year~compared~to~CT)
  } else {
    y_label <- paste("Difference in", units_labels[element], "compared to CT")
  }
  
  # Create the plot
  current_plot <- bra %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = y_label  # Use the correctly assigned y_label
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National commitments",
                 "GS" = "Global sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(bra$Year[!is.na(bra[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 22, face = "bold"),
      strip.text = element_text(size = 24, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 22, face = "bold"),
      legend.text = element_text(family = "Arial", size = 22),
      plot.title = element_text(color = "black", size = 22, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 22),
      axis.text.x = element_text(color = "black", size = 18),
      axis.text.y = element_text(color = "black", size = 18),
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
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_BRA_cum_" ,element, ".tiff")
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}
# plots_list



#Need to add point for the NC and GS final difference
#Need to adjust scale_y_continuous
#Do not repeat the same label in Scenarios
#Eliminate some years in the x-axis
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
db_manure <- db_manure %>% filter(Location == "Brazil")



brazil_data_inverse <- read_xlsx(here("data", "report_BRA_20240424_11H45_cumulative_inverse.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, NewForestChange, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,Nmanure,
         CalcWFblue)%>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(TotalN = Nmanure + CalcN_synth) 


brazil_data_inverse$scenarios <- substring(brazil_data_inverse$Pathway, 4)



# write_xlsx(brazil_data_inverse, here("data", "Decomposition", "brazil_deco2_inverse.xlsx"))
### On excel, added between each pathway the reference CT with Tradeadj, so when computing for the lag, we take in consideration again CT when passing from NC_xxx to GS_xxx


brazil_data_inverse <- read_xlsx(here("data","Decomposition", "brazil_deco2_inverse.xlsx"))

brazil_data_inverse$Pathway[brazil_data_inverse$Pathway == "NationalCommitments"] <- "NC_complete"
brazil_data_inverse$Pathway[brazil_data_inverse$Pathway == "GlobalSustainability"] <- "GS_complete"

brazil_data_inverse$Pathway[brazil_data_inverse$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
brazil_data_inverse$Pathway[brazil_data_inverse$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

brazil_data_inverse$scenarios <- substring(brazil_data_inverse$Pathway, 4)

brazil_data_inverse <- brazil_data_inverse %>% 
  filter(!Pathway %in% c("NC_tradeeffect","GS_tradeeffect" )) %>% 
  mutate(Year = as.factor(Year))



brazil_inverse <- brazil_data_inverse %>% 
  group_by(Year) %>% 
  mutate(
    Pathway_code = ifelse(str_starts(Pathway, "NC"), "NC", ifelse(str_starts(Pathway, "GS"), "GS", NA)),
    diff_kcal_feas = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, kcal_feas - lag(kcal_feas)),
    diff_ForestChange = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, ForestChange - lag(ForestChange)),
    diff_NewForestChange = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, NewForestChange - lag(NewForestChange)),
    diff_CalcFarmLabourFTE = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CalcFarmLabourFTE - lag(CalcFarmLabourFTE)),
    diff_CO2 = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CO2 - lag(CO2)),
    diff_CH4 = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CH4 - lag(CH4)),
    diff_N2O = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, N2O - lag(N2O)),
    diff_kcal_mder = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, kcal_mder - lag(kcal_mder)),
    diff_LNPPMatureForest = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, LNPPMatureForest - lag(LNPPMatureForest)),
    diff_LNPPMatureOtherLand = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, LNPPMatureOtherLand - lag(LNPPMatureOtherLand)),
    diff_TotalN = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, TotalN - lag(TotalN)),
    diff_CalcWFblue = ifelse(Pathway %in% c("NC_final", "GS_final"), NA, CalcWFblue - lag(CalcWFblue))
  )


brazil_data_inverse$scenarios <- substring(brazil_data_inverse$Pathway, 4)



#Remove NA values
brazil_inverse$Pathway_code <- factor(brazil_inverse$Pathway_code, levels = c("NC", "GS"))
brazil_inverse <- brazil_inverse[complete.cases(brazil_inverse$Pathway_code), ]


bra <- brazil_inverse



# write_xlsx(bra, here("data", "Decomposition", "brazil_prova_inverse.xlsx"))

# folder to store the plots
figure_directory <- here("output", "decomposition", "BRA_othermethod_inverse", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)


plots_list <- list()

for (element in elements) {
  
  # Create custom label depending on the element
  if (element == "GAS" || element == "CO2" || element == "CH4" || element == "N2O") {
    y_label <- expression(Difference~"in"~Mt~CO[2]*e~per~year~compared~to~CT)
  } else {
    y_label <- paste("Difference in", units_labels[element], "compared to CT")
  }
  
  # Create the plot
  current_plot <- bra %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T)) +
    geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    # geom_point(data = filter(bra, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
    #            aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
    #            size = 3, shape = 16, alpha = 0.7) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = y_label  # Use the correctly assigned y_label
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National commitments",
                 "GS" = "Global sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(bra$Year[!is.na(bra[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 22, face = "bold"),
      strip.text = element_text(size = 24, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 22, face = "bold"),
      legend.text = element_text(family = "Arial", size = 22),
      plot.title = element_text(color = "black", size = 22, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 22),
      axis.text.x = element_text(color = "black", size = 18),
      axis.text.y = element_text(color = "black", size = 18),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(1, 'mm'),
      legend.spacing.y = unit(0.5, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(5, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    )
  
  filename <- paste0(gsub("-", "", format(Sys.Date(), format = "%y%m%d")), "_BRA_cum_inverse_", element, ".tiff")
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

