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

conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)

here()

#Data -------------------------------------------------------------------

aus_data <- read_xlsx(here("data", "report_AUS_20240228_15H16.xlsx"), sheet = "Indicators") %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  rename(Pathway = `Current Trend`) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, NewForestChange, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)

aus_data$Pathway[aus_data$Pathway == "NationalCommitments"] <- "NC_complete"
aus_data$Pathway[aus_data$Pathway == "GlobalSustainability"] <- "GS_complete"

aus_data$Pathway[aus_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
aus_data$Pathway[aus_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

aus <- aus_data %>%
  group_by(Year) %>%
  mutate(
    Pathway_code = ifelse(str_starts(Pathway, "NC"), "NC", ifelse(str_starts(Pathway, "GS"), "GS", NA)),
    diff_kcal_feas = ifelse(Pathway != "Current Trend_Yes", kcal_feas - first(kcal_feas[Pathway == "Current Trend_Yes"]), NA),
    diff_ForestChange = ifelse(Pathway != "Current Trend_Yes", ForestChange - first(ForestChange[Pathway == "Current Trend_Yes"]), NA),
    diff_NewForestChange = ifelse(Pathway != "Current Trend_Yes", NewForestChange - first(NewForestChange[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcFarmLabourFTE = ifelse(Pathway != "Current Trend_Yes", CalcFarmLabourFTE - first(CalcFarmLabourFTE[Pathway == "Current Trend_Yes"]), NA),
    diff_CO2 = ifelse(Pathway != "Current Trend_Yes", CO2 - first(CO2[Pathway == "Current Trend_Yes"]), NA),
    diff_CH4 = ifelse(Pathway != "Current Trend_Yes", CH4 - first(CH4[Pathway == "Current Trend_Yes"]), NA),
    diff_N2O = ifelse(Pathway != "Current Trend_Yes", N2O - first(N2O[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


aus$scenarios <- substring(aus_data$Pathway, 4)



#Labelling -------------------------------------------------------------------

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
  "agrexp" = "Agricultural Expansion",
  "affor" = "Afforestation",
  "urban" = "Urbanization",
  "rumdensity" = "Ruminant Density",
  "pa" = "Protected Areas",
  "biofuel" = "Biofuel",
  "agropra" = "Agroforestry Practices",
  "irri" = "Irrigation",
  "final" = "Final",
  "agroforestry" = "Agroforestry",
  "grassland" = "Intensive/ Extensive grassland share",
  "peatland" = "Peatland",
  "live_rumdensity" = "Livestock productivity and Ruminant Density")

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
  "grassland" = "#008000",  
  "peatland" = "#FF4500",
  "live_rumdensity" = "#8B008B"
)


element_labels <- c(
  "CO2" = "CO2 Emissions", 
  "CH4" = "Methane (CH4) Emissions", 
  "N2O" = "Nitrous Oxide (N2O) Emissions", 
  "kcal_feas" = "Feasible Kcal", 
  "kcal_mder" = "MDER Kcal", 
  "ForestChange" = "Forest Change", 
  "NewForestChange" = "New Forest Change", 
  "CalcFarmLabourFTE" = "Farm Labour FTE",
  "LNPPMatureForest" = "LNPP Mature Forest", 
  "LNPPMatureOtherLand" = "LNPP Mature Other Land", 
  "TotalN" = "Total Nitrogen", 
  "CalcWFblue" = "Water Irrigation Requirements"
)

units_labels <- c(
  "CO2" = "(Mt CO2e per year)", 
  "CH4" = "(Mt CO2e per year)", 
  "N2O" = "(Mt CO2e per year)", 
  "kcal_feas" = "(kcal per capita per day)", 
  "kcal_mder" = "(kcal per capita per day)", 
  "ForestChange" = "(1000 ha per 5 year)", 
  "NewForestChange" = "(1000 ha per 5 year)", 
  "CalcFarmLabourFTE" = "(1000 FTE workers)",
  "LNPPMatureForest" = "(1000 ha)", 
  "LNPPMatureOtherLand" = "(1000 ha)", 
  "TotalN" = "(1000 tonnes)", 
  "CalcWFblue" = "(1000 tonnes)"
)

# List of elements for decomposition analysis
elements <- c("CH4"
              ,"CO2", "N2O", "kcal_feas",
              # "kcal_mder",
              "ForestChange", "NewForestChange",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)

#Reordering pathwyas + erasing CT
aus$Pathway_code <- factor(aus$Pathway_code, levels = c("NC", "GS"))
aus <- aus[complete.cases(aus$Pathway_code), ]


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
    geom_hline(yintercept = 0, linetype = "solid") +
    geom_bar(stat = "identity", data = filter(aus, !str_detect(Pathway, "complete") & !str_detect(Pathway, "tradeeffect")),
             aes(fill = scenarios)) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    geom_point(data = filter(aus, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "Net Difference"),
               size = 3, shape = 16) + 
    geom_point(data = filter(aus, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
               size = 3, shape = 16, alpha =0.7) + 
    scale_color_manual(values = c("black", "red"), name = "",
                       labels = c("NC/GS Trade Adjustment Effect on CT", "Net Difference")) +  
    labs(
      title = paste("Decomposition analysis: \nImpact of each scenario change on", element_labels[element]),
      x = "Year",
      y = paste("Compared to Current Trend \n", units_labels[element])
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "Scenarios", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_continuous(breaks = unique(aus$Year[!is.na(aus[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 12, face = "bold"),
      legend.title = element_text(family = "Arial", color = "steelblue", size = 12, face = "bold"),
      legend.text = element_text(family = "Arial", size = 12),
      plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
      axis.title.x = element_text(color = "steelblue", size = 12),
      axis.title.y = element_text(color = "steelblue", size = 12)
    )
  

  # Save the current plot as TIFF
  tiff(
    filename = here(figure_directory, paste0(element, ".tiff")),
    units = "in", height = 5, width = 14, res = 300
  )
  print(current_plot)
  dev.off()

  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

plots_list
