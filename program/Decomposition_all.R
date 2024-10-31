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
  mutate(GHG = CO2 + CH4 + N2O) %>% 
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
    diff_GHG = ifelse(Pathway != "Current Trend_Yes", GHG - first(GHG[Pathway == "Current Trend_Yes"]), NA),
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
  "GHG" = "AFOLU GHG (CO2 + CH4 + N2O)",
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
  "GHG" = "Mt CO2e per year", 
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
elements <- c("CH4","CO2", "N2O", "GHG",
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
figure_directory <- here("output", "decomposition", "AUS_with_trade", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
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
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
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
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
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
      text = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      strip.text = element_text(size = 28, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      legend.text = element_text(family = "Arial", size = 25),
      plot.title = element_text(color = "black", size = 26, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 23),
      axis.text.x = element_text(color = "black", size = 20),
      axis.text.y = element_text(color = "black", size = 20),
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
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_" ,element, ".tiff")
  
  # # # # Save the current plot as TIFF
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 13, width = 20, res = 600
  )
  print(current_plot)
  dev.off()
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

# plots_list






# LAND FIGURE (Cropland, Pasture, Other Land, Forest change)
for (element in elements) {
  # Create the plot
  current_plot <- aus %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(aus, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(aus, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) +
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange")
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange.tiff")
# 
tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()




# AFOLU GHG FIGURE (CO2, CH4, N2O)
for (element in elements) {
  # Create the plot
  current_plot <- aus %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(aus, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(aus, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("CO2", "CH4", "N2O", "GHG") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in Mt CO2e per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void() 

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_4GHG.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()






# KCAL FIGURE (kcal_plant, kcal_anim)
for (element in elements) {
  # Create the plot
  current_plot <- aus %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(aus, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(aus, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("kcal_plant", "kcal_anim") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in kcal per capita per day compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_kcal_anim_plant.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()


rm(list = ls()) 




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
db_manure <- db_manure %>% filter(Location == "Brazil")

bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,Nmanure,
         CalcWFblue) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>% 
  filter(Year %in% c("2030", "2050")) %>%
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GHG = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)

bra_data$Pathway[bra_data$Pathway == "NationalCommitments"] <- "NC_complete"
bra_data$Pathway[bra_data$Pathway == "GlobalSustainability"] <- "GS_complete"

bra_data$Pathway[bra_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
bra_data$Pathway[bra_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"




# Commodities -----------------------------
bra_comm <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Commodities") %>%
  rename(Pathway = `Current Trend`) %>%
  filter(Year %in% c("2030", "2050"))%>%
  select(Location, Pathway, Year, Product, kcalfeasprod) %>%
  unique()



bra_comm$Pathway[bra_comm$Pathway == "NationalCommitments"] <- "NC_complete"
bra_comm$Pathway[bra_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

bra_comm$Pathway[bra_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
bra_comm$Pathway[bra_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

bra_kcal <- bra_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


bra_kcal_final <- bra_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)


#Final Database ---------------------------------------
bra_data <- left_join(bra_data, bra_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 



bra <- bra_data %>%
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
    diff_GHG = ifelse(Pathway != "Current Trend_Yes", GHG - first(GHG[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


# write.xlsx(bra, file = here("data", "Decomposition", paste0(gsub("-", "",format(Sys.Date(),format = "%y%m%d")), "_", "bra.xlsx")))


bra$scenarios <- substring(bra_data$Pathway, 4)




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
elements <- c("CH4","CO2", "N2O", "GHG",
              "kcal_feas","kcal_anim", "kcal_plant",
              "ForestChange",  "Cropland_change", "Pasture_change", "OtherLand_change",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)


#Reordering pathwyas + erasing CT
bra$Pathway_code <- factor(bra$Pathway_code, levels = c("NC", "GS"))
bra <- bra[complete.cases(bra$Pathway_code), ]

bra<- bra %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>% 
  mutate(Year = as.factor(Year))


#Plot -------------------------------------------------------------------

# folder to store the plots
figure_directory <- here("output", "decomposition", "BRA_with_trade", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

#Loop
plots_list <- list()

for (element in elements) {
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
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(bra$Year[!is.na(bra[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      strip.text = element_text(size = 22, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 30, face = "bold"),
      legend.text = element_text(family = "Arial", size = 18),
      plot.title = element_text(color = "black", size = 20, face = "bold"), 
      axis.title = element_text(color = "black", size = 16),
      axis.text.x = element_text(color = "black", size = 16),
      axis.text.y = element_text(color = "black", size = 16),
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
  
  # Adjust filename for JPEG
  filename <- paste0(gsub("-", "", format(Sys.Date(), format = "%y%m%d")), "_", element, ".jpeg")
  
  # Save the plot as a JPEG
  jpeg(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

# plots_list



# # MULTIPLE FIGURE ("kcal_feas", "CH4", "TotalN", "CalcWFblue")
# for (element in elements) {
#   # Create the plot
#   current_plot <- bra %>%
#     group_by(Pathway_code) %>%
#     ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
#     geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
#              aes(fill = scenarios), colour = "white", size = 0.2) +
#     geom_hline(yintercept = 0, linetype = "solid") +
#     guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T)) +
#     geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
#                aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
#                size = 3, shape = 16) +
#     scale_color_manual(values = c("black"), name = "",
#                        labels = c("All scenarios combined")) +
#     labs(
#       x = "",
#       y = paste("Difference in", units_labels[element], "\ncompared to Current Trends")
#     ) +
#     facet_grid(. ~ Pathway_code, scales = "free_y",
#                labeller = labeller(Pathway_code = c(
#                  "NC" = "NC",
#                  "GS" = "GS"
#                ))) +
#     scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
#     theme_minimal() +
#     theme(
#       text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
#       legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
#       legend.text = element_text(family = "Arial", size = 13),
#       axis.title.x = element_text(color = "black", size = 12),
#       axis.title.y = element_text(color = "black", size = 12),
#       legend.position = "none",
#       legend.direction = "horizontal",
#       legend.box = "vertical",
#       legend.box.spacing = unit(0.5, 'mm'),
#       legend.spacing.x = unit(3, 'mm'),
#       legend.spacing.y = unit(3, 'mm'),
#       legend.box.margin = unit(0.5, "lines"),
#       legend.key.size = unit(5, "mm"),
#       panel.spacing = unit(1, "lines"),
#       legend.key.width = unit(8, "mm"),
#       legend.key.height = unit(8, "mm")
#     )
#   
#   plots_list[[element]] <- current_plot
# }
# 
# 
# selected_elements <- c("kcal_feas", "CH4", "TotalN", "CalcWFblue")
# selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])
# 
# combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")
# 
# legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))
# 
# # Create a dummy plot for the y-axis label
# # y_axis_label <- ggplot() +
# #   annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
# #            angle = 90, family = "Arial", size = 6, fontface = "bold") +
# #   theme_void()
# 
# # Combine the y-axis label, combined plots, and legend
# final_plot <- plot_grid(
#   plot_grid(combined_plot, ncol = 2),
#   legend, ncol = 1, rel_heights = c(1, 0.5)
# )
# 
# filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_MULTIPLEPLOT.tiff")
# 
# tiff(
#   filename = here(figure_directory, filename),
#   units = "in", height = 10, width = 16, res = 600
# )
# print(final_plot)
# dev.off()













# LAND FIGURE (Cropland, Pasture, Other Land, Forest change)
for (element in elements) {
  # Create the plot
  current_plot <- bra %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) +
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange")
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()




# AFOLU GHG FIGURE (CO2, CH4, N2O)
for (element in elements) {
  # Create the plot
  current_plot <- bra %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("CO2", "CH4", "N2O", "GHG") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in Mt CO2e per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void() 

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_4GHG.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()






# KCAL FIGURE (kcal_plant, kcal_anim)
for (element in elements) {
  # Create the plot
  current_plot <- bra %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(bra, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(bra, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("kcal_plant", "kcal_anim") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in kcal per capita per day compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_kcal_anim_plant.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()

rm(list = ls()) 



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
db_manure <- db_manure %>% filter(Location == "Colombia")


col_data <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  mutate(Pathway = ifelse(Pathway == "GS_rumdensity", "GS_popactivity", 
                          ifelse(Pathway == "GS_rum", "GS_rumdensity", Pathway))) %>%
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,Nmanure,
         CalcWFblue) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GHG = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)



col_data$Pathway[col_data$Pathway == "NationalCommitments"] <- "NC_complete"
col_data$Pathway[col_data$Pathway == "GlobalSustainability"] <- "GS_complete"

col_data$Pathway[col_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
col_data$Pathway[col_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"


# Commodities -----------------------------
col_comm <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Year %in% c("2030", "2050"))%>% 
  select(Location, Pathway, Year, Product, kcalfeasprod)

col_comm$Pathway[col_comm$Pathway == "NationalCommitments"] <- "NC_complete"
col_comm$Pathway[col_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

col_comm$Pathway[col_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
col_comm$Pathway[col_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

col_kcal <- col_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


col_kcal_final <- col_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

#Final Database ---------------------------------------
col_data <- left_join(col_data, col_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 


col <- col_data %>%
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
    diff_GHG = ifelse(Pathway != "Current Trend_Yes", GHG - first(GHG[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


col$scenarios <- substring(col_data$Pathway, 4)




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
elements <- c("CH4","CO2", "N2O","GHG",
              "kcal_feas","kcal_anim", "kcal_plant",
              "ForestChange",  "Cropland_change", "Pasture_change", "OtherLand_change",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)

#Reordering pathwyas + erasing CT
col$Pathway_code <- factor(col$Pathway_code, levels = c("NC", "GS"))
col <- col[complete.cases(col$Pathway_code), ]

col<- col %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>% 
  mutate(Year = as.factor(Year))


#Plot -------------------------------------------------------------------

# folder to store the plots
figure_directory <- here("output", "decomposition", "COL_with_trade", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

#Loop
plots_list <- list()

for (element in elements) {
  # Create the plot
  current_plot <- col %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(col, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 4, byrow = T))+
    geom_point(data = filter(col, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    # geom_point(data = filter(col, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
    #            aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
    #            size = 3, shape = 16, alpha =0.7) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(col$Year[!is.na(col[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      strip.text = element_text(size = 28, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      legend.text = element_text(family = "Arial", size = 25),
      plot.title = element_text(color = "black", size = 26, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 23),
      axis.text.x = element_text(color = "black", size = 20),
      axis.text.y = element_text(color = "black", size = 20),
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
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_" ,element, ".tiff")
  
  
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 13, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

# plots_list



# # MULTIPLE FIGURE ("CalcFarmLabourFTE", "CalcWFblue")
# for (element in elements) {
#   # Create the plot
#   current_plot <- col %>%
#     group_by(Pathway_code) %>%
#     ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
#     geom_bar(stat = "identity", data = filter(col, !str_detect(Pathway, "complete")),
#              aes(fill = scenarios), colour = "white", size = 0.2) +
#     geom_hline(yintercept = 0, linetype = "solid") +
#     guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T)) +
#     geom_point(data = filter(col, Pathway %in% c("NC_complete", "GS_complete")),
#                aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
#                size = 3, shape = 16) +
#     scale_color_manual(values = c("black"), name = "",
#                        labels = c("All scenarios combined")) +
#     labs(
#       x = "",
#       y = paste("Difference in", units_labels[element], "\ncompared to Current Trends")
#     ) +
#     facet_grid(. ~ Pathway_code, scales = "free_y",
#                labeller = labeller(Pathway_code = c(
#                  "NC" = "NC",
#                  "GS" = "GS"
#                ))) +
#     scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
#     theme_minimal() +
#     theme(
#       text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
#       legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
#       legend.text = element_text(family = "Arial", size = 13),
#       axis.title.x = element_text(color = "black", size = 12),
#       axis.title.y = element_text(color = "black", size = 12),
#       legend.position = "none",
#       legend.direction = "horizontal",
#       legend.box = "vertical",
#       legend.box.spacing = unit(0.5, 'mm'),
#       legend.spacing.x = unit(3, 'mm'),
#       legend.spacing.y = unit(3, 'mm'),
#       legend.box.margin = unit(0.5, "lines"),
#       legend.key.size = unit(5, "mm"),
#       panel.spacing = unit(1, "lines"),
#       legend.key.width = unit(8, "mm"),
#       legend.key.height = unit(8, "mm")
#     )
#   
#   plots_list[[element]] <- current_plot
# }
# 
# 
# selected_elements <- c("kcal_feas", "CH4","CalcFarmLabourFTE", "CalcWFblue")
# selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])
# 
# combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")
# 
# legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))
# 
# # Create a dummy plot for the y-axis label
# # y_axis_label <- ggplot() +
# #   annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
# #            angle = 90, family = "Arial", size = 6, fontface = "bold") +
# #   theme_void()
# 
# # Combine the y-axis label, combined plots, and legend
# final_plot <- plot_grid(
#   plot_grid(combined_plot, ncol = 2),
#   legend, ncol = 1, rel_heights = c(1, 0.5)
# )
# 
# filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_MULTIPLEPLOT.tiff")
# 
# tiff(
#   filename = here(figure_directory, filename),
#   units = "in", height = 10, width = 16, res = 600
# )
# print(final_plot)
# dev.off()






# LAND FIGURE (Cropland, Pasture, Other Land, Forest change)
for (element in elements) {
  # Create the plot
  current_plot <- col %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(col, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 4, byrow = T))+
    geom_point(data = filter(col, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) +
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 18, face = "bold"),
      legend.text = element_text(family = "Arial", size = 20),
      plot.title = element_text(color = "black", size = 24, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 16),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange")
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 7.5, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 13, width = 16, res = 600
)
print(final_plot)
dev.off()



# AFOLU GHG FIGURE (CO2, CH4, N2O)
for (element in elements) {
  # Create the plot
  current_plot <- col %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(col, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 4, byrow = T))+
    geom_point(data = filter(col, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 18, face = "bold"),
      legend.text = element_text(family = "Arial", size = 20),
      plot.title = element_text(color = "black", size = 24, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 16),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("CO2", "CH4", "N2O", "GHG") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in Mt CO2e per year compared to Current Trends",
           angle = 90, family = "Arial", size = 7.5, fontface = "bold") +
  theme_void() 

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_4GHG.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 13, width = 16, res = 600
)
print(final_plot)
dev.off()






# KCAL FIGURE (kcal_plant, kcal_anim)
for (element in elements) {
  # Create the plot
  current_plot <- col %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(col, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 4, byrow = T))+
    geom_point(data = filter(col, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 18, face = "bold"),
      legend.text = element_text(family = "Arial", size = 20),
      plot.title = element_text(color = "black", size = 24, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 16),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("kcal_plant", "kcal_anim") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in kcal per capita per day compared to Current Trends",
           angle = 90, family = "Arial", size = 7, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_kcal_anim_plant.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 16, res = 600
)
print(final_plot)
dev.off()

rm(list = ls()) 



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
db_manure <- db_manure %>% filter(Location == "Ethiopia")


eth_data <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,Nmanure,
         CalcWFblue) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GHG = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)

eth_data$Pathway[eth_data$Pathway == "NationalCommitments"] <- "NC_complete"
eth_data$Pathway[eth_data$Pathway == "GlobalSustainability"] <- "GS_complete"

eth_data$Pathway[eth_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
eth_data$Pathway[eth_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

# Commodities -----------------------------
eth_comm <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Commodities") %>%
  rename(Pathway = `Current Trend`) %>%
  filter(Year %in% c("2030", "2050"))%>%
  select(Location, Pathway, Year, Product, kcalfeasprod) %>%
  unique()



eth_comm$Pathway[eth_comm$Pathway == "NationalCommitments"] <- "NC_complete"
eth_comm$Pathway[eth_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

eth_comm$Pathway[eth_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
eth_comm$Pathway[eth_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

eth_kcal <- eth_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


eth_kcal_final <- eth_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

#Final Database ---------------------------------------
eth_data <- left_join(eth_data, eth_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 





eth <- eth_data %>%
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
    diff_GHG = ifelse(Pathway != "Current Trend_Yes", GHG - first(GHG[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


eth$scenarios <- substring(eth_data$Pathway, 4)



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
elements <- c("CH4","CO2", "N2O","GHG", 
              "kcal_feas","kcal_anim", "kcal_plant",
              "ForestChange",  "Cropland_change", "Pasture_change", "OtherLand_change",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)


#Reordering pathwyas + erasing CT
eth$Pathway_code <- factor(eth$Pathway_code, levels = c("NC", "GS"))
eth <- eth[complete.cases(eth$Pathway_code), ]

eth<- eth %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>%
  mutate(Year = as.factor(Year))


#Plot -------------------------------------------------------------------

# folder to store the plots
figure_directory <- here("output", "decomposition", "ETH_with_trade", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

#Loop
plots_list <- list()

for (element in elements) {
  # Create the plot
  current_plot <- eth %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(eth, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(eth, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    # geom_point(data = filter(eth, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
    #            aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
    #            size = 3, shape = 16, alpha =0.7) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(eth$Year[!is.na(eth[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      strip.text = element_text(size = 28, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      legend.text = element_text(family = "Arial", size = 25),
      plot.title = element_text(color = "black", size = 26, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 23),
      axis.text.x = element_text(color = "black", size = 20),
      axis.text.y = element_text(color = "black", size = 20),
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
  
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_" ,element, ".tiff")
  
  
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 13, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

# plots_list




# LAND FIGURE (Cropland, Pasture, Other Land, Forest change)
for (element in elements) {
  # Create the plot
  current_plot <- eth %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(eth, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(eth, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) +
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange")
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()




# AFOLU GHG FIGURE (CO2, CH4, N2O)
for (element in elements) {
  # Create the plot
  current_plot <- eth %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(eth, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(eth, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("CO2", "CH4", "N2O", "GHG") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in Mt CO2e per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void() 

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_4GHG.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()






# KCAL FIGURE (kcal_plant, kcal_anim)
for (element in elements) {
  # Create the plot
  current_plot <- eth %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(eth, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(eth, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("kcal_plant", "kcal_anim") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in kcal per capita per day compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_kcal_anim_plant.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()



rm(list = ls()) 



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
db_manure <- db_manure %>% filter(Location == "UK")


gbr_data <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,Nmanure,
         CalcWFblue) %>% 
  mutate(Cropland_change = CalcCropland - lag(CalcCropland)) %>% 
  mutate(Pasture_change = CalcPasture - lag(CalcPasture)) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = CalcOtherLand - lag(CalcOtherLand)) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GHG = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_synth + Nmanure)



gbr_data$Pathway[gbr_data$Pathway == "NationalCommitments"] <- "NC_complete"
gbr_data$Pathway[gbr_data$Pathway == "GlobalSustainability"] <- "GS_complete"

gbr_data$Pathway[gbr_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
gbr_data$Pathway[gbr_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"



gbr_comm <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Year %in% c("2030", "2050"))%>% 
  select(Location, Pathway, Year, Product, kcalfeasprod)

gbr_comm$Pathway[gbr_comm$Pathway == "NationalCommitments"] <- "NC_complete"
gbr_comm$Pathway[gbr_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

gbr_comm$Pathway[gbr_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
gbr_comm$Pathway[gbr_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"


mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

gbr_kcal <- gbr_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


gbr_kcal_final <- gbr_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

gbr_data <- left_join(gbr_data, gbr_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 


gbr <- gbr_data %>%
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
    diff_GHG = ifelse(Pathway != "Current Trend_Yes", GHG - first(GHG[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


gbr$scenarios <- substring(gbr_data$Pathway, 4)




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
elements <- c("CH4","CO2", "N2O", "GHG",
              "kcal_feas","kcal_anim", "kcal_plant"
              
              ,"ForestChange",  "Cropland_change", "Pasture_change", "OtherLand_change",
              "CalcFarmLabourFTE", "LNPPMatureForest", "LNPPMatureOtherLand",
              "TotalN", "CalcWFblue"
)

#Reordering pathwyas + erasing CT
gbr$Pathway_code <- factor(gbr$Pathway_code, levels = c("NC", "GS"))
gbr <- gbr[complete.cases(gbr$Pathway_code), ]

gbr<- gbr %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland")) %>%
  mutate(Year = as.factor(Year))


#Plot -------------------------------------------------------------------

# folder to store the plots
figure_directory <- here("output", "decomposition", "GBR_with_trade", paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d"))))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

#Loop
plots_list <- list()

for (element in elements) {
  # Create the plot
  current_plot <- gbr %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(gbr, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(gbr, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    # geom_point(data = filter(gbr, Pathway %in% c("NC_tradeeffect", "GS_tradeeffect")),
    #            aes(y = !!sym(paste0("diff_", element)), x = Year, color = "NC/GS Trade adjustment effect on CT"),
    #            size = 3, shape = 16, alpha =0.7) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "Year",
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "National Commitments",
                 "GS" = "Global Sustainability"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    scale_x_discrete(breaks = unique(gbr$Year[!is.na(gbr[, paste0("diff_", element)])])) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      strip.text = element_text(size = 28, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 26, face = "bold"),
      legend.text = element_text(family = "Arial", size = 25),
      plot.title = element_text(color = "black", size = 26, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 23),
      axis.text.x = element_text(color = "black", size = 20),
      axis.text.y = element_text(color = "black", size = 20),
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
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_" ,element, ".tiff")
  
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 13, width = 18, res = 600
  )
  print(current_plot)
  dev.off()
  
  # Append the current plot to the list
  plots_list[[element]] <- current_plot
}

plots_list






# LAND FIGURE (Cropland, Pasture, Other Land, Forest change)
for (element in elements) {
  # Create the plot
  current_plot <- gbr %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(gbr, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(gbr, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) +
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange")
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()




# AFOLU GHG FIGURE (CO2, CH4, N2O)
for (element in elements) {
  # Create the plot
  current_plot <- gbr %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(gbr, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(gbr, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 18, face = "bold"),
      legend.text = element_text(family = "Arial", size = 20),
      plot.title = element_text(color = "black", size = 24, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 16),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("CO2", "CH4", "N2O", "GHG") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in Mt CO2e per year compared to Current Trends",
           angle = 90, family = "Arial", size = 7.5, fontface = "bold") +
  theme_void() 

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_4GHG.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 13, width = 16, res = 600
)
print(final_plot)
dev.off()






# KCAL FIGURE (kcal_plant, kcal_anim)
for (element in elements) {
  # Create the plot
  current_plot <- gbr %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = Year, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(gbr, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(gbr, Pathway %in% c("NC_complete", "GS_complete")),
               aes(y = !!sym(paste0("diff_", element)), x = Year, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "NC",
                 "GS" = "GS"
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 20, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 18, face = "bold"),
      legend.text = element_text(family = "Arial", size = 20),
      plot.title = element_text(color = "black", size = 24, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(color = "black", size = 16),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element])
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("kcal_plant", "kcal_anim") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in kcal per capita per day compared to Current Trends",
           angle = 90, family = "Arial", size = 7, fontface = "bold") +
  theme_void()

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_kcal_anim_plant.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 13, width = 16, res = 600
)
print(final_plot)
dev.off()

rm(list = ls()) 

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
  mutate(Cropland_change = (CalcCropland - lag(CalcCropland))/5) %>% 
  mutate(Pasture_change = (CalcPasture - lag(CalcPasture))/5) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = (CalcOtherLand - lag(CalcOtherLand))/5) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GAS = CO2 + CH4 + N2O) %>% 
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
    diff_GAS = ifelse(Pathway != "Current Trend_Yes", GAS - first(GAS[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) %>% 
  filter(Pathway != "GS_live_rumdensity") 



aus$scenarios <- substring(aus_data$Pathway, 4)

#Reordering pathwyas + erasing CT
aus$Pathway_code <- factor(aus$Pathway_code, levels = c("NC", "GS"))
aus <- aus[complete.cases(aus$Pathway_code), ]

aus$ALPHA3 <- "AUS"

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
db_manure <- db_manure %>% filter(Location == "Brazil")

bra_data <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue) %>% 
  mutate(Cropland_change = (CalcCropland - lag(CalcCropland))/5) %>% 
  mutate(Pasture_change = (CalcPasture - lag(CalcPasture))/5) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = (CalcOtherLand - lag(CalcOtherLand))/5) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GAS = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)

bra_data$Pathway[bra_data$Pathway == "NationalCommitments"] <- "NC_complete"
bra_data$Pathway[bra_data$Pathway == "GlobalSustainability"] <- "GS_complete"

bra_data$Pathway[bra_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
bra_data$Pathway[bra_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"




# Commodities -----------------------------
bra_comm <- read_xlsx(here("data", "report_BRA_20240306_10H44.xlsx"), sheet = "Commodities") %>%
  rename(Pathway = `Current Trend`) %>%
  filter(Year %in% c("2030", "2050"))%>%
  select(Location, Pathway, Year, Product, kcalfeasprod) %>%
  unique()



bra_comm$Pathway[bra_comm$Pathway == "NationalCommitments"] <- "NC_complete"
bra_comm$Pathway[bra_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

bra_comm$Pathway[bra_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
bra_comm$Pathway[bra_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

bra_kcal <- bra_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


bra_kcal_final <- bra_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)


#Final Database ---------------------------------------
bra_data <- left_join(bra_data, bra_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 



bra <- bra_data %>%
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
    diff_GAS = ifelse(Pathway != "Current Trend_Yes", GAS - first(GAS[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


bra$scenarios <- substring(bra_data$Pathway, 4)


#Reordering pathwyas + erasing CT
bra$Pathway_code <- factor(bra$Pathway_code, levels = c("NC", "GS"))
bra <- bra[complete.cases(bra$Pathway_code), ]


bra$ALPHA3 <- "BRA"



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
db_manure <- db_manure %>% filter(Location == "Colombia")


col_data <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  mutate(Pathway = ifelse(Pathway == "GS_rumdensity", "GS_popactivity", 
                          ifelse(Pathway == "GS_rum", "GS_rumdensity", Pathway))) %>%
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue) %>% 
  mutate(Cropland_change = (CalcCropland - lag(CalcCropland))/5) %>% 
  mutate(Pasture_change = (CalcPasture - lag(CalcPasture))/5) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = (CalcOtherLand - lag(CalcOtherLand))/5) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GAS = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)



col_data$Pathway[col_data$Pathway == "NationalCommitments"] <- "NC_complete"
col_data$Pathway[col_data$Pathway == "GlobalSustainability"] <- "GS_complete"

col_data$Pathway[col_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
col_data$Pathway[col_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"


# Commodities -----------------------------
col_comm <- read_xlsx(here("data", "report_COL_20240516_9H40.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Year %in% c("2030", "2050"))%>% 
  select(Location, Pathway, Year, Product, kcalfeasprod)

col_comm$Pathway[col_comm$Pathway == "NationalCommitments"] <- "NC_complete"
col_comm$Pathway[col_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

col_comm$Pathway[col_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
col_comm$Pathway[col_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

col_kcal <- col_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


col_kcal_final <- col_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

#Final Database ---------------------------------------
col_data <- left_join(col_data, col_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 


col <- col_data %>%
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
    diff_GAS = ifelse(Pathway != "Current Trend_Yes", GAS - first(GAS[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


col$scenarios <- substring(col_data$Pathway, 4)




#Reordering pathwyas + erasing CT
col$Pathway_code <- factor(col$Pathway_code, levels = c("NC", "GS"))
col <- col[complete.cases(col$Pathway_code), ]

col$ALPHA3 <- "COL"


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
db_manure <- db_manure %>% filter(Location == "Ethiopia")


eth_data <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue) %>% 
  mutate(Cropland_change = (CalcCropland - lag(CalcCropland))/5) %>% 
  mutate(Pasture_change = (CalcPasture - lag(CalcPasture))/5) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = (CalcOtherLand - lag(CalcOtherLand))/5) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GAS = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)

eth_data$Pathway[eth_data$Pathway == "NationalCommitments"] <- "NC_complete"
eth_data$Pathway[eth_data$Pathway == "GlobalSustainability"] <- "GS_complete"

eth_data$Pathway[eth_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
eth_data$Pathway[eth_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

# Commodities -----------------------------
eth_comm <- read_xlsx(here("data", "report_ETH_20240426_12H01.xlsx"), sheet = "Commodities") %>%
  rename(Pathway = `Current Trend`) %>%
  filter(Year %in% c("2030", "2050"))%>%
  select(Location, Pathway, Year, Product, kcalfeasprod) %>%
  unique()



eth_comm$Pathway[eth_comm$Pathway == "NationalCommitments"] <- "NC_complete"
eth_comm$Pathway[eth_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

eth_comm$Pathway[eth_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
eth_comm$Pathway[eth_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

eth_kcal <- eth_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


eth_kcal_final <- eth_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

#Final Database ---------------------------------------
eth_data <- left_join(eth_data, eth_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 





eth <- eth_data %>%
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
    diff_GAS = ifelse(Pathway != "Current Trend_Yes", GAS - first(GAS[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


eth$scenarios <- substring(eth_data$Pathway, 4)


#Reordering pathwyas + erasing CT
eth$Pathway_code <- factor(eth$Pathway_code, levels = c("NC", "GS"))
eth <- eth[complete.cases(eth$Pathway_code), ]

eth$ALPHA3 <- "ETH"

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
db_manure <- db_manure %>% filter(Location == "UK")


gbr_data <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Indicators") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Pathway!= "Current Trend") %>% 
  left_join(db_manure, by = c("Location", "Year", "Pathway")) %>% 
  select(Pathway, Year, kcal_feas, 
         ForestChange, CalcCropland, CalcPasture, CalcOtherLand, NewOtherLand, 
         CalcFarmLabourFTE,
         CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
         kcal_feas, kcal_mder,
         LNPPMatureForest, LNPPMatureOtherLand,
         CalcN_org, CalcN_synth,
         CalcWFblue) %>% 
  mutate(Cropland_change = (CalcCropland - lag(CalcCropland))/5) %>% 
  mutate(Pasture_change = (CalcPasture - lag(CalcPasture))/5) %>% 
  mutate(CalcOtherLand= CalcOtherLand + NewOtherLand) %>% 
  mutate(OtherLand_change = (CalcOtherLand - lag(CalcOtherLand))/5) %>% 
  filter(Year %in% c("2030", "2050")) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O) %>% 
  mutate(GAS = CO2 + CH4 + N2O) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)



gbr_data$Pathway[gbr_data$Pathway == "NationalCommitments"] <- "NC_complete"
gbr_data$Pathway[gbr_data$Pathway == "GlobalSustainability"] <- "GS_complete"

gbr_data$Pathway[gbr_data$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
gbr_data$Pathway[gbr_data$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"



gbr_comm <- read_xlsx(here("data", "report_GBR_20240306_10H43.xlsx"), sheet = "Commodities") %>% 
  rename(Pathway = `Current Trend`) %>% 
  filter(Year %in% c("2030", "2050"))%>% 
  select(Location, Pathway, Year, Product, kcalfeasprod)

gbr_comm$Pathway[gbr_comm$Pathway == "NationalCommitments"] <- "NC_complete"
gbr_comm$Pathway[gbr_comm$Pathway == "GlobalSustainability"] <- "GS_complete"

gbr_comm$Pathway[gbr_comm$Pathway == "Current Trend_Yes_NC_trade"] <- "NC_tradeeffect"
gbr_comm$Pathway[gbr_comm$Pathway == "Current Trend_Yes_GS_trade"] <- "GS_tradeeffect"


mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

gbr_kcal <- gbr_comm %>% 
  left_join (mapping, by ="Product") %>% 
  mutate("Anim_Plant" = ifelse(PROD_GROUP %in% c("ANIMFAT", "EGGS", "FISH", "MILK", "REDMEAT", "PORK", "POULTRY"), "ANIM", "PLANT")) %>%
  distinct(Location, Year, Pathway, Product, .keep_all = TRUE) %>% 
  group_by(Pathway, Location, Year, Anim_Plant) %>% 
  mutate(kcal_anim_plant = sum(kcalfeasprod)) %>% 
  select(-kcalfeasprod, -PROD_GROUP, -Product) %>% 
  unique()


gbr_kcal_final <- gbr_kcal %>%
  pivot_wider(names_from = Anim_Plant, values_from = kcal_anim_plant, values_fn = list(kcal_anim_plant = function(x) x[which.min(!is.na(x))])) %>%
  rename(kcal_anim = ANIM, kcal_plant = PLANT) %>%
  replace(is.na(.), 0)

gbr_data <- left_join(gbr_data, gbr_kcal_final %>% select(Pathway, Year, kcal_plant, kcal_anim), by = c("Pathway", "Year")) %>%
  unique() 


gbr <- gbr_data %>%
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
    diff_GAS = ifelse(Pathway != "Current Trend_Yes", GAS - first(GAS[Pathway == "Current Trend_Yes"]), NA),
    diff_kcal_mder = ifelse(Pathway != "Current Trend_Yes", kcal_mder - first(kcal_mder[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureForest = ifelse(Pathway != "Current Trend_Yes", LNPPMatureForest - first(LNPPMatureForest[Pathway == "Current Trend_Yes"]), NA),
    diff_LNPPMatureOtherLand = ifelse(Pathway != "Current Trend_Yes", LNPPMatureOtherLand - first(LNPPMatureOtherLand[Pathway == "Current Trend_Yes"]), NA),
    diff_TotalN = ifelse(Pathway != "Current Trend_Yes", TotalN - first(TotalN[Pathway == "Current Trend_Yes"]), NA),
    diff_CalcWFblue = ifelse(Pathway != "Current Trend_Yes", CalcWFblue - first(CalcWFblue[Pathway == "Current Trend_Yes"]), NA)
  ) 


gbr$scenarios <- substring(gbr_data$Pathway, 4)




#Labelling -------------------------------------------------------------------


#Reordering pathwyas + erasing CT
gbr$Pathway_code <- factor(gbr$Pathway_code, levels = c("NC", "GS"))
gbr <- gbr[complete.cases(gbr$Pathway_code), ]
gbr$ALPHA3 <- "UK"




all <- bind_rows(aus, bra, col, eth, gbr) %>% 
  filter(Year == "2050") %>% 
  filter(str_starts(Pathway, "GS_")) %>% 
  filter(!Pathway %in% c("GS_agroforestry", "GS_peatland", "GS_popactivity", "GS_grassland",
                         "NC_agroforestry", "NC_peatland", "NC_popactivity", "NC_grassland" ))  





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
elements <- c("GAS",
              "kcal_feas",
              "ForestChange", "Cropland_change", "Pasture_change", "OtherLand_change",
              "TotalN"
)



# Create the directory for saving plotss
figure_directory <- here("output", "decomposition", "all_country_with_trade", gsub("-", "", format(Sys.Date(),format = "%y%m%d")))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

# Loop to create and save plots
plots_list <- list()

for (element in elements) {
  
  current_plot <- all %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = ALPHA3, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(all, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 4, byrow = T))+
    geom_point(data = filter(all, Pathway == "GS_complete"),
               aes(y = !!sym(paste0("diff_", element)), x = ALPHA3, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = paste("Difference in", units_labels[element], "compared to Current Trends")
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "",
                 "GS" = ""
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.text = element_text(family = "Arial", size = 18),
      plot.title = element_text(color = "black", size = 20, face = "bold"), 
      axis.title.x = element_text(color = "black", size = 18),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(1, 'mm'),
      legend.spacing.y = unit(0.5, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(6, "mm"),
      legend.key.height = unit(6, "mm")
    )
  
  filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_" ,element, "_all_country2050.tiff")
  
  tiff(
    filename = here(figure_directory, filename),
    units = "in", height = 10, width = 16, res = 600
  )
  print(current_plot)
  dev.off()
  
  plots_list[[element]] <- current_plot
}

plots_list










# Create the directory for saving plots
figure_directory <- here("output", "decomposition", "all_country_with_trade", gsub("-", "", format(Sys.Date(),format = "%y%m%d")))
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

plots_list <- list()
legends <- list()


for (element in elements) {
  # Create the plot
  current_plot <- all %>%
    group_by(Pathway_code) %>%
    ggplot(aes(x = ALPHA3, y = !!sym(paste0("diff_", element)))) +
    geom_bar(stat = "identity", data = filter(all, !str_detect(Pathway, "complete")),
             aes(fill = scenarios), colour = "white", size = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid") +
    guides(fill = guide_legend(override.aes = list(shape = NA), nrow = 3, byrow = T))+
    geom_point(data = filter(all, Pathway == "GS_complete"),
               aes(y = !!sym(paste0("diff_", element)), x = ALPHA3, color = "All scenarios combined"),
               size = 3, shape = 16) + 
    scale_color_manual(values = c("black"), name = "",
                       labels = c("All scenarios combined")) +
    labs(
      x = "",
      y = ""  
    ) +
    facet_grid(. ~ Pathway_code, scales = "free_y",
               labeller = labeller(Pathway_code = c(
                 "NC" = "",
                 "GS" = ""
               ))) +
    scale_fill_manual(values = pathway_colors[pathway_colors != "complete"], name = "", labels = pathway_labels[pathway_labels != "complete"]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "black", size = 16, face = "bold"),
      legend.title = element_text(family = "Arial", color = "black", size = 14, face = "bold"),
      legend.text = element_text(family = "Arial", size = 13),
      plot.title = element_text(color = "black", size = 14, face = "bold"),
      axis.title.x = element_text(color = "black", size = 12),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.box= "vertical",
      legend.box.spacing = unit(0.5, 'mm'),
      legend.spacing.x = unit(3, 'mm'),
      legend.spacing.y = unit(3, 'mm'),
      legend.box.margin = unit(0.5, "lines"),
      legend.key.size = unit(5, "mm"),  
      panel.spacing = unit(1, "lines"),
      legend.key.width = unit(8, "mm"),
      legend.key.height = unit(8, "mm")
    ) +
    labs(title = element_labels[element]) 
  
  plots_list[[element]] <- current_plot
}

selected_elements <- c("Cropland_change", "Pasture_change", "OtherLand_change", "ForestChange") 
selected_plots <- lapply(selected_elements, function(element) plots_list[[element]])

combined_plot <- plot_grid(plotlist = selected_plots, ncol = 2, align = "v")

legend <- cowplot::get_legend(plots_list[[selected_elements[1]]] + theme(legend.position = "right"))

# Create a dummy plot for the y-axis label
y_axis_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Difference in 1000ha per year compared to Current Trends",
           angle = 90, family = "Arial", size = 6, fontface = "bold") +
  theme_void()  # Remove all other plot elements

# Combine the y-axis label, combined plots, and legend
final_plot <- plot_grid(
  plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1)),
  legend, ncol = 1, rel_heights = c(1, 0.3)
)

filename <- paste0(gsub("-", "", format(Sys.Date(),format = "%y%m%d")), "_Landchange_all_country2050.tiff")

tiff(
  filename = here(figure_directory, filename),
  units = "in", height = 10, width = 16, res = 600
)
print(final_plot)
dev.off()










