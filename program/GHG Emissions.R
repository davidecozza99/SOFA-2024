# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
library(hrbrthemes)
conflict_prefer("filter", "dplyr")

here()

#Data ---------------------------------------------------------------
scenathon<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>%
  filter(Year %in% c("2020", "2030", "2040", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, calccropn2o, calccropch4, calccropco2, calcliven2o, calclivech4, calcdeforco2, calcotherlucco2, calcsequestco2)

#GHG Aggregation -----------------------------------------------------
GHG_final <- scenathon %>% 
  group_by(alpha3, pathway, Year) %>% 
  mutate(CO2 = calccropco2 + calcdeforco2 + calcotherlucco2 + calcsequestco2) %>% 
  mutate(CH4 = calccropch4 + calclivech4) %>% 
  mutate(N2O = calccropn2o + calcliven2o)


# List of countries
GHG_final$pathway <- factor(GHG_final$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(GHG_final, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway3 <- ggplot(country_data, aes(x = Year)) +
    geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
    geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
    geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
                       name = "") +  
    labs(
      title = paste(countries_labels[country], ": AFOLU GHG Emissions by Gas"),
      x = "Year",
      y = "Mt CO2e"
    ) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trend",
                                               "NationalCommitments" = "National Commitments Pathway",
                                               "GlobalSustainability" = "Global Sustainability Pathway"))) +
    theme_minimal() +
    theme(
      text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
      legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
      legend.text = element_text(family = "Courier New", size = 12),
      plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
      axis.title.x = element_text(color = "steelblue", size = 12),
      axis.title.y = element_text(color = "steelblue", size = 12)
    ) +
    scale_x_continuous(breaks = c(2020, 2030,2040, 2050)) 
    # scale_y_continuous(breaks = seq(floor(-max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
    #                                 ceiling(max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
    #                                 10))
    # 
  # # Save the plot as a TIFF file
  # tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "GHG_pathway.tiff")),
  #      units = "in", height = 5, width = 14, res = 300)
  # print(p_pathway3)
  # dev.off()
}
p_pathway3

























#################### GRAPH FOR SDR Report #################
##### RE update with the 240504 database
##### Put ex-post lines between the gas type

# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(gridExtra)


conflict_prefer("filter", "dplyr")

here()

#Data extraction ---------------------------------------------------------------
scenathon<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>%
  select(alpha3, pathway, Year, calccropn2o, calccropch4, calccropco2, calcliven2o, calclivech4, 
         ghgbiofuels,
         calcdeforco2, calcotherlucco2, 
         calcsequestco3,
         calcsequestaband, calcsequestaffor) %>% 
  group_by(Year, pathway) %>% 
  mutate(N2O_crop = sum(calccropn2o)) %>% 
  mutate(CH4_crop = sum(calccropch4)) %>% 
  mutate(CO2_crop = sum(calccropco2)) %>% 
  mutate(N2O_live = sum(calcliven2o)) %>% 
  mutate(CH4_live = sum(calclivech4)) %>% 
  mutate(CO2_def = sum(calcdeforco2)) %>% 
  mutate(CO2_other = sum(calcotherlucco2)) %>%
  mutate(calcsequestco3 = as.numeric(calcsequestco3)) %>%
  mutate(CO2_peat = sum(calcsequestco3, na.rm = TRUE)) %>% 
  mutate(CO2_aband = sum(calcsequestaband)) %>% 
  mutate(CO2_affor = sum(calcsequestaffor)) %>% 
  mutate(CO2_bio = sum(ghgbiofuels)) %>% 
  select(-alpha3, -calccropn2o, -calccropch4, -calccropco2, -calcliven2o, -calclivech4, -calcdeforco2, -calcotherlucco2, - ghgbiofuels, 
         -calcsequestco3,
         -calcsequestaband, -calcsequestaffor) %>% 
  distinct() 

#Data manipulation -------------------------------------------------------------
df_long <- melt(scenathon, id.vars = c("Year", "pathway"), variable.name = "Gas", value.name = "Emission") 
df_long$Gas <- as.character(df_long$Gas)
df_long <- df_long %>% 
  mutate(Gas_type = case_when(
    startsWith(Gas, "CO2") ~ "CO2",
    startsWith(Gas, "CH4") ~ "CH4",
    startsWith(Gas, "N2O") ~ "N2O",
    TRUE ~ NA_character_
  )) 


# df_long2 <- melt(scenathon, id.vars = c("Year", "pathway"), variable.name = "Gas", value.name = "Emission") 
# df_long2$Gas <- as.character(df_long2$Gas)


# df_long2 <- df_long2 %>% 
#   mutate(Gas_type = case_when(
#     startsWith(Gas, "CO2") ~ "CO2",
#     startsWith(Gas, "CH4") ~ "CH4",
#     startsWith(Gas, "N2O") ~ "N2O",
#     TRUE ~ NA_character_
#   )) %>% 
#   group_by(Year, pathway, Gas_type) %>% 
#   summarize(Emission_sum = sum(Emission, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = Gas_type, values_from = Emission_sum)
# 
# df_long_final <- left_join(df_long, df_long2) %>% 
#   select(-Gas_type)




#Preparing aesthetic for the plot ----------------------------------------------

gas_colors <- c("N2O_crop" = "#F7C44E", "N2O_live" = "#B65356",
                "CH4_crop" = "#F7C44E", "CH4_live" = "#B65356",
                "CO2_crop" = "#F7C44E", "CO2_def" = "#0CA48D",
                "CO2_other" ="#D5E59E","CO2_peat" = "#004D40",
                "CO2_aband" = "#66BB6A",
                "CO2_affor" = "#1B5E20",
                "CO2_bio"= "grey"
)


gas_labels <- c(
#"N2O_crop" = "N2O from Crop", "N2O_live" = "N2O from Livestock",
#"CH4_crop" = "CH4 from Crop", "CH4_live" = "CH4 from Livestock",
  "CO2_crop" = "Crop", "CH4_live" = "Livestock", "CO2_def" = "Deforestation", "CO2_other" ="Other Land Use", 
  "CO2_peat" = "Peatland",
  "CO2_aband" = "Abandoned Agr. Land", "CO2_affor" = "Afforestation", 
  "CO2_bio" ="Savings from Biofuels")

# 
# gas_colors <- c(
#   "N2O_crop" = "steelblue", 
#   "N2O_live" = "lightblue",
#   "CH4_crop" = "#7B1FA2", 
#   "CH4_live" = "#BA68C8",
#   "CO2_crop" = "#66BB6A",   
#   "CO2_def" = "#4CAF50",    
#   "CO2_other" = "#388E3C",   
#   "CO2_peat" = "#2E7D32",    
#   "CO2_aband" = "#1B5E20",   
#   "CO2_affor" = "#004D40",
#   "CO2_bio"= "#003300"        
# )
# 
# 
# gas_labels <- c(
#            "N2O_crop" = "N2O from Crop", "N2O_live" = "N2O from Livestock \n",
#            "CH4_crop" = "CH4 from Crop", "CH4_live" = "CH4 from Livestock \n",
#            "CO2_crop" = "CO2 from Crop",  "CO2_def" = "CO2 from Deforestation", 
#            "CO2_other" ="CO2 from Other Land Use", "CO2_peat" = "CO2 from Peatland",
#            "CO2_aband" = "CO2 from Abandoned Agr. Land", "CO2_affor" = "CO2 from Afforestation", 
#            "CO2_bio" ="CO2 savings from Biofuels"
#   )

df_long$Gas <- factor(df_long$Gas, levels = c(
  "N2O_crop", "N2O_live",
  "CH4_crop", "CH4_live",
  "CO2_crop", "CO2_def", 
  "CO2_other", "CO2_peat",
  "CO2_aband",   
  "CO2_affor",
  "CO2_bio"   
))


#Plot --------------------------------------------------------------------------

unique_pathways <- unique(df_long$pathway)
df_long$pathway <- factor(df_long$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


plot_list <- list()

for (current_pathway in unique_pathways) {


  p <- ggplot(df_long, aes(x = Year, y = Emission, fill = Gas)) +
    geom_area() +
    labs(title = paste("AFOLU GHG emissions"), x = "Year", y = "MtCO2eq", fill = "") +
    scale_fill_manual(values = gas_colors, labels = gas_labels, breaks = names(gas_labels)) +
    scale_y_continuous(breaks = c(-2500, 0, 5000, 10000)) +
  
    # geom_line(aes(y = CO2, color = "CO2"), size = 2) +
    # geom_line(aes(y = CH4, color = "CH4"), size = 2) +
    # geom_line(aes(y = N2O, color = "N2O"), size = 2) +
    # scale_color_manual(values = c("blue", "green", "red"),
    #                    labels = c("CH4", "CO2", "N2O"),
    #                    name = "Gas Type") +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trend",
                                               "NationalCommitments" = "National Commitments Pathway",
                                               "GlobalSustainability" = "Global Sustainability Pathway"))) +
   theme_minimal() +
    theme(
      text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
      legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),
      legend.text = element_text(family = "Courier New", size = 12),
      plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
      axis.title.x = element_text(color = "steelblue", size = 12),
      axis.title.y = element_text(color = "steelblue", size = 12)
    )
    plot_list[[current_pathway]] <- p
    # # Save the plot as a JPEG file
    # jpeg(here("output", "figures", paste0(gsub("-", "", Sys.Date()), "_", "GHG_SDR_Report_clara.jpeg")),
    #      units = "in", height = 5, width = 14, res = 300)
    # print(p)
    # dev.off()
    
}

p








 


