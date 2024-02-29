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
scenathon<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, CalcCropN2O, CalcCropCH4, CalcCropCO2, CalcLiveN2O, CalcLiveCH4, CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2)

#GHG Aggregation -----------------------------------------------------
GHG_final <- scenathon %>% 
  group_by(alpha3, pathway, Year) %>% 
  mutate(CO2 = CalcCropCO2 + CalcDeforCO2 + CalcOtherLUCCO2 + CalcSequestCO2) %>% 
  mutate(CH4 = CalcCropCH4 + CalcLiveCH4) %>% 
  mutate(N2O = CalcCropN2O + CalcLiveN2O)



# List of countries
countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")
GHG_final$pathway <- factor(GHG_final$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))


# #First Option ---------------------------------------------------------
# p <- ggplot(GHG_final, aes(x = Year)) +
#   geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
#   geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
#   geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   ggtitle("GHG Emissions by Gas") +
#   scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
#                      name = "Gas Type") +  
#   labs(title = "Ethiopia: GHG Emissions by Gas",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = " GHG Emissions (Mt CO2e)") +
#   scale_x_continuous(breaks = c(2020, 2030, 2050)) +
#   scale_y_continuous(breaks = seq(floor(-max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/1000)*1000,
#                                   ceiling(max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/1000)*1000,
#                                   50)) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12))
# 
# p
# 


#Second Option ----------------------------------------------------------



# Plot
# p_pathway <- ggplot(GHG_final, aes(x = Year)) +
#   geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
#   geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
#   geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   ggtitle("GHG Emissions by Gas") +
#   scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
#                      name = "Gas Type") +  
#   labs(title = "UK: GHG Emissions by Gas",
#        x = "Year",
#        y = " GHG Emissions (Mt CO2e)") +
#   facet_wrap(~ pathway, scales = "free_y", ncol = 1,
#              labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
#                                              "NationalCommitments" = "National Commitments Pathway",
#                                              "GlobalSustainability" = "Global Sustainability Pathway"))) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12)) +
#   scale_x_continuous(breaks = c(2020, 2030, 2050)) +
#   scale_y_continuous(breaks = seq(floor(-max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/10)*10,
#                                   ceiling(max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/10)*10,
#                                   10))
# 
# 
# 
# print(p_pathway)


# p_pathway3 <- ggplot(GHG_final, aes(x = Year)) +
#   geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
#   geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
#   geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   ggtitle("GHG Emissions by Gas") +
#   scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
#                      name = "Gas Type") +  
#   labs(title = "UK: GHG Emissions by Gas",
#        x = "Year",
#        y = " GHG Emissions (Mt CO2e)") +
#   facet_grid(. ~ pathway, scales = "free_y",
#              labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
#                                              "NationalCommitments" = "National Commitments Pathway",
#                                              "GlobalSustainability" = "Global Sustainability Pathway"))) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12)) +
#   scale_x_continuous(breaks = c(2020, 2030, 2050)) +
#   scale_y_continuous(breaks = seq(floor(-max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/10)*10,
#                                   ceiling(max(abs(GHG_final$CO2), abs(GHG_final$CH4), abs(GHG_final$N2O))/10)*10,
#                                   10))
# 
# 
# 
# p_pathway3
# 
# 
# 
# 
# tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "GHG_pathway.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_pathway)
# dev.off()
# 
# 
# tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "GHG_pathway3.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_pathway3)
# dev.off()
# 
# ----------------------------------------------------------



for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(GHG_final, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway3 <- ggplot(country_data, aes(x = Year)) +
    geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
    geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
    geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle("GHG Emissions by Gas") +
    scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
                       name = "Gas Type") +  
    labs(
      title = paste(countries_labels[country], ": GHG Emissions by Gas"),
      x = "Year",
      y = " GHG Emissions (Mt CO2e)"
    ) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
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
    scale_x_continuous(breaks = c(2020, 2030, 2050)) +
    scale_y_continuous(breaks = seq(floor(-max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
                                    ceiling(max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
                                    10))
  
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "GHG_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway3)
  dev.off()
}


