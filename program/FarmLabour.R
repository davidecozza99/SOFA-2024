# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
library(scales)
conflict_prefer("filter", "dplyr")

here()

#Data ---------------------------------------------------------------
FTE<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, CalcFarmLabourFTE)


#Pathway selection ---------------------------------------------------------------
# 
# FTE <- scenathon %>% 
#   filter(alpha3 == country)
# 



#Plot Pathway ---------------------------------------------------------------


# p_pathway <- ggplot(FTE, aes(x = as.factor(Year))) +
#   geom_bar(aes(y = CalcFarmLabourFTE, fill = "Total FTE's"), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   scale_fill_manual(values = c("Total FTE's" = "forestgreen"),
#                     name = "") +
#   labs(title = "UK: Farm Labour FTE",
#        x = "Year") +
#   scale_y_continuous(breaks = seq(floor(-max(abs(FTE$CalcFarmLabourFTE))/5)*5,
#                                   ceiling(max(abs(FTE$CalcFarmLabourFTE))/5)*5,
#                                   5),
#                      labels = scales::comma_format()) + 
#   facet_grid(. ~ pathway, scales = "free_y",
#              labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
#                                              "NationalCommitments" = "National Commitments Pathway",
#                                              "GlobalSustainability" = "Global Sustainability Pathway"))) +
#   
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_blank())  # Remove y-axis title
# 
# print(p_pathway)




#Plot Pathway ---------------------------------------------------------------
countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")

FTE$pathway <- factor(FTE$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(FTE, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_bar(aes(y = CalcFarmLabourFTE, fill = "Total FTE's"), stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_fill_manual(values = c("Total FTE's" = "forestgreen"),
                      name = "") +
    labs(
      title = paste(countries_labels[country], ": Farm Labour FTE"),
      x = "Year",
      y = "Farm Labour FTE",
      fill = ""
    ) +
    scale_y_continuous(breaks = seq(floor(-max(abs(country_data$CalcFarmLabourFTE))/5)*5,
                                    ceiling(max(abs(country_data$CalcFarmLabourFTE))/5)*5,
                                    5),
                       labels = scales::comma_format()) + 
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
      axis.title.y = element_blank()  # Remove y-axis title
    )
  
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "FarmLabour_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway)
  dev.off()
}
