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

country="GBR"
#Data ---------------------------------------------------------------
scenathon<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, CalcFarmLabourFTE)


#Pathway selection ---------------------------------------------------------------
# 
# FTE <- scenathon %>% 
#   filter(alpha3 == country)
# 


#Plot ---------------------------------------------------------------
# p <- ggplot(FTE, aes(x = as.factor(Year))) +
#   geom_bar(aes(y = CalcFarmLabourFTE, fill = "Farm Labour FTE"), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   scale_fill_manual(values = c("Farm Labour FTE" = "darkgreen"),
#                     name = "Legend") +
#   labs(title = "Ethiopia: Farm Labour FTE",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = "FTE's") +
#   scale_y_continuous(breaks = seq(floor(-max(abs(FTE$CalcFarmLabourFTE))/10000)*10000,
#                                   ceiling(max(abs(FTE$CalcFarmLabourFTE))/10000)*10000,
#                                   10000)) +
#   
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12))
# 
# print(p)





#Plot Pathway ---------------------------------------------------------------
FTE <- scenathon %>% 
  filter(alpha3 == country)
FTE$pathway <- factor(FTE$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))


p_pathway <- ggplot(FTE, aes(x = as.factor(Year))) +
  geom_bar(aes(y = CalcFarmLabourFTE, fill = "Total FTE's"), stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_fill_manual(values = c("Total FTE's" = "forestgreen"),
                    name = "") +
  labs(title = "UK: Farm Labour FTE",
       x = "Year") +
  scale_y_continuous(breaks = seq(floor(-max(abs(FTE$CalcFarmLabourFTE))/5)*5,
                                  ceiling(max(abs(FTE$CalcFarmLabourFTE))/5)*5,
                                  5),
                     labels = scales::comma_format()) + 
  facet_grid(. ~ pathway, scales = "free_y",
             labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
                                             "NationalCommitments" = "National Commitments Pathway",
                                             "GlobalSustainability" = "Global Sustainability Pathway"))) +
  
  theme_minimal() +
  theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
        legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
        legend.text = element_text(family = "Courier New", size = 12),
        plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
        axis.title.x = element_text(color = "steelblue", size = 12),
        axis.title.y = element_blank())  # Remove y-axis title

print(p_pathway)

tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "FarmLabour_pathway.tiff")),
     units = "in", height = 5, width = 14, res = 300)
plot(p_pathway)
dev.off()
