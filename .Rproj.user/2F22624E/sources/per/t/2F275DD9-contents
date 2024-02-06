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

country="COL"
#Data ---------------------------------------------------------------
scenathon<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2040", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, CalcWFblue)


#Pathway Selection ----------------------------------------------------
# water<- scenathon %>% 
#   filter(alpha3 == country)

# #First Option ---------------------------------------------------------
# p <- ggplot(water, aes(x = Year)) +
#   geom_line(aes(y = CalcWFblue, color = "CalcWFblue"), size = 1.5) +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   ggtitle("Water Use for Irrigation") +
#   scale_color_manual(values = c("CalcWFblue" = "#8da0cb"),
#                      name = "") +  
#   labs(title = "Ethiopia: Water Use for Irrigation",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = " Water use (million cubic metres)") +
#   scale_x_continuous(breaks = c(2020, 2030, 2050)) +
#   scale_y_continuous(breaks = seq(floor(-max(abs(water$CalcWFblue))/1000)*1000,
#                                   ceiling(max(abs(water$CalcWFblue))/1000)*1000,
#                                   200)) +
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




#Second Option ---------------------------------------------------------
water<- scenathon %>% 
  filter(alpha3 == country)
water$pathway <- factor(water$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))


p_pathway <- ggplot(water, aes(x = Year)) +
  geom_line(aes(y = CalcWFblue, color = "Water Use"), size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid") +
  ggtitle("Water Use for Irrigation") +
  scale_color_manual(values = c("Water Use" = "#8da0cb"),
                     name = "") +  
  labs(title = "Colombia: Water Use for Irrigation",
       x = "Year",
       y = " Water use (million cubic metres)") +
  scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
  scale_y_continuous(breaks = seq(0, max(water$CalcWFblue), 100)) +
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
        axis.title.y = element_text(color = "steelblue", size = 12))

p_pathway


tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "WaterUse_pathway.tiff")),
     units = "in", height = 5, width = 14, res = 300)
plot(p_pathway)
dev.off()
