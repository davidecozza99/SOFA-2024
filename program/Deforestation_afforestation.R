# Libraries ---------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(readxl)
library(conflicted)
library(readxl)
library(ggplot2)
conflict_prefer("filter", "dplyr")

here()

#Data ---------------------------------------------------------------
deforestation<- read_csv(here("data", "FullDataBase.csv")) %>% 
  rename(alpha3 = country) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, ForestChange, NewForestChange)



# #Data ---------------------------------------------------------------
# deforestation_old<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
#   filter(TradeAdjusment == "Yes") %>% 
#   filter(Year %in% c("2020", "2030", "2050"))%>% 
#   filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
#   select(alpha3, Year, pathway, ForestChange, NewForestChange)
# 



# #Pathway ---------------------------------------------------------------
# deforestation <- deforestation %>% 
#   filter(alpha3 == country)
#   
#   

# 
# #Plot ---------------------------------------------------------------
# p <- ggplot(deforestation, aes(x = as.factor(Year))) +
#   geom_bar(aes(y = ForestChange, fill = "Deforestation"), stat = "identity", position = "dodge") +
#   geom_bar(aes(y = NewForestChange, fill = "Afforestation"), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   scale_fill_manual(values = c("Deforestation" = "darkred", "Afforestation" = "darkgreen"),
#                     name = "Legend") +
#   labs(title = "Ethiopia: Deforestation and Afforestation",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = "Change in Forest Area (1000 ha)") +
#   scale_y_continuous(breaks = seq(floor(-max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/500)*500,
#                                   ceiling(max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/500)*500,
#                                   500)) +
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

# #Plot 2 ---------------------------------------------------------------
# deforestation <- scenathon %>% 
#   filter(alpha3 == country)
# deforestation$pathway <- factor(deforestation$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))
# 
# 
# p_pathway <- ggplot(deforestation, aes(x = as.factor(Year))) +
#   geom_bar(aes(y = ForestChange, fill = "Deforestation"), stat = "identity", position = "dodge") +
#   geom_bar(aes(y = NewForestChange, fill = "Afforestation"), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   scale_fill_manual(values = c("Deforestation" = "darkred", "Afforestation" = "darkgreen"),
#                     name = "Legend") +
#   labs(title = "Ethiopia: Deforestation and Afforestation",
#        x = "Year",
#        y = "Change in Forest Area (1000 ha)") +
#   scale_y_continuous(breaks = seq(floor(-max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/500)*500,
#                                   ceiling(max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/500)*500,
#                                   500)) +
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
#         axis.title.y = element_text(color = "steelblue", size = 12))
# 
# print(p_pathway)
# 


# #Plot  ---------------------------------------------------------------
countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")

  deforestation$pathway <- factor(deforestation$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

for (country in countries) {
  p_pathway3 <- ggplot(deforestation, aes(x = as.factor(Year))) +
    geom_bar(aes(y = ForestChange, fill = "Deforestation", group = pathway), stat = "identity", position = "dodge") +
    geom_bar(aes(y = NewForestChange, fill = "Afforestation", group = pathway), stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_fill_manual(values = c("Deforestation" = "darkred", "Afforestation" = "darkgreen"),
                      name = "") +
    labs(title = paste0(countries_labels[country], ": Deforestation and Afforestation"),
         x = "5-Year Period",
         y = "Change in Forest Area (1000 ha)") +
    scale_y_continuous(breaks = seq(floor(-max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
                                    ceiling(max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
                                    50)) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
                                               "NationalCommitments" = "National Commitments Pathway",
                                               "GlobalSustainability" = "Global Sustainability Pathway"))) +
    scale_x_discrete(labels = c("2015-2020", "2025-2030", "2045-2050")) +  # Update X-axis labels
    theme_minimal() +
    theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
          legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
          legend.text = element_text(family = "Courier New", size = 12),
          plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
          axis.title.x = element_text(color = "steelblue", size = 12),
          axis.title.y = element_text(color = "steelblue", size = 12))
  
  tiff(here("output", "figures", paste0(gsub("-", "",Sys.Date()), "_", "Deforestation.tiff")),
  units = "in", height = 5, width = 14, res = 300)
  plot(p_pathway3)
  dev.off()
}


#Save --------------------------------------------------------------------------

# tiff(here("output", "figures", paste0(gsub("-", "",Sys.Date()), "_", "Deforestation.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p)
# dev.off()
# 
# 
# tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "Deforestationpathway.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_pathway)
# dev.off()



  
  
  
  
  
  
  
  
  
  
  
  # deforestation$pathway <- factor(deforestation$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))
  # 
  # p_pathway3 <- ggplot(deforestation, aes(x = as.factor(Year))) +
  #   geom_bar(aes(y = ForestChange, fill = "Deforestation", group = pathway), stat = "identity", position = "dodge") +
  #   geom_bar(aes(y = NewForestChange, fill = "Afforestation", group = pathway), stat = "identity", position = "dodge") +
  #   geom_hline(yintercept = 0, linetype = "solid") +
  #   scale_fill_manual(values = c("Deforestation" = "darkred", "Afforestation" = "darkgreen"),
  #                     name = "") +
  #   labs(title = "UK: Deforestation and Afforestation",
  #        x = "5-Year Period",
  #        y = "Change in Forest Area (1000 ha)") +
  #   scale_y_continuous(breaks = seq(floor(-max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
  #                                   ceiling(max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
  #                                   50)) +
  #   facet_grid(. ~ pathway, scales = "free_y",
  #              labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
  #                                              "NationalCommitments" = "National Commitments Pathway",
  #                                              "GlobalSustainability" = "Global Sustainability Pathway"))) +
  #   scale_x_discrete(labels = c("2015-2020", "2025-2030", "2045-2050")) +  # Update X-axis labels
  #   theme_minimal() +
  #   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
  #         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
  #         legend.text = element_text(family = "Courier New", size = 12),
  #         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
  #         axis.title.x = element_text(color = "steelblue", size = 12),
  #         axis.title.y = element_text(color = "steelblue", size = 12))
  # 
  # p_pathway3
  # 
  # 
  # tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "Deforestation_pathway3.tiff")),
  #      units = "in", height = 5, width = 14, res = 300)
  # plot(p_pathway3)
  # dev.off() 
  # 
  # 
  # 
  # 
  
  
  
  
