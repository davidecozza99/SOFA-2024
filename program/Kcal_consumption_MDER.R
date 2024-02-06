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

country="AUS"

#Data ---------------------------------------------------------------
scenathon<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, kcal_feas, kcal_mder)

# #Pathway Selection ----------------------------------------------------

kcal<- scenathon %>%
  filter(alpha3 == country)

# 
# # Plot ----------------------------------------------------------------
# p <- ggplot(kcal, aes(x = as.factor(Year))) +
#   geom_bar(aes(y = kcal_feas, fill = "Kcal Consumption"), stat = "identity", position = "dodge") +
#   geom_point(aes(y = kcal_mder, fill = "Kcal MDER"), shape = 16, size = 3, color = "steelblue", position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   scale_fill_manual(values = c("Kcal Consumption" = "#fc8d62", "Kcal MDER" = "steelblue"),
#                     name = NULL) +  # Remove legend title
#   labs(title = "Ethiopia: Kcal Consumption compared to MDER",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = "Kcal") +
#   scale_y_continuous(breaks = seq(floor(-max(abs(c(kcal$kcal_feas, kcal$kcal_mder)))/500)*500,
#                                   ceiling(max(abs(c(kcal$kcal_feas, kcal$kcal_mder)))/500)*500,
#                                   500)) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12)) +
#   guides(fill = guide_legend(override.aes = list(
#     shape = c(22, 16), 
#     size = c(4, 3), 
#     color = c("#fc8d62", "steelblue")
#   )))
# 
# print(p)


# Plot Pathway  ----------------------------------------------------------------
kcal$pathway <- factor(kcal$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))


p_pathway <- ggplot(kcal, aes(x = as.factor(Year))) +
  geom_bar(aes(y = kcal_feas, fill = "Kcal Consumption"), stat = "identity", position = "dodge") +
  geom_point(aes(y = kcal_mder, fill = "Kcal MDER"), shape = 16, size = 3, color = "steelblue", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_fill_manual(values = c("Kcal Consumption" = "#fc8d62", "Kcal MDER" = "steelblue"),
                    name = NULL) +
  labs(title = "Australia: Kcal Consumption compared to MDER",
       x = "Year",
       y = "Kcal") +
  scale_y_continuous(breaks = seq(0, max(kcal$kcal_feas+250), 200)) +
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
        axis.title.y = element_text(color = "steelblue", size = 12)) +
  guides(fill = guide_legend(override.aes = list(
    shape = c(22, 16), 
    size = c(4, 3), 
    color = c("#fc8d62", "steelblue")
  )))
p_pathway


tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "Nutrition_pathway.tiff")),
     units = "in", height = 5, width = 14, res = 300)
plot(p_pathway)
dev.off()
