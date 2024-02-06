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

country="GBR"
#Data ---------------------------------------------------------------
scenathon<- read_csv(here("data", "extracted_indicator2023.csv")) %>% 
  filter(TradeAdjusment == "Yes") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, CalcN_org, CalcN_synth) %>% 
  mutate(TotalN = CalcN_org + CalcN_synth)

#Pathway/Country Selection ----------------------------------------------------
# 
# nitrogen <- scenathon %>% 
#   filter(alpha3 == "ETH")
# # 
# 

# #Plot ---------------------------------------------------------------
# p <- ggplot(nitrogen, aes(x = Year)) +
#   geom_line(aes(y = CalcN_org, color = "Organic Nitrogen"), size = 1.5) +
#   geom_line(aes(y = CalcN_synth, color = "Synthetic Nitrogen"), size = 1.5) +
#   geom_line(aes(y = TotalN, color = "Total Nitrogen"), size = 1.5, linetype = "dashed") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   ggtitle("Nitrogen Use") +
#   scale_color_manual(values = c("Total Nitrogen" = "#5DA5DA", "Organic Nitrogen" = "#60BD68", "Synthetic Nitrogen" = "#F17CB0"),
#                      name = "Nitrogen Type") +
#   labs(title = "Ethiopia: Nitrogen Use",
#        subtitle = "Current Trend Pathway",
#        x = "Year",
#        y = "Nitrogen (1000 tonnes)") +
#   scale_y_continuous(breaks = seq(0, max(nitrogen$CalcN_org + nitrogen$CalcN_synth), 250)) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12),
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12))
# 
# p

#Plot Pathway ---------------------------------------------------------------
# nitrogen <- scenathon %>% 
#   filter(alpha3 == "ETH")
# 
# nitrogen$pathway <- factor(nitrogen$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))
# 

# p_pathway <- ggplot(nitrogen, aes(x = Year)) +
#   geom_line(aes(y = CalcN_org, color = "Organic Nitrogen"), size = 1.5) +
#   geom_line(aes(y = CalcN_synth, color = "Synthetic Nitrogen"), size = 1.5) +
#   geom_line(aes(y = TotalN, color = "Total Nitrogen"), size = 1.5, linetype = "dashed") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   ggtitle("Nitrogen Use") +
#   facet_wrap(~ pathway, scales = "free_y", ncol = 1,
#              labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
#                                              "NationalCommitments" = "National Commitments Pathway",
#                                              "GlobalSustainability" = "Global Sustainability Pathway"))) +
#   scale_color_manual(values = c("Total Nitrogen" = "#5DA5DA", "Organic Nitrogen" = "#60BD68", "Synthetic Nitrogen" = "#F17CB0"),
#                      name = "Nitrogen Type") +
#   labs(title = "Ethiopia: Nitrogen Use",
#        x = "Year",
#        y = "Nitrogen (1000 tonnes)") +
#   scale_y_continuous(breaks = seq(0, max(nitrogen$CalcN_org + nitrogen$CalcN_synth), 250)) +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#         legend.title = element_text(family = "Courier New", color = "steelblue", size = 12),
#         legend.text = element_text(family = "Courier New", size = 12),
#         plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#         axis.title.x = element_text(color = "steelblue", size = 12),
#         axis.title.y = element_text(color = "steelblue", size = 12))
# 
# p_pathway
# 


#Plot Pathway ---------------------------------------------------------------

nitrogen <- scenathon %>% 
  filter(alpha3 == country)

nitrogen$pathway <- factor(nitrogen$pathway, levels = c("CurrentTrend", "NationalCommitments", "GlobalSustainability"))


p_pathway3 <- ggplot(nitrogen, aes(x = Year)) +
  geom_line(aes(y = CalcN_org, color = "Organic Nitrogen"), size = 1.5, linetype = "dashed") +
  geom_line(aes(y = CalcN_synth, color = "Synthetic Nitrogen"), size = 1.5, linetype = "dashed") +
  geom_line(aes(y = TotalN, color = "Total Nitrogen"), size = 1.5, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  ggtitle("Nitrogen Use") +
  facet_grid(. ~ pathway, scales = "free_y",
             labeller = labeller(pathway = c("CurrentTrend" = "Current Trend",
                                             "NationalCommitments" = "National Commitments Pathway",
                                             "GlobalSustainability" = "Global Sustainability Pathway"))) +
  scale_color_manual(values = c("Total Nitrogen" = "#5DA5DA", "Organic Nitrogen" = "#60BD68", "Synthetic Nitrogen" = "#F17CB0"),
                     name = "Nitrogen Type") +
  labs(title = "UK: Nitrogen Use",
       x = "Year",
       y = "Nitrogen (1000 tonnes)") +
  scale_y_continuous(breaks = seq(0, max(nitrogen$TotalN+100), 100)) +
  theme_minimal() +
  theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
        legend.title = element_text(family = "Courier New", color = "steelblue", size = 12),
        legend.text = element_text(family = "Courier New", size = 12),
        plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
        axis.title.x = element_text(color = "steelblue", size = 12),
        axis.title.y = element_text(color = "steelblue", size = 12))


p_pathway3

tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "Nitrogen_pathway3.tiff")),
     units = "in", height = 5, width = 14, res = 300)
plot(p_pathway3)
dev.off()
