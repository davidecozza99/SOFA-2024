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


scenathon <- read_csv(here("data", "FullDataBase.csv")) %>% 
  rename(alpha3 = country, Year = year, LNPPMatureForest=lnppmatureforest, LNPPMatureOtherLand = lnppmatureotherland) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2015", "2020", "2025", "2030", "2045","2050"))%>% 
  filter(alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, pathway, Year, LNPPMatureForest, LNPPMatureOtherLand) 

naturalland <- scenathon %>% 
  arrange(alpha3, pathway, Year) %>%  
  group_by(alpha3, pathway) %>% 
  mutate(Diff_LNPPMatureForest = LNPPMatureForest - lag(LNPPMatureForest, default = first(LNPPMatureForest))) %>% 
  mutate(Diff_LNPPMatureOtherLand = LNPPMatureOtherLand - lag(LNPPMatureOtherLand, default = first(LNPPMatureOtherLand))) %>% 
  filter(Year %in% c("2020", "2030", "2050")) 

naturalland$pathway <- factor(naturalland$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

# List of countries
countries <- c("AUS", 
               "BRA"
               , "COL", "ETH", "GBR"
               )
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")



#Plot ------------------------------------------------
# Loop through each country

for (country in countries) {
  
  country_data <- subset(naturalland, alpha3 == country)
  
  
p_pathway <- ggplot(country_data, aes(x = as.factor(Year))) +
  geom_bar(aes(y = Diff_LNPPMatureForest, fill = "LNPP Forests", group = pathway), stat = "identity", position = "stack") +
  geom_bar(aes(y = Diff_LNPPMatureOtherLand, fill = "LNPP Other lands", group = pathway), stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_fill_manual(values = c("LNPP Forests" = "#8FBC8F", "LNPP Other lands" = "#FFD700"),
                    name = "") +
  labs(
    title = paste(countries_labels[country], ": Natural land loss"),
       x = "5-Year Period",
       y = "Change in Area (1000 ha)") +
  # scale_y_continuous(breaks = seq(floor(-max(abs(c(naturalland$Diff_LNPPMatureForest, naturalland$Diff_LNPPMatureOtherLand)))/50)*50,
  #                                 ceiling(max(abs(c(naturalland$Diff_LNPPMatureForest, naturalland$Diff_LNPPMatureOtherLand)))/50)*50,
  #                                 10)) +
  facet_grid(. ~ pathway, scales = "free_y",
             labeller = labeller(pathway = c("CurrentTrends" = "Current Trend",
                                             "NationalCommitments" = "National Commitments Pathway",
                                             "GlobalSustainability" = "Global Sustainability Pathway"))) +
  scale_x_discrete(labels = c("2015-2020", "2025-2030", "2045-2050")) +  
  theme_minimal() +
  theme(text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
        legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
        legend.text = element_text(family = "Courier New", size = 12),
        plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
        axis.title.x = element_text(color = "steelblue", size = 12),
        axis.title.y = element_text(color = "steelblue", size = 12))

# Save the plot as a TIFF file
tiff(here("output", "figures", country, paste0(gsub("-", "",Sys.Date()), "_", "naturalland_pathway.tiff")),
     units = "in", height = 5, width = 14, res = 300)
print(p_pathway)
dev.off()
}


print(p_pathway)






