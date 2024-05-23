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
deforestation<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>%
  rename(alpha3 = country, Year = year,  ForestChange=forestchange, NewForestChange = newforestchange) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, ForestChange, NewForestChange)


# #Plot  ---------------------------------------------------------------
deforestation$pathway <- factor(deforestation$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")


for (country in countries) {
  
  country_data <- subset(deforestation, alpha3 == country)
  
  
  p_pathway3 <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_bar(aes(y = ForestChange, fill = "Deforestation", group = pathway), stat = "identity", position = "dodge") +
    geom_bar(aes(y = NewForestChange, fill = "Afforestation", group = pathway), stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_fill_manual(values = c("Deforestation" = "darkred", "Afforestation" = "darkgreen"),
                      name = "") +
    labs(title = paste0(countries_labels[country], ": Deforestation and Afforestation"),
         x = "5-Year Period",
         y = "Change in Forest Area (1000 ha)") +
    # scale_y_continuous(breaks = seq(floor(-max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
    #                                 ceiling(max(abs(c(deforestation$ForestChange, deforestation$NewForestChange)))/50)*50,
    #                                 50)) +
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

  # tiff(here("output", "figures", country, paste0(gsub("-", "",Sys.Date()), "_", "Deforestation.tiff")),
  # units = "in", height = 5, width = 14, res = 300)
  # plot(p_pathway3)
  # dev.off()
}

