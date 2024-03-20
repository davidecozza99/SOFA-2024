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


scenathon <- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
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
  filter(Year %in% c("2020", "2030", "2050")) %>% 
  select(alpha3, pathway, Year, Diff_LNPPMatureForest, Diff_LNPPMatureOtherLand )

naturalland$pathway <- factor(naturalland$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

# List of countries
countries <- c("AUS",
               "BRA",
               "COL",
               "ETH", 
               "GBR"
               )
countries_labels <-c(
  "AUS" = "Australia",
  "BRA" = "Brazil",
  "COL" = "Colombia",
  "ETH" = "Ethiopia",
  "GBR" = "United Kingdom"
  )



#Plot ------------------------------------------------
# Loop through each country


for (country in countries) {
  # Subset data for the current country
  country_data <- subset(naturalland, alpha3 == country) %>% 
    rename("LNPP Mature Forest" = Diff_LNPPMatureForest,
           "LNPP Mature Other Land" = Diff_LNPPMatureOtherLand)
    
  # Plotting
  p_pathway <- country_data %>%
    pivot_longer(cols = c(`LNPP Mature Forest`, `LNPP Mature Other Land`),  # Enclose column names in backticks
                 names_to = "name", values_to = "value") %>%
    ggplot(aes(x = as.factor(Year), y = value, fill = name)) +
    geom_bar(stat = "identity", width = 0.5, position = "dodge") +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_fill_manual(values = c("LNPP Mature Forest" = "#8FBC8F", "LNPP Mature Other Land" = "#FFD700"),
                      name = "", labels = c("LNPP Mature Forest", "LNPP Mature Other Land")) +  # Corrected labels
    labs(title = paste(countries_labels[country], ": Natural land gain and loss"),
         x = "5-Year Period",
         y = "Change in Area (1000 ha)") +
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
  tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "naturalland_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway)
  dev.off()
}



