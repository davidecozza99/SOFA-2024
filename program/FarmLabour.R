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
FTE<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, , Year = year, CalcFarmLabourFTE = calcfarmlabourfte) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, CalcFarmLabourFTE)



#Plot Pathway ---------------------------------------------------------------
FTE$pathway <- factor(FTE$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")



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
      y = "1000 FTE workers"
    ) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trend",
                                               "NationalCommitments" = "National Commitments Pathway",
                                               "GlobalSustainability" = "Global Sustainability Pathway"))) +
    theme_minimal() +
    theme(
      text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
      legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),  
      legend.text = element_text(family = "Courier New", size = 12),
      plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
      axis.title.x = element_text(color = "steelblue", size = 12),
      axis.title.y = element_text(color = "steelblue", size = 12),
      
    )
  
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "FarmLabour_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway)
  dev.off()
}

p_pathway
