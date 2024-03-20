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

#Data ---------------------------------------------------------------
water<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  filter(iteration == "5") %>%
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(Year %in% c("2020", "2030", "2040", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, calcwfblue)

water$pathway <- factor(water$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))
countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")



for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(water, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year)) +
    geom_line(aes(y = calcwfblue, color = "Water Use for Irrigation"), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "solid") +
    ggtitle(paste(country, ": Water Use for Irrigation")) +
    scale_color_manual(values = c("Water Use for Irrigation" = "#8da0cb"),
                       name = "") +  
    labs(
      title = paste(countries_labels[country], ": Water Use for Irrigation"),
      x = "Year",
      y = "Million cubic metres"
    ) +
    scale_x_continuous(breaks = c(2020, 2030, 2040, 2050)) +
    # scale_y_continuous(breaks = seq(0, max(country_data$calcwfblue), 100)) +
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
      axis.title.y = element_text(color = "steelblue", size = 12)
    )
  
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "WaterUse_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway)
  dev.off()
}
