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
nitrogen<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  filter(iteration == "5") %>%
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3, Year, pathway, calcn_org, calcn_synth) %>% 
  mutate(TotalN = calcn_org + calcn_synth)


nitrogen$pathway <- factor(nitrogen$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia",
  "BRA" = "Brazil",
  "COL" = "Colombia",
  "ETH" = "Ethiopia",
  "GBR" = "United Kingdom")


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(nitrogen, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway <- ggplot(country_data, aes(x = Year)) +
    geom_line(aes(y = calcn_org, color = "Organic Nitrogen"), size = 1.0, linetype = "dashed") +
    geom_line(aes(y = calcn_synth, color = "Synthetic Nitrogen"), size = 1.0, linetype = "dashed") +
    geom_line(aes(y = TotalN, color = "Total Nitrogen"), size = 1.0, linetype = "solid") +
    geom_hline(yintercept = 0, linetype = "solid") +
    # ggtitle(paste(countries_labels, ": Nitrogen Use")) +
    facet_grid(. ~ pathway, scales = "free_y",
               labeller = labeller(pathway = c("CurrentTrends" = "Current Trend",
                                               "NationalCommitments" = "National Commitments Pathway",
                                               "GlobalSustainability" = "Global Sustainability Pathway"))) +
    scale_color_manual(values = c("Total Nitrogen" = "#5DA5DA", "Organic Nitrogen" = "#60BD68", "Synthetic Nitrogen" = "#F17CB0"),
                       name = "") +
    labs(
      title = paste(countries_labels[country], "Nitrogen Use"),
      x = "Year",
      y = "1000 tonnes"
    ) +
    # scale_y_continuous(breaks = seq(0, max(country_data$TotalN + 100), 100)) +
    theme_minimal() +
    theme(
      text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
      legend.title = element_text(family = "Courier New", color = "steelblue", size = 12),
      legend.text = element_text(family = "Courier New", size = 12),
      plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
      axis.title.x = element_text(color = "steelblue", size = 12),
      axis.title.y = element_text(color = "steelblue", size = 12)
    )
  
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "Nitrogen_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway)
  dev.off()
}

