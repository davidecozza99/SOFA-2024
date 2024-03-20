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
scenathon<- read_csv(here("data", "20240319_extracted_indicator.csv")) %>% 
  rename(alpha3 = country, Year = year) %>% 
  mutate(pathway = recode(pathway, "NationalCommitment" = "NationalCommitments")) %>% 
  filter(iteration == "5") %>%
  filter(Year %in% c("2020", "2030", "2040", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,pathway, Year, calccropn2o, calccropch4, calccropco2, calcliven2o, calclivech4, calcdeforco2, calcotherlucco2, calcsequestco2)

#GHG Aggregation -----------------------------------------------------
GHG_final <- scenathon %>% 
  group_by(alpha3, pathway, Year) %>% 
  mutate(CO2 = calccropco2 + calcdeforco2 + calcotherlucco2 + calcsequestco2) %>% 
  mutate(CH4 = calccropch4 + calclivech4) %>% 
  mutate(N2O = calccropn2o + calcliven2o)



# List of countries
GHG_final$pathway <- factor(GHG_final$pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))

countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")


for (country in countries) {
  # Subset data for the specific country
  country_data <- subset(GHG_final, alpha3 == country)
  
  # Create ggplot for the specific country
  p_pathway3 <- ggplot(country_data, aes(x = Year)) +
    geom_line(aes(y = CO2, color = "CO2"), size = 1.5) +
    geom_line(aes(y = CH4, color = "CH4"), size = 1.5) +
    geom_line(aes(y = N2O, color = "N2O"), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("CO2" = "#8da0cb", "CH4" = "#66c2a5", "N2O" = "#fc8d62"),
                       name = "") +  
    labs(
      title = paste(countries_labels[country], ": AFOLU GHG Emissions by Gas"),
      x = "Year",
      y = "Mt CO2e"
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
      axis.title.y = element_text(color = "steelblue", size = 12)
    ) +
    scale_x_continuous(breaks = c(2020, 2030,2040, 2050)) 
    # scale_y_continuous(breaks = seq(floor(-max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
    #                                 ceiling(max(abs(country_data$CO2), abs(country_data$CH4), abs(country_data$N2O))/10)*10,
    #                                 10))
    # 
  # Save the plot as a TIFF file
  tiff(here("output", "figures", country, paste0(gsub("-", "", Sys.Date()), "_", "GHG_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_pathway3)
  dev.off()
}
p_pathway3

