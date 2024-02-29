# Libraries --------------------------------------------------------------
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

#Data -------------------------------------------------------------------
scenathon<- read_csv(here("data", "FullProductDatabase.csv")) %>% 
  rename(alpha3 = country, Pathway = pathway) %>% 
  filter(iteration == "5") %>% 
  filter(Year %in% c("2020", "2030", "2050"))%>% 
  filter (alpha3 %in% c("AUS", "BRA", "COL", "ETH", "GBR")) %>% 
  select(alpha3,Pathway, Year, Product, kcalfeasprod)

mapping<- read_excel(here("data", "mapping_product_group.xlsx")) %>% 
  rename(Product = PRODUCT)

consumption <- scenathon %>% 
  inner_join(mapping, by ="Product") %>% 
  group_by(Pathway, alpha3, Year, PROD_GROUP) %>% 
  mutate(kcalfeasprod_productgroup = sum(kcalfeasprod)) %>% 
  ungroup()  %>% 
  group_by(Pathway, alpha3, Year) %>% 
  mutate(total_kcal = sum(kcalfeasprod)) %>% 
  ungroup() %>%
  select(-Product, -kcalfeasprod) %>%
  # filter(alpha3 == country) %>%
  unique

#Plot ---------------------------------------------------------

consumption$Pathway <- factor(consumption$Pathway, levels = c("CurrentTrends", "NationalCommitments", "GlobalSustainability"))


product_colors <- c(
  "BEVSPICES" = "#8B0000",    
  "CEREALS" = "#006400",      
  "EGGS" = "#00008B",         
  "FIBERINDUS" = "#8B4513",   
  "FRUVEG" = "#8A2BE2",        
  "MILK" = "#FFD700",         
  "NUTS" = "#8FBC8F",          
  "OLSCAKE" = "#A52A2A",      
  "OLSOIL" = "steelblue",        
  "PORK" = "#DC143C",         
  "POULTRY" = "#4B0082",
  "PULSES" = "#F17CB0",
  "REDMEAT" = "#B22222",       
  "ROOTS" = "#DAA520",        
  "SUGAR" = "#FF4500"       
)


product_labels <- c(
  "BEVSPICES" = "Beverages, Spices and Tobacco",
  "CEREALS" = "Cereals",
  "EGGS" = "Eggs",
  "FIBERINDUS" ="Fiber and Industrial Crops",
  "FRUVEG" = "Fruits & Vegetables",
  "MILK" ="Milk",
  "NUTS" = "Nuts",
  "OLSCAKE" = "Oil Cakes",
  "OLSOIL" = "Oilseeds and veg. oils",
  "PORK" = "Pork",
  "POULTRY" = "Poultry",
  "PULSES" = "Pulses",
  "REDMEAT" = "Beef, Goat & Lambs",
  "ROOTS" = "Roots and Tubers",
  "SUGAR" ="Sugar and Sugar Crops"
)

  

# p_consumption <- ggplot(consumption,  aes(x = as.factor(Year))) +
#   geom_bar(aes(y = kcalfeasprod_productgroup, fill = PROD_GROUP), stat = "identity", position = "stack") +
#   geom_hline(yintercept = 0, linetype = "solid") +
#   labs(
#     title = "Australia: Evolution of Consumption by Food Group",
#     x = "Year",
#     y = "Consumption (kcal)",
#     fill = "Food Group"
#   ) +
#   scale_y_continuous(breaks = seq(0, max(consumption$kcalfeasprod_productgroup + 2000), 250)) +
#   facet_grid(. ~ Pathway, scales = "free_y",
#              labeller = labeller(Pathway = c(
#                "CurrentTrend" = "Current Trend",
#                "NationalCommitments" = "National Commitments Pathway",
#                "GlobalSustainability" = "Global Sustainability Pathway"
#              ))) +
#   scale_fill_manual(values = product_colors, name = "Food Group", labels = product_labels) +  
#   theme_minimal() +
#   theme(
#     text = element_text(family = "Courier New", color = "black", size = 12, face = "bold"),
#     legend.title = element_text(family = "Courier New", color = "steelblue", size = 12, face = "bold"),
#     legend.text = element_text(family = "Courier New", size = 12),
#     plot.title = element_text(color = "steelblue", size = 14, face = "bold"),
#     axis.title.x = element_text(color = "steelblue", size = 12),
#     axis.title.y = element_text(color = "steelblue", size = 12)
#   )
# 
# p_consumption
# 
# 
# 
# tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "consumptionfoodgroup_pathway.tiff")),
#      units = "in", height = 5, width = 14, res = 300)
# plot(p_consumption)
# dev.off()
# 
# 
# 



# List of countries
countries <- c("AUS", "BRA", "COL", "ETH", "GBR")
countries_labels <-c(
  "AUS" = "Australia", 
  "BRA" = "Brazil", 
  "COL" = "Colombia", 
  "ETH" = "Ethiopia", 
  "GBR" = "United Kingdom")



# Loop through each country

for (country in countries) {

  country_data <- subset(consumption, alpha3 == country)
  
  # Create ggplot for the specific country
  p_consumption <- ggplot(country_data, aes(x = as.factor(Year))) +
    geom_bar(aes(y = kcalfeasprod_productgroup, fill = PROD_GROUP), stat = "identity", position = "stack") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(
      title = paste(countries_labels[country], ": Evolution of Consumption by Food Group"),
      x = "Year",
      y = "Consumption (kcal)",
      fill = "Food Group"
    ) +
    scale_y_continuous(breaks = seq(0, max(country_data$kcalfeasprod_productgroup + 2000), 250)) +
    facet_grid(. ~ Pathway, scales = "free_y",
               labeller = labeller(Pathway = c(
                 "CurrentTrends" = "Current Trend",
                 "NationalCommitments" = "National Commitments Pathway",
                 "GlobalSustainability" = "Global Sustainability Pathway"
               ))) +
    scale_fill_manual(values = product_colors, name = "Food Group", labels = product_labels) +  
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
  tiff(here("output", "figures",country, paste0(gsub("-", "",Sys.Date()), "_", "consumptionfoodgroup_pathway.tiff")),
       units = "in", height = 5, width = 14, res = 300)
  print(p_consumption)
  dev.off()
}


