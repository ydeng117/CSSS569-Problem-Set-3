library(gapminder)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(RColorBrewer)
library(firatheme)

# Data import and cleaning
## Import the Gapminder data
TFR <-read.csv("Data/total_fertility.csv", check.names = FALSE)
women_edu <- read.csv("Data/mean_years_in_school_women_of_reproductive_age.csv", check.names = FALSE)
Geo_prop <- read.csv("Data/Data_Geographies_-_v2_-_by_Gapminder_-_list-of-countries-etc.csv")

## Data Cleaning
### Using the univeral country names for the Gapminder data
TFR_country_period <- TFR %>% 
  pivot_longer(!country, names_to = "Year", values_to = "Total_Fertility_Rate") %>% 
  mutate(
    country = case_when(
      country == "UK" ~ "United Kingdom",
      country == "USA" ~ "United States",
      country == "UAE" ~ "United Arab Emirates",
      country == "North Macedonia" ~ "Macedonia, FYR",
      TRUE ~ country
    )
  )

women_edu_country_period <- women_edu %>%  
  pivot_longer(!country, names_to = "Year", values_to = "Mean_edu_year_women") %>%
  mutate(
    country = case_when(
      country == "UK" ~ "United Kingdom",
      country == "USA" ~ "United States",
      country == "UAE" ~ "United Arab Emirates",
      country == "North Macedonia" ~ "Macedonia, FYR",
      TRUE ~ country
    )
  )

## Data merging
tfr_women_edu_df <- left_join(TFR_country_period, women_edu_country_period,by = join_by(country, Year)) %>%
  left_join(Geo_prop, by = join_by(country == name)) %>%
  drop_na()