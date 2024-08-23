# 10 four panel scatter plot

rm(list = ls())

# load packages
library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(mapview)
library(tigris)
library(tidycensus)
library(ranger)
library(ggplot2)
library(tidyverse)
library(janitor)
library(ipumsr)
library(Hmisc)
library(purrr)
library(broom)

# paths
path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_out = file.path(path_project,"data/clean")
path_out = file.path(path_project,"output")

transfers = read_excel(paste(path_data_out, 
                             "transfers_dataset_counties_master.xlsx",
                             sep ="/")) %>%
  filter(!is.na(share_65_over))

# poverty
poverty = read.csv(paste(path_data_out, "poverty_rates.csv", sep ="/")) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, 5, pad="0")) %>%
  mutate(year = as.character(year))

transfers = left_join(transfers, poverty, by = c("year", "GeoFIPS"))

scatter_dynamic = transfers %>%
  filter(year == 2022 | year == 1970) %>%
  select(share_65_over, GeoFIPS,year,Percent.below.poverty.level, transfers_govt_pce_per_capita) %>%
  filter(GeoFIPS !=55078) %>% 
  pivot_wider(id_cols = c("GeoFIPS"),
              names_from = year,
              values_from = c("share_65_over", "Percent.below.poverty.level", "transfers_govt_pce_per_capita"))%>%
  mutate(pov = (Percent.below.poverty.level_2022-Percent.below.poverty.level_1970)/Percent.below.poverty.level_1970,
         old = (share_65_over_2022-share_65_over_1970)/share_65_over_1970,
         transfer = (transfers_govt_pce_per_capita_2022-transfers_govt_pce_per_capita_1970)/transfers_govt_pce_per_capita_1970)


# static - old
old_age_2022 = ggplot(scatter_dynamic, 
                      aes(x=share_65_over_2022, y=transfers_govt_pce_per_capita_2022)) + 
  geom_point(size=0.25)+
  labs(title = "2022 Old Age Share vs. 2022 Per Capita Transfers", x = "population share over 65", y="per capita transfers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 7, color = "#1a654d",hjust = 1),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))

# static - poverty
poverty_age_2022 = ggplot(scatter_dynamic, aes(x=Percent.below.poverty.level_2022, y=transfers_govt_pce_per_capita_2022)) + 
  geom_point(size=0.25)+
  labs(title = "2022 Poverty Rate vs. 2022 Per Capita Transfers", x = "share under poverty threshold", y="per capita transfers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 7, color = "#1a654d",hjust = 1),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))

# dynamic - old
old_growth = ggplot(scatter_dynamic, aes(x=old, y=transfer)) + 
  geom_point(size=0.25)+
  labs(title = "Old Age growth versus Transfers Growth, 1970-2022", x = "change in population share over 65", y = "change in per capita transfers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 7, color = "#1a654d",hjust = 1),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))

# dynamic - poverty
poverty_growth = ggplot(scatter_dynamic, aes(x=pov, y=transfer)) + 
  geom_point(size=0.25)+
  labs(title = "Poverty growth versus Transfers Growth, 1970-2022", x = "change in poverty rate", y = "change in per capita transfers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 7, color = "#1a654d",hjust = 1),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))

require(gridExtra)

scatter = grid.arrange(old_age_2022, poverty_age_2022, old_growth, poverty_growth)

ggsave(plot = scatter, 
       filename = paste(path_out,"fig 10.png"),bg="white")


# correlation of over 6 and poverty rate
transfers_2022 = transfers %>%
  filter(year==2022)

cov(transfers_2022$share_65_over, transfers_2022$Percent.below.poverty.level)
