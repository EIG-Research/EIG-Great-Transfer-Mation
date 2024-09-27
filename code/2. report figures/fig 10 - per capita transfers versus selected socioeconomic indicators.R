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
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")

transfers = read_excel(paste(path_data_clean, 
                             "transfers_dataset_counties_master.xlsx",
                             sep ="/")) %>%
  filter(!is.na(share_65_over))

# poverty
poverty = read.csv(paste(path_data_clean, "poverty_rates.csv", sep ="/")) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, 5, pad="0")) %>%
  mutate(year = as.character(year))

transfers = left_join(transfers, poverty, by = c("year", "GeoFIPS"))

scatter_dynamic = transfers %>%
  filter(year == 2022 | year == 1970) %>%
  select(share_65_over, GeoFIPS,year,Percent.below.poverty.level,
         transfers_govt_pce_per_capita) %>%
  filter(GeoFIPS !=55078) %>% 
  pivot_wider(id_cols = c("GeoFIPS"),
              names_from = year,
              values_from = c("share_65_over",
                              "Percent.below.poverty.level",
                              "transfers_govt_pce_per_capita")) %>%
  mutate(pov = (Percent.below.poverty.level_2022-Percent.below.poverty.level_1970)/Percent.below.poverty.level_1970,
         old = 100*(share_65_over_2022-share_65_over_1970)/share_65_over_1970,
         transfer = 100*(transfers_govt_pce_per_capita_2022-transfers_govt_pce_per_capita_1970)/transfers_govt_pce_per_capita_1970) %>%
mutate(share_65_over_2022 = share_65_over_2022*100)


##############################
# export data for datawrapper figure making
panel1 = scatter_dynamic %>%
  select(share_65_over_2022, transfers_govt_pce_per_capita_2022)

panel2 = scatter_dynamic %>%
  select(Percent.below.poverty.level_2022, transfers_govt_pce_per_capita_2022)

panel3 = scatter_dynamic %>%
  select(old, transfer)

panel4 = scatter_dynamic %>%
  select(pov, transfer)

    write.xlsx(panel1, paste(path_out, "fig10_panel1.xlsx", sep="/"))
    write.xlsx(panel2, paste(path_out, "fig10_panel2.xlsx", sep="/"))
    write.xlsx(panel3, paste(path_out, "fig10_panel3.xlsx", sep="/"))
    write.xlsx(panel4, paste(path_out, "fig10_panel4.xlsx", sep="/"))
