# 11 Sarasota and Roscommon

# remove dependencies
rm(list = ls())

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(stringr)

path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_out = file.path(path_project,"data/clean")
path_out = file.path(path_project,"output")

transfers =  read.xlsx(paste(path_data_out, "transfers_dataset_counties_master.xlsx", sep ="/")) %>%
  filter(GeoName == "Sarasota, FL" | GeoName == "Roscommon, MI") %>%
  mutate(non_transfer = personal_income_pce_per_capita-transfers_govt_pce_per_capita) %>%
  select(GeoName, year, transfers_govt_pce_per_capita, non_transfer)

transfers = transfers %>%
  pivot_wider(names_from = GeoName,
              values_from  = c(transfers_govt_pce_per_capita,non_transfer))

write.xlsx(transfers, paste(path_out, "fig 11.xlsx", sep="/"))
