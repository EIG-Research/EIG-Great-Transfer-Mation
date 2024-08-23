# 8 government transfers as a share of total income

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

transfers = read_excel(paste(path_data_out,"transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  filter(year==2022) %>%
  select(GeoName, GeoFIPS, share_transfers_govt_personal_income)

write.xlsx(transfers, paste(path_out, "fig 8.xlsx", sep = "/"))

