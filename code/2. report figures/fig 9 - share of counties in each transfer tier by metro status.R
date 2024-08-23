# 8 share of counties in each transfer tier, by metro status

# rural/urban definition for tiers by urban/rural.
# Metro central 
# Metro outlying
# Micropolitan
# Non-metro

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
# load data

transfers = read_excel(paste(path_data_out,"transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  filter(year==2022) %>%
  select(GeoName, urban_status, transfer_tiers)

table(transfers$urban_status)

#large metro (1 million +) 
  #431  

#medium metro (250k-1 million)
  #377 

#small metro (<250k) 
  #349 

#non-metro
  #1948

transfers = transfers %>%
  ungroup() %>%
  group_by(transfer_tiers) %>%
  count(urban_status) %>%
  filter(!is.na(transfer_tiers)) %>%
  filter(!is.na(urban_status)) %>%
  mutate(percentage = n/ sum(n)*100) %>%
  select(-c(n)) %>%
  pivot_wider(names_from = transfer_tiers, values_from = percentage)

write.xlsx(transfers, paste(path_out, "fig 9.xlsx", sep = "/"))
