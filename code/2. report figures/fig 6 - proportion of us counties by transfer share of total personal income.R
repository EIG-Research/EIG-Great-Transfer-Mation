# 6 share of counties in each transfer tier

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
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")

transfers = read_excel(paste(path_data_clean, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  select(year, GeoName, transfer_tiers)

transfers = transfers %>%
  group_by(year) %>%
  count(transfer_tiers) %>%
  mutate(percentage = n/ sum(n)*100) %>%
  filter(!is.na(transfer_tiers)) %>%
  select(-n)

# prepare for datawrapper
transfers <- transfers %>%
  pivot_wider(
    names_from = transfer_tiers,
    values_from = percentage
  )

write.xlsx(transfers, paste(path_out, '5 share of counties by transfer income tier.xlsx', sep = "/"))
