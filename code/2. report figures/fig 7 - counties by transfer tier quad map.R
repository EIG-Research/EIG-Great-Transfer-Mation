# 7 government transfer share of personal income maps
# builds transfer bin maps by county, by decade, 1970-2022

# remove dependencies
rm(list = ls())

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)

# project paths - update to your own folder paths
path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")

# load data
df_bea = read_excel(paste(path_data_clean, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  filter(!is.na(state)) %>%
  select(year, GeoName, GeoFIPS, transfer_tiers) %>%
  filter(!is.na(transfer_tiers))

###########################
# export for map
g1970 <- df_bea %>% filter(year==1970) # figure made
g2000 <- df_bea %>% filter(year==2000)
g2012 <- df_bea %>% filter(year==2012)
g2022 <- df_bea %>% filter(year==2022) # figure made

write.xlsx(g1970, paste(path_out, 'fig 7pop_bins_1970.xlsx', sep="/"))
write.xlsx(g2000, paste(path_out, 'fig 7pop_bins_2000.xlsx', sep="/"))
write.xlsx(g2012, paste(path_out, 'fig 7pop_bins_2012.xlsx', sep="/"))
write.xlsx(g2022, paste(path_out, 'fig 7pop_bins_2022.xlsx', sep="/"))
