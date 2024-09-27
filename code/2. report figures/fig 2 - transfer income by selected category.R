# 2 transfer income by selected category

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
transfers = read_excel(paste(path_data_out, "transfers_dataset_nation_master.xlsx", sep = "/")) %>%
  mutate(all_other = transfers_govt_pce - transfers_social_security_pce-transfers_medicare_pce-transfers_medicaid_pce-transfers_income_maintenance_pce-transfers_unempl_insurance_pce) %>%
  select(year, all_other, transfers_govt_pce, transfers_social_security_pce, transfers_medicare_pce, transfers_medicaid_pce, transfers_unempl_insurance_pce, transfers_income_maintenance_pce)

# save for datawrapper plot
write.xlsx(transfers, paste(path_out, "fig 2", sep="/"))
