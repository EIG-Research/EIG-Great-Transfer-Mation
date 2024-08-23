# 3 transfer income by selected category cumulative change

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
  filter(GeoName == "United States") %>%
  select(year, transfers_social_security_pce_per_capita, 
         transfers_medicare_pce_per_capita, 
         transfers_medicaid_pce_per_capita,
         transfers_income_maintenance_pce_per_capita,
         personal_income_pce_per_capita) %>%
  mutate(across(c(transfers_social_security_pce_per_capita, 
                  transfers_medicare_pce_per_capita, 
                  transfers_medicaid_pce_per_capita,
                  transfers_income_maintenance_pce_per_capita,
                  personal_income_pce_per_capita),
                ~./first(.)*100, # index to 100
                .names = "{.col}"))
  
  
write.xlsx(transfers, paste(path_out, "fig 3.xlsx", sep="/"))
