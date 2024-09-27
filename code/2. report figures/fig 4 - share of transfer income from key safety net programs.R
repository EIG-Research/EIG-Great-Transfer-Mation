# 4 share of transfer income from key safety net programs

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
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")

plot = read_excel(paste(path_data_clean, "transfers_dataset_nation_master.xlsx", sep = "/"))

plot = plot %>%
  mutate(social_security = transfers_social_security/transfers_govt*100,
         medicaid_medicare = (transfers_medicaid+transfers_medicare)/transfers_govt*100,
         income_maintenance = transfers_income_maintenance/transfers_govt*100) %>%
  select(year,social_security, medicaid_medicare,income_maintenance)

setwd(path_out)
write.xlsx(plot, "fig 4.xlsx")
