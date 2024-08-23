# tables 1 and 2 - counties with highest and lowest transfer share

# table: counties with highest and lowest transfer ratios.
# top 20, bottom 20, add in major city later

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

transfers = read_excel(paste(path_data_out, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  filter(year==2022) %>%
  mutate(income_no_transfers = personal_income_pce_per_capita - 
           transfers_govt_pce_per_capita) %>%
  select(GeoName, income_no_transfers, 
         personal_income_pce_per_capita, share_transfers_govt_personal_income, 
         population) %>%
  rename(County = GeoName,
         `Per Capita Income (excl. transfers)` = income_no_transfers,
         `Per Capita Income (incl transfers)` = personal_income_pce_per_capita,
         `Government Transfer Share` = share_transfers_govt_personal_income)

# select top 20 and bottom 20
# sort by transfers, select top
# sort by -transfers, select bottom.

transfers_ranking = transfers %>% filter(population >=100000) # 100K+

transfers_ranking = transfers_ranking[order(transfers_ranking$`Government Transfer Share`),] %>%
  select(-c(population))

low = transfers_ranking %>% slice(1:20)
high = tail(transfers_ranking, n=20)

write.xlsx(low, paste(path_out, "tab 1.xlsx", sep="/"))
write.xlsx(high, paste(path_out, "tab 2.xlsx", sep="/"))