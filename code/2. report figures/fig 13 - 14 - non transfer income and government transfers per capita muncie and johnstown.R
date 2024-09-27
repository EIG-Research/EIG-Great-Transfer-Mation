# 13 Muncie and Johnstown

rm(list = ls())

library(haven)
library(dplyr)
library(plotly)
library(tidyr)
library(readxl)

path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")

df_transfers =  read_excel(paste(path_data_clean, "transfers_dataset_counties_master.xlsx", sep ="/")) %>%
  filter(GeoName == "Cambria, PA" | GeoName == "Delaware, IN") %>%
  mutate(non_transfers = # non-transfer income
           personal_income_pce_per_capita-
           transfers_govt_pce_per_capita) %>%
  rename(transfers = transfers_govt_pce_per_capita) %>%
  select(GeoName, year, transfers, non_transfers)

# need united states
df_transfers_USA =read_excel(paste(path_data_clean, "transfers_dataset_nation_master.xlsx", sep ="/")) %>%
  mutate(non_transfers = # non-transfer income
           personal_income_pce_per_capita-
           transfers_govt_pce_per_capita) %>%
  rename(transfers = transfers_govt_pce_per_capita) %>%
  select(GeoName, year, transfers, non_transfers)

df_transfers = rbind(df_transfers, df_transfers_USA)

df_johnstown = df_transfers %>%
  filter(GeoName != "Delaware, IN")%>%
  pivot_wider(names_from = GeoName,
              values_from = c("non_transfers", "transfers")) %>%
  rename(earnings_county = `non_transfers_Cambria, PA`,
         earnings_nation = `non_transfers_United States`,
         transfers_county = `transfers_Cambria, PA`,
         tranfsers_nation = `transfers_United States`)

df_muncie = df_transfers %>%
  filter(GeoName != "Cambria, PA") %>%
  pivot_wider(names_from = GeoName,
              values_from = c("non_transfers", "transfers")) %>%
  rename(earnings_county = `non_transfers_Delaware, IN`,
         earnings_nation = `non_transfers_United States`,
         transfers_county = `transfers_Delaware, IN`,
         tranfsers_nation = `transfers_United States`)

# save
write.csv(df_muncie, paste(path_out, "fig 13.csv", sep = "/"))
write.csv(df_johnstown, paste(path_out, "fig 14.csv", sep = "/"))
