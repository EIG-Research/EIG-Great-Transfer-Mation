# AUTHOR: Sarah Eckhardt
# PROJECT: Transfer Income

# DESCRIPTION: this file compiles poverty rates by decade for analysis

# DATA SOURCE: poverty estimates by county - decade markers.
# https://www.census.gov/data/tables/time-series/dec/census-poverty.html

# remove dependencies
rm(list = ls())

#load packages
library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(usmap)
library(stringr)
library(tidycensus)

# set project paths
path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw/poverty estimates")
path_data_out = file.path(path_project,"data")
path_out = file.path(path_project,"output")

setwd(path_data_raw)

##############
# read in data

# 1970
p1970 = read_excel("1970-Census-by-County.xls",
                   skip = 4) %>%
  filter(!is.na(State)) %>%
  mutate(GeoFIPS = str_pad(`State/ County Codes`, 5, pad = "0")) %>%
  select(GeoFIPS, `Percent below poverty level`) %>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 1970)

# 1980
p1980 = read_excel("1980-Census-by-County.xls",
                   skip = 4) %>%
  filter(!is.na(State)) %>%
  mutate(GeoFIPS = str_pad(`State/ County Codes`, 5, pad = "0")) %>%
  select(GeoFIPS, `Percent below poverty level`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 1980)

# 1990
p1990 = read_excel("1990-Census-by-County.xls",
                   skip = 4)%>%
  filter(!is.na(STATE)) %>%
  rename(State = STATE,
         `County Name` = `COUNTY`,
         `State/ County Codes` = `STATE/ COUNTY CODE`,
         `Percent below poverty level` = `POVERTY RATE`) %>%
  mutate(GeoFIPS = str_pad(`State/ County Codes`, 5, pad = "0")) %>%
  select(GeoFIPS, `Percent below poverty level`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 1990)

# 2000
p2000 = read_excel("2000-Census-by-County.xls",
                   skip = 4) %>%
  filter(!is.na(State)) %>%
  mutate(GeoFIPS = str_pad(`State/ County Codes`, 5, pad = "0")) %>%
  select(GeoFIPS, `Percent below poverty level`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 2000)

# 2010
p2010 = read_excel("est10all.xls",
                   skip = 2) %>%
  filter(`County FIPS`!=0) %>%
  mutate(GeoFIPS = paste0(`State FIPS`, str_pad(`County FIPS`, 3, pad = "0"))) %>%
  select(GeoFIPS, `Poverty Percent All Ages`) %>%
  rename (`Percent below poverty level` = `Poverty Percent All Ages`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 2010)

# 2020
p2020 = read_excel("est20all.xls",
                   skip = 3) %>%
  filter(`County FIPS Code`!="000") %>%
  mutate(GeoFIPS = paste0(`State FIPS Code`, `County FIPS Code`))%>%
  select(GeoFIPS, `Poverty Percent, All Ages`) %>%
  rename (`Percent below poverty level` = `Poverty Percent, All Ages`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 2020)


# 2022
p2022 = read_excel("est22all.xls",
                         skip = 3) %>%
  mutate(GeoFIPS = paste0(`State FIPS Code`, `County FIPS Code`)) %>%
  select(GeoFIPS,`Poverty Percent, All Ages`)%>%
  rename (`Percent below poverty level` = `Poverty Percent, All Ages`)%>%
  mutate(`Percent below poverty level` = round(as.numeric(`Percent below poverty level`),2),
         year = 2022)

# append all poverty estimates.
poverty = bind_rows(p1970, p1980, p1990, p2000, p2010, p2020, p2022) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, 5, pad="0"))

write.csv(poverty, paste(path_data_out, "poverty_rates.csv", sep="/"))
