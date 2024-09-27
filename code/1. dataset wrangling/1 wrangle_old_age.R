# AUTHOR: Sarah Eckhardt
# PROJECT: Transfer Income

# DESCRIPTION: this file computes old-age (65+) population shares by county-year
#               using inter-census population estimates

# DATA SOURCES
# 1970-1979 
# https://www.census.gov/data/tables/time-series/demo/popest/pre-1980-county.html

# 1980-1989
# https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html

# 1990-1999 
# https://www.census.gov/data/tables/time-series/demo/popest/1990s-county.html

# 2000-2009 
# https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html

# ACS 5-year samples
# 2009-2022 population estimates by age for estimating old-age population share

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
path_data_raw = file.path(path_project,"data/raw")
path_data_population = file.path(path_data_raw, "population estimates")
path_data_out = file.path(path_project,"data")

############################################
## load pop data - share of population > 65
############################################
setwd(path_data_population)

# 1970-1979
# population by age, sex, race
df_1970_to_1979 <- read.csv("pop_1970_to_1979.csv") %>%
  mutate(population_under_65 = ages.0.4 + ages.5.9 + ages.10.14 + ages.15.19 
         + ages.20.24 + ages.24.29 + ages.30.34 + ages.35.39 + ages.40.44 +
           ages.45.49 + ages.50.54 + ages.55.59 + ages.60.64,
         population_over_65 = ages.65.69 + ages.70.74 + ages.75.79 + ages.80.84 + ages.85.and.older) %>%
  group_by(year, FIPS) %>%
  summarise(population_under_65 = sum(population_under_65),
            population_over_65 = sum(population_over_65)) %>%
  mutate(FIPS = str_pad(FIPS, 5, pad = "0"))



# 1980-1989
yearlist <- c(1980:1989)
df_list = list()
for(i in yearlist){
  df_list[[i]] = read_excel("pop_1980_to_1989.xls", skip = 5,
                     sheet = paste0(i)) %>%
    filter(!is.na(`Year of Estimate`)) %>%
    mutate(population_under_65 = `Under 5 years` + `5 to 9 years` + 
             `10 to 14 years` + `15 to 19 years` + `20 to 24 years` + 
             `25 to 29 years` + `30 to 34 years` + `35 to 39 years` + 
             `40 to 44 years` + `45 to 49 years` + `50 to 54 years` + 
             `55 to 59 years` + `60 to 64 years`,
           population_over_65 = `65 to 69 years` + `70 to 74 years` + 
             `75 to 79 years` + `80 to 84 years` + `85 years and over`) %>%
    rename(year = `Year of Estimate`, FIPS = `FIPS State and County Codes`) %>%
    group_by(year, FIPS) %>%
    summarise(population_under_65 = sum(population_under_65),
              population_over_65 = sum(population_over_65))
}
df_1980_to_1989 <- bind_rows(df_list)



# 1990-1999
yearlist <- c(1990:1999)
df_list = list()
for(i in yearlist){
  file = paste0("stch",i,".csv")
  df_list[[i]] <- read.csv(file)
}

df_1990_to_1999 <- bind_rows(df_list)

df_1990_to_1999 = df_1990_to_1999 %>%
  mutate(under_65 = 1*(agegroup<=13),
         FIPS = str_pad(county, 5, pad = "0")) %>%
  group_by(year, FIPS, under_65) %>%
  summarise(population = sum(pop))  %>%
  pivot_wider(names_from = under_65,
              values_from = population) %>%
  rename(population_under_65 = `1`,
         population_over_65 = `0`)



# 2000-2010
df_2000_to_2010 <- read.csv("pop_2000_2010.csv") %>%
  filter(AGEGRP !=0 & SEX==0) %>%
  mutate(under_65 = 1*(AGEGRP<=13),
         FIPS = paste0(str_pad(STATE, 2, pad = "0"), str_pad(COUNTY, 3, pad = "0"))) %>%
  select(FIPS, under_65, POPESTIMATE2000, POPESTIMATE2001,
         POPESTIMATE2002, POPESTIMATE2003, POPESTIMATE2004,
         POPESTIMATE2005, POPESTIMATE2006, POPESTIMATE2007,
         POPESTIMATE2008, POPESTIMATE2009, POPESTIMATE2010) %>%
  pivot_longer(cols = starts_with("POPESTIMATE"),
               names_to = "year", values_to = "population") %>%
  mutate(year= as.numeric(str_replace(year, "POPESTIMATE", ""))) %>%
  group_by(year, FIPS, under_65) %>%
  summarise(population = sum(population)) %>%
  pivot_wider(names_from = under_65, values_from = population) %>%
  rename(population_under_65 = `1`,
         population_over_65 = `0`)



# 2011-2019
# NOTE -- CC-EST2020-ALLDATA.csv IS TOO LARGE AFTER COMPRESSION FOR GITHUB STORAGE.
# FIND THE FILE HERE:
# https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html

df_2011_2019 = read.csv("CC-EST2020-ALLDATA.csv") %>%
  mutate(year = case_when(
    YEAR==4 ~ 2011,
    YEAR==5 ~ 2012,
    YEAR==6 ~ 2013,
    YEAR==7 ~ 2014,
    YEAR==8 ~ 2015,
    YEAR==9 ~ 2016,
    YEAR==10 ~ 2017,
    YEAR==11 ~ 2018,
    YEAR==12 ~ 2019,
  )) %>%
  filter(!is.na(year)) %>% # drop 2010 and 2020 estimates
  mutate(STATE = str_pad(STATE, 2, pad="0"),
         COUNTY = str_pad(COUNTY, 3,pad="0"),
         FIPS = paste0(STATE, COUNTY))

old = df_2011_2019 %>%
  filter(AGEGRP >=14) %>%
  ungroup() %>%
  group_by(FIPS, year) %>%
  summarise(population_over_65 = sum(as.integer(TOT_POP)))

all = df_2011_2019 %>%
  filter(AGEGRP == 0) %>%
  select(year, FIPS, TOT_POP)

df_2011_2019 = merge(old, all, by = c("year", "FIPS")) %>%
  mutate(population_under_65 = as.integer(TOT_POP)-population_over_65) %>%
  select(-c(TOT_POP))
rm(all, old)



# 2020-2022
df_2020_2022 = read.csv("cc-est2023-agesex-all.csv") %>%
  mutate(FIPS = paste0(str_pad(STATE, 2, pad="0"), 
                       str_pad(COUNTY, 3,pad="0"))) %>%
  mutate(year = case_when(
    YEAR==1 ~ 2020,
    YEAR==3 ~ 2021,
    YEAR==4 ~ 2022
  )) %>%
  filter(!is.na(year)) %>%
  mutate(population_under_65 = POPESTIMATE-AGE65PLUS_TOT) %>%
  rename(population_over_65 = AGE65PLUS_TOT) %>%
  select(year, FIPS, population_under_65,population_over_65)



#########################################################
# CT planning regions are reported for 2020-2022
# assume the old-age population for the most applicable planning region
# is equal to that of the county

setwd(path_data_raw)
ct_planning_regions = read_excel("ZIP-to-PlanningRegion.xlsx") %>%
  rename( FIPS = PlanningRegion_GeoID) %>%
  select(FIPS, PlanningRegion) %>%
  mutate(FIPS = str_pad(FIPS, 5, pad="0"))
ct_planning_regions=unique(ct_planning_regions)

ct_fips = merge(df_2020_2022, ct_planning_regions, by = "FIPS")

ct_planning_regions = read_excel("ct_planning_regions.xlsx")

# merge the ct 
ct_fips = merge(ct_fips, ct_planning_regions, by = c("FIPS", "PlanningRegion"))

ct_fips = ct_fips %>%
  filter(!is.na(GeoFIPS)) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, 5, pad="0")) %>%
  select(year, GeoFIPS, population_under_65, population_over_65) %>%
  rename(FIPS = GeoFIPS)

population = bind_rows(df_1970_to_1979, df_1980_to_1989,
                       df_1990_to_1999, df_2000_to_2010,
                       df_2011_2019, df_2020_2022, ct_fips) %>%
  mutate(total_population = population_under_65 + population_over_65,
         share_65_over = population_over_65/total_population) %>%
  select(-c(population_under_65)) %>%
  rename(GeoFIPS = FIPS)


rm(df_1970_to_1979, df_1980_to_1989,
   df_1990_to_1999, df_2000_to_2010,
   df_2011_2019, df_2020_2022,
   df_list)

# save output
write.xlsx(population, paste(path_data_out, "population_1970_to_2022.xlsx", sep = "/"))


# national population data
population_nation = population %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(total_population = sum(total_population, na.rm = TRUE),
            population_over_65 = sum(population_over_65, na.rm = TRUE)) %>%
  mutate(share_65_over = population_over_65/total_population)

write.xlsx(population, paste(path_data_out, "population_nation_1970_to_2022.xlsx", sep = "/"))
