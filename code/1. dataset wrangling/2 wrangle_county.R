# AUTHOR: Sarah Eckhardt
# PROJECT: Transfer Income

# DESCRIPTION: this file wrangles BEA CAINC4 and CAINC35 data files to construct
#             the master county-level file for EIG's 'Transfer-mation' project.
#             specific naming issues are addressed, and the old age population
#             estimates are merged in. see wrangle_old_age.R

#DATA SOURCE: https://apps.bea.gov/regional/downloadzip.cfm

# remove dependencies
rm(list = ls())

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(stringr)

# project paths
path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_clean = file.path(path_project,"data")

# load data and select desired line codes. combine.
linecodes_c4 = c("10", "20", "45", "46", "47","50","7010")

linecodes_c35 = c("2000", "2100", "2110", "2200", "2210", "2220", "2230", 
                  "2300", "2310", "2320", "2330", "2340", "2400", "2410", 
                  "2500", "2600", "2700", "5000", "3000", "4000")


# NOTE: CAINC4 IS TO LARGE A FILE FOR GITHUB, DOWNLOAD FROM THE BEA WEBSITE HERE:
# https://apps.bea.gov/regional/downloadzip.cfm, under "Personal Income"

df_cainc4 <- read.csv(paste(path_data_raw, "bea/CAINC4__ALL_AREAS_1969_2022.csv", sep = "/")) %>%
  filter( # includes United States totals
    LineCode %in% linecodes_c4) %>%
  mutate( # update county name with accent, causes merge errors
    GeoName = ifelse(GeoFIPS == " 35013", "Dona Ana, NM", GeoName)) 

  
df_cainc35 <- read.csv(paste(path_data_raw, "bea/CAINC35__ALL_AREAS_1969_2022.csv", sep = "/")) %>%
  filter(LineCode %in% linecodes_c35) %>%
  mutate( # update county name with accent, causes merge errors
    GeoName = ifelse(GeoFIPS == " 35013", "Dona Ana, NM", GeoName))

df_bea = bind_rows(df_cainc4, df_cainc35) # includes United States totals


# reshape
df_bea = df_bea %>%
  pivot_longer(
    cols = `X1969`:`X2022`,
    names_to = "year", 
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(gsub("X", "", year)),
         value = as.numeric(gsub("(NA)", "", value)),
         value = ifelse(Unit == "Thousands of dollars", value*1000, value)) %>%
  select(-c("Description", "Unit", "IndustryClassification", "TableName", "Region"))

# pivot wider and re-name
df_bea = df_bea %>%
  pivot_wider(
    names_from = LineCode,
    values_from = value) %>%
  rename(
    personal_income = `10`,
    population = `20`,
    net_earnings = `45`,
    dividends_interest_rent = `46`,
    transfers_all = `47`,
    wages_and_salaries = `50`,
    total_employment = `7010`,
    transfers_govt = `2000`,
    transfers_retirement_disability = `2100`,
    transfers_social_security = `2110`,
    transfers_medical = `2200`,
    transfers_medicare = `2210`,
    transfers_medicaid = `2220`,
    transfers_medical_military = `2230`,
    transfers_income_maintenance = `2300`,
    transfers_ssi = `2310`,
    transfers_eitc = `2320`,
    transfers_snap = `2330`,
    transfers_income_maintenance_other = `2340`,
    transfers_unempl_insurance = `2400`,
    transfers_state_unempl_insurance = `2410`,
    transfers_veterans = `2500`,
    transfers_education_training = `2600`,
    transfers_other = `2700`,
    transfers_refundable_tax_credits = `5000`,
    tranfers_non_for_profits = `3000`,
    transfers_businesses = `4000`) %>%
  separate(GeoName,c("county", "state"), sep = ",", remove = FALSE) %>% select(-c("county"))



#######
# fix naming issues

transfers = transfers %>%
  mutate(GeoName = gsub("\\*", "", GeoName),
         state= gsub("\\*", "", state),
         state = ifelse(state ==" Colonial Heights + Petersburg", "VA", state),
         state = ifelse(state ==" Fairfax City + Falls Church" , "VA", state),
         state = ifelse(state ==" Manassas + Manassas Park", "VA", state),
         state = ifelse(state ==" Staunton + Waynesboro", "VA", state),
         state = ifelse(state ==" Buena Vista + Lexington", "VA", state)) %>%
  mutate( # fix WI county boundary issue.
    drop = ifelse((GeoName == "Menominee, WI" & year < 1989) | 
                         (GeoName=="Shawano (includes Menominee), WI" & year >=1989), 
                       "drop", "keep")) %>%
  filter(drop =="keep") %>% # should not be there.
  select(-c(drop))



#######
# load PCE deflator for inflation adjustments - all dollars to be expressed in 2022 USD
df_pce = read_excel(paste(path_data_raw, "bea/BEA_deflator.xlsx", sep = "/")) %>% 
  rename(year = names(.)[1],
         pce_deflator = names(.)[3]) %>%
  select(-c("Gross domestic product")) %>%
  mutate(pce_deflator_2022 = (pce_deflator/116.043)*100) # / 2022 value

df_bea = left_join(df_bea, df_pce, by = "year") # ensures no drops



#######
# transformations for every variable.
transform_vars = names(df_bea)[7:32]

# inflation
df_bea = df_bea %>%
  mutate(across(transform_vars, 
                ~./pce_deflator_2022*100,
                .names = "{.col}_pce"))

# per cap transformations for every variable
df_bea = df_bea %>%
  mutate(across(transform_vars, 
                ~./population,
                .names = "{.col}_per_capita"))

# per capita inflation transformation
df_bea = df_bea %>%
  mutate(across(transform_vars, 
                ~./pce_deflator_2022*100/population,
                .names = "{.col}_pce_per_capita"))

# additional variables required
df_bea = df_bea %>%
  mutate(employ_to_pop_ratio = total_employment/population,
         share_transfers_govt_personal_income =  transfers_govt/personal_income, # the main ratio in question
         transfer_tiers = case_when(
           share_transfers_govt_personal_income<0.15 ~ "minimal (<15%)",
           share_transfers_govt_personal_income >=0.15 & 
             share_transfers_govt_personal_income <0.25 ~ "moderate (15-24.9%)",
           share_transfers_govt_personal_income >=0.25 ~ "significant (25%+)"))



#######
# fix VA FIPS codes
# note: CT not updated. when mapping, use a historical US county map overlay. 
fips_crosswalk = read_excel(paste(path_data_raw, "fips_crosswalk.xlsx", sep="/"))

df_bea = left_join(df_bea, fips_crosswalk, by = "GeoName")

df_bea = df_bea %>%
  mutate(
    GeoFIPS = ifelse(is.na(new_FIPS),GeoFIPS,new_FIPS)) %>%
  select(-c("new_FIPS", "error_FIPS"))


#######
# merge in old-age population share. from wrangle_old_age.do

population_old = read_excel(paste(path_data_out, 
                                  "population_1970_to_2022.xlsx", sep = "/"))%>%
  mutate(GeoFIPS = str_trim(GeoFIPS), year= str_trim(year))

df_bea =  df_bea %>% mutate(GeoFIPS = str_trim(GeoFIPS), year = str_trim(year))

df_bea = left_join(df_bea, population_old, by = c("year", "GeoFIPS"))


#######
# merge in MSA codes
classification = read_excel(paste(path_data_raw, "nchs/Ruralurbancontinuumcodes2023.xlsx",
                                  sep = "/")) %>%
  select(FIPS, RUCC_2023, Description) %>%
  mutate(urban_status = case_when(
    RUCC_2023 == 1 ~ "large metro (1 million +)",
    RUCC_2023 == 2 ~ "medium metro (250k-1 million)",
    RUCC_2023 == 3 ~ "small metro (<250k)",
    RUCC_2023 >3 & RUCC_2023 <=9 ~ "non-metro" 
  )) %>%
  rename(GeoFIPS = `FIPS`) %>%
  select(-c(RUCC_2023, Description))

df_bea = left_join(df_bea, classification, by ="GeoFIPS")




# drop non county observations, drop 1969
df_bea = df_bea %>%
  filter(!is.na(state)) %>% # drops states, USA totals, and regions.
  filter(year>1969)


# export data set
write.xlsx(df_bea, paste(path_data_out, 'transfers_dataset_counties_master.xlsx', sep = "/"))

