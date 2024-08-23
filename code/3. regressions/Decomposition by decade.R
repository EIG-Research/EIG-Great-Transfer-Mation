# Ben Glasner
# Tranformation Project
library(readxl)
library(tidyr)
library(dplyr)
library(fixest)
# library(did)
library(writexl)
library(ggplot2)
options(scipen=1000000)
set.seed(42)
#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "bglasner" = "C:/Users/bglasner/EIG Dropbox/Benjamin Glasner/GitHub/transfer-income",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/transfer-income",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/transfer-income"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]

# Define paths to data and output directories
path_data <- file.path(path_project, "Data")
path_output <- file.path(path_project, "Output")


# Set working directory for CEPR data
setwd(path_data)

################################
# Load data cleaned by Sarah
transfers_dataset <- read_excel("transfers_dataset_counties_master.xlsx")

poverty_rates <- read.csv("poverty_rates.csv")
medicaid <- read_excel("medicaid.xlsx")

medicaid <- medicaid %>% select(st, `State Name`, med_year) %>% distinct()

################################
# Select relevant data for regression analysis
regression_data  <- transfers_dataset  %>%
  filter(!is.na(state)) %>% 
  # filter(year>=2000) %>%
  filter(GeoFIPS != "55078") %>% # Check out what is going on here
  mutate(transfers_share_income_maintenance = case_when(
    year == 1970 ~ (transfers_ssi + transfers_snap + transfers_income_maintenance_other)/transfers_govt,
    year == 2022 ~ (transfers_ssi + transfers_eitc + transfers_snap + transfers_income_maintenance_other)/transfers_govt,
    TRUE ~ NA),
    transfer_share_veterans = 100*transfers_veterans/transfers_govt,
    transfer_share_unemployment_ins = 100*transfers_unempl_insurance/transfers_govt,
    transfer_share_Medicaid = 100*transfers_medicaid/transfers_govt,
    transfer_share_Medicare = 100*transfers_medicare/transfers_govt,
    transfer_share_Social_security = 100*transfers_social_security/transfers_govt,
    st = as.double(stringr::str_sub(GeoFIPS, 1, 2)),
    GeoFIPS = as.integer(GeoFIPS),
    employment_populaiton = 100*(total_employment/population),
    year = as.integer(year)
  )  %>% 
  left_join(poverty_rates) %>%
  select(year,GeoFIPS,state,st,
         transfers_all:transfers_refundable_tax_credits,
         share_transfers_govt_personal_income,transfers_govt_per_capita,personal_income_pce_per_capita,
         transfers_ssi,transfers_eitc,transfers_snap,transfers_income_maintenance_other,
         transfers_share_income_maintenance:transfer_share_Social_security,
         population,
         population_over_65, share_65_over, Percent.below.poverty.level,employment_populaiton
  ) %>%
  left_join(medicaid) %>%
  mutate(log_population = log(population),
         med_year = as.numeric(med_year),
         Medicaid = case_when(
           year >= med_year ~ 1,
           year< med_year ~0,
           is.na(med_year) ~ 0,
           TRUE ~  NA
         ),
         decade = case_when(
           year >= 1970 & year <= 1979 ~ "1970s",
           year >= 1980 & year <= 1989 ~ "1980s",
           year >= 1990 & year <= 1999 ~ "1990s",
           year >= 2000 & year <= 2009 ~ "2000s",
           year >= 2010 & year <= 2019 ~ "2010s",
           year >= 2020 & year <= 2023 ~ "2020s",
           TRUE ~ "Outside Range"  # Default case if year is outside the specified range
         ),
         share_65_over = share_65_over*100) %>%
  group_by(GeoFIPS) %>% 
  arrange(GeoFIPS,year) %>%
  mutate(share_transfers_govt_personal_income = share_transfers_govt_personal_income*100,
         diff_share_transfers_govt_personal_income = share_transfers_govt_personal_income - dplyr::lag(share_transfers_govt_personal_income)) %>%
  fill(Percent.below.poverty.level, .direction = "down") %>%
  ungroup() %>%
  select(year,decade,GeoFIPS,state,st,
         share_transfers_govt_personal_income,transfers_govt_per_capita,personal_income_pce_per_capita,
         share_65_over,Percent.below.poverty.level,employment_populaiton,log_population)


################################
# Fixed Effect Regression Results

feols_1 <- feols(share_transfers_govt_personal_income ~ share_65_over*decade + 
                   Percent.below.poverty.level*decade +
                   employment_populaiton + log_population | year,
                 cluster = c("year","st"),
                 data = regression_data)


feols_2 <- feols(transfers_govt_per_capita ~ share_65_over*decade + 
                   Percent.below.poverty.level*decade +
                   employment_populaiton + log_population | year,
                 cluster = c("year","st"),
                 data = regression_data)


feols_3 <- feols(personal_income_pce_per_capita ~ share_65_over*decade + 
                   Percent.below.poverty.level*decade +
                   employment_populaiton + log_population | year,
                 cluster = c("year","st"),
                 data = regression_data)



table1 <- etable(feols_1, 
                 feols_2, 
                 feols_3)

setwd(path_output)
write_xlsx(table1, "decade_decomp.xlsx")

################################
# median change in the share of the population aged 65 or older across counties was 7.6 percent, from 1970 to 2022

regression_data %>%
  filter(year %in% c(1970,2022)) %>%
  select(year,GeoFIPS,share_65_over) %>%
  group_by(GeoFIPS) %>%
  arrange(year) %>% 
  mutate(change_share_65 = share_65_over - lag(share_65_over)) %>%
  ungroup() %>%
  filter(year == 2022) %>% 
  summarise(median_change_share_65_over = median(change_share_65, na.rm = TRUE))

