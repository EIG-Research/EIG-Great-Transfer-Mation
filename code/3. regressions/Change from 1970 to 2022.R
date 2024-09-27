# Ben Glasner
# Tranformation Project
library(readxl)
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(writexl)

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
path_data <- file.path(path_project, "data")
path_output <- file.path(path_project)

# Set working directory for CEPR data
setwd(path_data)

################################
# Load data cleaned by Sarah
transfers_dataset <- read_excel("transfers_dataset_counties_master.xlsx")

poverty_rates <- read.csv("poverty_rates.csv")

################################
# Select relevant data for regression analysis
regression_data  <- transfers_dataset  %>%
  filter(!is.na(state)) %>% 
  filter(year>=2000) %>%
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
    year = as.integer(year),
    period = as.integer(year - 1969),
  )  %>% 
  left_join(poverty_rates) %>%
  select(year,GeoFIPS,state,st,period,
         transfers_all:transfers_refundable_tax_credits,
         share_transfers_govt_personal_income,transfers_govt_per_capita,personal_income_pce_per_capita,
         transfers_ssi,transfers_eitc,transfers_snap,transfers_income_maintenance_other,
         transfers_share_income_maintenance:transfer_share_Social_security,
         population,
         population_over_65, share_65_over, Percent.below.poverty.level,employment_populaiton
  ) %>%
  group_by(GeoFIPS) %>% 
  arrange(GeoFIPS,year) %>%
  fill(Percent.below.poverty.level, .direction = "down") %>%
  ungroup()

regression_data_plm <- pdata.frame(regression_data, index = c("GeoFIPS","period"))

regression_data_plm$Change_share_transfers_govt_personal_income <- regression_data_plm$share_transfers_govt_personal_income - lag(regression_data_plm$share_transfers_govt_personal_income,k = 1, shift = c("time"))
regression_data_plm$Change_transfers_govt_per_capita <- regression_data_plm$transfers_govt_per_capita - lag(regression_data_plm$transfers_govt_per_capita,k = 1, shift = c("time"))
regression_data_plm$Change_personal_income_pce_per_capita <- regression_data_plm$personal_income_pce_per_capita - lag(regression_data_plm$personal_income_pce_per_capita,k = 1, shift = c("time"))

regression_data_plm$Change_transfers_share_income_maintenance <- regression_data_plm$transfers_share_income_maintenance - lag(regression_data_plm$transfers_share_income_maintenance,k = 1, shift = c("time"))
regression_data_plm$Change_transfer_share_veterans <- regression_data_plm$transfer_share_veterans - lag(regression_data_plm$transfer_share_veterans,k = 1, shift = c("time"))
regression_data_plm$Change_transfer_share_unemployment_ins <- regression_data_plm$transfer_share_unemployment_ins - lag(regression_data_plm$transfer_share_unemployment_ins,k = 1, shift = c("time"))
regression_data_plm$Change_transfer_share_Medicaid <- regression_data_plm$transfer_share_Medicaid - lag(regression_data_plm$transfer_share_Medicaid,k = 1, shift = c("time"))
regression_data_plm$Change_transfer_share_Medicare <- regression_data_plm$transfer_share_Medicare - lag(regression_data_plm$transfer_share_Medicare,k = 1, shift = c("time"))
regression_data_plm$Change_transfer_share_Social_security <- regression_data_plm$transfer_share_Social_security - lag(regression_data_plm$transfer_share_Social_security,k = 1, shift = c("time"))

regression_data_plm$Change_log_population <- (log(regression_data_plm$population) - log(lag(regression_data_plm$population,k = 1, shift = c("time"))))
regression_data_plm$Change_share_65_over <- regression_data_plm$share_65_over - lag(regression_data_plm$share_65_over,k = 1, shift = c("time"))
regression_data_plm$Change_Percent.below.poverty.level <- regression_data_plm$Percent.below.poverty.level - lag(regression_data_plm$Percent.below.poverty.level,k = 1, shift = c("time"))
regression_data_plm$Change_employment_populaiton <- regression_data_plm$employment_populaiton - lag(regression_data_plm$employment_populaiton,k = 1, shift = c("time"))

regression_data <- as.data.frame(regression_data_plm) %>% 
  select(year,period,GeoFIPS,state,year,period,GeoFIPS,state,
         share_transfers_govt_personal_income,transfers_govt_per_capita,personal_income_pce_per_capita,
         Change_share_transfers_govt_personal_income,Change_transfers_govt_per_capita,Change_personal_income_pce_per_capita,
         Change_transfers_share_income_maintenance:Change_Percent.below.poverty.level,Change_employment_populaiton
  ) %>%
  filter(year == 2022) %>%
  mutate(across(Change_transfers_share_income_maintenance:Change_transfer_share_Social_security,
                ~ .*100,
                .names = "{.col}"),
         across(share_transfers_govt_personal_income,
                ~ .*100,
                .names = "{.col}"),
         across(Change_share_65_over,
                ~ .*100,
                .names = "{.col}"))

################################
# Run the models - transfer/income

model_1a <- feols(Change_share_transfers_govt_personal_income ~ Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)

model_1b <- feols(Change_share_transfers_govt_personal_income ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security | state,
                  data = regression_data)

model_1c <- feols(Change_share_transfers_govt_personal_income ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security +
                    Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)


table1 <- etable(model_1a, 
                 model_1b, 
                 model_1c)

################################
# Run the models - transfer

model_2a <- feols(Change_transfers_govt_per_capita ~ Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)

model_2b <- feols(Change_transfers_govt_per_capita ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security | state,
                  data = regression_data)

model_2c <- feols(Change_transfers_govt_per_capita ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security +
                    Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)


table2 <- etable(model_2a, 
                 model_2b, 
                 model_2c)

################################
# Run the models - per capita income

model_3a <- feols(Change_personal_income_pce_per_capita ~ Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)

model_3b <- feols(Change_personal_income_pce_per_capita ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security | state,
                  data = regression_data)

model_3c <- feols(Change_personal_income_pce_per_capita ~ Change_transfers_share_income_maintenance + 
                    Change_transfer_share_Medicaid + 
                    Change_transfer_share_Medicare + 
                    Change_transfer_share_Social_security +
                    Change_share_65_over + 
                    Change_employment_populaiton + 
                    Change_Percent.below.poverty.level + 
                    Change_log_population | state,
                  data = regression_data)


table3 <- etable(model_3a, 
                 model_3b, 
                 model_3c)



# Combine all data frames into one list with named elements
all_dfs <- list(
  "transfer as share of income" = table1,
  "transfers per capita" = table2,
  "income per capita" = table3
)

# Write to an Excel file
write_xlsx(all_dfs, path = paste0(path_output, "/regression results.xlsx"))
