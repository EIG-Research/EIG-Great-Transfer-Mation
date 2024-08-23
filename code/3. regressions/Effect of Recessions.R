# Ben Glasner
# Tranformation Project
library(readxl)
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
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
path_output <- file.path(path_project, "output")


# Set working directory for CEPR data
setwd(path_data)

################################
# Load data cleaned by Sarah
transfers_dataset <- read_excel("transfers_dataset.xlsx")

poverty_rates <- read.csv("poverty_rates.csv")

################################
# Select relevant data for regression analysis
regression_data  <- transfers_dataset  %>%
  filter(!is.na(state)) %>% 
  filter(GeoFIPS != "55078") %>% # Check out what is going on here
  mutate(GeoFIPS = as.integer(GeoFIPS),
         recession = case_when(
           year %in% c(1973:1975,1980,1981:1982,1990:1991,2001,2007:2009,2020) ~ 1,
           !year %in% c() ~ 0,
           TRUE ~ NA,
         ),
         period = year - 1969)  %>% 
  left_join(poverty_rates) %>%
  select(year,period,GeoFIPS,in_msa,state,
         recession,
         share_transfers_govt_personal_income,
         population,
         Percent.below.poverty.level,per_capita_income_pce
  )


regression_data_plm <- pdata.frame(regression_data, index = c("GeoFIPS","period"))
duplicates  <- as.data.frame(table(index(regression_data_plm), useNA = "ifany")) %>%
  filter(Freq == 2)

regression_data_plm <- regression_data %>%
  filter(!GeoFIPS %in% duplicates$GeoFIPS)
regression_data_plm <- pdata.frame(regression_data, index = c("GeoFIPS","period"))

regression_data_plm$recession_lag1 <- lag(regression_data_plm$recession,k = 1, shift = c("time"))
regression_data_plm$recession_lag2 <- lag(regression_data_plm$recession,k = 2, shift = c("time"))
regression_data_plm$recession_lag3 <- lag(regression_data_plm$recession,k = 3, shift = c("time"))
regression_data_plm$recession_lag4 <- lag(regression_data_plm$recession,k = 4, shift = c("time"))
regression_data_plm$recession_lag5 <- lag(regression_data_plm$recession,k = 5, shift = c("time"))

regression_data <- as.data.frame(regression_data_plm)

model_1 <- feols(share_transfers_govt_personal_income ~ recession + 
                   recession_lag1 + 
                   recession_lag2 +
                   recession_lag3 + 
                   recession_lag4 +
                   recession_lag5 + year,
                  data = regression_data)
summary(model_1)


simulation <- regression_data %>% 
  select(recession,recession_lag1:recession_lag5,year) %>% 
  distinct() %>%
  filter(year>=1980)

simulation$predictions <- predict(model_1, simulation)

simulation %>%
  ggplot(aes(x = year, y = predictions)) + 
  geom_line()
