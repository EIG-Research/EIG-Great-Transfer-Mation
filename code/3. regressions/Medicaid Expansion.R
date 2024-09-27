# Ben Glasner
# Tranformation Project
library(readxl)
library(tidyr)
library(dplyr)
library(fixest)
library(did)
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
path_data <- file.path(path_project, "data")
path_output <- file.path(path_project)


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
  mutate(med_year = as.numeric(med_year),
         Medicaid = case_when(
           year >= med_year ~ 1,
           year< med_year ~0,
           is.na(med_year) ~ 0,
           TRUE ~  NA
         ),
         CSDID_Treatment = case_when(
           !is.na(med_year) ~ med_year,
           is.na(med_year) ~ 0,
           TRUE ~ NA
         )) %>%
  group_by(GeoFIPS) %>% 
  arrange(GeoFIPS,year) %>%
  mutate(share_transfers_govt_personal_income = share_transfers_govt_personal_income*100,
         diff_share_transfers_govt_personal_income = share_transfers_govt_personal_income - dplyr::lag(share_transfers_govt_personal_income)) %>%
  fill(Percent.below.poverty.level, .direction = "down") %>%
  ungroup()




CSDID1 <- att_gt(yname="transfers_medical",
       gname="CSDID_Treatment",
       idname="GeoFIPS",
       tname="year",
       xformla=~employment_populaiton + population + Percent.below.poverty.level + share_65_over,
       data=regression_data,
       panel = TRUE,
       # anticipation = 1,
       # allow_unbalanced_panel = TRUE,
       control_group = "nevertreated",
       # control_group = "notyettreated",
       # weightsname = "pop_avg",
       alp = 0.05,
       bstrap = TRUE,
       cband = TRUE,
       biters = 1000,
       # clustervars = "state",
       # est_method = "reg",
       # print_details = FALSE,
       pl = TRUE)

dynamic1 <- aggte(CSDID1,
                      type = "dynamic",
                      # clustervars = "state",
                      balance_e = 4,min_e = -8,
                      na.rm = TRUE)

p <- ggdid(dynamic1) +
  geom_hline(yintercept = 0, color ="black") +
  labs(title = paste0("Change in Medical Transfers"))

plot(p)

dynamic1


CSDID2 <- att_gt(yname="transfer_share_Medicaid",
                 gname="CSDID_Treatment",
                 idname="GeoFIPS",
                 tname="year",
                 xformla=~employment_populaiton + population + Percent.below.poverty.level + share_65_over,
                 data=regression_data,
                 panel = TRUE,
                 # anticipation = 1,
                 # allow_unbalanced_panel = TRUE,
                 control_group = "nevertreated",
                 # control_group = "notyettreated",
                 # weightsname = "pop_avg",
                 alp = 0.05,
                 bstrap = TRUE,
                 cband = TRUE,
                 biters = 1000,
                 # clustervars = "state",
                 # est_method = "reg",
                 # print_details = FALSE,
                 pl = TRUE)

dynamic2 <- aggte(CSDID2,
                  type = "dynamic",
                  # clustervars = "state",
                  balance_e = 4,min_e = -8,
                  na.rm = TRUE)

p <- ggdid(dynamic2) +
  geom_hline(yintercept = 0, color ="black") +
  labs(title = paste0("Change in Medicaid Share of Transfers"))

plot(p)

dynamic2



CSDID3 <- att_gt(yname="share_transfers_govt_personal_income",
                 gname="CSDID_Treatment",
                 idname="GeoFIPS",
                 tname="year",
                 xformla=~employment_populaiton + population + Percent.below.poverty.level + share_65_over,
                 data=regression_data,
                 panel = TRUE,
                 # anticipation = 1,
                 # allow_unbalanced_panel = TRUE,
                 control_group = "nevertreated",
                 # control_group = "notyettreated",
                 # weightsname = "pop_avg",
                 alp = 0.05,
                 bstrap = TRUE,
                 cband = TRUE,
                 biters = 1000,
                 # clustervars = "state",
                 # est_method = "reg",
                 # print_details = FALSE,
                 pl = TRUE)

dynamic3 <- aggte(CSDID3,
                  type = "dynamic",
                  # clustervars = "state",
                  balance_e = 4,min_e = -8,
                  na.rm = TRUE)

p <- ggdid(dynamic3) +
  geom_hline(yintercept = 0, color ="black") +
  labs(title = paste0("Change in Government Transfers/Personal Income"))

plot(p)

dynamic3

egt <- as.data.frame(dynamic1$egt)
att.egt1 <- as.data.frame(dynamic1$att.egt)
se.egt1 <- as.data.frame(dynamic1$se.egt)
att.egt2 <- as.data.frame(dynamic2$att.egt)
se.egt2 <- as.data.frame(dynamic2$se.egt)
att.egt3 <- as.data.frame(dynamic3$att.egt)
se.egt3 <- as.data.frame(dynamic3$se.egt)


effect_estiamte <- as.data.frame(cbind(egt,
                    att.egt1,att.egt2,att.egt3,
                    se.egt1,se.egt2,se.egt3)) 

effect_estiamte <- effect_estiamte %>%
  mutate(`Change in Medical Transfers` = paste0(prettyNum(round(`dynamic1$att.egt`,0), big.mark = ",", scientific = FALSE)," (",prettyNum(round(`dynamic1$se.egt`,0), big.mark = ",", scientific = FALSE),")"),
         `Change in Medicaid Share of Transfers` = paste0(round(`dynamic2$att.egt`,3)," (",round(`dynamic2$se.egt`,3),")"),
         `Change in Government Transfers/Personal Income` = paste0(round(`dynamic3$att.egt`,3)," (",round(`dynamic3$se.egt`,3),")")) %>%
  rename(`Relative Timing` = `dynamic1$egt`) %>%
  select(`Relative Timing`,`Change in Medical Transfers`,`Change in Medicaid Share of Transfers`,`Change in Government Transfers/Personal Income`)

new_row <- data.frame(
  `Relative Timing` = "ATT",
  `Change in Medical Transfers` = paste0(prettyNum(round(dynamic1$overall.att,0), big.mark = ",", scientific = FALSE)," (",prettyNum(round(dynamic1$overall.se,0), big.mark = ",", scientific = FALSE),")"),
  `Change in Medicaid Share of Transfers` = paste0(round(dynamic2$overall.att,3)," (",round(dynamic2$overall.se,3),")"),
  `Change in Government Transfers/Personal Income` = paste0(round(dynamic3$overall.att,3)," (",round(dynamic3$overall.se,3),")")
)

names(new_row) <- c("Relative Timing","Change in Medical Transfers","Change in Medicaid Share of Transfers","Change in Government Transfers/Personal Income")

effect_estiamte <- rbind(effect_estiamte, new_row)

write_xlsx(effect_estiamte, "Medicaid_expansion_effect_csdid.xlsx")
