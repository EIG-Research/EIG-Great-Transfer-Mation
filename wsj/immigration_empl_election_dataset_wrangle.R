# PROJECT: The Great Transfer-Mation

# DESCRIPTION: this file compiles immigration, employment-to-population ratios, 
# election results, and topline economic and transfer characters for counties,
# 2000-2022.

# LAST UPDATED: Sept-12-2024
# LAST UPDATED BY: Sarah Eckhardt

# DATA SOURCES:
    # transfers county-level dataset, wrangled by EIG, sourced from BEA CAINC4 and CAINC35 files
    # employment to population ratios and immigration population shares from ACS 5-year files
    # and a 5% sample of the 2000 census

# note that the ACS does not fully cover counties prior to 2010. Neither does the 2000 5% sample


######################
# set-up

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
project_directories <- list(
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/EIG-Great-Transfer-Mation",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/EIG-Great-Transfer-Mation",
  "sarah" = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive/PROJECTS/ACTIVE/TRANSFERS"
)

# Set project path based on the current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_code = file.path(path_project, "code")
path_data = file.path(path_project, "data")
path_output = file.path(path_project, "output", "WSJ")


path_data_acs_immig = file.path(path_project,"data/raw/acs immigration/productDownload_2024-09-09T095338")
path_data_acs_empl = file.path(path_project,"data/raw/acs employment/productDownload_2024-09-09T122808")
path_data = file.path(path_project,"data/clean")


# read in immigration data
setwd(path_data_acs_immig)

immig_list = list()
for (year in c(2010:2022)) {
  
  file_name = paste0("ACSDT5Y", year, ".B05012","-Data.csv")
  
  print(file_name)
  
  # handle differing name patterns
  if (year<2019) {
      file = read.csv(file_name,
                      skip = 1) %>%
        mutate(
          `% of the Population Foreign-Born` =
            as.numeric(Estimate..Total..Foreign.Born)/as.numeric(Estimate..Total),
          year = year) %>%
        select(year, 
               Geography, 
               `% of the Population Foreign-Born`)
  }
  
  if (year>=2019) {
    file = read.csv(file_name,
                    skip = 1) %>%
      mutate(
        `% of the Population Foreign-Born` =
          as.numeric(Estimate..Total...Foreign.Born)/as.numeric(Estimate..Total.),
      year = year) %>%
      select(year, 
             Geography, 
             `% of the Population Foreign-Born`)
  }
  
  
  immig_list[[year]] = file
  

}

immig_df = bind_rows(immig_list) %>%
  mutate(GeoFIPS = gsub(".*US", "", Geography)) %>%
  select(-c(Geography))
  

# read in 2000-2009 data
setwd(paste(path_project,"data/raw/acs immigration",sep="/"))
immig_2000_2009 = read.csv("immig2000_2009.csv") %>%
  pivot_wider(names_from = "immig", values_from = "pop") %>%
  mutate(GeoFIPS = paste0(str_pad(statefip2,2, pad="0"), 
                          str_pad(countyfip,3, pad="0"))) %>%
  mutate(`% of the Population Foreign-Born` = `1` / (`1` + `0`)) %>%
  select(year, GeoFIPS,`% of the Population Foreign-Born`)

immig_df = bind_rows(immig_df, immig_2000_2009)


rm(immig_list, file, immig_2000_2009)


##############################
# emp to pop ratios
setwd(path_data_acs_empl)

empl_list = list()
for (year in c(2010:2022)) {
  
  file_name = paste0("ACSDT5Y", year, ".B23001","-Data.csv")
  
  if(year<2019) {
  file = read.csv(file_name, skip = 1)  %>%
    select(Geography, Estimate..Total, contains("In.labor.force..Civilian..Employed"), -c(contains("Margin.of.Error"))) %>%
    mutate(across(contains("Estimate"),
                  ~as.numeric(.)))
  
  file$employed = rowSums(file[,c(3:22)],na.rm=TRUE)
  
  file = file %>%
    mutate(emp_pop_ratio = employed/Estimate..Total,
           GeoFIPS = gsub(".*US", "", Geography)) %>%
    select(GeoFIPS, emp_pop_ratio) %>%
    mutate(year = year)
    
  }
  
  if(year>=2019) {
    file = read.csv(file_name, skip = 1) %>%
      select(Geography, Estimate..Total., contains("In.labor.force...Civilian...Employed"), -c(contains("Margin.of.Error"))) %>%
      mutate(across(contains("Estimate"),
                    ~as.numeric(.)))
    
    file$employed = rowSums(file[,c(3:22)],na.rm=TRUE)
    
    file = file %>%
      mutate(emp_pop_ratio = employed/Estimate..Total.,
             GeoFIPS = gsub(".*US", "", Geography)) %>%
      select(GeoFIPS, emp_pop_ratio) %>%
      mutate(year = year)
    
  }
  
  
  
  empl_list[[year]] = file
  
  
}

empl_df = bind_rows(empl_list)


##########
# earlier emp/pop data.
setwd(paste(path_project,"data/raw/acs employment",sep="/"))
empl_2000_2009 = read.csv("empl_pop_2000_2009.csv") %>%
  pivot_wider(names_from = "employed", values_from = "pop") %>%
  mutate(GeoFIPS = paste0(str_pad(statefip2,2, pad="0"), 
                          str_pad(countyfip,3, pad="0"))) %>%
  mutate(emp_pop_ratio = `1` / (`1` + `0`)) %>%
  select(year, GeoFIPS,emp_pop_ratio)


empl_df = bind_rows(empl_df, empl_2000_2009)



########
# full transfers data
dataset = read_excel(paste(path_data, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  select(GeoFIPS, GeoName, year, population, share_65_over,
         transfer_tiers,
         personal_income_pce,
         personal_income_pce_per_capita,
         wages_and_salaries_pce,
         wages_and_salaries_pce_per_capita,
         dividends_interest_rent_pce,
         dividends_interest_rent_pce_per_capita,
         transfers_govt_pce,
         transfers_govt_pce_per_capita,
         transfers_social_security_pce,
         transfers_medicare_pce,
         transfers_medicaid_pce,
         transfers_income_maintenance_pce,
         transfers_unempl_insurance_pce,
         transfers_govt_pce_per_capita,
         transfers_social_security_pce_per_capita,
         transfers_medicare_pce_per_capita,
         transfers_medicaid_pce_per_capita,
         transfers_income_maintenance_pce_per_capita,
         transfers_unempl_insurance_pce_per_capita,
         share_transfers_govt_personal_income
  ) %>%
  filter(year >= 2000) %>%
  mutate(year = as.numeric(year))


# merge in election data
election = read_excel("/Users/sarah/Downloads/countypres_2000-2020.xlsx") %>%
  mutate(GeoFIPS = str_pad(county_fips, pad="0", 5)) %>%
  filter(office =="US PRESIDENT") %>%
  group_by(year, GeoFIPS, party) %>%
  mutate(total_candidate_votes = sum(candidatevotes)) %>%
  mutate(candidate_share = total_candidate_votes/totalvotes) %>% # candidate's votign share
  filter(party %in% c("REPUBLICAN", "DEMOCRAT")) %>%
  select(GeoFIPS, year, candidate_share, party) %>%  filter(!is.na(GeoFIPS))

election = unique(election)


election = election %>%
  pivot_wider(id_cols = c("year", "GeoFIPS"),
              names_from = "party",
              values_from = "candidate_share") %>%
  mutate(winner = case_when(
    DEMOCRAT > REPUBLICAN ~ "Democrat",
    DEMOCRAT < REPUBLICAN ~ "Republican"
  ))


all_other = read_excel(paste(path_data,
                             "elections",
                             "DN House election results by county 2008-2022.xlsx",
                             sep="/")) %>%
  select(-c("Quintile (5=Distressed)", "State-county")) %>% 
  mutate(GeoFIPS = str_pad(FIPS, 5, pad="0")) %>%
  select(-c(FIPS))  %>%
  rename(`2008 R House share` =`R 2008 House share`,
         `2008 D House share` = `D 2008 House share`) %>%
  pivot_longer(cols = c("2008 House D", "2008 House R", "2008 Total House", "2008 R House share", 
                        "2008 D House share",
                        "2010 House D", "2010 House R", "2010 Total House", "2012 House D", "2012 House R",
                        "2012 Total House", "2014 House D", "2014 House R", "2014 Total House", 
                        "2016 House D", 
                        "2016 House R", "2016 Total House", "2018 House D", "2018 House R", 
                        "2018 Total House",
                        "2020 House D", "2020 House R", "2020 Total House", "2022 House D", 
                        "2022 House R", "2022 Total House")) %>%
  mutate(year = as.numeric(substr(name,1,4)),
         election = substr(name, 6, nchar(name))) %>%
  select(-c(name)) %>%
  pivot_wider(names_from = "election", values_from = "value") %>%
  select(-c(contains("share")))



  
# merge together
dataset_merged = left_join(dataset, immig_df, 
                    by = c("GeoFIPS", "year"))

dataset_merged = left_join(dataset_merged, election, by = c("GeoFIPS", "year"))
dataset_merged = left_join(dataset_merged, empl_df, by = c("GeoFIPS", "year"))
dataset_merged = left_join(dataset_merged, all_other, by = c("GeoFIPS", "year"))

# export for analysis
setwd(path_data)
write.csv(dataset_merged, "wsj_immig_election_emp.csv")
