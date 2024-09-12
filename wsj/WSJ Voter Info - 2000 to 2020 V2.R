# What are some of the most interesting local stories?
# find counties that have seen significant changes in their non-transfer income, transfer income, and political alignment 

# Last updated: September 9, 2024

rm(list = ls())  # Clear the workspace

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(readr)
library(plotly)

#################
### Set paths ###
#################

# Define project directories based on the user
project_directories <- list(
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/EIG-Great-Transfer-Mation",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/EIG-Great-Transfer-Mation"
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

# Define colors for political parties and flipped results
party_colors <- c("Rep Win" = "#E9141D", "Flipped Rep" = "#FFA500", "Dem Win" = "#2E74C0", "Flipped Dem" = "#800080")

#################
### Load Data ###
#################

# Load and process data: filter for 2000 and 2020, compute changes, and identify flipped results
transfers_pres_vote <- read_csv(file.path(path_data, "wsj_immig_election_emp.csv")) %>%
  filter(year %in% c(2000, 2020)) %>%
  arrange(year) %>%
  group_by(`GeoFIPS`) %>%
  mutate(State = stringr::str_sub(GeoFIPS, end = 2L),
         `Republican Share` = REPUBLICAN,
         Winner = case_when(REPUBLICAN > DEMOCRAT ~ "Rep Win", 
                            REPUBLICAN < DEMOCRAT ~ "Dem Win", 
                            TRUE ~ NA),
         Flipped = case_when(Winner == "Rep Win" & lag(Winner) == "Rep Win" ~ "Rep Win",
                             Winner == "Rep Win" & lag(Winner) == "Dem Win" ~ "Flipped Rep",
                             Winner == "Dem Win" & lag(Winner) == "Rep Win" ~ "Flipped Dem",
                             Winner == "Dem Win" & lag(Winner) == "Dem Win" ~ "Dem Win",
                             TRUE ~ NA),
         # Calculate changes in demographics, income, and political alignment
         `Change in the Share of County Population 65+` = share_65_over - lag(share_65_over),
         `Change in Government Transfers as a Share of Total Income` = (share_transfers_govt_personal_income - lag(share_transfers_govt_personal_income)) * 100,
         `Change in the Per Capita Total Income from All Sources` = personal_income_pce_per_capita - lag(personal_income_pce_per_capita),
         `Change in the Per Capita Government Transfers` = transfers_govt_pce_per_capita - lag(transfers_govt_pce_per_capita),
         `Per Capita Total Income Excluding Government Transfers` = personal_income_pce_per_capita - transfers_govt_pce_per_capita,
         `Change in the Per Capita Total Income Excluding Government Transfers` = `Per Capita Total Income Excluding Government Transfers` - lag(`Per Capita Total Income Excluding Government Transfers`),
         `Change in the % of the Population Foreign-Born` = `% of the Population Foreign-Born` - lag(`% of the Population Foreign-Born`),
         `Change in the Pres Republican Share` = (`Republican Share` - lag(`Republican Share`)) * 100) %>%
  rename(`Share of County Population 65+` = share_65_over,
         `Government Transfers as a Share of Total Income` = share_transfers_govt_personal_income,
         `Per Capita Total Income from All Sources` = personal_income_pce_per_capita,
         `Per Capita Government Transfers` = transfers_govt_pce_per_capita) %>%
  select(`GeoName`, State, year, `population`, 
         `Share of County Population 65+`, `Change in the Share of County Population 65+`,
         `Government Transfers as a Share of Total Income`, `Change in Government Transfers as a Share of Total Income`,
         `Per Capita Total Income from All Sources`, `Change in the Per Capita Total Income from All Sources`,
         `Per Capita Government Transfers`, `Change in the Per Capita Government Transfers`,
         `Per Capita Total Income Excluding Government Transfers`, `Change in the Per Capita Total Income Excluding Government Transfers`,
         `Republican Share`, `Change in the Pres Republican Share`, Winner, Flipped)

########################################
### Plotting Function for Scatter Plots ###
########################################
plot_list <- list()
# Define a reusable function to create scatter plots
create_scatter_plot <- function(data, x_var, y_var, color_var, title, x_title, y_title, file_name) {
  # p <- plot_ly(data = data, x = as.formula(paste0("~`", x_var, "`"))) %>%
  #   add_trace(y = as.formula(paste0("~`", y_var, "`")), 
  #             mode = 'markers', 
  #             text = ~GeoName, 
  #             hoverinfo = 'text',
  #             color = as.formula(paste0("~`", color_var, "`")), 
  #             colors = party_colors,
  #             marker = list(size = ~log(population), sizemode = 'radius'),
  #             opacity = 0.7) %>%
  #   layout(title = title, 
  #          yaxis = list(title = y_title),
  #          xaxis = list(title = x_title))
  setwd(path_output)
  write.csv(data, file = paste0(file_name, ".csv"), row.names = FALSE)
}

# Generate plots for 2000, 2020, and changes between 2000 and 2020
plots_data <- list(
  list(year = 2000, x_var = "Republican Share", y_var = "Government Transfers as a Share of Total Income", color_var = "Winner", 
       title = "Relationship between Transfer Share of Income and Voting, 2000", x_title = "Republican Share of the Presidential Vote", 
       y_title = "Government Transfers as a Share of Total Income", file_name = "transfer_vote_2000"),
  
  list(year = 2020, x_var = "Republican Share", y_var = "Government Transfers as a Share of Total Income", color_var = "Winner", 
       title = "Relationship between Transfer Share of Income and Voting, 2020", x_title = "Republican Share of the Presidential Vote", 
       y_title = "Government Transfers as a Share of Total Income", file_name = "transfer_vote_2020"),
  
  list(year = 2020, x_var = "Change in the Pres Republican Share", y_var = "Government Transfers as a Share of Total Income", color_var = "Flipped", 
       title = "Relationship between Transfer Share and Change in Voting, 2000 to 2020", x_title = "Change in the Republican Share of the Presidential Vote", 
       y_title = "Government Transfers as a Share of Total Income", file_name = "transfer_vote_change_2000_2020"),
  
  list(year = 2020, x_var = "Change in the Pres Republican Share", y_var = "Change in Government Transfers as a Share of Total Income", color_var = "Flipped", 
       title = "Relationship between Change in Transfer Share and Change in Voting, 2000 to 2020", x_title = "Change in the Republican Share of the Presidential Vote", 
       y_title = "Change in Government Transfers as a Share of Total Income", file_name = "transfer_change_vote_change_2000_2020")
)

# Apply the function to generate all plots
for (plot_info in plots_data) {
  use_data <- transfers_pres_vote %>% filter(year == plot_info$year) %>% ungroup()
  
  create_scatter_plot(
    use_data, 
    plot_info$x_var, plot_info$y_var, plot_info$color_var, 
    plot_info$title, plot_info$x_title, plot_info$y_title, plot_info$file_name
  )
}
