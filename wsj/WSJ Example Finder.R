# What are some of the most interesting local stories?
# find counties that have seen significant changes in their non-transfer income, transfer income, and political alignment 

# 09/09/2024

rm(list = ls())
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

# Define user-specific project directories
project_directories <- list(
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/EIG-Great-Transfer-Mation"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]
path_code = file.path(path_project, "code")
path_data = file.path(path_project, "data")
path_output = file.path(path_project, "output")

# party_colors <- c("Rep Win" = "#E9141D", "Dem Win" = "#2E74C0")
party_colors <- c("Rep Win (2010 & 2022)" = "#E9141D", 
                  "Flipped Rep Win (2022)" = "#FFA500",  # Orange for flipped Rep wins
                  "Dem Win (2010 & 2022)" = "#2E74C0", 
                  "Flipped Dem Win (2022)" = "#800080")  # Purple for flipped Dem wins

#################
### Load Data ###
#################

transfers = read_csv(file.path(path_data, "wsj_immig_election_emp.csv")) %>%
  filter(year %in% c(2010,2022)) %>%
  arrange(year) %>%
  group_by(`GeoFIPS`) %>%
  mutate(State = stringr::str_sub(GeoFIPS,end = 2L)) %>%
  mutate(`Republican Share` = `House R`/`Total House`,
         Winner = case_when(`House R` > `House D` ~ "Rep Win",
                            `House R` < `House D` ~ "Dem Win",
                            TRUE ~ NA),
         Flipped = case_when(
           Winner == "Rep Win" & lag(Winner) == "Rep Win" ~ "Rep Win (2010 & 2022)",
           Winner == "Rep Win" & lag(Winner) == "Dem Win" ~ "Flipped Rep Win (2022)",
           Winner == "Dem Win" & lag(Winner) == "Rep Win" ~ "Flipped Dem Win (2022)",
           Winner == "Dem Win" & lag(Winner) == "Dem Win" ~ "Dem Win (2010 & 2022)",
           TRUE ~ NA
         )) %>%
  mutate(`Change in the Share of County Population 65+` = share_65_over - lag(share_65_over),
         `Change in Government Transfers as a Share of Total Income` = (share_transfers_govt_personal_income - lag(share_transfers_govt_personal_income))*100,
         `Change in the Per Capita Total Income from All Sources` = personal_income_pce_per_capita - lag(personal_income_pce_per_capita),
         `Change in the Per Capita Government Transfers` = transfers_govt_pce_per_capita - lag(transfers_govt_pce_per_capita),
         `Per Capita Total Income Excluding Government Transfers` = personal_income_pce_per_capita - transfers_govt_pce_per_capita,
         `Change in the Per Capita Total Income Excluding Government Transfers` = `Per Capita Total Income Excluding Government Transfers` - lag(`Per Capita Total Income Excluding Government Transfers`),
         `% of the Population Foreign-Born` = `% of the Population Foreign-Born`*100,
         `Change in the % of the Population Foreign-Born` = `% of the Population Foreign-Born` - lag(`% of the Population Foreign-Born`),
         `Change in the House Republican Share` = (`Republican Share` - lag(`Republican Share`))*100,
  ) %>%
  rename(`Share of County Population 65+` = share_65_over,
         `Government Transfers as a Share of Total Income` = share_transfers_govt_personal_income,
         `Per Capita Total Income from All Sources` = personal_income_pce_per_capita,
         `Per Capita Government Transfers`= transfers_govt_pce_per_capita, 
         )  %>%
  filter(year == 2022)%>%
  select(`GeoName`, State,
         `population`, 
         `Share of County Population 65+`,`Change in the Share of County Population 65+`,
         `Change in Government Transfers as a Share of Total Income`, `Government Transfers as a Share of Total Income`,
         `Per Capita Total Income from All Sources`,`Change in the Per Capita Total Income from All Sources`,
         `Per Capita Government Transfers`,`Change in the Per Capita Government Transfers`,
         `Per Capita Total Income Excluding Government Transfers`,`Change in the Per Capita Total Income Excluding Government Transfers`,
         `% of the Population Foreign-Born`,`Change in the % of the Population Foreign-Born`,
         `Change in the House Republican Share`,Flipped)



##################################################################
### Government Transfers vs Republican Voting Share - Each State ###
##################################################################
plots_by_state <- list()
# unique_fips <- unique(transfers$State)
unique_fips <- c("04","12","13","26","42")
# Loop through each FIPS code
for (i in seq_along(unique_fips)) {
  
  # Filter the data for the current state
  test <- transfers %>%
    filter(State == unique_fips[[i]]) %>%
    select(`GeoName`, 
           `population`, 
           `Share of County Population 65+`,`Change in the Share of County Population 65+`,
           `Change in Government Transfers as a Share of Total Income`, 
           `Change in the Per Capita Total Income from All Sources`,
           `Change in the Per Capita Government Transfers`,
           `Change in the Per Capita Total Income Excluding Government Transfers`,
           `Change in the % of the Population Foreign-Born`,
           `Change in the House Republican Share`,
           Flipped
    )  %>%
    na.omit()
  
  # Add a trendline to the plot meant only to help visualize the relationship
  model1 <- lm(`Change in Government Transfers as a Share of Total Income` ~ poly(`Change in the % of the Population Foreign-Born`, 1, raw=TRUE), data = test)
  test$trendline <- predict(model1)
  
  # Create the trendline data frame
  trend_line_df <- test %>% 
    select(`Change in the % of the Population Foreign-Born`, trendline) %>%
    arrange(`Change in the % of the Population Foreign-Born`) %>%
    distinct()
  
  # Generate the plot for the current state
  plot <- plot_ly(data = test,
                  x = ~`Change in the % of the Population Foreign-Born`) %>%
    add_trace(y = ~`Change in Government Transfers as a Share of Total Income`, 
              mode = 'markers', 
              text = ~GeoName, 
              hoverinfo = 'text',
              color = ~Flipped, 
              colors = party_colors,
              marker = list(size = ~log(population), 
                            sizemode = 'radius'),
              opacity=0.7) %>%
    add_trace(data = trend_line_df,
              y = ~trendline,
              mode = 'line',
              name = 'Trend line') %>%
    layout(title = paste("Relationship between Immigration and the Transfer Share of Income,", 
                         "Change from 2010 to 2022 in State FIPS", unique_fips[[i]]),
           yaxis = list(title = "Change in Government Transfers as a Share of Total Income"),
           xaxis = list(title = "Change in the % of the Population Foreign-Born"))
  
  # Save the plot in the list, naming it by the FIPS code
  plots_by_state[[as.character(unique_fips[[i]])]] <- plot
}
