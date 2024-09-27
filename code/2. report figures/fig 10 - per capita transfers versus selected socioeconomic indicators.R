# 10 four panel scatter plot

rm(list = ls())

# load packages
library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(mapview)
library(tigris)
library(tidycensus)
library(ranger)
library(ggplot2)
library(tidyverse)
library(janitor)
library(ipumsr)
library(Hmisc)
library(purrr)
library(broom)

# paths
path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_out = file.path(path_project,"data/clean")
path_out = file.path(path_project,"output")

transfers = read_excel(paste(path_data_out, 
                             "transfers_dataset_counties_master.xlsx",
                             sep ="/")) %>%
  filter(!is.na(share_65_over))

# poverty
poverty = read.csv(paste(path_data_out, "poverty_rates.csv", sep ="/")) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, 5, pad="0")) %>%
  mutate(year = as.character(year))

transfers = left_join(transfers, poverty, by = c("year", "GeoFIPS"))

scatter_dynamic = transfers %>%
  filter(year == 2022 | year == 1970) %>%
  select(share_65_over, GeoFIPS,year,Percent.below.poverty.level,
         transfers_govt_pce_per_capita) %>%
  filter(GeoFIPS !=55078) %>% 
  pivot_wider(id_cols = c("GeoFIPS"),
              names_from = year,
              values_from = c("share_65_over",
                              "Percent.below.poverty.level",
                              "transfers_govt_pce_per_capita")) %>%
  mutate(pov = (Percent.below.poverty.level_2022-Percent.below.poverty.level_1970)/Percent.below.poverty.level_1970,
         old = 100*(share_65_over_2022-share_65_over_1970)/share_65_over_1970,
         transfer = 100*(transfers_govt_pce_per_capita_2022-transfers_govt_pce_per_capita_1970)/transfers_govt_pce_per_capita_1970) %>%
mutate(share_65_over_2022 = share_65_over_2022*100)


##############################
# export data for datawrapper figure making
panel1 = scatter_dynamic %>%
  select(share_65_over_2022, transfers_govt_pce_per_capita_2022)

panel2 = scatter_dynamic %>%
  select(Percent.below.poverty.level_2022, transfers_govt_pce_per_capita_2022)

panel3 = scatter_dynamic %>%
  select(old, transfer)

panel4 = scatter_dynamic %>%
  select(pov, transfer)

    write.xlsx(panel1, paste(path_out, "fig10_panel1.xlsx", sep="/"))
    write.xlsx(panel2, paste(path_out, "fig10_panel2.xlsx", sep="/"))
    write.xlsx(panel3, paste(path_out, "fig10_panel3.xlsx", sep="/"))
    write.xlsx(panel4, paste(path_out, "fig10_panel4.xlsx", sep="/"))



####################################
# graph of figure 10, R version.

# static - old
old_age_2022 = ggplot(scatter_dynamic, 
                      aes(x=share_65_over_2022, y=transfers_govt_pce_per_capita_2022)) + 
  geom_point(size=0.25)+
  labs(title = "65+ share vs. per capita transfers, 2022", 
       x = "% population 65+", 
       y="Per capita transfers") +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "black",hjust = -0.75),
        axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.title.y = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"))


# static - poverty
poverty_age_2022 = ggplot(scatter_dynamic, aes(x=Percent.below.poverty.level_2022, y=transfers_govt_pce_per_capita_2022)) + 
  geom_point(size=0.25)+
  labs(title = "Poverty rate vs. per capita transfers, 2022", 
       x = "% under poverty line", 
       y="Per capita transfers") +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "black",hjust = -1),
        axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.title.y = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"))

# dynamic - old
old_growth = ggplot(scatter_dynamic, aes(x=old, y=transfer)) + 
  geom_point(size=0.25)+
  labs(title = "65+ growth vs. transfers growth, 1970-2022\n", 
       x = "Change in population % 65+", 
       y = "Change in per capita transfers") +
  theme_minimal() +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "black",hjust = -1),
        axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.title.y = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"))

# dynamic - poverty
poverty_growth = ggplot(scatter_dynamic, aes(x=pov, y=transfer)) + 
  geom_point(size=0.25)+
  labs(title = "Poverty growth vs. transfers growth, 1970-2022\n", 
       x = "Change in poverty rate", 
       y = "Change in per capita transfers") +
  theme_minimal() +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "black",hjust =-3),
        axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.title.y = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"))

require(gridExtra)

scatter = grid.arrange(old_age_2022, poverty_age_2022, old_growth, poverty_growth,
                       top = grid::textGrob("Figure 10: Relationship between transfers, poverty, and aging\n",
                                            hjust = 0, x = 0,
                                            gp=grid::gpar(fontsize = 16, col = "#1a654d", fontface = "bold")),
                       bottom = grid::textGrob("\nSource: EIG analysis of Bureau of Economic Analysis data",
                                               hjust = 0, x = 0,
                                               gp = grid::gpar(fontsize = 10, col="black")))
ggsave(plot = scatter, 
       filename = paste(path_out,"fig 10.png",sep="/"),
       bg="white",
       width = 10, height = 10)

# correlation of over 6 and poverty rate
transfers_2022 = transfers %>%
  filter(year==2022)

cov(transfers_2022$share_65_over, transfers_2022$Percent.below.poverty.level)
