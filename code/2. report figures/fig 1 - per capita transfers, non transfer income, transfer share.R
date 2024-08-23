# 1 two pannel headline figure
# per capita transfers, non transfer income, and growth rate

rm(list = ls())

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(stringr)

path_project = "ENTER USER PROJECT PATH HERE"
path_data_raw = file.path(path_project,"data/raw")
path_data_out = file.path(path_project,"data/clean")
path_out = file.path(path_project,"output")


transfers = read_excel(paste(path_data_out, "transfers_dataset_nation_master.xlsx", sep = "/")) %>%
  mutate(non_transfer_income_pce_per_capita = 
           (personal_income_pce-transfers_govt_pce)/population) %>% # non-transfer income
  select(year, 
         transfers_govt_pce_per_capita, 
         non_transfer_income_pce_per_capita, 
         share_transfers_govt_personal_income) %>% 
  mutate(across(c(transfers_govt_pce_per_capita, 
                  non_transfer_income_pce_per_capita, 
                  share_transfers_govt_personal_income),
                ~./first(.)*100, # index to 100
                .names = "{.col}_mutate")) %>%
  ungroup()


# transfers and non-transfer indexed with 1970=100
graph1 = transfers %>%
  ggplot(aes(x = year, group=1)) +
  geom_line(aes(y = transfers_govt_pce_per_capita_mutate), color = "#e1ad28") +
  geom_line(aes(y = non_transfer_income_pce_per_capita_mutate), color = "#234f8b") +
  theme_minimal() +
  labs(title = "Figure 1: Per Capita Transfers, Non-Transfer Income", y = "1970=100") +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#1a654d"),
        panel.grid.major.x = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_text(size=7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6)) +
  scale_x_discrete(breaks = seq(1970,2022, by=5)) +
  annotate("text", x = "2013", y = 270, label = "Non-transfer income \n (increase of $31,305)", fontface = "bold", size =2.6)+
  annotate("text", x = "2012", y = 550, label = "transfer income \n(increase of $9,519)", fontface = "bold", size =2.6)


graph2 = transfers %>%
  mutate(share_transfers_govt_personal_income=share_transfers_govt_personal_income*100) %>%
  ggplot(aes(x = year, group=1)) +
  geom_line(aes(y = share_transfers_govt_personal_income), color = "#5e9c86") +
  theme_minimal() +
  labs(title = "Transfer Share of Total Income", y = "percent")+
  scale_y_continuous(limits = c(5,25)) +
  scale_x_discrete(breaks = seq(1970,2022, by=5)) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#1a654d"),
        axis.title.x= element_blank(),
        axis.title.y= element_text(size=7),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        panel.grid.major.x = element_blank()) +
  annotate("text", x="2016", y = 15, label = "increase from 8.2% \nto 17.6%", fontface = "bold", size =2.6)


require(gridExtra)
plot = grid.arrange(graph1, graph2, ncol = 2)


ggsave(plot = plot, filename = paste(path_out, "fig 1.png",sep="/"),bg="white")

