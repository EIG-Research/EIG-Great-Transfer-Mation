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
path_data_clean = file.path(path_project,"data")
path_out = file.path(path_project,"output")


transfers = read_excel(paste(path_data_clean, "transfers_dataset_nation_master.xlsx", sep = "/")) %>%
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
  geom_line(aes(y = transfers_govt_pce_per_capita_mutate), color = "#e1ad28", size = 1.5) +
  geom_line(aes(y = non_transfer_income_pce_per_capita_mutate), color = "#234f8b", size = 1.5) +
  theme_minimal() +
  labs(y = "1970=100") +
  theme(panel.grid.major.x = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_text(size=12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(limits = c(100,750)) +
  scale_x_discrete(breaks = seq(1970,2022, by=10)) +
  annotate("text", x = "2011", y = 290, label = "Non-transfers:\nincrease of $31k", size =4)+
  annotate("text", x = "2008", y = 550, label = "Transfers:\nincrease of $10k",  size =4)


graph2 = transfers %>%
  mutate(share_transfers_govt_personal_income=share_transfers_govt_personal_income*100) %>%
  ggplot(aes(x = year, group=1)) +
  geom_line(aes(y = share_transfers_govt_personal_income), color = "#5e9c86", size = 1.5) +
  theme_minimal() +
  labs(title = "", y = "Percent")+
  scale_y_continuous(limits = c(5,25)) +
  scale_x_discrete(breaks = seq(1970,2022, by=10)) +
  theme(axis.title.x= element_blank(),
        axis.title.y= element_text(size=12, color = "darkgrey"),
        axis.text.x = element_text(size=12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color = "darkgrey"),
        panel.grid.major.x = element_blank()) +
  annotate("text", x="2013", y = 12, label = "Transfer share of\n income: increase of\n9.4 % pts", size=4)


require(gridExtra)
library(grid)

plot = grid.arrange(graph1, graph2, ncol = 2, 
                    top = 
                      grid::textGrob("Figure 1: Per capita transfers, non-transfer income vs. transfer share of total income",
                                     hjust = 0, x = 0,
           gp=grid::gpar(fontsize = 16, col = "#1a654d", fontface = "bold")),
           bottom = grid::textGrob("\nSource: EIG analysis of Bureau of Economic Analysis data",
                                   hjust = 0, x = 0,
                                   gp = grid::gpar(fontsize = 10, col="black")))

ggsave(plot = plot, filename = paste(path_out, "fig 1.png",sep="/"),
       bg="white",
       width = 10, height = 5,
       dpi = 1000)



#######
# for report, export figure data for datawrapper plot.
panel1 = transfers %>%
  select(year, 
         transfers_govt_pce_per_capita_mutate, 
         non_transfer_income_pce_per_capita_mutate)

panel2 = transfers %>%
  mutate(share_transfers_govt_personal_income=share_transfers_govt_personal_income*100) %>%
  select(year, 
         share_transfers_govt_personal_income)

setwd(path_out)
write.csv(panel1, "fig1_panel1.csv")
write.csv(panel2, "fig1_panel2.csv")
