# distribution of transfer incomes and non transfer incomes 
# all counties, 2022

# remove dependencies
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
                     
plot = read_excel(paste(path_data_clean, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
  filter(year==2022) %>%
  mutate(income_no_transfers = personal_income_pce_per_capita - transfers_govt_pce_per_capita) %>%
  mutate(transfer_income = personal_income_pce_per_capita-income_no_transfers) %>%
  select(income_no_transfers, transfer_income) %>%
  rename(`Non-transfer income`= income_no_transfers,
         `Transfer income` = transfer_income) %>%
  pivot_longer(cols = c(`Non-transfer income`, `Transfer income`),
               names_to = "type") %>%
  mutate(value = value/1000) %>%
  filter(value<100)


plot %>% 
  ggplot(aes(x =value, color=type, fill=type)) +
  geom_histogram( color="darkgrey", alpha=0.5, binwidth = 2, position="identity") +
  scale_fill_manual(values=c("#e1ad28", "#234f8b")) +
  theme_classic() +
  labs(fill="", 
       title = "Figure 5: Distribution of transfer and non-transfer incomes, 2022", 
       x = "Dollars (thousands per capita)",
       y= "Number of counties",
       caption = c("Note: 15 counties with non-tranfer income >100k per capita removed for visual clarity.",
                   "\nSource: EIG analysis of Bureau of Economic Analysis data")) +
  scale_y_discrete(limits = c(0,100,200,300,400,500,600,700,800,900))+
  theme(plot.title = element_text(color = "#1a654d", face="bold", size = 9,
                                  hjust = -0.2),
        legend.position = "top",
        legend.text = element_text(size = 7, color = "black"),
        axis.text.x = element_text(size = 7, color = "darkgrey"),
        axis.title.x = element_text(size=7, color="darkgrey"),
        axis.title.y = element_text(size=7, color="darkgrey"),
        plot.caption = element_text(size = c(5,5), color = c("black","black"), hjust = c(-0.12,-0.10)),
        axis.text.y = element_text(size = 7, color = "darkgrey"))

ggsave(paste(path_out, "fig 5.png",sep="/"), bg="white",
       width = 6.4, height = 3.0,
       dpi = 1000)



# alternative: density plot
density_plot = plot %>% 
  ggplot(aes(x =value, color=type, fill=type)) +
  geom_density( color="#e9ecef", alpha=0.5, position="identity") +
  scale_fill_manual(values=c("#e1ad28", "#234f8b")) +
  theme_minimal() +
  labs(fill="", 
       title = "Distribution of Transfer Incomes and Non-Transfer\nIncomes, 2022", 
       x = "dollars (thousands per capita)",
       y= "density",
       caption = "note: 15 counties with non-tranfer income over 100k per capita removed for visual clarity.") +
  theme(plot.title = element_text(color = "#1a654d", face="bold", size = 12),
        legend.position = "top")

density_plot
