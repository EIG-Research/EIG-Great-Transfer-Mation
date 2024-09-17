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
path_data_out = file.path(path_project,"data/clean")
path_out = file.path(path_project,"output")
                     
  
plot = read_excel(paste(path_data_out, "transfers_dataset_counties_master.xlsx", sep = "/")) %>%
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

library(scales)
squash_axis <- function(from, to, factor) { 
  # A transformation function that squashes the range of [from, to] by factor on a given axis 
  
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  #
  # Returns:
  #   A transformation called "squash_axis", which is capsulated by trans_new() function
  
  trans <- function(x) {    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}

plot %>% 
  ggplot(aes(x =value, color=type, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.5, binwidth = 2, position="identity") +
  scale_fill_manual(values=c("#e1ad28", "#234f8b")) +
  theme_minimal() +
  labs(fill="", 
       title = "Figure 5: Distribution of transfer and non-transfer incomes, 2022", 
       x = "Dollars (thousands per capita)",
       y= "Number of counties",
       caption = c("Note: 15 counties with non-tranfer income >100k per capita removed for visual clarity.",
       "\nSource: EIG analysis of Bureau of Economic Analysis data")) +
  coord_trans(y = squash_axis(300, 800, 10)) +
  scale_y_discrete(limits = c(0,100,200,300,800,900))+
  theme(plot.title = element_text(color = "#1a654d", face="bold", size = 16,
                                  hjust = -0.2),
    legend.position = "top",
    legend.text = element_text(size = 12, color = "darkgrey"),
    axis.text.x = element_text(size = 12, color = "darkgrey"),
    axis.title.x = element_text(size=12, color="darkgrey"),
    axis.title.y = element_text(size=12, color="darkgrey"),
    plot.caption = element_text(size = c(12,12), color = c("black","black"), hjust = c(-0.2,-0.12)),
    axis.text.y = element_text(colour = c('darkgrey','darkgrey','darkgrey','red', 'red'), size = 12))

ggsave(paste(path_out, "fig 5.png",sep="/"), bg="white",
       width = 10, height = 6,
       dpi = 1000)

# alternative plot --

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
