# 12 histograms of wage growth

# aim: argue that this is not just a federal-government-is-more-generous argument.
# talk about real wage growth.
# how many places had real wage growth over the period?

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

# load data

# national growth rate
read_excel(paste(path_data_out, "transfers_dataset_nation_master.xlsx", sep ="/")) %>%
  filter(year==1970 | year==2022) %>%
  mutate(net_earnings = net_earnings_pce_per_capita + 
           dividends_interest_rent_pce_per_capita) %>%
  select(year, net_earnings) %>%
  pivot_wider(names_from = year, values_from = net_earnings) %>%
  mutate(difference = `2022`/`1970`)
  
# net earnings + dividends, interest, rent
df_transfers =  read_excel(paste(path_data_out, "transfers_dataset_counties_master.xlsx", sep ="/")) %>%
  filter(year==1970 | year==2022) %>%
  mutate(net_earnings = net_earnings_pce_per_capita + 
           dividends_interest_rent_pce_per_capita) %>%
  select(GeoName, year, net_earnings, transfer_tiers) %>%
  ungroup() %>%
  group_by(GeoName) %>%
  mutate(transfer_tiers = ifelse(year==2022, transfer_tiers, NA)) %>%
  mutate(transfer_tiers = max(transfer_tiers, na.rm = TRUE))

#############################
# per capita
df_per_cap = df_transfers %>%
  pivot_wider(values_from = c("net_earnings"),
              names_from = year) %>%
  rename(earnings_1970 = `1970`,
         earnings_2022 = `2022`) %>%
  mutate(earnings_change = earnings_2022/earnings_1970) %>%
  mutate(earnings_change = ifelse(earnings_change>6, NA, earnings_change))

unique(df_per_cap$transfer_tiers)

colors = c("high transfer tier (25%+)" = "#e1ad28", "mid transfer tier (15-25%)" = "#b3d6dd",
           "low transfer tier (0-15%)" = "#1a654d")

# color plot
png(paste(path_out,"full_histogram.png",sep="/"))

ggplot(df_per_cap, aes(x=earnings_change, fill = transfer_tiers)) +
  geom_histogram(alpha = 0.6) +
  geom_vline(xintercept = 2.38) + # is this right?
  theme_classic() +
  scale_fill_manual(values = colors) +
  xlab("Net earnings (2022/1970)") +
  labs(title ="Growth in Per-Capita Net Earnings 1970-2022", fill="")

dev.off()

########################################
# by earnings tier.

plot1 <-  df_per_cap %>%
  filter(transfer_tiers=="high transfer tier (25%+)") %>%
  mutate(lag = earnings_change<2.38) %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.5, color = "#e1ad28", fill = "#e1ad28") +
  geom_vline(xintercept = 2.38) +
  theme_classic() +   
  labs(title ="",
       subtitle = "High Transfer Ratio (25%+)",
       x = "(net earnings 2022) / (net earnings 1970)") +
  theme(axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size=7),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 8, 
                                  color = "#1a654d", face = "bold"))

plot1

plot2 <- df_per_cap %>%
  filter(transfer_tiers=="mid transfer tier (15-25%)") %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.8, color = "#b3d6dd", fill = "#b3d6dd") +
  theme_classic() +   
  geom_vline(xintercept = 2.38) +
  labs(title ="",
       subtitle = "Mid Transfer Ratio (15-25%)",
       x = "(net earnings 2022) / (net earnings 1970)") +
  theme(axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size=7),        
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 8, 
                                  color = "#1a654d", face = "bold",hjust = 1))

plot3 <- df_per_cap %>%
  filter(transfer_tiers=="low transfer tier (0-15%)") %>%
  mutate(earnings_change = ifelse(earnings_change>6, NA, earnings_change))%>%
  mutate(lag = earnings_change<2.38) %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.5, color = "#1a654d", fill = "#1a654d") +
  theme_classic() +   
  geom_vline(xintercept = 2.38) +
  labs(title ="", 
       subtitle = "Low Transfer Ratio (0-15%)",
       x = "(net earnings 2022) / (net earnings 1970)") +
  theme(axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size=7),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 8, 
                                  color = "#1a654d", face = "bold",hjust = 1))


require(gridExtra)
library(grid)
plot = grid.arrange(plot1, plot2,plot3, ncol=3, 
                    top = 
                      textGrob("Figure 12: Real Growth in Per-Capita Earnings 1970-2022"))


ggsave(plot = plot, paste(path_out, "fig 12.png",sep="/"))
