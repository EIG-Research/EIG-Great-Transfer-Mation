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
library(matrixStats)

path_project = "ENTER USER PROJECT PATH HERE
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



# set color scheme
colors = c("significant (25%+)" = "#e1ad28", "moderate (15-24.9%)" = "#b3d6dd",
           "minimal (<15%)" = "#1a654d")

#########################
# plots by earnings tier.
#########################

###################################
# histogram exports for datawrapper
df_per_cap_binned = df_per_cap %>%
  na.omit() %>%
  ungroup() %>%
  group_by(transfer_tiers) %>%
  mutate(bin = cut(earnings_change, breaks = 30)) %>%
  group_by(transfer_tiers, bin) %>%
  summarise(count = n())

      df_sig = df_per_cap_binned %>%  
        filter(transfer_tiers=="significant (25%+)") %>%
            select(bin, count)
      
      df_mod = df_per_cap_binned %>%  
        filter(transfer_tiers=="moderate (15-24.9%)") %>%
            select(bin, count)
      
      df_min = df_per_cap_binned %>%  
        filter(transfer_tiers=="minimal (<15%)") %>%
              select(bin, count)

# export
  write.xlsx(df_sig, "fig12_panel1.xlsx")
  write.xlsx(df_mod, "fig12_panel2.xlsx")
  write.xlsx(df_min, "fig12_panel3.xlsx")


#################
# build plot in R
plot1 <-  df_per_cap %>%
  filter(transfer_tiers=="significant (25%+)") %>%
  mutate(lag = earnings_change<2.38) %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.5, color = "#e1ad28", fill = "#e1ad28") +
  geom_vline(xintercept = 2.38) +
  labs(title ="",
       subtitle = "Significant (25+%)",
       y = "counties",
       x = "(earnings 2022)/(earnings 1970)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size = 12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color ="darkgrey"),
        plot.subtitle = element_text(face="bold", size = 12),
        axis.title.y = element_text(size = 12, color = "darkgrey"))

plot2 <- df_per_cap %>%
  filter(transfer_tiers=="moderate (15-24.9%)") %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.8, color = "#b3d6dd", fill = "#b3d6dd") +
  geom_vline(xintercept = 2.38) +
  labs(title ="",
       subtitle = "Moderate (15-24.9%)",
       x = "(earnings 2022)/(earnings 1970)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size = 12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color ="darkgrey"),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(face="bold", size = 12))

plot3 <- df_per_cap %>%
  filter(transfer_tiers=="minimal (<15%)") %>%
  mutate(earnings_change = ifelse(earnings_change>6, NA, earnings_change))%>%
  mutate(lag = earnings_change<2.38) %>%
  ggplot(aes(x=earnings_change)) +
  geom_histogram(alpha = 0.5, color = "#1a654d", fill = "#1a654d") +
  geom_vline(xintercept = 2.38) +
  labs(title ="", 
       subtitle = "Minimal (<15%)",
       x = "(earnings 2022)/(earnings 1970)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, color = "darkgrey"),
        axis.text.x = element_text(size = 12, color = "darkgrey"),
        axis.text.y = element_text(size=12, color ="darkgrey"),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(face="bold", size = 12))

require(gridExtra)
library(grid)
plot = grid.arrange(plot1, plot2,plot3, ncol=3, 
                    top = 
                      textGrob("Figure 12: Real growth in per-capita earnings 1970-2022",
                               gp=grid::gpar(fontsize = 16, col = "#1a654d", fontface = "bold"),
                               hjust = 0, x = 0),
                    bottom = grid::textGrob("\nSource: EIG analysis of Bureau of Economic Analysis data",
                                            hjust = 0, x = 0,
                                            gp = grid::gpar(fontsize = 10, col="black")))


ggsave(plot = plot, paste(path_out, "fig 12.png",sep="/"),
       width = 10, height = 6)
