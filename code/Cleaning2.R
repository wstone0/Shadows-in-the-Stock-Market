# This file takes the full, clean dataset and adds the needed columns for the shinys to run.
library(tidyverse)

full_data <- read_csv('../data/long_data.csv')

volume_cat_big <- full_data %>% 
  group_by(stock_name) %>% 
  summarize(med_volume = median(Volume)) %>% 
  filter(med_volume > 10000000)

big_stock_data <- full_data %>% 
  mutate(Size = ifelse(stock_name %in% volume_cat_big$stock_name, "big", "small"))

metrics <- c("Open", "High", "Low", "Close", "Adj.Close")

for(metric in metrics) {
  label <- str_c(metric, "_cat")
  big_stock_data <- big_stock_data %>%
    mutate({{metric}} := round(!!sym(metric), 2)) %>%
    mutate({{metric}} := as.character(!!sym(metric))) %>%
    mutate({{label}} := ifelse(str_detect(!!sym(metric), "\\..$"), paste0(str_sub(!!sym(metric), -1, -1), "0"), str_sub(!!sym(metric), -2, -1))) %>%
    mutate({{label}} := ifelse(str_detect(!!sym(metric), "\\."), !!sym(label), 0))
}

# Create the median_volume variable based on the Volume per stock_name
big_stock_data <- big_stock_data %>%
  group_by(stock_name) %>%
  mutate(median_volume = median(Volume, na.rm = TRUE)) %>%  # Calculate the median volume per stock_name
  mutate(volume_range = case_when(
    median_volume <= 10000 ~ "0-10 thousand",
    median_volume <= 100000 ~ "10 thousand-100 thousand",
    median_volume <= 1000000 ~ "100 thousand-1 million",
    median_volume <= 10000000 ~ "1 million-10 million",
    median_volume > 10000000 ~ "Above 10 million"
  )) %>%
  ungroup()  # Ungroup to remove stock_name grouping

# Save the modified dataset to a new CSV file
write_csv(big_stock_data,file = '../data/finished_data.csv')
