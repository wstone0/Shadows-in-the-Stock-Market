library(tidyverse)
dat <- read.csv("../data/All_Tickers_5_Year_Data.csv")

dat_clean <- dat
#dat_clean <- dat[ , colSums(is.na(dat))==0]
dat_clean$Date <- as.Date(dat$Date)

dat_pivot_longer <- pivot_longer(dat_clean, cols= -Date, names_to="stock", values_to="values")

dat_separated <- dat_pivot_longer %>%
  mutate(stock_name = str_extract(stock, "[^_]+"), bench_mark = str_extract(stock, "(?<=_).*") )

dat_separated <- dat_separated %>%
  filter(!is.na(values)) %>%
  select(-stock)

rm(dat_pivot_longer,dat_clean,dat)

dat_wider <- dat_separated %>%
  pivot_wider(names_from=bench_mark, values_from=values)

volume_cat_big <- dat_wider %>% 
  group_by(stock_name) %>% 
  summarize(med_volume = median(Volume)) %>% 
  filter(med_volume > 10000000)

big_stock_data <- dat_wider %>% 
  mutate(Size = ifelse(stock_name %in% volume_cat_big$stock_name, "big", "small"))

metrics <- c("Open", "High", "Low", "Close", "Adj.Close")

# Create the columns we need for our analysis with dynamic naming.
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
