library(readr)
library(tidyverse)

data <- read_csv("finished_data copy.csv")

sp500_symbols <- tq_index("SP500")$symbol %>%
  intersect(data$stock_name)

data <- data %>% 
  filter(stock_name %in% sp500_symbols) %>% 
  select(-Volume)

# Check memory size in R:
print(object.size(data), units = "Mb")

# Save with gzip compression explicitly:
saveRDS(data, "sp500.rds", compress = "gzip")