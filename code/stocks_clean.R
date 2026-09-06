library(readr)
library(tidyverse)

data <- read_rds("shiny_rds.rds")

short_stocks <- data %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  arrange(stock_name) %>% 
  slice(1:5000000)

# Check memory size in R:
print(object.size(short_stocks), units = "Mb")

# Save with gzip compression explicitly:
saveRDS(short_stocks, "short_stocks.rds", compress = "gzip")