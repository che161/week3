#installed.package("tidyverse")
library(tidyverse)
read_csv("data/gapminder_data.csv")
gapminder_1977 <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 1977)
gapminder_1977
