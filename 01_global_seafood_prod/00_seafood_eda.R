# set up ----
library(tidyverse)
library(janitor)
library(here)

# data import ----
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

tidy_fish <- read_csv(here("01_global_seafood_prod", "tidy_fish.csv"))

# what are we working with? ----
glimpse(fishery)  # world-level data
glimpse(production)  # country-level data

glimpse(fishery)
fishery %>% 
  clean_names() %>% 
  glimpse()

fishery %>% 
  clean_names() %>% 
  pivot_longer(cols = artisanal_small_scale_commercial:subsistence,
               names_to = "sector_catch_type") %>% 
  glimpse()

tidy_fish <- fishery %>% 
  clean_names() %>% 
  pivot_longer(cols = artisanal_small_scale_commercial:subsistence,
               names_to = "sector_catch_type")

write_csv(tidy_fish, here("01_global_seafood_prod", "tidy_fish.csv"))

tidy_fish %>% 
  ggplot(aes(x = sector_catch_type, y = value, fill = sector_catch_type)) +
  geom_col()

tidy_fish %>% 
  ggplot(aes(x = year, y = value, group = sector_catch_type)) +
  geom_line(aes(color = sector_catch_type))
