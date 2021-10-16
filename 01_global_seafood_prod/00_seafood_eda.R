# set up ----
library(tidyverse)
library(janitor)
library(here)

# data import ----
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')
cap_v_farm <- captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
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

# making additional datasets
# can't merge with tidy_fish: tidy_fish only has "World" as the entity, there 
# are no individual country codes /shakes_fist
glimpse(tidy_fish)
tidy_fish %>% 
  select(entity) %>%  # world
  unique()

glimpse(cap_v_farm)
cap_v_farm %>%   
  select(Entity) %>%  # country
  unique()

glimpse(consumption)
consumption %>% 
  select(Entity) %>%  # country
  unique()

glimpse(stock)  # uh-oh
stock %>%
  select(Entity) %>%  # regions, not countries
  unique()

glimpse(production)
production %>% 
  select(Entity) %>%  # country
  unique()

# mega merge
big_fish <- cap_v_farm %>% 
  left_join(consumption, by = c("Entity", "Code", "Year")) %>% 
  left_join(production, by = c("Entity", "Code", "Year")) %>% 
  clean_names()

glimpse(big_fish)

bigger_fish <- big_fish %>% 
  select(-code, -fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020) %>% 
  pivot_longer(aquaculture_production_metric_tons:capture_fisheries_production_metric_tons,
               names_to = "production_type", values_to = "production_amt_in_tons") %>% 
  mutate(production_type = str_replace(production_type, "_production_metric_tons", "")) %>% 
  mutate(production_type = str_replace(production_type, "_fisheries", "")) %>%  # shhhhh
  pivot_longer(commodity_balances_livestock_and_fish_primary_equivalent_pelagic_fish_2763_production_5510_tonnes:commodity_balances_livestock_and_fish_primary_equivalent_marine_fish_other_2764_production_5510_tonnes,
               names_to = "seafood_type", values_to = "prod_in_thousand_tonnes") %>% 
  mutate(seafood_type = str_replace(seafood_type, "commodity_balances_livestock_and_fish_primary_equivalent_", "")) %>% 
  mutate(seafood_type = str_replace(seafood_type, "_2763_production_5510_tonnes", "")) %>% 
  mutate(seafood_type = str_replace(seafood_type, "_2765_production_5510_tonnes", "")) %>%
  mutate(seafood_type = str_replace(seafood_type, "_2766_production_5510_tonnes", "")) %>%
  mutate(seafood_type = str_replace(seafood_type, "_2762_production_5510_tonnes", "")) %>%
  mutate(seafood_type = str_replace(seafood_type, "_2761_production_5510_tonnes", "")) %>%
  mutate(seafood_type = str_replace(seafood_type, "_2767_production_5510_tonnes", "")) %>%
  mutate(seafood_type = str_replace(seafood_type, "_2764_production_5510_tonnes", "")) 

glimpse(bigger_fish)


