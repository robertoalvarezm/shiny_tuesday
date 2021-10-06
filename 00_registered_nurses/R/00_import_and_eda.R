# setup ----
library(tidyverse)
library(datadigest)
library(janitor)
library(ggiraph)
library(here)

# data import ----
nurses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

nurses_tidy <- read_csv(here("00_registered_nurses/data", "nurses_tidy.csv"))

# initial eda ----
glimpse(nurses)
codebook(nurses)
nurses %>% 
  select(`Location Quotient`) %>% 
  summary()

# taming and tidying maybe ----
nurses_tidy <- nurses %>% 
  clean_names() %>% 
  select(-hourly_10th_percentile:-annual_90th_percentile) 

write_csv(nurses_tidy, here("00_registered_nurses/data", "nurses_tidy.csv"))\

# troubleshooting ----
codebook(nurses)
codebook(nurses_tidy)

# building some things ----
## map work ----
#' what would be cool for our map?
#'   - create a map of the US
#'   - map colors change based on input selected
#'   
#' icing on the cakes is to use the shiny386 theme
map <- map_data("state")
map

ggplot(map, aes(long, lat, group = group)) +
  geom_polygon() + 
  coord_map() +
  theme_void()

ggplot(map, aes(round(long, 1), round(lat, 1), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void()

ggplot(map, aes(round(long, 0), round(lat, 0), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void()

ggplot(map, aes(round(long, 0.75), round(lat, 0.75), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void()

# interactive pixel map ----
ggplot(map, aes(round(long, 0), round(lat, 0), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon_interactive() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void()
