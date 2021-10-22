# setup and data import ----
library(tidyverse)
library(here)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# what are we working with? ----
glimpse(pumpkins)

## need to convert some character columns to numeric
### place, weight_lbs, ott, est_weight, pct_chart
pumpkins_01 <- pumpkins %>% 
  filter(!is.na(as.numeric(place))) %>%  # removes all non-numeric places
  mutate(place = parse_number(place),
         weight_lbs = parse_number(weight_lbs),
         ott = parse_number(ott),
         est_weight = parse_number(est_weight),
         pct_chart = parse_number(pct_chart)) 

## decode type columns into their word equivalents, convert to factor
# from dela: 
type_conversion <- tidyr::tribble(~type_code, ~type, 'F', "Field Pumpkin", 'P', "Giant Pumpkin", 
                 'S', "Giant Squash", 'W', "Giant Watermelon", 'L', 
                 "Long Gourd", 'T', "Tomato" ) 
type_conversion

pumpkins_02 <- pumpkins_01 %>% 
  separate(col = id, 
           into = c("year", "type_code"),
           sep = "-") %>% 
  left_join(type_conversion, by = "type_code") %>% 
  select(-type_code) %>% 
  mutate(type = as.factor(type))

glimpse(pumpkins_02)

## need to convert some character columns to factors
### state_prov, country, gpc_site
pumpkins_03 <- pumpkins_02 %>% 
  mutate(state_prov = as.factor(state_prov),
         country = as.factor(country),
         gpc_site = as.factor(gpc_site))

glimpse(pumpkins_03)

# use year to calculate pumpkin age - is there a point where a pumpkin
# gets too old and stats decrease?

pumpkins_tidy <- pumpkins_03 %>% 
  mutate(year = parse_number(year),
         pumpkin_vintage = 2021 - year) %>% 
  select(-variety)

write_csv(pumpkins_tidy, here("03_gourd_wars/data", "pumpkins_tidy.csv"))

## app EDA ----
tidy_p <- read_csv(here("03_gourd_wars/data", "pumpkins_tidy.csv"))

# wrangling  ----
tidy_p %>% 
  count(type)

glimpse(tidy_p)

# mini data frame with MVP stats
pumpkin_long <- tidy_p %>% 
  select(type, place, weight_lbs, ott, est_weight, pumpkin_vintage) %>% 
  pivot_longer(place:pumpkin_vintage,
               names_to = "stats") %>% 
  mutate(stats = as.factor(stats),
         type = as.factor(type)) 

write_csv(pumpkin_long, here("03_gourd_wars/data", "pivoted_pumpkins.csv"))

# sample plots ----
plong <- read_csv(here("03_gourd_wars/data", "pivoted_pumpkins.csv"))
glimpse(plong)

# calculating stats for the overall average
plong %>% 
  mutate(type = as.factor(type),
         stats = as.factor(stats)) %>% 
  group_by(type) %>% 
  summarise(avg_value = mean(value, na.rm = TRUE))

plong %>% 
  mutate(type = as.factor(type),
         stats = as.factor(stats)) %>% 
  group_by(type, stats) %>% 
  summarise(avg_value = mean(value, na.rm = TRUE)) %>% 
  filter(type == "Field Pumpkin") %>% 
  ggplot(aes(x = stats, y = avg_value)) +
  geom_col() + 
  coord_flip() +
  theme_bw()

glimpse(plong)
plong %>% 
  distinct(type)

# working on reactive graph
plong %>% 
  filter(type == "Field Pumpkin") %>%
  group_by(type)
  mutate(avg_value = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = stats, y = avg_value)) +
  geom_col() + 
  coord_flip() +
  theme_bw()
  
# stat distribution within a type
## decision to be made:
##   - how is fighter selected?
#'     -- could be a random individual from a class
#'     -- could be a representative individual from the class, using aggregate//averaged stats

glimpse(plong)

plong %>% 
  group_by(type) %>% 
  ggplot(aes(x = value, y = type)) +
  geom_boxplot() +
  facet_wrap(vars(stats), scales = "free_x")
