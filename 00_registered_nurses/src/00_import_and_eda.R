# setup ----
library(tidyverse)
#library(datadigest)
library(janitor)
#library(ggiraph)
library(leaflet)
library(sf)
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

write_csv(nurses_tidy, here("00_registered_nurses/data", "nurses_tidy.csv"))

# troubleshooting ----
#codebook(nurses)
#codebook(nurses_tidy)

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

ggplotly(ggplot(map, aes(round(long, 0.75), round(lat, 0.75), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void())


# interactive pixel map ----
ggplot(map, aes(round(long, 0), round(lat, 0), 
                group = group,
                fill = as.factor(group))) +
  geom_polygon_interactive() +
  guides(fill = FALSE) +
  coord_map() +
  theme_void()


# leaflet with nurses_tidy ----
glimpse(nurses_tidy)
# state is our geographic information

state_map <- st_read(here("00_registered_nurses/src/spatial", "cb_2020_us_place_500k.shp"))

leaflet(state_map) %>%
  #setView(-96, 37.8, 4) %>%
  addTiles() %>% 
  # addProviderTiles("MapBox", options = providerTileOptions(
  #   id = "mapbox.light",
  #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons()

  # common layers:
  #  basemaps
  #  points
  #  lines
  #  polygons

# maps but now with plotly ----

library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")

fig <- plot_ly(df,
               type = 'choropleth',
               locations = df$USA)  
fig

fig2 <- plot_geo(df, locationmode = 'USA-states')
fig2




# leaflet, again. because ----
states <- spData::us_states %>% 
  rename(state = NAME)

# works
leaflet(states) %>% 
  addTiles() %>% 
  addPolygons()

# join nurses_tidy with states - START HERE
nurses_states <- states %>% 
  right_join(nurses_tidy, by = "state") %>% 
  glimpse()

# class(nurses_states$state)
# class(states$state)
# 
# str(nurses_states)
# str(states)

leaflet(nurses_states) %>% 
  addTiles() %>% 
  addPolygons() 

## adding color to the leaflet map ----
bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf)
pal <- colorBin("YlOrRd", domain = nurses_states$hourly_wage_avg, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g average hourly wage",
  nurses_states$state, nurses_states$hourly_wage_avg
) %>% lapply(htmltools::HTML)

leaflet(nurses_states) %>% 
  setView(lng = -99, lat = 43, zoom = 3) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(hourly_wage_avg),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )) 

