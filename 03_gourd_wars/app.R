# set up ----
library(shiny)
library(readr)
library(here)

# data import ----
plong <- read_csv(here("03_gourd_wars", "pivoted_pumpkins.csv"))
plong %>% 
  mutate(type = as.factor(type),
         stats = as.factor(stats)) %>% 
  group_by(type) %>% 
  summarise(avg_value = mean(value, na.rm = TRUE)) ->
  plong_table

# user interface ----
ui <- fluidPage(
  
  fluidRow(
    tags$h1("Select your fighter:")
  ),
  
  ## want to replace button with pixel art
  # icon() a wrapper on img tags, could we pass src = "..." to icon?
  # -OR- use an image tag inside the button
  fluidRow(
    column(width = 2,
           actionButton(inputId = "field_pumpkin_button",
                        label = "Field Pumpkin",
                        width = "100%",
                        icon = )
           ),
    column(width = 2,
           actionButton(inputId = "giant_pumpkin_button",
                        label = "Giant Pumpkin",
                        width = "100%")
           ),
    column(width = 2,
           actionButton(inputId = "giant_squash_button",
                        label = "Giant Squash",
                        width = "100%")
           ),
    column(width = 2,
           actionButton(inputId = "giant_watermelon_button",
                        label = "Giant Watermelon",
                        width = "100%")
    ),
    column(width = 2,
           actionButton(inputId = "long_gourd_button",
                        label = "Long Gourd",
                        width = "100%")
    ),
    column(width = 2,
           actionButton(inputId = "tomato_button",
                        label = "Tomato",
                        width = "100%")
    )
  ),
  
  br(),
  
  fluidRow(
    plotOutput(outputId = "fighter_stats")
  )
)

# server ----
server <- function(input, output, session) {
  # bar chart of stats
  #stats <- eventReactive("field_pumpkin_button")
}

# run app ----
shinyApp(ui, server)