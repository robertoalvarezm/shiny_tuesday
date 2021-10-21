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
  
  fluidRow(
    # # button tests
    # column(width = 2, 
    #        id = "test_button",
    #        class = "btn action-button",
    #        img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSMarYB40UbCVZrK2iqGCeuUjGzsDOHNiqqPWfLWcijFP202PAmew75Zh2JIfpQhFOd3Bs&usqp=CAU")
    # ),
    
    column(width = 2, 
           id = "field_pumpkin_button",
           class = "btn action-button",
           img(src = "field_pumpkin.png",
               height = 150, width = 150)
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
    column(width = 12,
           style = "text-align:center",
           textOutput(outputId = "printed_prompt")
    )
  ),
  
  fluidRow(
    plotOutput(outputId = "fighter_stats")
  )
)

# server ----
server <- function(input, output, session) {
  
  out <- eventReactive(input$field_pumpkin_button, {
    paste("IT WORKS")
  })
  
  output$printed_prompt <- renderText({
    out()
  })
}

# run app ----
shinyApp(ui, server)