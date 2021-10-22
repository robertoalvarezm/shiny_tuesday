# set up ----
library(shiny)
library(readr)
library(dplyr)
library(DT)
library(here)

# data import ----
plong <- read_csv(here("03_gourd_wars/data", "pivoted_pumpkins.csv"))
plong_table <- plong %>% 
  mutate(type = as.factor(type),
         stats = as.factor(stats)) %>% 
  group_by(type) %>% 
  summarise(avg_value = mean(value, na.rm = TRUE)) 

# user interface ----
ui <- fluidPage(
  
  fluidRow(
    tags$h1("Select your fighter:")
  ),
  
  fluidRow(
    
    # button with image (instead of text) ----
    column(width = 2, 
           id = "field_pumpkin_button",
           class = "btn action-button",
           img(src = "field_pumpkin.png",
               height = 125, width = 125)
    ),
    
    # standard actionButtons - swap later ----
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
  ),
  
  fluidRow(
    DTOutput(outputId = "test_table")
  )
)

# server ----
server <- function(input, output, session) {
  
  # making each button reactive for table//graph filtration
  current_type <- reactiveVal()
  observeEvent(input$field_pumpkin_button, current_type("Field Pumpkin"))
  observeEvent(input$giant_pumpkin_button, current_type("Giant Pumpkin"))
  observeEvent(input$giant_squash_button, current_type("Giant Squash"))
  observeEvent(input$giant_watermelon_button, current_type("Giant Watermelon"))
  observeEvent(input$long_gourd_button, current_type("Long Gourd"))
  observeEvent(input$tomato_button, current_type("Tomato"))
  
  # table from reactiveVal
  output$test_table <- renderDataTable({
    req(current_type())
    plong_table %>%
      filter(type == current_type())
  })
  
  # graph from reactiveVal
  output$fighter_stats <- renderPlot({
    req(current_type()) 
    plong %>% 
      mutate(type = as.factor(type),
             stats = as.factor(stats)) %>% 
      group_by(type, stats) %>% 
      summarise(avg_value = mean(value, na.rm = TRUE),
                dplyr.summarize.inform = FALSE) %>% 
      filter(type == current_type()) %>% 
      ggplot(aes(x = stats, y = avg_value)) +
      geom_col() + 
      coord_flip() +
      theme_bw()
  })
  
  # button testing (server) ----
  out <- eventReactive(input$field_pumpkin_button, {
    paste("IT WORKS")
  })
  
  output$printed_prompt <- renderText({
    out()
  })
}

# run app ----
shinyApp(ui, server)