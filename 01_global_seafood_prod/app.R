# set up ----
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(thematic)
library(ggiraph)
library(DT)
library(bslib)
library(here)

# data import ----
tidy_fish <- read_csv(here("01_global_seafood_prod", "tidy_fish.csv"))

# user interface ----
ui <- fluidPage(
  
  theme = bs_theme(version = 4,
                   bootswatch = "minty"),
  
  fillPage(padding = c(20, 80, 20, 80)), 
  
  titlePanel("Title goes here - give me a minute"),
  br(),
  
  sidebarLayout(
    
    ## sidebar panel ----
    sidebarPanel(
      
      tags$strong("Inputs for the bar plot"),
      br(),
      ## bar plot----  
      ### year input
      selectizeInput(inputId = "year",
                     label = "Select a year",
                     #' because there are duplicate years, need to pull out
                     #' unique years only in order for selected to work
                     choices = unique(tidy_fish$year), 
                     selected = "1950"),  
      
      ## line graph----
      ### sector catch type
      tags$strong("Inputs for the line graph"),
      checkboxGroupInput(inputId = "sector_catch_type",
                         label = "Select a type",
                         choices = unique(tidy_fish$sector_catch_type),
                         selected = unique(tidy_fish$sector_catch_type)),
      
      ## data table and column chart
      tags$strong("Inputs for column chart and data table"),
      sliderInput(inputId = "year_range",
                  label = "Select a range of years",
                  min = 1950L, max = 2010L, sep = "", step = 1,
                  value = c(1950L, 1980L))
    ),
    
    ## main panel ----
    mainPanel(fluidRow(
      splitLayout(cellWidths = c("50%", "50%"),
                  girafeOutput("giraffe_bar"), girafeOutput("giraffe_plot")),
      plotOutput("col_chart"),
      DTOutput("table")
    ))
  )
)

# server ----
server <- function(input, output, session) {
  
  ## reactive dataframes because you know we need 'em ----
  reactive_fish_year <- reactive({
    tidy_fish %>% filter(year %in% input$year)
  })
  
  reactive_fish_type <- reactive({
    tidy_fish %>% filter(sector_catch_type %in% input$sector_catch_type)
  })
  
  ## bar plot (server) ----
  output$giraffe_bar <- renderGirafe({
    
    req(input$year)
    
    gg_bar <- ggplot(reactive_fish_year(),
                     aes(x = sector_catch_type, y = value, fill = sector_catch_type,
                         tooltip = paste(year, value, sep = "\n"),
                         data_id = sector_catch_type)) +
      geom_col_interactive() + 
      theme_bw() +
      theme(legend.position = "none")
    
    girafe(ggobj = gg_bar,
           options = list(opts_selection(type = "single", only_shiny = FALSE)))
  })
  
  ## line graph (server) ----
  output$giraffe_plot <- renderGirafe({
    
    req(input$year)
    
    gg_line <- ggplot(reactive_fish_type(),
                   aes(x = year, y = value, group = sector_catch_type,
                       tooltip = paste(sector_catch_type, value, year, sep = "\n"), 
                       data_id = NULL,
                       color = sector_catch_type)) +
      geom_line_interactive(aes(color = sector_catch_type),
                            size = 1.5) +
      theme_bw() +
      theme(legend.position = "none")
    
    girafe(ggobj = gg_line,
           options = list(opts_selection(type = "multiple", only_shiny = FALSE)))
  })
  
  ## column chart (server) ----
  # col_chart
  output$col_chart <- renderPlot({
    
    ggplot(reactive_fish_year(),
           aes(x = sector_catch_type, y = value, fill = sector_catch_type)) +
      geom_col() +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none")
    
  })
  
  ## DT datatable (to start - server)
  # table
  
}

# RUN THE APP! ----
thematic_shiny()
shinyApp(ui, server)