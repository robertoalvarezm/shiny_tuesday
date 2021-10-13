# set up ----
library(shiny)
library(ggplot2)
library(readr)
library(here)

# data import ----
tidy_fish <- read_csv(here("01_global_seafood_prod", "tidy_fish.csv"))

# user interface ----
ui <- fluidPage(
  
  sidebarLayout(
    
    ## sidebar panel ----
    sidebarPanel(
      
      ### year input ----
      selectizeInput(inputId = "year",
                     label = "Select a year",
                     #' because there are duplicate years, need to pull out
                     #' unique years only in order for selected to work
                     choices = unique(tidy_fish$year), 
                     selected = "1950") 
    ),
    
    ## main panel ----
    mainPanel(
      
    ### sector type barplot (ui) ----
    plotOutput(outputId = "sector_type_barplot"),
     
    ### sector type line graph (ui)
    plotOutput(outputId = "sector_type_line_graph",
                click = "plot_click"),
    
    tableOutput(outputId = "table_data")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  ## reactive dataframe because you know we need one ----
  reactive_fish <- reactive({
    tidy_fish %>% filter(year %in% input$year)
  })
  
  ## sector type barplot (server) ----
  output$sector_type_barplot <- renderPlot({
    
    req(input$year)
    
    ggplot(data = reactive_fish(),
         aes(x = sector_catch_type, y = value)) + 
    geom_col()
    
  }, res = 96)
  
  ## sector type line graph (server) ----
  output$sector_type_line_graph <- renderPlot({
    
    req(input$year)
    
    ggplot(data = tidy_fish,
           aes(x = year, y = value, group = sector_catch_type)) +
      geom_line(aes(color = sector_catch_type))
  }, res = 96)
  
  ## sector type data table (server) ----
  output$table_data <- renderTable({
    nearPoints(tidy_fish,
               input$plot_click,
               xvar = "year", yvar = "value")
  })
}

# RUN THE APP! ----
shinyApp(ui, server)