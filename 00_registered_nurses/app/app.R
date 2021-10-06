# set up ----
library(shiny)
library(shiny386)
library(ggplot2)
library(datadigest)
library(here)
library(readr)

# data import ----
nurses_tidy <- read_csv(here("00_registered_nurses/data", "nurses_tidy.csv"))

# ui ----
ui <- fluidPage(
  fillPage(padding = c(20, 100, 20, 100)),
  
  style = "max-height: 500vh; overflow-y: auto;", 
  
  sidebarLayout(
    
    # sidebar panel ----
    sidebarPanel(),
    
    # main panel ----
    mainPanel(
      
      # pixel map output ----
      plotOutput(outputId = "pixel_map"),
      
      # datadigest output ----
      codebookOutput(outputId = "cool_table")
    )
    
  )
  
)

# server ----
server <- function(input, output, session) {
  
  # pixel map render ----
  output$pixel_map <- renderPlot({
    
    ggplot(map, aes(round(long, 0), round(lat, 0), 
                    group = group,
                    fill = as.factor(group))) +
      geom_polygon() +
      guides(fill = FALSE) +
      coord_map() +
      theme_void()
    
  })
  
  # datadigest render ----
  output$cool_table <- renderExplorer({
    codebook(nurses_tidy)
  })
  
}

# run app ----
shinyApp(ui, server)