# set up ----
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(thematic)
library(ggiraph)
library(bslib)
library(here)

# data import ----
tidy_fish <- read_csv(here("01_global_seafood_prod", "tidy_fish.csv"))

# user interface ----
ui <- fluidPage(
  
  theme = bs_theme(version = 4,
                   bootswatch = "minty"),
  
  sidebarLayout(
    
    ## sidebar panel ----
    sidebarPanel(
      
      tags$strong("Inputs for the bar plot"),
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
                         selected = unique(tidy_fish$sector_catch_type))
    ),
    
    ## main panel ----
    mainPanel(
      
      ### sector type barplot (ui) ----
      # plotOutput(outputId = "sector_type_barplot",
      #            hover = hoverOpts(id = "plot_hover", delayType = "throttle")),
      
      girafeOutput("giraffe_bar"),
      
      ### sector type line graph (ui)
      # # ggplot
      # plotOutput(outputId = "sector_type_line_graph",
      #            brush = "plot_brush"),
      
      # girafe
      girafeOutput("giraffe_plot"),
      
      # table from brushed lines in ggplot line graph
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
  
  reactive_fish_table <- reactive({
    tidy_fish %>% filter(sector_catch_type %in% input$sector_catch_type)
  })
  
  ## sector type barplot (server) ----
  # output$sector_type_barplot <- renderPlot({
  #   
  #   req(input$year)
  #   
  #   ggplot(data = reactive_fish(),
  #          aes(x = sector_catch_type, y = value,
  #              fill = sector_catch_type)) + 
  #     geom_col() + 
  #     theme_bw() +
  #     theme(legend.position = "none")
  #   
  # }, res = 96)
  
  ## sector type line graph (server) ----
  ### plot ----
  # output$sector_type_line_graph <- renderPlot({
  #   
  #   req(input$year)
  #   
  #   ggplot(data = reactive_fish_table(),
  #          aes(x = year, y = value, group = sector_catch_type)) +
  #     geom_line(aes(color = sector_catch_type),
  #               size = 1.5) + 
  #     theme_bw() +
  #     theme(legend.position = "none")
  #   
  # }, res = 96)
  
  output$giraffe_bar <- renderGirafe({
    
    req(input$year)
    
    gg_bar <- ggplot(reactive_fish(),
                     aes(x = sector_catch_type, y = value, fill = sector_catch_type,
                         tooltip = paste(year, value, sep = "\n"),
                         data_id = sector_catch_type)) +
      geom_col_interactive() + 
      theme_bw() +
      theme(legend.position = "none")
    
    girafe(ggobj = gg_bar,
           options = list(opts_selection(type = "single", only_shiny = FALSE)))
  })
  
  ### now in giraffe flavor ----
  
  output$giraffe_plot <- renderGirafe({
    
    req(input$year)
    
    gg_line <- ggplot(reactive_fish_table(),
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
  

  ## sector type data table (server) ----
  output$table_data <- renderTable({
    
    brushedPoints(tidy_fish,
                  input$plot_brush)

  })
}

# RUN THE APP! ----
thematic_shiny()
shinyApp(ui, server)