# set up ----
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(here)

# data ----
nurses_tidy <- read_csv(here("00_registered_nurses/data", "nurses_tidy.csv"))
states <- spData::us_states %>% rename(state = NAME)
nurses_states <- states %>% right_join(nurses_tidy, by = "state")

# leaflet customizations ----
# min: 0.32
# max: 307060
bins <- c(0, 5, 100, 500, 1000, 10000, 100000, 300000, 310000, Inf)
pal <- colorBin("YlOrRd", domain = nurses_states$hourly_wage_avg, bins = bins)

# user interface (ui) ----
ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(
        inputId = "year",
        label = "Select a year",
        min = 1998L, max = 2010L,
        value = 1999L, step = 1,
        sep = "", 
        animate = animationOptions(interval = 750,
                                   loop = TRUE)
      ),
      
      selectizeInput(inputId = "variable_of_interest",
                     label = "Select a variable",
                     choices = c("Total employed RNs" = "total_employed_rn",
                                 "Average hourly wage" = "hourly_wage_avg",
                                 "Average annual salary" = "annual_salary_avg",
                                 "Location quotient" = "location_quotient"),
                     selected = "hourly_wage_avg")
    ),
    
    mainPanel(
      
      leafletOutput("mymap")
    
      )
  )
  
)

# server ----
server <- function(input, output, session) {
  
  nurses_filter <- reactive({
    
    nurses_states %>% filter(year %in% input$year)
  
  })
  
  output$mymap <- renderLeaflet({
    leaflet(nurses_filter()) %>% 
      setView(lng = -99, lat = 43, zoom = 3) %>% 
      #addTiles() %>% 
      addPolygons(fillColor = reformulate(paste0("pal(", input$variable_of_interest, ")")),
        #fillColor = ~pal(hourly_wage_avg),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf(
                    "<strong>%s</strong><br/>%g average hourly wage",
                    nurses_filter()$state, nurses_filter()$hourly_wage_avg) %>% 
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                  )) 
  })
  
}

# run app ----
shinyApp(ui, server)