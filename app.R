library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(formattable)
source("assist/utility.R")
load("data/pseudo_data.RData")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
# ================ UI: Regional Map ===============================================
ui_recommend_table <- function(box_width = 12) {
  box(width = box_width,
      formattableOutput("recommend_table")
      )
}

ui_tab_regional_map <- function() {
  div(class = "outer",
      # Main: map -----------------------------------------------------------------
      leafletOutput("regional_map", width = "100%", height = "100%"),
      # Sidebar:  -----------------------------------------------------------------
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 400, height = "auto",
                    
                    tags$strong(h1("Where to Go Next?")),
                    
                    br(),
                    
                    box(collapsible = T, width = 12, status = "danger",
                        fluidRow(
                          # Refresh Button
                          column(width = 7,
                                 actionBttn(
                                   inputId = "refresh",
                                   label = "Run Algorithm", 
                                   style = "unite",
                                   color = "danger",
                                   icon = icon("sync")
                                 )
                          ),
                          # Last Run Date
                          column(width = 5,
                                 htmlOutput("last_run_date")
                          )
                        ),
                        
                        br(),
                        
                        # Recommendations List
                        fluidRow(
                          ui_recommend_table()
                        )
                      )
                    
        ),
      )
}
# ================ UI: Layout =====================================================
ui <- navbarPage("SARS-COV-2 Mobile Testing", id = "nav", selected = "Regional Map",
  useShinydashboard(),
  theme = shinytheme("yeti"),
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css")
  ),
  tabPanel("Regional Map",
           ui_tab_regional_map()
           ),
  tabPanel("Locations"),
  tabPanel("Instructions")
)


server <- function(input, output, session) {
  
  tested_data <- reactive({
    # TODO: PSEUDO DATA
    return(pseudo_data)
  })
  
  algo_data <- reactive({
    # TODO: PSEUDO DATA
    return(pseudo_recommendation)
  })
  
  output$last_run_date <- renderUI({
    date <- as.character(Sys.Date())
    text <- paste0(
      tags$h5(tags$strong("Last Run: ")),
      tags$u(date)
    )
    return(HTML(text))
  })
  
  output$recommend_table<- renderFormattable({
    algo_recommendation <- algo_data()
    algo_recommendation %>%
      rename(`Recommended Location` = formatted_address, `Confidence` = confidence) %>%
      arrange(-Confidence) %>%
      mutate(Confidence = percent(Confidence)) %>%
      formattable(align = c("l", "c"),
      list(
        `Recommended Location` = 
          formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
        `Confidence` = color_tile(customGreen0, customGreen)
      )
    )
  })
  
  output$regional_map <- renderLeaflet({
    # Color tiles
    # TODO: corresponding to the input data
    # TODO: Categorize the testing size 
    testing_data <- tested_data()
    #bins <- c(0, 0.02, 0.05, 0.07, 0.10, 0.12, 0.15, 0.17, 0.20, 1)
    pal <- colorBin("YlOrRd", domain = testing_data$positivity, bins = 5)
    
    leaflet(data = testing_data) %>%
      #addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addCircles(
        ~lng, ~lat,
        weight = 2, color = "black", fillColor = ~pal(positivity),
        opacity = ~ positivity, #~expit(positivity),
        #fillOpacity = ~positivity,
        radius = ~total_test,
        popup = ~formatted_address) %>% 
      addLegend(pal = pal, values = ~positivity, 
                opacity = 0.7, title = NULL, position = "bottomleft",
                labFormat = labelFormat(
                  prefix = "(", suffix = ")%", between = ", ",
                  transform = function(x) 100 * x
                ))
  })
}

shinyApp(ui, server)
