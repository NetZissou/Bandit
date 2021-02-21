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
ui_recommend_table <- function() {
  # box(width = box_width, collapsed = F,
  #     formattableOutput("recommend_table")
  #     )
  formattableOutput("recommend_table")
}

ui_tab_regional_map <- function() {
  div(class = "outer",
      # Main: map -----------------------------------------------------------------
      leafletOutput("regional_map", width = "100%", height = "100%"),
      # Sidebar:  -----------------------------------------------------------------
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = 60, left = "auto", right = 10, bottom = "auto",
                    width = "auto", height = "auto",
                    
                    tags$strong(h1("What's Next?")),
                    # 
                    # br(),
                    
                    box(collapsible = T, collapsed = T, width = 12, status = "danger", 
                        
                        fluidRow(
                          # Refresh Button
                          column(width = 5,
                                 actionBttn(
                                   inputId = "refresh",
                                   label = "Run Algorithm", 
                                   style = "unite",
                                   color = "danger",
                                   icon = icon("sync")
                                 )
                          ),
                          # Last Run Date
                          column(width = 7,
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
  collapsible = TRUE,
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
      tags$h5(tags$strong("Last Run: "), tags$u(date))
    )
    return(HTML(text))
  })
  
  output$recommend_table<- renderFormattable({
    DISPLAY_NUMBER <- 3
    algo_recommendation <- algo_data()
    algo_recommendation %>%
      rename(`Recommended Location (top 3)` = formatted_address, `Confidence` = confidence) %>%
      arrange(-Confidence) %>%
      head(DISPLAY_NUMBER) %>%
      mutate(Confidence = percent(Confidence)) %>%
      formattable(align = c("l", "c"),
      list(
        `Recommended Location (top 3)` = 
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
    RADIUS_MIN <- 300
    RADIUS_MAX <- 900
    
    popup_content <- 
      testing_data %>% 
      select(formatted_address, address, total_test, positivity) %>%
      pmap_chr(get_pop_content)
    testing_data <- 
      testing_data %>%
      mutate(popup_content = popup_content)
    
    leaflet(data = testing_data,
            options = leafletOptions(zoomControl = FALSE)) %>%
      #addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addCircles(
        ~lng, ~lat,
        weight = 2, color = "black", 
        fillColor = ~pal(positivity), stroke = TRUE,
        labelOptions = labelOptions(noHide = F, direction = 'auto'),
        options = markerOptions(riseOnHover = TRUE),
        #opacity = ~ positivity, #~expit(positivity),
        #fillOpacity = ~positivity,
        radius = ~normalize(total_test, min = RADIUS_MIN, max = RADIUS_MAX),
        popup = ~popup_content) %>% 
      addLegend(
        pal = pal, values = ~positivity, 
        opacity = 0.7, title = "Positivity", position = "bottomright",
        labFormat = labelFormat(
          prefix = "(", suffix = ")%", between = ", ",
          transform = function(x) 100 * x
          )
        )
  })
}

shinyApp(ui, server)
