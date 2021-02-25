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
                    draggable = FALSE, top = 60, left = 10, right = "auto", bottom = "auto",
                    width = "auto", height = "auto",
                    
                    br(),
                    
                    dropdownButton(
                      # box(width = 12, status = "danger", 
                      #     tags$strong(h1("What's Next?")),
                      #     helpText("Click widget for more information.")
                      # ),
                      # 
                      # br(),
                      br(),
                      tags$strong(h1("What's Next?")),
                      helpText("Click widget in the map for more information."),
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
                      # fluidRow(
                      #   ui_recommend_table()
                      # ),
                      ui_recommend_table(),
                      circle = TRUE, status = "danger", right = FALSE,
                      icon = icon("gear"), width = "350px",
                      
                      tooltip = tooltipOptions(title = "Click to see inputs !", 
                                               placement = "right")
                    )
                    
        ),
      )
}

# TODO: Apply renderUI 
ui_location_box <- function() {
  fluidRow(
    column(width = 8,
           box(width = 12, status = "primary",
               column(width = 7,
                      h3("Location Name"),
                      tags$strong("Category: "), br(),
                      tags$strong("Census Name & Number: "), br(),
                      tags$strong("Street Address: "), br(),
                      br(),
                      actionBttn(
                        inputId = "Id107",
                        label = "Data Entry",
                        style = "unite", 
                        color = "primary")
                      ),
               column(width = 5,
                      leafletOutput("location_map", height = 200)
                      )
           )
    ),
    column(width = 4,
           valueBox(
             value = "1,345",
             subtitle = "Lines of code written",
             icon = icon("code"),
             width = 12,
             color = "red",
             href = NULL),
           
           valueBox(
             value = "1,345",
             subtitle = "Lines of code written",
             icon = icon("code"),
             width = 12,
             color = "red",
             href = NULL)
    )
  )
}

ui_tab_location <- function() {
  sidebarLayout(
    sidebarPanel(width = 6, style = "overflow-y:scroll; max-height: 600px",
                 ui_location_box()
                 ),
    mainPanel(width = 6,
              
              )
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
  tabPanel("Locations",
           ui_tab_location(),
           ),
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
    # Preparing ------------------------------------------------------------------ #
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
    
    #pin-outline
    icons <- awesomeIcons(
      icon = "map-pin",
      iconColor = "black",
      library = "fa",
      markerColor  = "red"
    )
    # Map ------------------------------------------------------------------------ #
    leaflet(data = testing_data,
            options = leafletOptions(zoomControl = FALSE)) %>%
      #addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addAwesomeMarkers(
        ~lng, ~lat,
        icon=icons,
        popup = ~popup_content
      ) %>%
      addCircles(
        ~lng, ~lat,
        weight = 2, color = "black", 
        fillColor = ~pal(positivity), stroke = TRUE,
        labelOptions = labelOptions(noHide = F, direction = 'auto'),
        options = markerOptions(riseOnHover = TRUE),
        #opacity = ~ positivity, #~expit(positivity),
        #fillOpacity = ~positivity,
        radius = ~normalize(total_test, min = RADIUS_MIN, max = RADIUS_MAX)) %>% 
      addLegend(
        pal = pal, values = ~positivity, 
        opacity = 0.7, title = "Positivity", position = "topright",
        labFormat = labelFormat(
          prefix = "(", suffix = ")%", between = ", ",
          transform = function(x) 100 * x
          )
        )
  })
  
  output$location_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })
}

shinyApp(ui, server)
