if(startsWith(Sys.info()['nodename'], "o")){
  # FIXME: not sure why we still need to do this since opening a terminal you see
  # that with the proper .Renviron that LD_LIBRARY_PATH has gdal and if you do
  # ldd /users/PZS0562/efranz/R/x86_64-pc-linux-gnu-library/3.6/rgdal/libs/rgdal.so
  # you see libgdal.so.20 point to /usr/local/R/gnu/9.1/3.6.3/site/lib/gdal/2.4.4/lib/libgdal.so.20
  # and you can confirm that /usr/local/R/gnu/9.1/3.6.3/site/lib/gdal/2.4.4/lib/libgdal.so.20 exists
  # and /usr/local/R/gnu/9.1/3.6.3/site/lib/gdal/2.4.4/lib is in the LD_LIBRARY_PATH env var
  #
  dyn.load("/usr/local/R/gnu/9.1/3.6.3/site/lib/gdal/2.4.4/lib/libgdal.so")
}
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(formattable)
library(sf)
source("assist/utility.R")
load("data/pseudo_data.RData")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
LOCATION_SELECTION <- location_info %>% distinct(location_name) %>% pull()
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
    column(width = 3,
           h3(textOutput("box_location_name")),
           tags$strong("Category: "), br(),
           tags$strong("Census Name & Number: "), br(),
           tags$strong(textOutput("box_location_address")), br(),
           #br(),
           actionBttn(
             inputId = "data_entry_button",
             label = "Data Entry",
             style = "unite", 
             color = "primary")
           ),
    column(width = 6,
           leafletOutput("box_location_map", height = 220)
           ),
    column(width = 3,
           # valueBoxOutput("box_number_of_test"),
           # valueBoxOutput("box_positivity")
           valueBox(
             value = textOutput("box_test_number"),
             subtitle = "Test number",
             icon = icon("clipboard", lib = "font-awesome"),
             width = 12,
             color = "red",
             href = NULL),
           valueBox(
             value = textOutput("box_positivity"),
             subtitle = "Positivity",
             icon = icon("notes-medical", lib = "font-awesome"),
             width = 12,
             color = "red",
             href = NULL)
           )
  )
}

ui_tab_location <- function() {
  # sidebarLayout(
  #   sidebarPanel(width = 6, style = "overflow-y:scroll; max-height: 600px",
  #                ui_location_box()
  #                ),
  #   mainPanel(width = 6,
  #             
  #             )
  # )
  sidebarLayout(
    sidebarPanel(width = 12,
                 fluidRow(
                   column(width = 6,
                          searchInput(
                            inputId = "search", label = HTML("<strong>Search a region</strong>"),
                            value = "thompson library Columbus",
                            placeholder = "thompson library Columbus",
                            btnSearch = icon("search"),
                            btnReset = icon("remove"),
                            width = "450px"
                          )
                        ),
                   column(width = 6,
                          selectInput(
                            inputId = "select_search", label = HTML("<strong>Select a region</strong>"),
                            choices = LOCATION_SELECTION,
                            selected = "thompson library Columbus",
                            width = "400px"
                          )
                        )
                 ),
                 ui_location_box()
                 ),
    mainPanel(width = 12,
              
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
           ui_tab_location()
           ),
  tabPanel("Instructions")
)


server <- function(input, output, session) {
  
  location_data <- reactive({
    # TODO: PSEUDO DATA
    return(pseudo_data)
  })
  
  location_data <- reactive({
    # TODO: PSEUDO DATA
    return(location_info)
  })
  recommendation_data <- reactive({
    # TODO: PSEUDO DATA
    return(recommendation)
  })
  test_data <- reactive({
    # TODO: PSEUDO DATA
    return(tests)
  })
  schedule_data <- reactive({
    # TODO: PSEUDO DATA
    return(schedule)
  })
  user_data <- reactive({
    # TODO: PSEUDO DATA
    return(users)
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
    algo_recommendation <- recommendation_data()
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
    location_info <- location_data()
    #bins <- c(0, 0.02, 0.05, 0.07, 0.10, 0.12, 0.15, 0.17, 0.20, 1)
    pal <- colorBin("YlOrRd", domain = location_info$positivity, bins = 5)
    RADIUS_MIN <- 300
    RADIUS_MAX <- 900
    
    popup_content <- 
      location_info %>% 
      select(address, location_name, test_number, positivity) %>%
      pmap_chr(get_pop_content)
    location_info <- 
      location_info %>%
      mutate(popup_content = popup_content)
    
    #pin-outline
    icons <- awesomeIcons(
      icon = "map-pin",
      iconColor = "black",
      library = "fa",
      markerColor  = "red"
    )
    # Map ------------------------------------------------------------------------ #
    leaflet(data = location_info,
            options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE)) %>%
      #addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addAwesomeMarkers(
        ~lng, ~lat,
        layerId = ~location_name,
        icon=icons,
        popup = ~popup_content
      ) %>%
      addCircles(
        ~lng, ~lat,
        weight = 2, color = "black",
        fillColor = ~pal(positivity), stroke = TRUE,
        layerId = ~address,
        labelOptions = labelOptions(noHide = F, direction = 'auto'),
        options = markerOptions(riseOnHover = TRUE),
        #opacity = ~ positivity, #~expit(positivity),
        #fillOpacity = ~positivity,
        radius = ~normalize(test_number, min = RADIUS_MIN, max = RADIUS_MAX)) %>%
      # addCircles(
      #   ~lng, ~lat,
      #   weight = 2, color = "black", 
      #   fillColor = ~pal(positivity), stroke = TRUE,
      #   labelOptions = labelOptions(noHide = F, direction = 'auto'),
      #   options = markerOptions(riseOnHover = TRUE),
      #   #opacity = ~ positivity, #~expit(positivity),
      #   #fillOpacity = ~positivity,
      #   radius = ~normalize(total_test, min = RADIUS_MIN, max = RADIUS_MAX)) %>% 
      addLegend(
        pal = pal, values = ~positivity, 
        opacity = 0.7, title = "Positivity", position = "topright",
        labFormat = labelFormat(
          prefix = "(", suffix = ")%", between = ", ",
          transform = function(x) 100 * x
          )
        )
  })
  
  # observe({
  #   click<-input$map_marker_click
  #   if(is.null(click))
  #     return()
  #   text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
  #   text2<-paste("You've selected point ", click$id)
  #   # map$clearPopups()
  #   # map$showPopup( click$lat, click$lng, text)
  #   output$Click_text<-renderText({
  #     click$id
  #   })
  # 
  # })
  observeEvent(input$regional_map_marker_click, { 
    p <- input$regional_map_marker_click
    # output$Click_text <- renderText({
    #   p$id
    # })
    updateSearchInput(
      session = session,
      inputId = "search",
      value = p$id,
      trigger = TRUE
    )
    updateSelectInput(
      session = session,
      inputId = "select_search",
      selected = p$id
    )
    #print(p)
  }, ignoreInit = TRUE)
  
  observe({
    selection <- input$select_search
    updateSearchInput(
      session = session,
      inputId = "search",
      value = selection,
      trigger = TRUE
    )
  })
  
  # Observe search input
  observe({
    target_region <- input$search
    location_info <- location_data() %>%
      filter(location_name == target_region)
    output$box_location_name <- renderText({
      location_info$location_name
    })
    output$box_location_address <- renderText({
      location_info$address
    })
    output$box_location_map <- renderLeaflet({
      popup_content <- 
        location_info %>% 
        select(address, location_name, test_number, positivity) %>%
        pmap_chr(get_pop_content)
      location_info <- 
        location_info %>%
        mutate(popup_content = popup_content)
      
      #pin-outline
      icons <- awesomeIcons(
        icon = "map-pin",
        iconColor = "black",
        library = "fa",
        markerColor  = "red"
      )
      # Map ------------------------------------------------------------------------ #
      leaflet(data = location_info,
              options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE)) %>%
        #addTiles() %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addAwesomeMarkers(
          ~lng, ~lat,
          layerId = ~location_name,
          icon=icons,
          popup = ~popup_content
        )
    })
    # output$box_number_of_test <-renderValueBox({
    #   valueBox(
    #     value = location_info$test_number,
    #     subtitle = "Test number",
    #     icon = icon("code"),
    #     width = 12,
    #     color = "red",
    #     href = NULL)
    # })
    # output$box_positivity <- renderValueBox({
    #   valueBox(
    #     value = location_info$positivity,
    #     subtitle = "Positivity",
    #     icon = icon("code"),
    #     width = 12,
    #     color = "red",
    #     href = NULL)
    # })
    output$box_positivity <- renderText({
      as.character(percent(location_info$positivity))
    })
    output$box_test_number <- renderText({
      location_info$test_number
    })
  })
  
  # output$location_map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%  # Add default OpenStreetMap map tiles
  #     addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  # })
}

shinyApp(ui, server)
