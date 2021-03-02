library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(formattable)
library(shinyjs)
#library(sf)
source("assist/utility.R")
load("data/pseudo_data.RData")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
# TODO: Make this interactive
LOCATION_SELECTION <- location_info %>% distinct(location_name) %>% pull()
# ================ UI: Regional Map ===============================================
ui_recommend_table <- function() {
  formattableOutput("recommend_table")
}

ui_tab_regional_map <- function() {
  div(class = "outer",
      # Main: map -----------------------------------------------------------------
      leafletOutput("regional_map", width = "100%", height = "100%"),
      # Sidebar:  -----------------------------------------------------------------
      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      #               draggable = FALSE, top = 60, left = 10, right = "auto", bottom = "auto",
      #               width = "auto", height = "auto",
      absolutePanel(id = "controls", top = 5, left = 5, right = "auto", bottom = "auto",
                    width = "360px", height = "auto", draggable = TRUE,              
                    br(),
                    ## TODO: Uncomment this if we want to bring dropdown back again
                    # dropdownButton(
                    #   # box(width = 12, status = "danger", 
                    #   #     tags$strong(h1("What's Next?")),
                    #   #     helpText("Click widget for more information.")
                    #   # ),
                    #   # 
                    #   
                    #   circle = TRUE, status = "danger", right = FALSE,
                    #   icon = icon("gear"), width = "350px",
                    #   
                    #   tooltip = tooltipOptions(title = "Click to see inputs !", 
                    #                            placement = "right")
                    # ),
                    box(id = "control_box", width = 12, title = tagList(shiny::icon("gear"), "Toolkit"), 
                        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                        fluidRow(
                          column(width = 3, align = "left",
                                 switchInput("show_all",
                                             label = "Locations",
                                             size = "mini",
                                             onStatus = "success",
                                             onLabel = "All",
                                             offLabel = "",
                                             value = TRUE)
                                 ),
                          column(width = 1),
                          column(width = 3, align = "left",
                                 switchInput("show_legend",
                                             label = "Legend",
                                             size = "mini",
                                             onStatus = "success",
                                             value = TRUE)
                          )
                          
                        ),
                      tabsetPanel(
                        tabPanel(title = "Recommendation",
                                 #br(),
                                 tags$strong(h1("What's Next?")),
                                 helpText("Click widget in the map for more information."),
                                 fluidRow(
                                   # Refresh Button
                                   column(width = 5, align="center",
                                          actionBttn(
                                            inputId = "refresh",
                                            label = "Run Algorithm", 
                                            style = "unite",
                                            size = "sm",
                                            color = "danger",
                                            icon = icon("sync")
                                          )
                                   ),
                                   # Last Run Date
                                   column(width = 7, align="center",
                                          htmlOutput("last_run_date")
                                   )
                                 ),
                                 
                                 br(),
                                 
                                 #ui_recommend_table(),
                                 
                                 selectizeInput(
                                   inputId = "goto",
                                   label = "Qucik view location",
                                   choices = "",
                                   multiple = FALSE,
                                   options = list(
                                     placeholder = 'Search/Select a location to view',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )
                                 )
                                             
                        ),
                        tabPanel(title = "Controls",
                                 
                                 # TODO: Update progress bar
                                 progressBar(id = "assign_progress", value = 10,
                                             total = 20, status = "danger", title = "Location assigned"),
                                 
                                 dateInput("map_date", label = "Show date"),
                                 # TODO: Update the input option as well after selection
                                 selectizeInput(
                                   inputId = "assign_location_choice",
                                   label = "Select locations to assign",
                                   choices = LOCATION_SELECTION,
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = 'Search/Select locations or from the map',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6, #align="center",
                                          selectInput(
                                            inputId = "assign_group_choice",
                                            label = "Select group to assign",
                                            choices = c(1:3), # TODO: update the group id
                                            multiple = T
                                          )),
                                   column(width = 6, #align="center",
                                          br(),
                                          actionBttn("assign",
                                                     label = "Assign to group",
                                                     style = "unite", 
                                                     color = "primary",
                                                     size = 'sm'))
                                 )
                                 
                                 
                        )
                      )
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
           tags$strong(textOutput("box_location_address"))
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
ui_data_entry <- function() {
  fluidRow(
    box(title = "Data Entry", width = 4, status = "danger",
        dateInput("entry_date", "Entry date: "),
        numericInput(inputId = "entry_group_id", label = "Group ID", value = 1),
        selectInput(
          inputId = "entry_location", label = "Entry location",
          choices = LOCATION_SELECTION,
          selected = "thompson library Columbus",
          width = "400px"
        ),
        selectInput(
          inputId = "entry_result", label = "Test result",
          choices = c("Positive" = 1, "Negative" = 0),
          selected = "Negative",
          width = "400px"
        ),
        actionBttn(
          inputId = "submit",
          label = "Submit",
          style = "unite", 
          color = "primary")
    ),
    box(title = "Location Test Data", width = 8, status = "info",
        DT::dataTableOutput("test_data_table")
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
                          selectizeInput(
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
              ui_data_entry()
              )
  )
}
# ================ UI: Layout =====================================================
ui <- bootstrapPage(
  navbarPage("SARS-COV-2 Mobile Testing", id = "nav", selected = "Regional Map",
  useShinydashboard(),
  #useShinydashboardPlus(),
  useShinyjs(),
  useSweetAlert(),
  collapsible = TRUE,
  theme = shinytheme("yeti"),
  tags$head(
    # Include our custom CSS
    #includeCSS("styles.css")
    tags$style(type = "text/css", 
    "div.outer {
      position: fixed;
      top: 41px;
      left: 0;
      right: 0;
      bottom: 0;
      overflow: hidden;
      padding: 0;}
    body, label, input, button, select { 
    font-family: 'Helvetica Neue', Helvetica;
    font-weight: 200;
    } h1, h2, h3, h4 { font-weight: 400; }
    #control_box {
      opacity: 0.85;
    }")
  ),
  tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  tabPanel("Regional Map",
           ui_tab_regional_map()
           ),
  tabPanel("Locations",
           ui_tab_location()
           ),
  tabPanel("Instructions")
  
  #dateInput("test_date", "Select a date")
  #moves navbar right
  
))


server <- function(input, output, session) {
  
  
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
  # ===================================================== #
  # ================ Recommendation Table ===============
  # ===================================================== #
  # output$recommend_table<- renderFormattable({
  #   DISPLAY_NUMBER <- 3
  #   algo_recommendation <- recommendation_data()
  #   algo_recommendation %>%
  #     rename(`Recommendation` = location_name, 
  #            `Address`  =formatted_address,
  #            `Confidence` = confidence) %>%
  #     arrange(-Confidence) %>%
  #     head(DISPLAY_NUMBER) %>%
  #     mutate(Confidence = percent(Confidence)) %>%
  #     formattable(align = c("l", "c"),
  #     list(
  #       `Recommendation` = 
  #         formatter("span", style = ~ style(color = "black",font.weight = "bold")),
  #       `Address` = formatter("span", style = ~ style(color = "grey")),
  #       `Confidence` = color_tile(customGreen0, customGreen)
  #     )
  #   )
  # })
  # ===================================================== #
  # ================ Main Regional Map ==================
  # ===================================================== #
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
      # Tiles
      addProviderTiles(providers$Stamen.Toner) %>%
      # Markers: All
      addAwesomeMarkers(
        ~lng, ~lat,
        layerId = ~location_name,
        icon=icons,
        popup = ~popup_content
      ) %>%
      # Circles: All
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
      addLegend(
        pal = pal, values = ~positivity, 
        opacity = 0.7, title = "Positivity", position = "bottomright",
        labFormat = labelFormat(
          prefix = "(", suffix = ")%", between = ", ",
          transform = function(x) 100 * x
          )
        )
  })
  # Show/Hide Legend
  observeEvent(input$show_legend, {
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
    
    proxy <- leafletProxy("regional_map", data = location_info)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$show_legend) {
      pal <- pal
      proxy %>% addLegend(
        pal = pal, values = ~positivity, 
        opacity = 0.7, title = "Positivity", position = "bottomright",
        labFormat = labelFormat(
          prefix = "(", suffix = ")%", between = ", ",
          transform = function(x) 100 * x
        )
      )
    }
  })
  # ===================================================== #
  # ================ Main map click event ===============
  # ===================================================== #
  
  # ----------------------------- Map click ------------------------------------ #
  observeEvent(input$regional_map_marker_click, { 
    p <- input$regional_map_marker_click
    # output$Click_text <- renderText({
    #   p$id
    # })
    current_location_selection <- input$assign_location_choice
    updated_location_selection <- c(p$id, current_location_selection)
    updateSelectizeInput(
      session = session,
      inputId = "assign_location_choice",
      selected = updated_location_selection
    )
    # updateSearchInput(
    #   session = session,
    #   inputId = "search",
    #   value = p$id,
    #   trigger = TRUE
    # )
    updateSelectizeInput(
      session = session,
      inputId = "select_search",
      selected = p$id
    )
    updateSelectizeInput(
      session = session,
      inputId = "entry_location",
      selected = p$id
    )
    #print(p)
  }, ignoreInit = TRUE)
  
  # --------------------------------Goto ------------------------------------- #
  observe({
    goto_location_name <- str_extract(input$goto, "[^:]+")
    isolate(
      goto_location_info <- location_data() %>%
        filter(location_name == goto_location_name)
    )
    goto_lat <- goto_location_info %>% pull(lat)
    goto_lng <- goto_location_info %>% pull(lng)
    #cat(goto_lat, goto_lng)
    proxy <- leafletProxy("regional_map")
    proxy %>% setView(lng = goto_lng, lat = goto_lat, zoom = 15)
  })
  # ===================================================== #
  # ================ Search input update event ==========
  # ===================================================== #
  
  observe({
    recommendation_df <- recommendation_data()
    location_df <- location_data()
    
    algo_recommendation_location <- 
      recommendation_df %>%
      rename(address = formatted_address) %>%
      # Sort by confidence
      arrange(-confidence) %>%
      mutate(
        pct_confidence = percent(confidence),
        location_confidence = glue::glue("{location_name}: {pct_confidence}")
      ) %>%
      pull(location_confidence)
    other_reommendation_location <- 
      location_df %>%
      pull(location_name) %>%
      setdiff(., recommendation_df$location_name )
    updateSelectizeInput(
      session = session, 
      inputId = "goto",
      #selected = algo_recommendation_location[1],
      choices = list(
        `Recommendation` = algo_recommendation_location,
        `Other Location` = other_reommendation_location
      )
    )
    
  })
  
  observe({
    selection <- input$select_search
    updateSelectizeInput(
      session = session,
      inputId = "entry_location",
      selected = selection
    )
  })
  
  # Observe search input
  observe({
    target_region <- input$select_search
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
              options = leafletOptions(zoomControl = TRUE, attributionControl=FALSE)) %>%
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
  # ===================================================== #
  # ================ Data Entry =========================
  # ===================================================== #
  entry_data <- reactive({
    #input$data_entry_button
    input$submit
    data <- isolate(
      c(
        "date" = as.double(ymd(input$entry_date)), 
        "result" = input$entry_result, 
        "location_name" = input$entry_location, 
        "group_id" = input$entry_group_id
      )
    )
    return(data)
  })
  
  observeEvent(input$submit,{
    saveData(entry_data())
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "Data recieved",
      type = "success"
    )
  })

  output$test_data_table <- DT::renderDataTable({
    input$submit
    location <- input$entry_location
    loadData() %>%
      as_tibble() %>%
      mutate(
        date = as_date(date),
        result = ifelse(result == 1, "Positive", "Negative")
      ) %>%
      filter(location_name == location)
  })
}

shinyApp(ui, server)
