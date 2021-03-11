library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(formattable)
library(shinyjs)
library(shinyTime)
#library(sf)
source("assist/utility.R")
load("data/pseudo_data.RData")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
# TODO: Make this interactive
LOCATION_SELECTION <- location_geo_info %>% distinct(location_name) %>% pull()
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
        selectInput(
          inputId = "entry_location", label = "Entry location",
          choices = LOCATION_SELECTION,
          selected = "thompson library Columbus",
          width = "400px"
        ),
        
        dateInput("entry_date", "Entry date: "),
        fluidRow(
          column(width = 6,
                 timeInput("entry_start_time", "Start time: ", value = Sys.time(), seconds = FALSE)
                 ),
          column(width = 6,
                 timeInput("entry_end_time", "Start time: ", value = Sys.time(), seconds = FALSE)
                 )
        ),
        numericInput(inputId = "entry_group_id", label = "Group ID", value = 1),
        
        fluidRow(
          column(width = 6,
                 numericInput("entry_positive", "Positives: ", value = 0)
          ),
          column(width = 6,
                 numericInput("entry_total", "Total tests: ", value = 0)
          )
        ),
        textAreaInput("entry_note", "Addition Note: ", 
                      placeholder = "Add notes, or leave this empty."
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

# =========================== Server Side ===========================================
server <- function(input, output, session) {
  
  
  location_data <- reactive({
    # TODO: PSEUDO DATA
    return(location_geo_info)
  })
  # recommendation_data <- reactive({
  #   input$refresh
  #   recent_recommendation <- get_recommend_location()
  #   return()
  # })
  test_data <- reactive({
    input$submit
    recent_test <- loadData_Dropbox()
    return(recent_test)
  })
  assignment_data <- reactive({
    input$assign
    recent_assignment <- loadAssigment_Dropbox()
    return(recent_assignment)
  })
  
  map_info <- reactive({
    # --------------------------------- #
    # Gather Information from tables
    # --------------------------------- #
    location_info <- location_data()
    
    # test)
    test_summarised <- test_data() %>%
      group_by(location_name) %>%
      summarise_at(vars(positive, total), .funs = list(sum)) %>%
      mutate(
        positivity = positive/total
      )
    location_info <- 
      location_info %>% 
      left_join(
        test_summarised, by = "location_name"
      )
    # recommendation)
    recommended_locations <- recommendation_data$location_name
    location_info <- location_info %>%
      mutate(recommended = location_name %in% recommended_locations)
    
    # assignment)
    selected_date <- input$map_date
    assignment <- assignment_data()
    
    location_info <- location_info %>%
      left_join(assignment %>%
                  filter(date == ymd(20210317)), by = "location_name") %>%
      mutate(assigned = location_name %in% assignment$location_name) 
    
    location_info <- location_info %>%
      mutate(
        icon_type = case_when(
          assigned ~ "green",
          !assigned&recommended ~ "red",
          TRUE ~ "regular"
        )
      )
    #print(location_info)
    # --------------------------------- #
    # Specfiy the size and palette
    # --------------------------------- #
    pal <- colorBin("YlOrRd", domain = location_info$positivity, bins = 5)
    # --------------------------------- #
    # Generating Pop information
    # --------------------------------- #
    # TODO: recommendation status and assignment status
    popup_content <- 
      location_info %>% 
      select(address, location_name, 
             total, positivity, assigned, group_id, recommended) %>%
      pmap_chr(get_pop_content)
    location_info <- 
      location_info %>%
      mutate(popup_content = popup_content)
    
    map_info_list <- list(
      location_info = location_info,
      pal = pal
    )
    return(map_info_list)
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
  pin_icons <- awesomeIconList(
    green =  makeAwesomeIcon(
      icon = "glyphicon-check",
      iconColor = "black",
      library = "glyphicon",
      markerColor  = "green"
    ),
    red =  makeAwesomeIcon(
      icon = "glyphicon-pushpin",
      iconColor = "black",
      library = "glyphicon",
      markerColor  = "red"
    ),
    regular =  makeAwesomeIcon(
      icon = "glyphicon-map-marker",
      iconColor = "black",
      library = "glyphicon",
      markerColor  = "lightgray"
    )
  )
  
  RADIUS_MIN <- 300
  RADIUS_MAX <- 900
  
  output$regional_map <- renderLeaflet({
    map_info_list <- map_info()
    location_info <- map_info_list$location_info
    pal <- map_info_list$pal
    # Map ------------------------------------------------------------------------ #
    leaflet(data = location_info,
            options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE)) %>%
      # Tiles
      addProviderTiles(providers$Stamen.Toner) %>%
      # Markers: All
      addAwesomeMarkers(
        ~lng, ~lat,
        layerId = ~location_name,
        icon= ~pin_icons[icon_type],
        popup = ~popup_content
      ) %>%
      # Circles: All
      addCircles(
        ~lng, ~lat,
        weight = 2, color = "black",
        fillColor = ~pal(positivity), stroke = TRUE,
        layerId = ~location_name,
        labelOptions = labelOptions(noHide = F, direction = 'auto'),
        options = markerOptions(riseOnHover = TRUE),
        #opacity = ~ positivity, #~expit(positivity),
        #fillOpacity = ~positivity,
        radius = ~normalize(total, min = RADIUS_MIN, max = RADIUS_MAX)) %>%
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
    map_info_list <- map_info()
    location_info <- map_info_list$location_info
    pal <- map_info_list$pal
    
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
      goto_location_info <-  map_info()$location_info %>%
        filter(location_name == goto_location_name)
    )
    goto_lat <- goto_location_info %>% pull(lat)
    goto_lng <- goto_location_info %>% pull(lng)
    #cat(goto_lat, goto_lng)
    proxy <- leafletProxy("regional_map")
    proxy %>% setView(lng = goto_lng, lat = goto_lat, zoom = 15)
  })
  observe({
    recommendation_df <- recommendation_data
    location_df <- location_data()
    
    algo_recommendation_location <-
      recommendation_df %>%
      arrange(-confidence) %>%
      mutate(
        pct_confidence = percent(confidence),
        location_confidence = glue::glue("{location_name}: {pct_confidence}")
      ) %>%
      pull(location_confidence)
    other_reommendation_location <-
      location_df %>%
      pull(location_name) %>%
      setdiff(., recommendation_df$location_name)
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
  # ===================================================== #
  # ================ Search input update event ==========
  # ===================================================== #
  
  
  
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
    isolate(
      location_info <- map_info()$location_info %>%
        filter(location_name == target_region)
    )
    output$box_location_name <- renderText({
      location_info$location_name
    })
    output$box_location_address <- renderText({
      location_info$address
    })
    output$box_location_map <- renderLeaflet({
      
      # Map ------------------------------------------------------------------------ #
      leaflet(data = location_info,
              options = leafletOptions(zoomControl = TRUE, attributionControl=FALSE)) %>%
        #addTiles() %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addAwesomeMarkers(
          ~lng, ~lat,
          layerId = ~location_name,
          icon= ~pin_icons[icon_type],
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
      location_info$total
    })
  })
  # ===================================================== #
  # ================ Data Entry =========================
  # ===================================================== #
  entry_data <- reactive({
    #input$data_entry_button
    input$submit
    data <- isolate(
      tibble(
        "group_id" = input$entry_group_id,
        "location_name" = input$entry_location,
        "date" = ymd(input$entry_date), 
        "start_time" = ymd_hm(input$entry_start_time),
        "end_time" = ymd_hm(input$entry_end_time),
        "positive" = input$entry_positive,
        "total" = input$entry_total,
        "note" = input$entry_note
      )
    )
    return(data)
  })
  
  observeEvent(input$submit,{
    saveData_Dropbox(entry_data())
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
    loadData_Dropbox() %>%
      as_tibble() %>%
      mutate(
        date = ymd(date),
        start_time = as_datetime(start_time),
        end_time = as_datetime(end_time)
      ) %>%
      filter(location_name == location)
  })
}

shinyApp(ui, server)
