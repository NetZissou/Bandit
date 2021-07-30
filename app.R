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
#load("data/location_geo_info.RData")
load("data/location_geo_info_prod.RData")
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
                    width = "380px", height = "auto", draggable = TRUE,              
                    br(),
                    box(id = "control_box", width = 12, 
                        title = tagList(shiny::icon("gear"), "Where to Go Next?"), 
                        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                        
                        fluidRow(
                          column(width = 3, align = "left",
                                 switchInput("show_all",
                                             label = "Recommend",
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
                        
                        selectizeInput(
                          inputId = "goto",
                          label = "Qucik view location",
                          choices = "",
                          multiple = FALSE,
                          options = list(
                            placeholder = 'Search/Select a location to view',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),
                        
                        fluidRow(
                          # Refresh Button
                          column(width = 6, 
                                 tags$strong("Last Run: "), tags$u("2021-03-11")
                          ),
                          # Last Run Date
                          column(width = 6,
                                 actionBttn(
                                   inputId = "refresh",
                                   label = "Recommend", 
                                   style = "unite",
                                   size = "sm",
                                   color = "danger",
                                   icon = icon("sync")
                                 )
                                 
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 dateInput("map_date", 
                                           value = as.character(Sys.Date()),
                                           label = "")
                          ),
                          column(width = 6, 
                                 br(),
                                 actionBttn("assign",
                                            label = "Assign to group",
                                            style = "unite", 
                                            color = "primary",
                                            size = 'sm')
                          )
                        ),
                        
                        DT::DTOutput("assignment_table", width = "50%"),
                        #verbatimTextOutput('sel')
                        
                        
                    )
      ),
  )
}

# TODO: Apply renderUI 
ui_location_box <- function() {
  fluidRow(
    column(width = 3,
           h3(textOutput("box_location_name")),
           tags$strong("Category: "), textOutput("box_location_type"),
           tags$strong("Address: "), textOutput("box_location_address")
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
        
        dateInput("entry_date", "Entry date: ", value = as.character(Sys.Date())),
        fluidRow(
          column(width = 6,
                 timeInput("entry_start_time", "Start time: ", 
                           seconds = FALSE, value = Sys.time()-hours(6))
          ),
          column(width = 6,
                 timeInput("entry_end_time", "End time: ", 
                           seconds = FALSE, value = Sys.time())
          )
        ),
        numericInput(inputId = "entry_group_id", label = "Group ID", value = 1),
        
        fluidRow(
          column(width = 6,
                 numericInput("entry_positive", "Positives: ", value = 0, min = 0)
          ),
          column(width = 6,
                 numericInput("entry_total", "Total tests: ", value = 0, min = 0)
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
                            opacity: 0.85;}
                          @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);
                          ")
             ),
             #tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
             
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
  # test_data <- reactive({
  #   input$submit
  #   recent_test <- loadData_Dropbox_cleaned()
  #   return(recent_test)
  # })
  test_data <- reactiveValues(
    data = loadData_Dropbox_cleaned()
  )
  observeEvent(input$submit, {
    test_data$data = loadData_Dropbox_cleaned()
  }, priority = -1)
  assignment_data <- reactiveValues(
    data = loadAssigment_Dropbox_cleaned()
  )
  observeEvent(input$assign, {
    assignment_data$data = loadAssigment_Dropbox_cleaned()
  }, priority = -1)
  observeEvent(input$ok, {
    assignment_data$data = loadAssigment_Dropbox_cleaned()
  }, priority = -2)
  
  
  bandit_algorithm_data <- reactive({
    
    location_info <- location_data()
    
    test_summarised <- test_data$data %>%
      group_by(location_name) %>%
      summarise_at(vars(positive, total), .funs = list(sum)) %>%
      mutate(
        positivity = ifelse(total>0, positive/total, 0)
      )
    
    location_info <- 
      location_info %>% 
      left_join(
        test_summarised, by = "location_name"
      ) %>%
      replace_na(list(
        positive = 0,
        total = 0,
        positivity = 0
      ))
    
    bandit_data <- location_info %>%
      mutate(
        negative = total - positive
      ) %>%
      select(location_name, positive, negative) 
    
    return(bandit_data)
  })
  
  bandit_algorithm_recommendation <- reactive({
    input$refresh
    bandit_data <- bandit_algorithm_data() %>%
      mutate(
        prob = map2_dbl(positive, negative, get_beta_obs, n = 1)
      ) %>%
      arrange(-prob) %>%
      mutate(
        prob = round(prob, digits = 4)
      )
    return(bandit_data)
  })
  
  map_info <- reactive({
    # --------------------------------- #
    # Gather Information from tables
    # --------------------------------- #
    location_info <- location_data()
    
    # test)
    test_summarised <- test_data$data %>%
      group_by(location_name) %>%
      summarise_at(vars(positive, total), .funs = list(sum)) %>%
      mutate(
        positivity = ifelse(total>0, positive/total, 0)
      )
    location_info <- 
      location_info %>% 
      left_join(
        test_summarised, by = "location_name"
      )
    # recommendation)
    N_RECOMMENDATION <- 5
    algo_recommendations <- bandit_algorithm_recommendation() %>%
      head(N_RECOMMENDATION) %>%
      filter(prob > 0)
    recommended_locations <- algo_recommendations %>% pull(location_name)
    location_info <- location_info %>%
      mutate(recommended = location_name %in% recommended_locations)
    
    # assignment)
    selected_date <- input$map_date
    assignment <- assignment_data$data %>%
      filter(date == ymd(selected_date))
    
    location_info <- location_info %>%
      left_join(assignment, by = "location_name") %>%
      mutate(assigned = location_name %in% assignment$location_name) 
    
    location_info <- location_info %>%
      mutate(
        icon_type = case_when(
          assigned ~ "green",
          !assigned&recommended ~ "red",
          TRUE ~ "regular"
        )
      ) %>%
      replace_na(list(
        positive = 0,
        total = 0,
        positivity = 0,
        group_id = 0
      )) %>%
      select(
        -date, 
        -assign_datetime
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
      mutate(
        popup_content = popup_content,
        label_content = ifelse(assigned, 
                               paste0('Assigned to group#', group_id),
                               'Unassigned'
                               )
      )
    
    map_info_list <- list(
      location_info = location_info,
      pal = pal
    )
    return(map_info_list)
  })
  
  
  # observeEvent(input$refresh, {
  #   print(bandit_algorithm_recommendation())
  # })
  
  map_info_recommened <- reactive({
    map_info_list <- map_info()
    map_info_recommened_list <- list(
      location_info = map_info_list$location_info %>% filter(recommended),
      pal = map_info_list$pal
    )
    return(map_info_recommened_list)
  })
  
  last_click_location <- reactive({
    p <- input$regional_map_marker_click
    return(p$id)
  })
  
  
  # ===================================================== #
  # ---------------- Assignment Table -------------------
  # ===================================================== #
  
  
  # assignment_table_data <- recommendation_data %>%
  #   arrange(-confidence) %>%
  #   head(5) %>%
  #   rename(Location = location_name)
  # 
  # for (i in 1:nrow(assignment_table_data)) {
  #   assignment_table_data$assignment[i] <- 
  #     as.character(
  #       selectInput(paste0("sel", i), "",
  #                   selected = "Select",
  #                   choices = set_names(c("Select",  0, group_info$group_id),
  #                                       c("Select",  "Unassign", group_info$group_id)),
  #                   multiple = F,
  #                   width = "80px"))
  # }
  
  
  assignment_table_data <- reactive({
    algo_recommendation <- bandit_algorithm_recommendation() %>%
      select(Location = location_name, confidence = prob) %>%
      head(5)
    
    
    generate_selectInput <- function(index) {
      
      result <- as.character(
        selectInput(paste0("sel", index), "",
                    selected = "Select",
                    choices = set_names(c("Select",  0, group_info$group_id),
                                        c("Select",  "Unassign", group_info$group_id)),
                    multiple = F,
                    width = "80px"))
      return(result)
    }
    
    table_data <- algo_recommendation %>%
      mutate(
        index = row_number(),
        assignment = map_chr(index, generate_selectInput)
      ) %>%
      select(-index)
    
    return(table_data)
  })
  
  
  
  output$assignment_table <- DT::renderDT({
      assignment_table_data()
    }, escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  # output$sel = renderPrint({
  #   assign_entry()
  # })
  
  assign_entry <- reactive({
    input$assign
    
    table_data <- assignment_table_data()
    assign_list <- 
      sapply(1:nrow(table_data), function(i) input[[paste0("sel", i)]])
    
    assigned_index <- which(assign_list != "Select")
    assign_date <- input$map_date
    data <- tibble(
      group_id = numeric(),
      location_name = character(),
      date = Date(),
      assign_datetime = Date()
    )
    if (length(assigned_index) > 0) {
      data <- tibble(
        group_id = unlist(assign_list)[assigned_index],
        location_name = table_data[assigned_index,]$Location,
        date = ymd(assign_date),
        assign_datetime = Sys.time()
      )
    }
    return(data)
  })
  
  
  observeEvent(input$assign,{
    
    data <- isolate(assign_entry())
    if (nrow(data) > 0) {
      saveAssignment_Dropbox_cleaned(entry_data = data, 
                                     cleaned_data = assignment_data$data)
      
      sendSweetAlert(
        session = session,
        title = "SUCCESS !!",
        text = "Schedule Updated",
        type = "success"
      )  
    } else {
      sendSweetAlert(
        session = session,
        title = "INVALID",
        text = "Please assign at least one team",
        type = "warning"
      )  
    }
    
  }, priority = 2)
  
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
        popup = ~popup_content,
        label = ~label_content,
        labelOptions = labelOptions(
          noHide = F, textsize = "15px", opacity = 0.85
          )
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
  
  # Show recommended only
  
  observeEvent(input$show_all, {
    if (!input$show_all) {
      map_info_recommened_list <- map_info_recommened()
      location_info <- map_info_recommened_list$location_info
      pal <- map_info_recommened_list$pal
      if (nrow(location_info) == 0) {
        proxy <- leafletProxy("regional_map", data = location_info) %>%
          clearControls() %>%
          clearMarkers() %>%
          clearShapes() %>%
          setView(lat = 39.9612, lng = -82.9988, zoom = 11)
      } else {
        proxy <- leafletProxy("regional_map", data = location_info) %>%
          clearControls() %>%
          clearMarkers() %>%
          clearShapes() %>%
          # Markers: All
          addAwesomeMarkers(
            ~lng, ~lat,
            layerId = ~location_name,
            icon= ~pin_icons[icon_type],
            popup = ~popup_content,
            label = ~label_content,
            labelOptions = labelOptions(
              noHide = F, textsize = "15px", opacity = 0.85
            )
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
      }
    } else {
      map_info_list <- map_info()
      location_info <- map_info_list$location_info
      pal <- map_info_list$pal
      # Map ------------------------------------------------------------------------ #
      leafletProxy("regional_map", data = location_info) %>%
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        # Markers: All
        addAwesomeMarkers(
          ~lng, ~lat,
          layerId = ~location_name,
          icon= ~pin_icons[icon_type],
          popup = ~popup_content,
          label = ~label_content,
          labelOptions = labelOptions(
            noHide = F, textsize = "15px", opacity = 0.85
          )
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
    }
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
    #recommendation_df <- recommendation_data
    recommendation_df <- bandit_algorithm_recommendation() %>%
      select(location_name, confidence = prob) %>%
      head(5)
    location_df <- location_data()
    
    algo_recommendation_location <-
      recommendation_df %>%
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
  
  # ------------------------------- MMAP Click assign ------------------------ #
  observeEvent(input$button_click, {
    clicked_location <- last_click_location()
    showModal(modalDialog(
      title = paste0("Assign / UnAssign ", clicked_location),
      textInput("click_assign_location", "Location",value = clicked_location),
      selectInput("click_assign_group", "Select group to assign (or unassign)",
                  selected = "Select",
                  choices = set_names(c("Select",  0, group_info$group_id),
                                      c("Select",  "Unassign", group_info$group_id)),
                  multiple = F),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    ))
  })
  
  click_assign_entry <- reactive({
    
    assign_group <- input$click_assign_group
    assign_location <- input$click_assign_location
    assign_date <- input$map_date
    
    data <- tibble(
      group_id = numeric(),
      location_name = character(),
      date = Date(),
      assign_datetime = Date()
    )
    
    if (assign_group != "Select") {
      data <- tibble(
        group_id = assign_group,
        location_name = assign_location,
        date = assign_date,
        assign_datetime = Sys.time()
      )
    }
    return(data)
  })
  
  observeEvent(input$ok,{
    #freezeReactiveValue(input, "")
    removeModal()
    data <- isolate(click_assign_entry())
    if (nrow(data) > 0) {
      saveAssignment_Dropbox_cleaned(entry_data = data,
                                     cleaned_data = assignment_data$data)

      sendSweetAlert(
        session = session,
        title = "SUCCESS !!",
        text = "Schedule Updated",
        type = "success"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "INVALID",
        text = "Please assign at least one team",
        type = "warning"
      )
    }

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
    location_info <- map_info()$location_info %>%
      filter(location_name == target_region)
    output$box_location_name <- renderText({
      location_info$location_name
    })
    output$box_location_type <- renderText({
      location_info$type
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
          popup = ~popup_content,
          label = ~label_content,
          labelOptions = labelOptions(
            noHide = F, textsize = "15px", opacity = 0.85
          )
        )
    })
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
        "start_time" = as_datetime(input$entry_start_time),
        "end_time" = as_datetime(input$entry_end_time),
        "positive" = input$entry_positive,
        "total" = input$entry_total,
        "note" = input$entry_note
      )
    )
    return(data)
  })
  
  observeEvent(input$submit,{
    saveData_Dropbox_cleaned(entry_data(), test_data$data)
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
    test_data$data %>%
      filter(location_name == location)
  })
}

shinyApp(ui, server)
