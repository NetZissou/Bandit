library(tidyverse)
library(lubridate)
library(rdrop2)
library(DBI)
library(RSQLite)
sqlitePath <- "data/banditdb.sqlite"
token <- readRDS("droptoken.rds")
drop_auth(rdstoken = "droptoken.rds")
DROPBOX_TEST_DATA_PATH <- "Bandit/test data"
DROPBOX_TEST_CLEANED_DATA_PATH <- "Bandit/test_cleaned"
DROPBOX_ASSIGNMENT_DATA_PATH <- "Bandit/assignment"
DROPBOX_ASSIGNMENT_CLEANED_DATA_PATH <- "Bandit/assignment_cleaned"

logit <- function(p) {
  return(
    log(p/(1-p))
  )
}

expit <- function(etas) {
  return(
    exp(etas)/(1+exp(etas))
  )
}

get_pop_content <- function(address, location_name, 
                            total, positivity, assigned, group_id, recommended) {

  assign_status <- function(assigned, group_id) {
    if (assigned) {
      #tags$span(style="color:green",tags$strong(paste0("Assigned to Group#", group_id)))
      paste0("Assigned to Group#", group_id)
    } else {
      #tags$span(style="color:gray",tags$strong("Unassigned"))
      "Unassigned"
    }
  }
  
  recommend_status <- function(recommended) {
    if (recommended) {
      #tags$span(style="color:red",tags$strong(tags$strong("Yes")))
      "Yes"
    } else {
      #tags$span(style="color:gray",tags$strong("No"))
      "No"
    }
  }
  
  
  content <- 
    paste(
    tags$h4(tags$strong(location_name)),
    address, br(),
    tags$strong("Number of test: "), tags$u(total), "    ",
    tags$strong("Positivity: "), tags$u(percent(positivity)), br(),
    tags$strong("Recommend Status: ", recommend_status(recommended)), br(),
    tags$strong("Assign Status: ", assign_status(assigned, group_id)), br(),
    actionLink(inputId = "modal", 
               label = "Assign / Unassign", 
               onclick = 'Shiny.setInputValue(\"button_click\", this.id, {priority: \"event\"})')
  )
  return(content)
}

# ==================================================================== #
# ----------------------- Database Management ------------------------
# ==================================================================== #

saveData_SQLite <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    TEST_TABLE, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData_SQLite <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", TEST_TABLE)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

# Read in tibble
saveData_Dropbox <- function(data) {
  
  fileName <- paste0("test_data_entry_", 
                     as.character(Sys.time()), ".csv")
  filePath <- file.path(tempdir(), fileName)
  write_csv(data, filePath)
  suppressMessages(
    drop_upload(filePath, path = DROPBOX_TEST_DATA_PATH)
  )
}

loadData_Dropbox <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(DROPBOX_TEST_DATA_PATH)
  filePaths <- filesInfo$path_display
  
  data <- suppressMessages(
    map_df(
      filePaths, 
      drop_read_csv,
      stringsAsFactors = FALSE
    )  %>%
      mutate(
        date = ymd(date)
      ) %>%
      as_tibble()
  )
  return(data)
}

saveAssignment_Dropbox <- function(data) {
  
  fileName <- paste0("assignment_entry", 
                     "_", as.character(Sys.time()), ".csv")
  filePath <- file.path(tempdir(), fileName)
  write_csv(data, filePath)
  suppressMessages(
    drop_upload(filePath, path = DROPBOX_ASSIGNMENT_DATA_PATH)
  )
}


loadAssigment_Dropbox <- function() {
  data <- tibble(
    group_id = numeric(),
    location_name = character(),
    date = Date(),
    assign_datetime = Date()
  )
  # Read all the files into a list
  filesInfo <- drop_dir(DROPBOX_ASSIGNMENT_DATA_PATH)
  if (nrow(filesInfo) > 0) {
    filePaths <- filesInfo$path_display
    
    data <- suppressMessages(
      map_df(
        filePaths, 
        drop_read_csv,
        stringsAsFactors = FALSE
      )
    )
    data <- data %>%
      as_tibble() %>%
      mutate(
        date = ymd(date),
        assign_datetime = ymd_hms(assign_datetime)
      ) %>%
      group_by(
        location_name, date
      ) %>%
      filter(assign_datetime == max(assign_datetime)) %>%
      ungroup() %>%
      filter(group_id > 0)
  } 
  return(data)
}


# ==================================================================== #
# -------------------- Database Management Cleaned -------------------
# ==================================================================== #

loadData_Dropbox_cleaned <- function() {
  
  data <- tibble(
    group_id = numeric(),
    location_name = character(),
    date = Date(),
    start_time = POSIXct(),
    end_time = POSIXct(),
    positive = numeric(),
    total = numeric(),
    note = character()
  )
  
  # Read all the files into a list
  filesInfo <- drop_dir(DROPBOX_TEST_CLEANED_DATA_PATH)
  if (nrow(filesInfo) > 0) {
    filePaths <- filesInfo$path_display
    
    # data <- suppressMessages(
    #   map_df(
    #     filePaths, 
    #     drop_read_csv,
    #     stringsAsFactors = FALSE
    #   ) %>%
    #     mutate(
    #       date = ymd(date)
    #     ) %>%
    #     as_tibble()
    # )
    data <- map_df(
      filePaths, 
      drop_read_csv,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        date = ymd(date),
        start_time = as_datetime(start_time),
        end_time = as_datetime(end_time)
      ) %>%
      as_tibble()
    
  }
  return(data)
}

saveData_Dropbox_cleaned <- function(entry_data, cleaned_data) {
  
  # Save Single instance
  fileName <- paste0("test_data_entry_",
                     as.character(Sys.time()), ".csv")
  filePath <- file.path(tempdir(), fileName)
  write_csv(entry_data, filePath)
  suppressMessages(
    drop_upload(filePath, path = DROPBOX_TEST_DATA_PATH)
  )
  # Save appened version
  cleaned_data <- cleaned_data %>%
    bind_rows(
      entry_data
    )
  cleanedFileName <- paste0("test_data_entry.csv")
  cleanedFilePath <- file.path(tempdir(), cleanedFileName)
  write_csv(cleaned_data, cleanedFilePath)
  #suppressMessages(
    drop_upload(cleanedFilePath, path = DROPBOX_TEST_CLEANED_DATA_PATH)
  #)
}


saveAssignment_Dropbox_cleaned <- function(entry_data, cleaned_data) {
  
  cleaned_data <- cleaned_data %>%
    bind_rows(
      entry_data %>%
        mutate(
          group_id = as.numeric(group_id)
        )
    ) %>%
    mutate(
      date = ymd(date),
      assign_datetime = ymd_hms(assign_datetime)
    ) %>%
    group_by(
      location_name, date
    ) %>%
    filter(assign_datetime == max(assign_datetime)) %>%
    ungroup() %>%
    filter(group_id > 0)
  fileName <- paste0("assignment_entry.csv")
  filePath <- file.path(tempdir(), fileName)
  write_csv(cleaned_data, filePath)
  #suppressMessages(
    drop_upload(filePath, path = DROPBOX_ASSIGNMENT_CLEANED_DATA_PATH)
  #)
}

loadAssigment_Dropbox_cleaned <- function() {
  data <- tibble(
    group_id = numeric(),
    location_name = character(),
    date = Date(),
    assign_datetime = Date()
  )
  # Read all the files into a list
  filesInfo <- drop_dir(DROPBOX_ASSIGNMENT_CLEANED_DATA_PATH)
  if (nrow(filesInfo) > 0) {
    filePaths <- filesInfo$path_display
    
    data <- map_df(
      filePaths, 
      drop_read_csv,
      stringsAsFactors = FALSE
    )
    data <- data %>%
      as_tibble() %>%
      mutate(
        date = ymd(date),
        assign_datetime = ymd_hms(assign_datetime)
      )
  } 
  return(data)
}

# ==================================================================== #
# ----------------------- Algorithm Recommendation -------------------
# ==================================================================== #

get_recommend_location <- function() {
  return(recommendation_data)
}











