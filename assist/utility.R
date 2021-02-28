library(DBI)
library(RSQLite)
sqlitePath <- "data/banditdb.sqlite"
TEST_TABLE <- "tests"


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

get_pop_content <- function(address, location_name, test_number, positivity) {
  
  content <- 
    paste(
    tags$h4(tags$strong(location_name)),
    address, br(),
    tags$strong("Number of test: "), tags$u(test_number), "    ",
    tags$strong("Positivity: "), tags$u(percent(positivity))
  )
  return(content)
}


saveData <- function(data) {
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

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", TEST_TABLE)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

