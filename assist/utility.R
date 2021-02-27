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

