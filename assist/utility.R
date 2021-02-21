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

get_pop_content <- function(formatted_address, address, total_test, positivity) {
  
  content <- 
    paste(
    tags$h4(tags$strong(address)),
    formatted_address, br(),
    tags$strong("Number of test: "), tags$u(total_test), "    ",
    tags$strong("Positivity: "), tags$u(percent(positivity))
  )
  return(content)
}

