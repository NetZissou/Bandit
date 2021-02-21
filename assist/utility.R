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