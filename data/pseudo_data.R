# pseudo data

library(httr)
library(jsonlite)
library(tidyverse)

LOCATION_CANDIDATES <- c(
  "thompson library Columbus",
  "18th library Columbus",
  "Ohio Union Columbus",
  "Condado Tacos Columbus",
  "Buckeye Donuts Columbus",
  'Lennox Town Center Columbus'
)

get_location_geometry <- function(address = "Museum Contemporary Australia") {
  base_gmp_url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"
  res <- GET(
    url = base_gmp_url,
    query = list(
      "key" = Sys.getenv("GMP_API"),
      "input" = address,
      "inputtype" = "textquery",
      "fields" = "formatted_address,geometry"
    )
  )
  response <- content(res, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame() %>%
    select(1:3) %>%
    set_names(c("formatted_address", "lat", "lng")) %>%
    mutate(address = address) %>%
    as_tibble()
  return(df)
}

get_multiple_locations <- function(addresses = LOCATION_CANDIDATES) {
  return(map_df(addresses, get_location_geometry))
}

pseudo_data <-
  get_multiple_locations(addresses = LOCATION_CANDIDATES) %>%
  mutate(
    total_test = c(200, 400, 600, 800, 1000, 1000, 800),
    positivity = c(0.1, 0.25, 0.12, 0.3, 0.03, 0.17, 0.59)
  )

pseudo_recommendation <- pseudo_data %>%
  select(formatted_address) %>%
  mutate(confidence = c(0.97, 0.95, 0.96, 0.90, 0.92, 0.97, 0.98))

save(pseudo_data, pseudo_recommendation, file = "data/pseudo_data.RData")
