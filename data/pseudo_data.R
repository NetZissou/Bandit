# pseudo data

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
LOCATION_CANDIDATES <- c(
  "thompson library Columbus",
  "18th library Columbus",
  "Ohio Union Columbus",
  "Condado Tacos Columbus",
  "Buckeye Donuts Columbus N High St",
  "Buckeye Donuts Columbus S High St",
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
      "fields" = "formatted_address,geometry,name"
    )
  )
  response <- content(res, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame() %>%
    select(1:4) %>%
    set_names(c("formatted_address", "location_name", 
                "lat", "lng")) %>%
    as_tibble()
  if (nrow(df) == 0) {
    cat(address, ": Failed to connect\n")
    df <- tibble(
      formatted_address = character(),
      location_name = character(),
      lat = numeric(),
      lng = numeric()
    )
  }
  return(df)
}


get_location_geometry_safely <- 
  possibly(
    get_location_geometry,
    otherwise = tibble(
      formatted_address = character(),
      location_name = character(),
      lat = numeric(),
      lng = numeric()
    )
  )
get_multiple_locations <- function(addresses = LOCATION_CANDIDATES) {
  return(map_df(addresses, get_location_geometry_safely))
}

pseudo_data <-
  get_multiple_locations(addresses = LOCATION_CANDIDATES) %>%
  mutate(
    total_test = c(200, 400, 600, 800, 1000, 1000, 800),
    positivity = c(0.1, 0.25, 0.12, 0.3, 0.03, 0.17, 0.59)
  )

pseudo_recommendation <- pseudo_data %>%
  select(address, formatted_address) %>%
  mutate(confidence = c(0.97, 0.95, 0.96, 0.90, 0.92, 0.97, 0.98))


# Users --------------------------------------------------------

users <- tibble(
  name = c("Daria", "Dane", "David", "Eli", "Gregg", "Sam", "Net"),
  type = c("admin", "admin", "member", "member", "member", "member", "admin"),
  group_id = c(1, 1, 1, 2, 2, 3, 3)
)

schedule <- tibble(
  group_id = c(1, 1, 2, 2, 2, 3, 3, 3),
  location_name = c("thompson library Columbus",
                    "18th library Columbus",
                    "Ohio Union Columbus",
                    "Condado Tacos Columbus",
                    "Buckeye Donuts Columbus",
                    'Lennox Town Center Columbus',
                    "Condado Tacos Columbus",
                    "Buckeye Donuts Columbus"),
  date = c(ymd(20210301), ymd(20210301), ymd(20210301), ymd(20210301),
           ymd(20210302), ymd(20210302), ymd(20210302), ymd(20210303))
)

tests <- tibble(
  date = c(rep(ymd(20210301), 50), rep(ymd(20210302), 50)),
  result = rbernoulli(100, .1),
  location_name = sample(LOCATION_CANDIDATES, 100, replace = T),
  group_id = sample(c(1,2,3), 100, replace = T)
)

location_info <- pseudo_data %>%
  select(
    location_name = address, address = formatted_address, 
    lat, lng,
    test_number = total_test, 
    positivity)


recommendation <- pseudo_recommendation %>%
  rename(location_name = address)

save(pseudo_data, pseudo_recommendation, 
     users, schedule, tests, 
     location_info, recommendation, file = "data/pseudo_data.RData")
