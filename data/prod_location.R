# pseudo data

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

get_location_geometry <- function(address = "Museum Contemporary Australia", ohio_only = FALSE) {
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
  
  if (ohio_only) {
    df <- df %>%
      filter(
        stringr::str_detect(formatted_address, "OH")
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

# ======================================================================== #
# --------------------- Adding Locations --------------------------------- #
# ======================================================================== #

# location_geo_info data format spec

location_geo_info <-
  tibble::tibble(
    location_name = character(),
    address = character(),
    lat = numeric(),
    lng = numeric(),
    type = character()
  )

# current location candidate from google sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1l43ZF_6z14ZkJ5y2u8Ne4CvqrNAZ3hk2Ivgw5YbyylY/edit#gid=578810276"
sheet_location <- 
  googlesheets4::read_sheet(
    ss = sheet_url,
    sheet = 6 # Final list
  ) %>%
  filter(Status == "Go") %>%
  select(
    location_name = `Building/Park`,
    address = Address,
    type = Type
  )
sheet_location_geo_info <- 
  sheet_location %>%
  bind_cols(
    map_dfr(
      sheet_location$address,
      get_location_geometry,
      ohio_only = TRUE
    ) %>%
      select(lat, lng)
  ) %>%
  select(
    location_name,
    address,
    lat, lng,
    type
  )

location_geo_info <- 
  location_geo_info %>%
  bind_rows(
    sheet_location_geo_info
  )
# Time: 07/30/2021
# Adding Baker Systems Engineering building 

geo_info_baker_system_engineering <- 
  get_location_geometry("Baker Systems Engineering building Columbus Ohio") %>%
  transmute(
    location_name,
    address = formatted_address,
    lat, lng,
    type = "Education"
  )
location_geo_info <- 
  location_geo_info %>%
  bind_rows(geo_info_baker_system_engineering)



save(
  location_geo_info,
  file = "data/location_geo_info_prod.RData"
)


