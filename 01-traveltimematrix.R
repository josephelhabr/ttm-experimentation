library(r5r)
library(dplyr)
library(tigris)
library(sf)

options(java.parameters = "-Xmx4G")

data_path <-
  "00_raw_data"

r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

#cleaning SNAP data

snap <- read.csv("00_raw_data/hist_snap_retailer_final2021.csv")

snap_travis <-
  filter(snap, state == "TX", county == "TRAVIS")

snap_travis_coordinates <-
  filter(snap, state == "TX", county == "TRAVIS") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

snap_travis_real <- #eliminating store that was way out
  snap_travis_coordinates[-1358,] 

snap_travis_final <- #another store that was way out
  snap_travis_real[-1454,]

#getting block groups

travis_bgs <-
  block_groups(state = "TX", county = "TRAVIS", year = "2018")

# 5-minute blocks between 9am and 6pm central, 108 departures
set.seed(732)
departure_datetimes <- 
  seq(as.POSIXct("11-09-2022 9:00:00", format = "%m-%d-%Y %H:%M:%S"), 
      as.POSIXct("11-09-2022 17:55:00", format = "%m-%d-%Y %H:%M:%S"), 
      by = "5 min") +
  runif(108, 0, 300)

#r5r needs points as inputs and specific column names

travis_bgs_pt <- #origins (block groups)
  block_groups(state = "TX", county = "TRAVIS", year = 2018) %>%
  st_centroid() %>%
  st_transform("+init=epsg:4326") %>%
  transmute(
    id = GEOID, 
    lat = as.numeric(INTPTLAT), 
    lon = as.numeric(INTPTLON), 
    geometry = geometry)

snap_stores_pt <- #destinations (SNAP stores)
  snap_travis %>%
  transmute( 
    id = as.character(fips_bg),
    lat = as.numeric(y),
    lon = as.numeric(x)) 
  
  #figure out later (is this necessary?)
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")

#set parameters for travel time matrices

mode <- c("WALK", "TRANSIT")
max_walk_time <- 60   # minutes
max_trip_duration <- 180 #minutes
departure_datetime <- as.POSIXct("11-09-2022 12:00:00", format = "%m-%d-%Y %H:%M:%S")

#block group to SNAP time travel matrix

ttm_snap <- travel_time_matrix(
  r5r_core,
  mode = mode,
  origins = travis_bgs_pt,
  destinations = snap_stores_pt, 
  departure_datetime = departure_datetime,
  max_walk_time = max_walk_time,
  max_trip_duration = max_trip_duration
)

ttm_snap_car <- travel_time_matrix(
  r5r_core,
  mode = "CAR",
  origins = travis_bgs_pt,
  destinations = snap_stores_pt, 
  departure_datetime = departure_datetime,
  max_walk_time = max_walk_time,
  max_trip_duration = max_trip_duration
)

#save ttm_snap

save(ttm_snap, file = "01_tidy_data/ttm_snap.Rdata")
save(ttm_snap_car, file = "01_tidy_data/ttm_snap_car.Rdata")

#create the travel time matrices for am and pm windows (IRRELEVANT NOW)

ttm_am <- travel_time_matrix(
  r5r_core,
  time_window = 120,
  mode = mode,
  origins = travis_bgs_pt,
  destinations = travis_bgs_pt, 
  departure_datetime = departure_datetime_am,
  max_trip_duration = max_trip_duration
  )

ttm_ev <- travel_time_matrix(
  r5r_core,
  time_window = 120,
  mode = mode,
  origins = travis_bgs_pt,
  destinations = travis_bgs_pt,
  departure_datetime = departure_datetime_ev,
  max_trip_duration = max_trip_duration
  )
