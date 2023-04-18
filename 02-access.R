library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(r5r)
library(ggspatial)

travis_bgs <- 
  block_groups(state = "TX", county = "TRAVIS", year = 2019)

options(java.parameters = "-Xmx4G")

data_path <-
  "00_raw_data"

r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

tx_counties <-
  counties("TX")

austin_highways <-
  primary_roads("TX", "TRAVIS") #revisit this

ggplot() + #revisit this
  geom_sf(data = austin_highways, col = "black", fill = "white", size = .25)

#plot SNAP locations

ggplot() + 
  geom_sf(data = travis_bgs) +
  geom_sf(data = snap_travis_real, col = "red") + 
  ggthemes::theme_map()

#number of SNAP locations in each block group in travis county

snap_locations_bgs <-
  snap_travis_real %>%
  st_drop_geometry() %>%
  group_by(fips_bg) %>%
  summarize(num_stores = n()) %>%
  mutate(fips_bg = as.character(fips_bg))

#loading the travel time matrix (AM only) IRRELEVANT NOW

load("01_tidy_data/ttm_AMonly.RData")

#extract OSM network (revisit this)

street_net <- street_network_to_sf(r5r_core)

#define cuts

cut_times <- function(times_to_cut) {
  the_cuts <- cut(times_to_cut, breaks = c(0, 10, 20, 30, 40, 50, 60, 120, 180))
}

#shortest travel time by public transit to the 3rd nearest grocery store

food_access_3nearest <- #irrelevant now, old ttm
  inner_join(ttm_am, snap_locations_bgs, by = c("to_id" = "fips_bg")) %>%
  arrange(from_id, travel_time_p50, num_stores, .desc = TRUE) %>%
  group_by(from_id) %>%
  mutate(num_stores_cumul = cumsum(num_stores)) %>%
  filter(num_stores_cumul >= 3) %>%
  summarize(tt_3rd = min(travel_time_p50))

food_access_3nearestsnap <- #snap TRANSIT ttm
  inner_join(ttm_snap, snap_locations_bgs, by = c("to_id" = "fips_bg")) %>%
  arrange(from_id, travel_time_p50, num_stores, .desc = TRUE) %>%
  group_by(from_id) %>%
  mutate(num_stores_cumul = cumsum(num_stores)) %>%
  filter(num_stores_cumul >= 3) %>%
  summarize(tt_3rd = min(travel_time_p50)) %>%
  mutate(time_cuts = cut_times(time))

food_access_3nearestsnap_car <- #snap CAR ttm
  inner_join(ttm_snap_car, snap_locations_bgs, by = c("to_id" = "fips_bg")) %>%
  arrange(from_id, travel_time_p50, num_stores, .desc = TRUE) %>%
  group_by(from_id) %>%
  mutate(num_stores_cumul = cumsum(num_stores)) %>%
  filter(num_stores_cumul >= 3) %>%
  summarize(tt_3rd = min(travel_time_p50))

tt3_plot_data <- #old ttm
  inner_join(travis_bgs, food_access_3nearest, by = c("GEOID" = "from_id"))

tt3_plot_datasnap <- #snap TRANSIT ttm
  inner_join(travis_bgs, food_access_3nearestsnap, by = c("GEOID" = "from_id"))

tt3_plot_datasnap_car <- #snap CAR ttm
  inner_join(travis_bgs, food_access_3nearestsnap_car, by = c("GEOID" = "from_id"))

ggplot() + #old ttm
  geom_sf(data = tt3_plot_data, aes(color = tt_3rd, fill = tt_3rd)) +
  geom_sf(data = snap_travis_real, color = "black", alpha = 0.5, size = .8, stroke = 0) +
  scale_color_viridis_c(direction = -1) +
  scale_fill_viridis_c(direction = -1) + 
  ggthemes::theme_map()

ggplot() + #TRANSIT snap ttm
  geom_sf(data = tt3_plot_datasnap, aes(color = tt_3rd, fill = tt_3rd)) +
  geom_sf(data = snap_travis_final, color = "black", alpha = 0.5, size = .45, stroke = 0) +
  geom_sf(data = tx_counties, col = grey(.1), fill = "transparent") + 
  coord_sf(xlim = c(-98.2, -97.4), ylim = c(30.03, 30.65), expand = TRUE) +
  annotation_scale(aes(unit_category = c("imperial"))) +
  annotation_north_arrow(aes(location = "br"), style = north_arrow_minimal()) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), panel.spacing = unit(0, 
          "lines"), plot.background = element_blank())

ggplot() + #CAR snap ttm
  geom_sf(data = tt3_plot_datasnap_car, aes(color = tt_3rd, fill = tt_3rd)) +
  geom_sf(data = snap_travis_final, color = "black", alpha = 0.5, size = .45, stroke = 0) +
  geom_sf(data = tx_counties, col = grey(.1), fill = "transparent") + 
  coord_sf(xlim = c(-98.2, -97.4), ylim = c(30.03, 30.65), expand = TRUE) +
  annotation_scale(aes(unit_category = c("imperial"))) +
  annotation_north_arrow(aes(location = "br"), style = north_arrow_minimal()) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), panel.spacing = unit(0, 
          "lines"), plot.background = element_blank())
  