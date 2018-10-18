# This is my test code
library(tidyverse)
shp_path <- system.file("data","sa2_2011.Rda", package = "sugaRbag")
load(system.file("data","capital_cities.Rda", package = "sugaRbag"))

shp_sf <- read_shape(shp_path, simplify = TRUE)
centroids <- create_centroids(shp_sf)

bbox <- tibble::tibble(min = c(min(centroids$longitude),
                               min(centroids$latitude)),
                       max = c(max(centroids$longitude),
                               max(centroids$latitude)))

hex_size <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2]) / 10
buffer_dist <- (bbox$max[1] - bbox$min[1]) / 10

hex_grid <- create_grid(centroids, bbox, hex_size, buffer_dist)

#ggplot(hex_grid, aes(x=hex_long, y=hex_lat)) + geom_point(size=0.1)

# Turn grid into integer grid
nlong <- length(unique(hex_grid$hex_long))
nlat <- length(unique(hex_grid$hex_lat))
hex_grid <- hex_grid %>%
    mutate(hex_long_int = dense_rank(hex_long)-1,
           hex_lat_int = dense_rank(hex_lat)-1)

# Check bounds of hex grid are external to bounds of centroids
summary(hex_grid$hex_long)
summary(hex_grid$hex_lat)
summary(centroids$longitude)
summary(centroids$latitude)

# Round centroids to closest grid point
centroids <- centroids %>%
    mutate(long_int = round((longitude-min(hex_grid$hex_long))/(max(hex_grid$hex_long)-min(hex_grid$hex_long))*nlong, 0),
           lat_int = round((latitude-min(hex_grid$hex_lat))/(max(hex_grid$hex_lat)-min(hex_grid$hex_lat))*nlat, 0))

ggplot(hex_grid, aes(x=hex_long_int, y=hex_lat_int)) +
    geom_point(size=0.02) +
    geom_point(data=centroids, aes(x=long_int, y=lat_int), colour="red", size=0.1)

cent_hull <- chull(centroids$long_int, centroids$lat_int)
ggplot(hex_grid, aes(x=hex_long_int, y=hex_lat_int)) +
    geom_point(size=0.02) +
    geom_point(data=centroids[cent_hull,], aes(x=long_int, y=lat_int), colour="red", size=1)

# LATITUDE ROWS FILTER
# amount of latitude in sliding window

size_lat = round(max(hex_grid$hex_lat_int)/20,0)

# find the min and max longitude for each latitude
range_rows <- centroids %>%
    group_by(lat_int) %>%
    summarise(min = min(longitude), max = max(longitude))

# rolling window to find min and max long for sets of lats
long_mean_range <- function(...) {
    data <- list(...)
    tibble(lat_int = data$lat_int[[1]], long_min = min(data$min), long_max = max(data$max))
}

# TODO issue with extremes, cannot use extreme range values
slide_rows <- tsibble::pslide_dfr(range_rows, long_mean_range, .size = lat_size, .partial = TRUE)
# fill ourselves?
# boundary values?

# LONGITUDE COLS FILTER
size_long = round(max(hex_grid$hex_long_int)/20,0)

# find the min and max longitude for each latitude
range_cols <- centroids %>%
    group_by(long_int) %>%
    summarise(min = min(latitude), max = max(latitude))

# rolling window to find min and max long for sets of lats
lat_mean_range <- function(...) {
    data <- list(...)
    tibble(long_int = data$long_int[[1]], lat_min = min(data$min), lat_max = max(data$max))
}

# TODO issue with extremes, cannot use extreme range values
slide_cols <- tsibble::pslide_dfr(range_cols, lat_mean_range, .size = size_long, .partial = TRUE)

buff_grid <- # NA for values in buffer zone, not covered by rolling averaging
    left_join(hex_grid, slide_rows, by = c("hex_lat_int" = "lat_int")) %>%
    rowwise %>%
    mutate(long_buffer = ifelse(between(hex_long,long_min, long_max), "in", "out")) %>%
    mutate(lat_buffer = ifelse(between(hex_lat,lat_min, lat_max), "in", "out")) %>% filter(lat_buffer =="in" | long_buffer == "in")

ggplot(buff_grid, aes(x=hex_long_int, y=hex_lat_int)) +
    geom_point(size=0.02) +
    geom_path(data=centroids[cent_hull,], aes(x=long_int, y=lat_int), colour="red", size=1)
