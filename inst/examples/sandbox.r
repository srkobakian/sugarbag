# This is my test code
library(tidyverse)
library(purrr)
library(sugaRbag)
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


lat_size = round(nlat/20,0)
long_size = round(nlong/20,0)

# make a list of groups, manual sliding windows
nlat_list <-map2(seq(1:nlat), lat_size + seq(1:nlat), c)
nlong_list <-map2(seq(1:nlong), long_size + seq(1:nlong), c)

lat_window <- function(x, cents = centroids, maximum = nlat){
    max_int = x[2]
    while (max_int > maximum){
        max_int = max_int - 1
    }

    cents_in <- filter(cents, between(lat_int, x[1], max_int))
    return(cents_in)
}

long_window <- function(x, cents = centroids, maximum = nlong){
    max_int = x[2]
    while (max_int > maximum){
        max_int = max_int - 1
    }

    cents_in <- filter(cents, between(long_int, x[1], max_int))
    return(cents_in)
}

# LATITUDE ROWS FILTER
# amount of latitude in sliding window
lat_windows <- map(.x = nlat_list, .f = lat_window)

# find the min and max longitude for each latitude
range_rows <- map_dfr(.x = lat_windows, .f = function(x) { x %>%
        dplyr::summarise(long_min = min(x$long_int), long_max = max(x$long_int)) }) %>% bind_cols(lat_id = seq(1:nlat), .)

# LONGITUDE COLS FILTER
long_windows <- map(.x = nlong_list, .f = long_window, centroids, nlong)

# find the min and max longitude for each latitude
range_cols <- map_dfr(.x = long_windows, .f = function(x) { x %>%
        dplyr::summarise(lat_min = min(x$lat_int), lat_max = max(x$lat_int))
}) %>% bind_cols(long_id = seq(1:nlong), .)


buff_grid <- hex_grid %>%
    left_join(., range_rows, by = c("hex_lat_int" = "lat_id")) %>%
    left_join(., range_cols, by = c("hex_long_int" = "long_id")) %>%
    rowwise %>%
    mutate(long_buffer = ifelse(between(hex_long_int,long_min, long_max), "in", "out")) %>%
    mutate(lat_buffer = ifelse(between(hex_lat_int,lat_min, lat_max), "in", "out")) %>%
    filter(lat_buffer =="in" | long_buffer == "in")

ggplot(buff_grid, aes(x=hex_long_int, y=hex_lat_int)) +
    geom_point(size=0.02) +
    geom_path(data=centroids[cent_hull,], aes(x=long_int, y=lat_int), colour="red", size=1)

