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

hex_grid <- create_grid(centroids, bbox, hex_size, buffer_dist)

ggplot(hex_grid, aes(x=hex_long, y=hex_lat)) + geom_point(size=0.1)

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

