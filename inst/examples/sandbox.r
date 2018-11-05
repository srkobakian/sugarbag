# This is my test code
library(tidyverse)
library(purrr)
library(sf)
library(viridis)
library(ggthemes)
library(sugaRbag)
library(plotly)
#shp_path <- system.file("data","sa2_2011.Rda", package = "sugaRbag")
#load(system.file("data","capital_cities.Rda", package = "sugaRbag"))

#shp_sf <- read_shape(shp_path, simplify = TRUE)
shp_sf <- read_shape("data/sa2_2011.Rda", simplify = TRUE)
load("data/capital_cities.Rda")

###############################################################################
# consider only VIC
vic_sf <- shp_sf %>% filter(STE_NAME11 == "Victoria")

# Make a basic map
ggplot(vic_sf) +
    geom_sf(aes(fill = SA4_NAME11, label = SA2_NAME11)) +
    scale_fill_viridis_d() +
    guides(fill = FALSE)
ggplotly()


# Facet by
ggplot(vic_sf) +
    geom_sf(aes(fill = SA4_NAME11, label = SA2_NAME11)) +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, grid = facet_grid) +
    guides(fill = FALSE)

# create centroids

sf_id = "SA2_NAME11"
centroids <- create_centroids(vic_sf, id = sf_id)

# centroids <- create_centroids(shp_sf)

# Set the bounding box
bbox <- tibble::tibble(min = c(min(centroids$longitude),
                               min(centroids$latitude)),
                       max = c(max(centroids$longitude),
                               max(centroids$latitude)))

# Set hexagon size and buffer
hex_size <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2]) / 10
buffer_dist <- (bbox$max[1] - bbox$min[1]) / 5

# Create a basic hexgrid
hex_grid <- create_grid(centroids, bbox, hex_size, buffer_dist)

#ggplot(hex_grid, aes(x=hex_long, y=hex_lat)) + geom_point(size=0.1)

# Turn grid into integer grid
nlong <- length(unique(hex_grid$hex_long))
nlat <- length(unique(hex_grid$hex_lat))
hex_grid <- hex_grid %>%
    mutate(hex_long_int = dense_rank(hex_long)-1,
           hex_lat_int = dense_rank(hex_lat)-1)

# Check bounds of hex grid are external to bounds of centroids
#summary(hex_grid$hex_long)
#summary(hex_grid$hex_lat)
#summary(centroids$longitude)
#summary(centroids$latitude)

# Round centroids to closest grid lines
centroids <- centroids %>%
    mutate(long_int = round((longitude-min(hex_grid$hex_long))/(max(hex_grid$hex_long)-min(hex_grid$hex_long))*nlong, 0),
           lat_int = round((latitude-min(hex_grid$hex_lat))/(max(hex_grid$hex_lat)-min(hex_grid$hex_lat))*nlat, 0))

#ggplot(hex_grid, aes(x=hex_long_int, y=hex_lat_int)) +
#    geom_point(size=0.02) +
#    geom_point(data=centroids, aes(x=long_int, y=lat_int), colour="red", size=0.5)


lat_size = round(nlat/20,0)
long_size = round(nlong/20,0)

# make a list of groups, manual sliding windows
nlat_list <-map2(seq(1:nlat), lat_size + seq(1:nlat), c)
nlong_list <-map2(seq(1:nlong), long_size + seq(1:nlong), c)


lat_window <- function(x, cents = centroids, maximum = nlat){
    max_int = min(x[2],maximum)

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
range_rows <- map_dfr(.x = lat_windows,
    .f = function(x) {x %>%
        dplyr::summarise(
            long_min = ifelse(is_empty(long_int), NA, min(x$long_int)),
            long_max = ifelse(is_empty(long_int), NA, max(x$long_int))
        )}
    )

# smooth the minimums
av_range_rows <- map_dfr(.x = nlat_list, .f = function(x, rows = range_rows) {
    rows[x[1]:min(x[2], NROW(rows)),] %>%
        dplyr::summarise(mean_long_min = mean(long_min, na.rm=T), mean_long_max = mean(long_max, na.rm=T))
}) %>%
    bind_cols(lat_id = c(seq(1:nlat) +lat_size), .)

# LONGITUDE COLS FILTER
long_windows <- map(.x = nlong_list, .f = long_window, centroids, nlong)

# find the min and max longitude for each latitude
range_cols <- map_dfr(.x = long_windows, .f = function(x) { x %>%
        dplyr::summarise(
            lat_min = ifelse(is_empty(lat_int), NA, min(x$lat_int)),
            lat_max = ifelse(is_empty(lat_int), NA, max(x$lat_int))
        )}
)

# smooth the minimums
av_range_cols <- map_dfr(.x = nlong_list, .f = function(x, cols = range_cols) {
    cols[x[1]:min(x[2], NROW(cols)),] %>%
        dplyr::summarise(mean_lat_min = mean(lat_min, na.rm=T), mean_lat_max = mean(lat_max, na.rm=T))
}) %>%
    bind_cols(long_id = c(seq(1:nlong) + round(long_size/2)), .)


# APPLY A BUFFER
# change buffer to amount of hexagons (ints) either side
# will be called as create_buffer within create_grid function
hex_buffer <- floor(buffer_dist/hex_size)

buff_grid <- hex_grid %>%
    left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
    left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
    rowwise %>%
    mutate(long_buffer = ifelse(between(hex_long_int,mean_long_min - hex_buffer,
        mean_long_max + hex_buffer), "in", "out")) %>%
    mutate(lat_buffer = ifelse(between(hex_lat_int,mean_lat_min - hex_buffer,
        mean_lat_max + hex_buffer), "in", "out")) %>%
    filter(lat_buffer =="in" | long_buffer == "in")


#ggplot(buff_grid, aes(x=hex_long_int, y=hex_lat_int)) + geom_point(size=0.02) + geom_point(data=centroids, aes(x=long_int, y=lat_int), colour="red", size=1)

# FIND CLOESEST FOCAL POINT,
# provides columns "points", "longitude1", "latitude1", "focal_distance", "angle"
s_centroids <- split(x = centroids, seq(NROW(centroids)))

centroids <- bind_cols(centroids,
    purrr::map_dfr(.x = s_centroids,
        .f = closest_focal_point,
        focal_points = capital_cities))


###############################################################################
# ALLOCATE POLYGON CENTROIDS TO A HEXMAP POINT
system.time(
    hexmap_allocation <- allocate(centroids = centroids,
        hex_grid = buff_grid,
        hex_size = 0.02,
        filter_dist = 1000, # no issue with filtering when filter_dist is 1000
        focal_points = capital_cities[1],
        show_progress = TRUE,
        id = sf_id)
)

# Join to the shape file data set
# hexmap_allocation <- left_join(shp_sf, hexmap_allocation)

# for VIC
hexmap_df <- left_join(vic_sf, hexmap_allocation, by = c("SA2_NAME11"))

# JOIN OTHER DATA HERE
###############################################################################


ggplot(vic_sf) +
    geom_sf(aes(fill = population, label = SA2_NAME11)) +
    scale_fill_viridis() +
    theme_foundation()
ggplotly()

# Facetted SA4
ggplot(vic_sf) +
    geom_sf(aes(fill = population, label = SA2_NAME11)) +
    scale_fill_viridis() +
    facet_geo(~ SA4_NAME11, grid = facet_grid, grid = facet_grid) +
    theme_foundation()




# converting to fortified tibble

hexmap2 <- sfc_to_tibble(hexmap_df)

g_h <- ggplot(data=hexmap2) +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), fill= "white") +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
            label = SA2_NAME11), position = "identity", stat = "identity") +
    scale_fill_viridis_d() +
    guides(fill = FALSE)


ggplot(data=hexmap2) +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), fill= "white") +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
        label = SA2_NAME11), position = "identity", stat = "identity") +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, scales = "free", grid = facet_grid) +
    guides(fill = FALSE)


# Interctive MAP
ggplotly(g_h)

# plot to check long lat against Melbourne
#ggplot(hex_grid, aes(hex_long, hex_lat)) + geom_point() +geom_point(data = centroid_allocation, aes(hex_long, hex_lat), colour= "red") + geom_point(aes(x=longitude, y=latitude),centroid, colour="blue") + geom_point(data = centroid_allocation %>% filter(SA2_NAME11 == "Melbourne"), aes(hex_long, hex_lat), colour= "yellow")



# Facet use geofacet
ggplot(hexmap2, aes(hex_long, hex_lat)) +
    geom_hex(position = "identity", stat = "identity") +
    facet_geo(~ SA4_NAME11, grid = facet_grid, scales = "free")
