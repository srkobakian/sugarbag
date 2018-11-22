## add 6 hex points

# split the data frame with hexagon centroids

load(system.file("data","hexmap_qld_3.Rda", package = "sugaRbag"))
hex_size <- 0.3

# only use a subset
data <- hexmap_qld_3
sf_id = "SA2_NAME11"


# Calculate 7 points, given the set of degrees and original hex long and lat
hex_points <- function(area, degrees = c(0, 60, 120, 180, 240, 300, 360)) {
    points <- geosphere::destPoint(
            p = c(area$hex_long, area$hex_lat),
            d = 111139*hex_size,
            a=6378160, f=1/298.257222101, b = degrees)

    points_tbl <- tibble(hexv_long = points[,1],
            hexv_lat = points[,2]) %>%
        mutate(sf_id = area[[sf_id]])

    colnames(points_tbl)[3] <- sf_id

    return(points_tbl)
}


hex_points_df <- data %>%
    split(.[[sf_id]]) %>%
    map_dfr(hex_points)

system.time(
hex_points_df <- full_join(data,
    hex_points_df,
    by = c("SA2_NAME11")))


# create a data frame with 7 points per hexagon centroids

# find how many rows in the tibble


ghex <- ggplot(hex_points_df, aes(hexv_long, hexv_lat)) +
    geom_polygon(aes(label = SA2_NAME11, group = SA2_NAME11))
ghex
