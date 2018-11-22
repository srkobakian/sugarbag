## add 6 hex points

# split the data frame with hexagon centroids

load(system.file("data","hexmap_qld_3.Rda", package = "sugaRbag"))
hex_size <- 0.3

# only use a subset
data <- hexmap_qld_3[1:5,]
sf_id = "SA2_NAME11"

hex_points <- function(data) {

points <-
    c(0, 60, 120, 180, 240, 300, 360) %>%
    map_dfr(~ {
        point <- geosphere::destPoint(
            p = c(data$hex_long, data$hex_lat),
            d = 111139*hex_size,
            a=6378160, f=1/298.257222101, b = .x)

        return(tibble(hexv_long = point[1],
            hexv_lat = point[2]))
    }
    ) %>% mutate(data[sf_id])

return(points)
}



points <- data %>% rowwise %>%
    map_dfr(~ {
        browser()
        hpoints <- hex_points(data = .x)
    return(hpoints)})


hex_points_df <- full_join(data,
    points,
    by = c(SA2_NAME11 = "sf_id"))



# create a data frame with 7 points per hexagon centroids

# find how many rows in the tibble


ghex <- ggplot(hex_points_df, aes(hex_point_long, hex_point_lat)) +
    geom_point(aes(label = SA2_NAME11)) + geom_point(aes(hex_long, hex_lat), colour = "red")
ggplotly(ghex)
