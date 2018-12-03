## add 6 hex points


library(tidyverse)

load(system.file("data","hexmap_qld_3.Rda", package = "sugaRbag"))
load(system.file("data","fortified_qld_3.Rda", package = "sugaRbag"))

hexmap_qld_3 <- hexmap_qld_3 %>% mutate(shape = "hexagon")

fortified_qld_3 <- fortified_qld_3 %>% mutate(shape = "geography")


# only use a subset
data <- hexmap_qld_3
sf_id = "SA2_NAME11"


# Calculate 7 points, given the set of degrees and original hex long and lat
hex_points <- function(area, degrees = c(0, 60, 120, 180, 240, 300, 360)) {
    #browser()
    points <- geosphere::destPoint(
            p = c(area$hex_long, area$hex_lat),
            d = (hex_size/cos(30*pi/180))*111111*(1/2),
            a=6378160, f=1/298.257222101, b = degrees)

    points_tbl <- tibble(hexv_long = points[,1],
            hexv_lat = points[,2]) %>%
        mutate(sf_id = area[[sf_id]], hexv_id =1:7)

    colnames(points_tbl)[3] <- sf_id

    return(points_tbl)
}



# create a data frame with 7 points per hexagon centroids

hex_points_df <- data %>%
    split(.[[sf_id]]) %>%
    map_dfr(hex_points)

system.time(
hex_points_df <- full_join(data,
    hex_points_df,
    by = c("SA2_NAME11")))


# find how many rows in the tibble


ghex <- ggplot(hex_points_df, aes(hexv_long, hexv_lat)) +
    geom_polygon(aes(label = SA2_NAME11, group = SA2_NAME11), fill=NA, colour = "black") + coord_equal()
ghex

x = 1

# joining hex and geo points
hex_points_df <- fortified_qld_3 %>% group_by(SA2_NAME11) %>% summarise(count = n()) %>% left_join(hex_points_df)

s_hpdf <- split(hex_points_df, hex_points_df[[sf_id]])
quotient <- s_hpdf[[x]]$count[1] %/% 6 # the quotient, amount for points 1-6
modulous <- s_hpdf[[x]]$count[1] %% 6 # the remainder, amount to go into 7th point

# situation1: 7 geo points, match 7 hex points with 7 geo
if (quotient == 1 && modulous = 0) {
    s_hpdf[[x]] %>% dplyr::select(hexv_long, hexv_lat, hexv_id)
}



# situation2: < 7 geo points, match 7 hex points with < 7 geo by repeating geo points

if (quotient == 0) {
    # repeat polygon points to make 6 or 7 hex points
    # for example robertson has 5 points
    s_hpdf[["Robertson"]] %>%
        dplyr::select(hexv_long, hexv_lat, hexv_id)

}


# situation3: > 7 geo points, match 7 hex points with > 7 geo by repeating hex points
quotient <- s_hpdf[["Brisbane City"]]$count[1] %/% 6
modulous <- s_hpdf[["Brisbane City"]]$count[1] %% 6
if (quotient > 0) {
    hp <- s_hpdf[["Brisbane City"]] %>%
        dplyr::select(hexv_long, hexv_lat, hexv_id)

    hp1_7 <- bind_rows(
        bind_cols(hexv_long = rep(hp$hexv_long[1:6], times = quotient),
    hexv_lat = rep(hp$hexv_lat[1:6], times = quotient),
    hexv_id = rep(hp$hexv_id[1:6], times = quotient)) %>% arrange(hexv_id),

        bind_cols(hexv_long = rep(hp$hexv_long[7], times = modulous),
        hexv_lat = rep(hp$hexv_lat[7], times = modulous),
        hexv_id = rep(hp$hexv_id[7], times = modulous)))


}
