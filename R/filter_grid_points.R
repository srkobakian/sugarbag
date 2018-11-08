#' Filter full set of grid points for those within range of original point
#'
#' Takes only the closest available gridpoints as possible hexagon centroids
#' to allocate polygons.
#'
#' @param f_grid complete grid of hexagon centroids
#' @param f_centroid longitude latitude of centroid of polygon
#' @param focal_points a tibble of focal locations, an optional argument that
#' allows allocation of polygons to hexagon centroids in ascending order of
#' the distance to the closest focal point. It also filters the grid points to
#' those within a 30 degree range of the angle from focal point to centroid.
#' The default "capitals" uses the locations of the Australian capital cities
#' as focal points.
#' @param f_dist a distance in metres, used as a boundary to filter
#' the hexagon centroids considered for each polygon centroid to be allocated.
#'
#' @return a tibble of filtered grid points
#' @export
#'
#' @importFrom geosphere finalBearing destPoint
#' @importFrom tibble tibble as.tibble
#'
#'
#'
filter_grid_points <- function(f_grid, f_centroid, focal_points = NULL, f_dist = filter_dist){

    # Filter distance in degrees for initial filter step
    fdist_d <- f_dist/1000
    flat <- f_centroid$latitude
    flong <- f_centroid$longitude


    grid <- f_grid %>% ungroup() %>%
        filter(flat - fdist_d < hex_lat & hex_lat < flat + fdist_d) %>%
        filter(flong - fdist_d < hex_long & hex_long < flong + fdist_d)

    grid <- grid %>% mutate(
        hex_lat_c = hex_lat - flat,
        hex_long_c = hex_long - flong) %>%
        mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1/2))


    # Filter for angle within circle
    if ("focal_distance" %in% colnames(f_centroid)) {

        f_angle <- f_centroid %>%
            mutate(angle = (atan2(sin(longitude1-longitude)*cos(latitude1),
                cos(latitude)*sin(latitude1) - sin(latitude)*cos(latitude1)*cos(latitude-longitude))*360/pi)) %>%
            pull(angle)
        #f_angle <- geosphere::finalBearing(
        #   cbind(f_centroid$longitude1,f_centroid$latitude1), c(flong, flat), a=6378160, f=0)


        grid <- grid %>%
            # create circle of radius: fdist_d
            filter(hyp < fdist_d) %>%
            mutate(
                # geosphere takes a long time
                angle = f_angle,
                angle_plus = (angle + 40)%%360,
                angle_minus = (angle - 40)%%360,
                atan = atan2(hex_lat_c, hex_long_c),
                hex_angle = (atan*360/pi))


        if (grid$angle_minus[1]< grid$angle_plus[1]) {
            grid <- grid %>%
                # create slice of 60 degrees from centroid
                filter(angle_minus < hex_angle & hex_angle < angle_plus)
        }
        else {
            grid <- grid %>%
                # create slice of 60 degrees from centroid
                filter(hex_angle < angle_plus | angle_minus > hex_angle)
        }

    }


    return(grid)
}

utils::globalVariables(c("hex_lat", "hex_long", ".", "assigned"))
