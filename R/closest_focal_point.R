#' For the polygon provided, find the closest focal point in the set provided
#'
#' For one row of an sf data frame, calculate the distance to the closest focal
#' point. Return the name of the focal point, and the angle between focal point and centroid.
#'
#' @param polygon
#' @param focal_points
#'
#' @return data frame containing the name and location of the closest focal
#' @export
#'
#' @examples
closest_focal_point <- function(polygon, focal_points) {

    focal_distance <- geosphere::distVincentyEllipsoid(
        c(polygon$long_c, polygon$lat_c), focal_points[ ,2:3],
        a=6378160, b=6356774.719, f=1/298.257222101)

    # closest city
    focal_distance_df <- focal_points %>% dplyr::bind_cols(., focal_distance = focal_distance) %>% top_n(-1, wt = focal_distance)

    # angle from city to centroid
    focal_distance_df$angle <- geosphere::finalBearing(focal_distance_df[ ,2:3], c(polygon$long_c, polygon$lat_c),
        a=6378160, f=0)

    return(focal_distance_df)
}
