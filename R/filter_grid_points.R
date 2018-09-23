#' Filter full set of grid points for those within range of original point
#'
#' Takes only the closest available gridpoints as possible hexagon centroids
#' to allocate polygons.
#'
#' @param fgrid complete grid of hexagon centroids
#' @param folong longitude of centroid of polygon
#' @param folat latitude of centroid of polygon
#' @param focal_points a tibble of focal locations, an optional argument that
#' allows allocation of polygons to hexagon centroids in ascending order of
#' the distance to the closest focal point. It also filters the grid points to
#' those within a 30 degree range of the angle from focal point to centroid.
#' The default "capitals" uses the locations of the Australian capital cities
#' as focal points.
#' @param filter_distance a distance in metres, used as a boundary to filter
#' the hexagon centroids considered for each polygon centroid to be allocated.
#'
#' @return a tibble of filtered grid points
#' @export
#'
#' @importFrom geosphere finalBearing destPoint
#' @importFrom tibble tibble as.tibble
#' @importFrom sp point.in.polygon
#'
#'
#' @examples
#'
#'

filter_grid_points <- function(fgrid, folong, folat, focal_distance = NULL, filter_distance){

    grid <- fgrid %>% filter(!assigned) %>%
        filter(between(hex_lat, folat-filter_distance, folat+filter_distance)) %>%
        filter(between(hex_long, folong-filter_distance, folong+filter_distance))

    if (!is.null(focal_distance)) {


        # create filter shape using long lats
        triangle <- rbind(c(folong, folat),
            destPoint(c(folong, folat),angle-30,5000/(filter_distance)),
            destPoint(c(folong, folat),angle+30,5000/(filter_distance)),
            c(folong, folat)) %>% as.tibble()

        fgrid <- fgrid %>% filter(!assigned) %>%
            filter(dplyr::between(hex_lat, min(triangle$lat), max(triangle$lat))) %>%
            filter(dplyr::between(hex_long, min(triangle$lon),  max(triangle$lon)))

        overlap = point.in.polygon(fgrid$hex_long, fgrid$hex_lat,
            triangle$lon, triangle$lat)

        fgrid <- cbind(fgrid, overlap) %>% filter(overlap==1)

    }
    return(fgrid)
}

utils::globalVariables(c("hex_lat", "hex_long", ".", "assigned"))
