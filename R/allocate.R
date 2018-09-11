#' allocate
#'
#' Chooses a hexagon centroid for each polygon in the shape file, from a grid
#' spanning the longitudes and latitudes in the expanded bounding box.
#'
#'
#' @param centroids a data frame with centroids of non empty polygons
#' @param grid a data frame containing points to allocate hexagons
#' @param radius distance between each grid point
#'
#' @return
#' @export
#'
#' @examples
allocate <- function(centroids, grid, radius) {
    # consider whether people should be able to control distance of filter for speed
    ###########################################################################
    # filter the grid for appropriate hex positions

    ### create a triangle of points to form filtered area

    triangle <- rbind(
        c(long = centroids$long_c, lat = centroids$lat_c),
        geosphere::destPoint(c(centroids$long_c, centroids$lat_c),centroids$angle+30,5000),
        geosphere::destPoint(c(centroids$long_c, centroids$lat_c),centroids$angle-30,5000),
        c(long = centroids$long_c, lat = centroids$lat_c)) %>% as.tibble

    grid <- grid %>% dplyr::filter(!assigned)

    sf_triangle <- sf::st_polygon(list(as.matrix(triangle)))
    ##
    ##
    #compare points using sf package
    overlap = point.in.polygon(grid$hex_long, grid$hex_lat,
        triangle$lon, triangle$lat)

    grid <- cbind(grid, overlap) %>% filter(overlap==1)





}
