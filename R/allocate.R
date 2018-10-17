#' Allocate polygon centroids to hexagons in a grid
#'
#' Chooses a hexagon centroid for each polygon in the shape file, from a grid
#' spanning the longitudes and latitudes in the expanded bounding box.
#'
#'
#' @param centroids a data frame with centroids of non empty polygons
#' @param hex_size distance between each grid point
#' @param hex_grid a data frame containing all possible hexagon points
#' @param filter_dist distance around centroid to consider hex points
#'
#' @return a data frame of one allocation
#' @export
#'
#' @examples
allocate <- function(centroids, hex_grid, hex_size, filter_dist, focal_points, show_progress) {

    if (!is.null(focal_points)) {
        s_centroids <- centroids %>% arrange(focal_distance) %>% split(.$sf_id)
        print("Allocating centroids, in order of distance to closest focal point.")
    } else {
        s_centroids <- centroids %>% split(.$sf_id)
    }

    # Set up allocation data frame
    centroid_allocation <- NULL

    ###########################################################################

    for (centroid in s_centroids) {

    # Indicate progression
    if (show_progress) {
    suppressWarnings(centroid %>% select(sf_id) %>% pull(sf_id) %>% as.character() %>% print())
    }

    # filter the grid for appropriate hex positions

    # find appropriate filtering distance
    if (filter_dist < 1000 | is.null(filter_dist)) {
        # assume filter distance in degrees
        filter_dist <- 1000
    }

    # a possible expansion according to size of grid
    #width = max(grid$hex_long)-min(grid$hex_long)

    f_grid = NULL

    # filter for only the available hex grid points
    if (!is.null(centroid_allocation)) {
        hex_grid <- hex_grid %>% filter(!(id %in% centroid_allocation$id))
    }

    # filter grid for avaiable points
    while(NROW(f_grid) == 0) {
        f_grid <- filter_grid_points(f_grid = hex_grid, f_centroid = centroid, f_dist = filter_dist)
        if (NROW(f_grid) == 0) {
        filter_dist <- filter_dist*1.5
        print(paste("Filter Distance expanded 50% to ", filter_dist))
        }
    }

    # Choose first avaiable point

    cent <- centroid %>% select(sf_id, longitude, latitude, focal_point = points, focal_dist = focal_distance, focal_angle = angle)
    hex <- f_grid %>% top_n(n=-1, wt = hyp) %>% select(hex_long, hex_lat, hex_id = id)

    centroid_allocation <- bind_rows(centroid_allocation, dplyr::bind_cols(cent, hex))
    }

    # Return it to the data frame
    return(centroid_allocation)
}


utils::globalVariables(c("sf_id", "longitude", "latitude", "assigned"))
