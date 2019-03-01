#' Create a grid of evenly spaced points to allow hexagons to tesselate
#'
#' This function takes the bounding box of a group of polygons, or a specific
#' table of minimum and maximum longitudes and latitudes to create points
#' for each polygon to be allocated to that will tesselate into hexagons.
#'
#' @param centroids data frame of centroids to be allocated
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param buffer_dist distance to extend beyond the geometry provided
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return grid
#' @export
#'
#' @import dplyr
#'
#' @examples
#' # Create a set of centroids for grid to overlay
#' centroids <- create_centroids(tas_lga, "LGA_CODE16")
#' # Create the grid
#' grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2, verbose = FALSE)
#'
#'

create_grid <- function(centroids, hex_size, buffer_dist, verbose = FALSE) {

    if (verbose){
        message("Creating hexagon grid.")
    }
    # filter grid points to be within buffer_dist

    grid <- tibble::as.tibble(
        expand.grid(hex_long = seq(
            (min(centroids$longitude) - buffer_dist),
            (max(centroids$longitude) + buffer_dist),
        hex_size),
        hex_lat = seq(
            (min(centroids$latitude) - buffer_dist),
            (max(centroids$latitude) + buffer_dist),
            hex_size)
            )
        )


    # Shift the longitude of every second latitude - to make hex structure
    shift_lat <- grid %>% dplyr::select(hex_lat) %>%
        dplyr::distinct() %>%
        dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()

    grid <- grid %>%
        dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
            hex_long + (hex_size / 2))) %>%
        dplyr::mutate(id=1:NROW(.)) %>%
        dplyr::mutate(assigned=FALSE)


    grid <- grid %>%
        mutate(hex_long_int = dense_rank(hex_long)-1,
            hex_lat_int = dense_rank(hex_lat)-1)

    return_grid <- create_buffer(centroids, grid, hex_size, buffer_dist)

    return(return_grid)
}
