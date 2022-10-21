#' Create a grid of evenly spaced points to allow hexagons to tessellate
#'
#' This function takes the output from the create_centroids function, or a
#' set of centroids in a table with the columns latitude and longitude
#'
#' @param centroids data frame of centroids to be allocated, this should have
#' columns for longitude and latitude value of centroids, as 
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param buffer_dist distance to extend beyond the geometry provided
#' @param latitude the column name for the latitude values of the centroids
#' @param longitude the column name for the longitude values of the centroids
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return grid
#' @export
#' 
#' @importFrom tibble tibble as_tibble
#'
#' @examples
#' # Create a set of centroids for grid to overlay
#' centroids <- create_centroids(tas_lga, "lga_code_2016")
#' # Create the grid
#' grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2, verbose = FALSE)
#' 
create_grid <- function(centroids, hex_size, buffer_dist, latitude = "latitude", longitude = "longitude", verbose = FALSE) {
  if (verbose) {
    message("Creating hexagon grid.")
  }
  # filter grid points to be within buffer_dist

  grid <- tibble::as_tibble(
    expand.grid(
      hex_long = seq(
        (min(centroids[["longitude"]], na.rm = TRUE) - buffer_dist),
        (max(centroids[["longitude"]], na.rm = TRUE) + buffer_dist),
        hex_size
      ),
      hex_lat = seq(
        (min(centroids[["latitude"]], na.rm = TRUE) - buffer_dist),
        (max(centroids[["latitude"]], na.rm = TRUE) + buffer_dist),
        hex_size*sqrt(3)/2
      )
    )
  )


  # Shift the longitude of every second latitude - to make hex structure
  shift_lat <- grid %>%
    dplyr::select(hex_lat) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::row_number() %% 2 == 1) %>%
    unlist()

  grid <- grid %>%
    dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
      hex_long + (hex_size / 2)
    )) %>%
    dplyr::mutate(id = 1:NROW(.)) %>%
    dplyr::mutate(assigned = FALSE)


  grid <- grid %>%
    mutate(
      hex_long_int = dense_rank(hex_long) - 1,
      hex_lat_int = dense_rank(hex_lat) - 1
    )

  # Define a buffer zone for possible allocation around the existing centroids
  # Can be set to zero for hard boundaries near borders of islands or countries
  return_grid <- create_buffer(centroids, grid, hex_size, buffer_dist)

  return(return_grid)
}
