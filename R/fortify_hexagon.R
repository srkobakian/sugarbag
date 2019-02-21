#' Creates the points that define a hexagon polygon for plotting
#'
#' @param data data frame containing the longitude, latitude of a hexagon centroid
#' @param hex_size the distance between the hexagon centroids
#'
#' @return a data frame of the seven points used to draw a hexagon
#' @export
#'
#'
#' @examples
#' # Create centroids set
#' centroids <- create_centroids(tas_sa2, "SA2_5DIG16")
#' # Create hexagon location grid
#' grid <- create_grid(centroids = centroids, hex_size = 0.1, buffer_dist = 0.1)
#' # Allocate polygon centroids to hexagon grid points
#' hex_allocated <- allocate(centroids = centroids,
#' hex_grid = grid,
#' hex_size = 0.1, # same size used in create_grid
#' filter_dist = 0.25,
#' focal_points = sugaRbag::capital_cities,
#' width = 30,
#' id = "SA2_5DIG16", verbose = TRUE) # same column used in create_centroids
#'
#' fortify_hexagon(hex_allocated[1,], 0.1)

fortify_hexagon <- function(data, hex_size) {

  # create a model hexagon, with centre at (0,0)
  c_x <- 0
  c_y <- 0
  c_hex <- tibble::tibble(hexv_long = c(
      c_x + 0, c_x + 0.5*hex_size,
      c_x + 0.5*hex_size, c_x + 0,
      c_x - 0.5*hex_size, c_x - 0.5*hex_size),
    hexv_lat = c(c_y - hex_size/(sqrt(3)), c_y - hex_size/(2*sqrt(3)), c_y + hex_size/(2*sqrt(3)), c_y + hex_size/(sqrt(3)), c_y + hex_size/(2*sqrt(3)), c_y - hex_size/(2*sqrt(3))))

  # translate to hexagon centroid location
  c_hex <- c_hex %>%
      mutate(hexv_long = hexv_long + data$hex_long,
    hexv_lat = hexv_lat + data$hex_lat,
        hexv_id = 1:6)

  return(c_hex)
}
