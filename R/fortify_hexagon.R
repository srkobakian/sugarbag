#' Creates the points that define a hexagon polygon for plotting
#'
#' @param data a data frame created by the allocate function
#' @param sf_id a string to indicate the column to identify individual polygons
#' @param hex_size a float value in degrees for the diameter of the hexagons
#'
#' @return a data frame of the seven points used to draw a hexagon
#' @export
#'
#' @examples
#' # same column is used in create_centroids
#' fortify_hexagon(data = tas_lga_hexctr, sf_id = "lga_code_2016", hex_size = 0.2)
fortify_hexagon <- function(data, sf_id, hex_size) {
  
  # Split data by sf_id
  hexagons <- data %>%
    group_nest(!!sf_id := as.character(!!sym(sf_id)), .key = "grouped") %>%
    mutate(hex = purrr::map(
      .x = grouped,
      .f = function(hexdata = .x, size = hex_size) {
        # create a model hexagon, with centre at (0,0)
        c_x <- 0
        c_y <- 0
        c_hex <- tibble::tibble(
          long = c(
            c_x + 0, c_x + 0.5 * size,
            c_x + 0.5 * size, c_x + 0,
            c_x - 0.5 * size, c_x - 0.5 * size
          ),
          lat = c(
            c_y - size / (sqrt(3)), c_y - size / (2 * sqrt(3)), 
            c_y + size / (2 * sqrt(3)), c_y + size / (sqrt(3)), 
            c_y + size / (2 * sqrt(3)), c_y - size / (2 * sqrt(3)
          ))
        )

        # translate to hexagon centroid location
        c_hex <- c_hex %>%
          mutate(
            long = long + hexdata$hex_long,
            lat = lat + hexdata$hex_lat,
            point_id = 1:6
          )

        return(c_hex)
        # close map function
      }
      # close mutate
    )) %>%
    unnest("grouped") %>%
    unnest("hex") %>%
    mutate(poly_type = "hex")

  return(hexagons)
}


utils::globalVariables(c("grouped", ".x", "long", "lat", "hex", "centroids"))
