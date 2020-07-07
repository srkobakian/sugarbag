#' Filter full set of grid points for those within range of original point
#'
#' Takes only the closest available gridpoints as possible hexagon centroids
#' to allocate polygons.
#'
#' @param f_grid complete grid of hexagon centroids
#' @param focal_points a tibble of focal locations, an optional argument that
#' allows allocation of polygons to hexagon centroids in ascending order of
#' the distance to the closest focal point. It also filters the grid points to
#' those within a 30 degree range of the angle from focal point to centroid.
#' The default "capitals" uses the locations of the Australian capital cities
#' as focal points.
#' @param f_dist a distance in degrees, used as a boundary to filter
#' the hexagon centroids considered for each polygon centroid to be allocated.
#' @param angle_width a numeric used to filter the hexagon grid
#' @param f_centroid the longitude and latitude values for the current centroid
#' @param h_size a float value in degrees for the diameter of the hexagons
#'
#' @return a tibble of filtered grid points
#'
#' @importFrom geosphere finalBearing destPoint
#' @importFrom tibble tibble as_tibble
#'
#'
#'
filter_grid_points <- function(f_grid, f_centroid, focal_points = NULL, f_dist = filter_dist, angle_width = width, h_size = hex_size) {
  
  # Initialise point to centre around, may be overwritten if using focal_points
  flong <- f_centroid$longitude
  flat <- f_centroid$latitude
  
  # Check if there is a focal point set to order allocation
  if (!is.null(focal_points)) {
    
    # Filter distance in degrees for initial filter step
    distance <-
      (((
        f_centroid$latitude - f_centroid$focal_latitude
      ) ^ 2) + ((
        f_centroid$longitude - f_centroid$focal_longitude
      ) ^ 2)) ^ (1 / 2)
    
    if (distance > h_size) {
      # Consider areas closer to capital city than centroid
      angle_toward <-
        geosphere::finalBearing(
          c(f_centroid$longitude, f_centroid$latitude),
          c(f_centroid$focal_longitude, f_centroid$focal_latitude),
          a = 6378160,
          f = 1 / 298.257222101
        )
      
      close_centroid <- geosphere::destPoint(
        p = c(f_centroid$longitude, f_centroid$latitude),
        b = angle_toward,
        d = h_size * 111139,
        a = 6378160,
        f = 1 / 298.257222101
      )
      
      flong <- close_centroid[1]
      flat <- close_centroid[2]
    } }

  
  grid <- f_grid %>%
    ungroup() %>%
    filter((flat - f_dist) < hex_lat & hex_lat < (flat + f_dist)) %>%
    filter((flong - f_dist) < hex_long & hex_long < (flong + f_dist))

  grid <- grid %>%
    group_by(id) %>%
    mutate(
      hex_lat_c = (hex_lat - flat),
      hex_long_c = (hex_long - flong)
    ) %>%
    mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1 / 2))


  # Filter for angle within circle
  if ("focal_distance" %in% colnames(f_centroid)) {
    f_angle <- f_centroid %>%
      mutate(
        atan = atan2(latitude - focal_latitude, longitude - focal_longitude),
        angle = (atan * 180 / pi),
        pangle = ifelse(angle < 0, angle + 360, angle)
      ) %>%
      pull(pangle)


    grid <- grid %>%
      # create circle of radius: f_dist
      filter(hyp < f_dist) %>%
      mutate(
        # geosphere takes a long time
        angle = f_angle,
        angle_plus = (angle + angle_width) %% 360,
        angle_minus = (angle - angle_width) %% 360,
        atan = atan2(hex_lat_c, hex_long_c),
        hex_angle = (atan * 180 / pi),
        hex_angle = ifelse(hex_angle < 0, hex_angle + 360, hex_angle)
      )

    # Check that there were available points within hyp distance
    if (NROW(grid) == 0) {
      return(grid)
    }

    if (grid$angle_minus[1] < grid$angle_plus[1]) {
      grid <- grid %>%
        # create slice of width *2 degrees from centroid
        filter(angle_minus < hex_angle & hex_angle < angle_plus)
    }
    else {
      grid <- grid %>%
        # create slice of width *2 degrees from centroid
        filter(hex_angle < angle_plus | angle_minus > hex_angle)
    }
  }
  
  return(grid)
}

utils::globalVariables(c("hex_lat", "hex_long", ".", "pangle", "assigned", "filter_dist", "rownumber"))
