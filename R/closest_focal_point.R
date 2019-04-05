#' For the polygon provided, find the closest focal point in the set provided
#'
#' For one row of an sf data frame, calculate the distance to the closest focal
#' point. Return the name of the focal point, and the angle between focal point and centroid.
#'
#' @param centroid a data frame describing one centroid
#' @param focal_points a data frame of the longitude and latitude values
#'
#' @return data frame containing the name and location of the closest focal
#' @export
#'
#' @examples
#' # Create a set of polygon centroids
#' centroids <- create_centroids(tas_sa2, "SA2_5DIG16")
#' 
#' # Find the closest capital city for the first centroid
#' closest_focal_point(centroids[1, ], capital_cities)
closest_focal_point <- function(centroid, focal_points) {
  if ("long" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "long")] <- "longitude"
  }
  if ("lat" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "lat")] <- "latitude"
  }

  # When applying to hexagon grid
  if ("hex_long" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "hex_long")] <- "longitude"

    colnames(focal_points)[which(colnames(focal_points) == "hex_lat")] <- "latitude"
  }


  # create a martix for distance calculations
  fp_matrix <- as.matrix(focal_points[c("longitude", "latitude")])

  focal_distance <- geosphere::distVincentyEllipsoid(
    c(centroid$longitude, centroid$latitude), fp_matrix,
    a = 6378160, b = 6356774.719, f = 1 / 298.257222101
  )

  # closest point
  focal_distance_df <- focal_points %>% dplyr::bind_cols(., focal_distance = focal_distance) %>% top_n(-1, wt = focal_distance)

  # angle from city to centroid
  focal_distance_df$angle <- geosphere::finalBearing(focal_distance_df[, 2:3], c(centroid$longitude, centroid$latitude),
    a = 6378160, f = 0
  )

  return(focal_distance_df)
}
