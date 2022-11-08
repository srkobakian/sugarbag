find_lat_group <- function(data, hull_points, buffer_dist) {
  # find which row lat belongs to in hull points data
  # if outside of hull, will not return any TRUE values
  # need to account for the buffer in this situation
  hex_lat <- data$hex_lat[1]

  # new column between indicates which gradient should be applied for the latitude should be TRUE for two rows
  lat_hull <- hull_points %>%
    rowwise() %>%
    mutate(between = ifelse(between(hex_lat, latitude, next_lat), TRUE, FALSE)) %>%
    filter(between == TRUE)

  # when latitude is within convex hull
  if (NROW(lat_hull) > 0) {
    message("Found match")
    # algebra to find long for minimum:
    # ensure very small gradients do not affect divisions
    min_long <- lat_hull[which.min(lat_hull$latitude), ] %>%
      mutate(
        new_grad = ifelse(abs(gradient) < 1, 1, gradient),
        cut_long = longitude - ((latitude - hex_lat) / new_grad) - buffer_dist
      ) %>%
      pull(cut_long)
    # algebra to find lat for maximum
    max_long <- lat_hull[which.max(lat_hull$latitude), ] %>%
      mutate(
        new_grad = ifelse(abs(gradient) < 1, 1, gradient),
        cut_long = longitude - ((latitude - hex_lat) / new_grad) + buffer_dist
      ) %>%
      pull(cut_long)
    # return the desired grid longitudes for given lat
    buffer_data <- data %>% rowwise() %>% filter(between(hex_long, min_long, max_long))

    return(buffer_data)
  }
  # have an if for outside of hull, inside hull+buffer_dist

  else {
    return(NULL)
  }
}
