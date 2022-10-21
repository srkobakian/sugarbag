#' Expand points to extend beyond the outermost centroids
#'
#' Called from within create_grid function, this function takes the bounding
#' box of a group of polygons, or a specific table of minimum and maximum
#' longitudes and latitudes to create points for each polygon to be allocated
#' to that will tessellate into hexagons.
#'
#' @param centroids data frame of centroids to be allocated
#' @param grid data frame of hexagon centroids
#' @param buffer_dist distance to extend beyond the geometry provided
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return data frame of hexagon centroids
#'
#' @examples
#' lga_centroids <- create_centroids(sugarbag::tas_lga, "lga_code_2016")
#' lga_grid <- create_grid(lga_centroids, hex_size = 0.2, buffer_dist = 1.2)
#' 
create_buffer <- function(centroids, grid, hex_size, buffer_dist, verbose = FALSE) {
  
  # filter grid points to be within buffer_dist
  # ALLOCATE CENTROID TO GRID ROW: 
  # Round polygon centroids to the nearest grid row
  # Scale rows 0 to n
  # Round to closest integer
  # To find a grid row number for centroid

  # Number of grid rows and columns
  nlong <- length(unique(grid$hex_long))
  nlat <- length(unique(grid$hex_lat))

  # Add grid row and column values to centroids data set 
  centroids <- centroids %>%
    mutate(
      # long int for integer value of longitude column
      long_int = round((longitude - min(grid$hex_long)) / 
                         (max(grid$hex_long) - min(grid$hex_long)) * nlong, 0),
      # lat int for integer value of latitude column
      lat_int = round((latitude - min(grid$hex_lat)) / 
                        (max(grid$hex_lat) - min(grid$hex_lat)) * nlat, 0)
    )

  # Amount of lats and longs in each group of rows and columns
  lat_size <- round(nlat / 20, 0)
  long_size <- round(nlong / 20, 0)


  # Make a list of the min and max of the groups
  # Effectively creates manual sliding windows
  nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)
  nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)


  # Function to return the centroids that fall in the latitude window
  lat_window <- function(bounds, cents = centroids, maximum = nlat) {
    
    max_int <- min(bounds[2], maximum)

    cents_in <- filter(cents, between(lat_int, bounds[1], max_int))
    return(cents_in)
  }
  
  # Function to return the centroids that fall in the longitude window
  long_window <- function(bounds, cents = centroids, maximum = nlong) {
    
    max_int <- bounds[2]
    
    while (max_int > maximum) {
      max_int <- max_int - 1
    }

    cents_in <- filter(cents, between(long_int, bounds[1], max_int))
    return(cents_in)
  }

  # LATITUDE ROWS FILTER
  # amount of latitude in sliding window
  lat_windows <- purrr::map(.x = nlat_list, .f = lat_window, centroids, nlat)
  
  # LONGITUDE COLS FILTER
  long_windows <- purrr::map(.x = nlong_list, .f = long_window, centroids, nlong)
  
  #########################################################
  ###                ROLLING MIN & MAX                  ###
  
  # DEFINE FUNCTION
  # find the min and max longitude for each latitude
  range_rows <- purrr::map_dfr(
    .x = lat_windows,
    .f = function(data) {
      data %>%
        dplyr::summarise(
          long_min = ifelse(purrr::is_empty(long_int), NA, min(data$long_int)),
          long_max = ifelse(purrr::is_empty(long_int), NA, max(data$long_int))
        )
    }
  )

  # find the min and max longitude for each latitude
  range_cols <- purrr::map_dfr(.x = long_windows, .f = function(data) {
    data %>%
      dplyr::summarise(
        lat_min = ifelse(purrr::is_empty(lat_int), NA, min(data$lat_int)),
        lat_max = ifelse(purrr::is_empty(lat_int), NA, max(data$lat_int))
      )
  })
  
  #########################################################
  ###                ROLLING AVERAGES                   ###
  
  
  mean_range <- function(bounds, data) {
    data[bounds[1]:min(bounds[2], NROW(data)), ] %>%
      dplyr::summarise(across(ends_with("min"), 
                         ~mean(.x, na.rm = TRUE), .names = "mean_{col}"),
                       across(ends_with("max"), 
                              ~mean(.x, na.rm = TRUE), .names = "mean_{col}")) %>% 
      # in cases where all values are NA mean is Nan
      # make mean value of NaN be NA
      dplyr::summarise(across(starts_with("mean"), ~ifelse(.x == "NaN", NA, .x)))
  }
  
  # smooth the minimums
  av_range_rows <- purrr::map_dfr(.x = nlat_list, mean_range, range_rows) %>%
    bind_cols(lat_id = c(seq(1:nlat) + lat_size), .)

  
  # smooth the minimums
  av_range_cols <- purrr::map_dfr(.x = nlong_list, mean_range, range_cols) %>%
    bind_cols(long_id = c(seq(1:nlong) + round(long_size / 2)), .)


  # APPLY A BUFFER
  # change buffer to amount of hexagons (ints) either side
  hex_buffer <- floor(buffer_dist / hex_size)

  if (verbose) {
    message("Applying buffer zone to hexagon grid.")
  }
  
  
  buff_grid <- grid %>%
    left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
    left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
    rowwise() %>%
    mutate(long_buffer = ifelse(between(
      hex_long_int, mean_long_min - hex_buffer,
      mean_long_max + hex_buffer), "in", "out")) %>%
    mutate(lat_buffer = ifelse(between(
      hex_lat_int, mean_lat_min - hex_buffer,
      mean_lat_max + hex_buffer), "in", "out")) %>%
    filter(lat_buffer == "in" | long_buffer == "in")


  return(buff_grid)
}
