#' Allocate polygon centroids to hexagons in a grid
#'
#' Chooses a hexagon centroid for each polygon in the shape file, from a grid
#' spanning the longitudes and latitudes in the expanded bounding box.
#'
#'
#' @param centroids a data frame with centroids of non empty polygons
#' @param hex_grid a data frame containing all possible hexagon points
#' @param sf_id a string to indicate the column to identify individual polygons
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param hex_filter amount of hexagons around centroid to consider
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param verbose a boolean to indicate whether to show polygon id
#' @param width a numeric indicating the angle used to filter the hexagon grid
#'
#' @return a data frame of all allocated hexagon points
#' @export
#'
#' @importFrom tidyr unnest
#' @importFrom progress progress_bar
#'
#' @examples
#' # Create centroids set
#' centroids <- create_centroids(tas_lga, sf_id = "LGA_CODE16")
#' # Create hexagon location grid
#' data(capital_cities)
#' grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
#' # Allocate polygon centroids to hexagon grid points
#' hex_allocated <- allocate(
#'   centroids = centroids,
#'   hex_grid = grid,
#'   hex_size = 0.2, # same size used in create_grid
#'   hex_filter = 10,
#'   focal_points = capital_cities,
#'   width = 30, verbose = TRUE
#' )
#' # same column used in create_centroids
#' # create a set of hexagon points for plotting
#' fort_hex <- fortify_hexagon(data = hex_allocated, sf_id = "LGA_CODE16", hex_size = 0.2)
#' # plot the hexagons
allocate <-
  function(centroids, hex_grid, sf_id = names(centroids)[1], hex_size, hex_filter, focal_points = NULL, width, verbose) {
    
    # If there are focal points
    
    # consider focal point distance - if they were provided
    if (!is.null(focal_points)) {
      
      # distance between centroids and all focal points
      if (verbose) {
        message("Finding closest point in focal_points data set.")
      }
      
      centroids <- centroids %>%
        group_nest(!!sym(sf_id)) %>%
        mutate(closest = purrr::map(data, closest_focal_point, focal_points = focal_points)) %>%
        unnest(c("data", "closest")) %>%
        arrange(focal_distance) %>% 
        mutate(rownumber = row_number()) 
      
      if (verbose) {
        message("Allocating centroids, in order of distance to closest focal point.")
      }
    } else {
      # Is there is a variable to arrange allocation order
      if (!is.null(order_sf_id)) {
        # if no focal point data set is provided:
        # Check if areas should be arranged by a variable
        centroids <- centroids %>%
          group_nest(!!sym(names(centroids)[1])) %>%
          arrange(!!sym(order_sf_id)) %>% 
          mutate(rownumber = row_number()) 
        
      } else{
        # If no focal points, and no variable to order by
        centroids <- centroids %>%
          group_nest(!!sym(names(centroids)[1])) %>%
          mutate(closest = purrr::map(data, closest_focal_point, focal_points = 
                                        tibble(mean = "mean", 
                                               longitude = mean(centroids$longitude), 
                                               latitude = mean(centroids$latitude)))) %>%
          unnest(c("data", "closest")) %>%
          arrange(focal_distance) %>% 
          mutate(rownumber = row_number()) 
        
      }
    }  
    
    # Set up allocation data frame
    centroid_allocation <- NULL
    
    # keep value to reset expand distance to half of original given distance
    expand_dist <- (hex_filter * hex_size) / 2
    expanded_times <- 0
    
    ###########################################################################
    
    # Allocate geographic units one by one 
    p <- progress::progress_bar$new(total = NROW(centroids),
                                    format = " allocating [:bar] :percent eta: :eta",)
    p$tick(0)
    
    # Allocate in order of the row numbers
    for (centroidnum in centroids$rownumber) {
      
      # Operate on one centroid
      centroid <- filter(centroids, rownumber == centroidnum)
      
      # Indicate progression
      if (verbose) {
        p$tick()
      }
      
      # Initialise data set 
      f_grid <- NULL # filter the grid for appropriate hex positions
      
      # filter for only the available hex grid points
      if (!is.null(centroid_allocation)) {
        hex_grid <- hex_grid %>% filter(!assigned)
      }
      
      # If the grid boundary point was reached, expand angle?
      max_dist <- hex_filter * hex_size * 10 # can only expand 10 times
      filter_dist <- hex_filter * hex_size
      
      # filter grid for available points
      while (NROW(f_grid) == 0) {
        if (filter_dist < max_dist) {
          f_grid <- filter_grid_points(
              f_grid = hex_grid,
              f_centroid = centroid,
              focal_points = focal_points,
              f_dist = filter_dist,
              angle_width = width,
              h_size = hex_size
            )
          # Expand the distance considered if no points were available
          if (NROW(f_grid) == 0) {
            filter_dist <- filter_dist + expand_dist
            expanded_times <- expanded_times + 1
          }
        }
        # prevent endless loop
        else {
          # Too much loss of spatial pattern beyond 80 degrees
          if (width < 80) {
            width <- width + 5
          }
          else {
            stop("This hexmap could not be completed. Try expanding the buffer distance.")
          }
          message(
            paste(
              "Issue at ",
              pull(centroid[, 1]),
              ": Cannot expand further, trying a wider angle of ",
              width,
              " degrees."
            )
          )
        }
      }
      
      hex <- f_grid %>%
        ungroup() %>%
        filter(hyp == min(hyp)) %>%
        select(hex_long, hex_lat, hex_id = id)
      
      # Filter for the geographical neighbours of the area
      # To be implemented at a later date
      
      # Choose first available point
      cent <- centroid
      if ("focal_distance" %in% colnames(cent)) {
        cent <-
          cent %>% dplyr::rename(focal_dist = focal_distance, focal_angle = angle)
      }
      
      # update grid to show this centroid as assigned
      hex_grid[which(hex_grid$id == hex$hex_id),]$assigned <- TRUE
      
      centroid_allocation <-
        bind_rows(centroid_allocation, dplyr::bind_cols(cent, hex)) %>%
        as_tibble()
    }
    
    
    if (expanded_times > 0) {
      message(paste(
        "\nFilter distance was expanded for ",
        expanded_times,
        "area(s)."
      ))
    }
    
    # Return all allocations to the data frame
    return(centroid_allocation)
    
  }
