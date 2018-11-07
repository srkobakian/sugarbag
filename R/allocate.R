#' Allocate polygon centroids to hexagons in a grid
#'
#' Chooses a hexagon centroid for each polygon in the shape file, from a grid
#' spanning the longitudes and latitudes in the expanded bounding box.
#'
#'
#' @param centroids a data frame with centroids of non empty polygons
#' @param hex_grid a data frame containing all possible hexagon points
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param filter_dist distance around centroid to consider grid points
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param verbose a boolean to indicate whether to show polygon id
#' @param id a string to indicate the column to identify individual polygons
#'
#' @return a data frame of one allocation
#'
#'
allocate <- function(centroids, hex_grid, hex_size, filter_dist, focal_points = NULL, verbose, id) {

    if (!is.null(focal_points)) {
        a_centroids <- centroids %>% arrange(focal_distance)
        s_centroids <- split(a_centroids, a_centroids[["focal_distance"]])
        print("Allocating centroids, in order of distance to closest focal point.")
    } else {
        s_centroids <- split(centroids, centroids[[id]])
    }

    # Set up allocation data frame
    centroid_allocation <- NULL

    # keep value to reset expanded distances
    expand_dist <- filter_dist

    ###########################################################################
    p <- progress_estimated(NROW(centroids), min_time = 3)

    for (centroid in s_centroids) {

        # Indicate progression
        if (verbose) {

            p$tick()$print()
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
            hex_grid <- hex_grid %>% filter(!assigned)
        }

        # Make this find if the grid boundary point was reached, expand angle?
        max_dist <- filter_dist*10

        # filter grid for avaiable points
        while(NROW(f_grid) == 0) {
            if (filter_dist < max_dist) {
                f_grid <- filter_grid_points(f_grid = hex_grid, f_centroid = centroid, f_dist = filter_dist)
                if (NROW(f_grid) == 0) {
                    filter_dist <- filter_dist + expand_dist
                    print(paste("Filter Distance expanded by ", expand_dist, " to ", filter_dist))
                }
            }
            # prevent endless loop
            else break
        }

        # Choose first avaiable point
        cent <- centroid %>% dplyr::rename(focal_point = points, focal_dist = focal_distance, focal_angle = angle)

        # Filter should give one hex point
        hex <- f_grid %>% ungroup %>% filter(hyp == min(hyp)) %>%
            select(hex_long, hex_lat, hex_id = id)

        #update grid to show this centroid as assigned
        hex_grid[which(hex_grid$id == hex$hex_id),]$assigned <- TRUE

        centroid_allocation <- bind_rows(centroid_allocation, dplyr::bind_cols(cent, hex)) %>% as.tibble()
    }

    # Returnall allocations to the data frame
    return(centroid_allocation)
}


utils::globalVariables(c("sf_id", "longitude", "latitude", "assigned"))
