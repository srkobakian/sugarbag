#' Allocate polygon centroids to hexagons in a grid
#'
#' Chooses a hexagon centroid for each polygon in the shape file, from a grid
#' spanning the longitudes and latitudes in the expanded bounding box.
#'
#'
#' @param centroids a data frame with centroids of non empty polygons
#' @param hex_grid a data frame containing all possible hexagon points
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param hex_filter amount of hexagons around centroid to consider
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param verbose a boolean to indicate whether to show polygon id
#' @param width a numeric indicating the angle used to filter the hexagon grid
#' @param sf_id a string to indicate the column to identify individual polygons
#'
#' @return a data frame of one allocation
#' @importFrom ggplot2 sym
#' @export
#'
#' @examples
#' # Create centroids set
#' centroids <- create_centroids(tas_lga, sf_id = "LGA_CODE16")
#' # Create hexagon location grid
#' data(capital_cities)
#' grid <- create_grid(centroids = centroids, hex_size = 0.1, buffer_dist = 0.1)
#' # Allocate polygon centroids to hexagon grid points
#' hex_allocated <- allocate(centroids = centroids,
#' hex_grid = grid,
#' hex_size = 0.1, # same size used in create_grid
#' hex_filter = 10,
#' focal_points = capital_cities,
#' width = 30, verbose = TRUE)
#' # same column used in create_centroids
#'
allocate <- function(centroids, hex_grid, sf_id = names(centroids)[1], hex_size, hex_filter, focal_points = NULL, width, verbose) {

    if (!is.null(focal_points)) {
        if(!("focal_distance" %in% colnames(centroids))) {
            s_centroids <- centroids %>%
                group_nest(!!sf_id := as.character(!!sym(sf_id))) %>%
                mutate(closest = purrr::map(data, closest_focal_point, focal_points = focal_points)) %>%
                tidyr::unnest(data, closest) %>% arrange(focal_distance)
            
        } else {
                    s_centroids <- centroids %>% arrange(focal_distance)
        }

        s_centroids <- split(s_centroids, s_centroids[["focal_distance"]])
        message("Allocating centroids, in order of distance to closest focal point.")
    } else {
        s_centroids <- split(centroids, centroids[[sf_id]])
    }

    # Set up allocation data frame
    centroid_allocation <- NULL

    # keep value to reset expanded distances
    expand_dist <- hex_filter

    ###########################################################################
    p <- progress_estimated(NROW(centroids), min_time = 3)

    for (centroid in s_centroids) {

        # Indicate progression
        if (verbose) {

            p$tick()$print()
        }

        # filter the grid for appropriate hex positions
        f_grid = NULL

        # filter for only the available hex grid points
        if (!is.null(centroid_allocation)) {
            hex_grid <- hex_grid %>% filter(!assigned)
        }

        # Make this find if the grid boundary point was reached, expand angle?
        max_dist <- hex_filter*10

        # filter grid for avaiable points
        while(NROW(f_grid) == 0) {
            if (hex_filter < max_dist) {
                f_grid <- filter_grid_points(f_grid = hex_grid, f_centroid = centroid, focal_points = focal_points, f_dist = hex_filter, angle_width = width, h_size = hex_size)
                if (NROW(f_grid) == 0) {
                    hex_filter <- hex_filter + expand_dist
                    print(paste("Filter Distance expanded by ", expand_dist, " to ", hex_filter))
                }
            }
            # prevent endless loop
            else {

                hex_filter <- max_dist/5
                width <- width + 5
                message("Cannot expand further, trying a wider angle.")

                #break
            }
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


