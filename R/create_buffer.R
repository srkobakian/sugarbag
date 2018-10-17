#' Expand points to extend beyond the outermost centroids
#'
#' This function takes the bounding box of a group of polygons, or a specific
#' table of minimum and maximum longitudes and latitudes to create points
#' for each polygon to be allocated to that will tesselate into hexagons.
#'
#' @param centroids data frame of centroids to be allocated
#' @param grid data frame of hexagon centroids
#' @param buffer_dist distance to extend beyond the geometry provided
#' @param hex_size a float value in degrees for the diameter of the hexagons
#'
#' @return data frame of hexagon centroids
#' @export
#'
#' @import dplyr
#'
#'
#'

create_buffer <- function(centroids, grid, hex_size, buffer_dist) {
    ###########################################################################
    # filter grid points to be within buffer_dist

    # ALLOCATE CENTROID TO GRID ROW: Round polygon centroids to the nearest grid row
    # Scale rows 0 to n
    # Round to closest integer
    # To find a grid row number for centroid
    grid <- grid %>%
        group_indices(., hex_lat) %>%
        mutate(grid, hex_row = .)
    centroids <- centroids %>% mutate(
        from_min = (latitude-min(latitude)),
        standardised = from_min/max(from_min),
        cent_row = standardised*max(grid$hex_row),
        lat_row = round(cent_row, 0)) %>%
        dplyr::select(sf_id, longitude, latitude, lat_row)


    # find convex hull
    chull_grid <- chull(centroids$longitude, centroids$latitude)
    hull_points <- centroids %>%
        mutate(c_id = row_number()) %>%
        filter(c_id %in% chull_grid)

    # FOLLOW THE CONVEX HULL
    # find min max long for each latitude group according to chull group
    # find gradient between i and i+1 hull values
    #add gradient between point and next as column value
    hull_points <- hull_points %>%
        # add Point B to row of Point A
        mutate(order = row_number(), next_long = lead(longitude), next_lat = lead(latitude))

    hull_points[NROW(hull_points),]$next_long <- hull_points[1,]$next_long
    hull_points[NROW(hull_points),]$next_lat <- hull_points[1,]$next_lat
    hull_points <- hull_points %>%
    mutate(gradient = round(((next_lat - latitude)/(next_long - longitude)), 4))

    # find the min& max long for buffer for each lat
    # use a map for each unique latitude, get back two values, minimum long, maximum long
    grid_rows <- split(x = grid, f = grid$hex_row)
    grid_rows <- bind_cols(grid_rows,
        purrr::map_dfr(.x = grid_rows,
            .f = find_lat_group, hull_points = hull_points, buffer_dist = buffer_dist))

    centroids <- bind_cols(centroids,
        purrr::map_dfr(.x = s_centroids,
            .f = closest_focal_point,
            focal_points = focal_points))

    return(grid)
}
