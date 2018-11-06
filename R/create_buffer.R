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

    nlong <- length(unique(grid$hex_long))
    nlat <- length(unique(grid$hex_lat))

    centroids <- centroids %>%
        mutate(long_int = round((longitude-min(grid$hex_long))/(max(grid$hex_long)-min(grid$hex_long))*nlong, 0),
            lat_int = round((latitude-min(grid$hex_lat))/(max(grid$hex_lat)-min(grid$hex_lat))*nlat, 0))

    # Amount of lats and longs in each group
    lat_size = round(nlat/20,0)
    long_size = round(nlong/20,0)


    # make a list of groups, manual sliding windows
    nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)
    nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)


    lat_window <- function(x, cents = centroids, maximum = nlat){
        max_int = min(x[2],maximum)

        cents_in <- filter(cents, between(lat_int, x[1], max_int))
        return(cents_in)
    }

    long_window <- function(x, cents = centroids, maximum = nlong){
        max_int = x[2]
        while (max_int > maximum){
            max_int = max_int - 1
        }

        cents_in <- filter(cents, between(long_int, x[1], max_int))
        return(cents_in)
    }

    # LATITUDE ROWS FILTER
    # amount of latitude in sliding window
    lat_windows <- purrr::map(.x = nlat_list, .f = lat_window)

    # find the min and max longitude for each latitude
    range_rows <- purrr::map_dfr(.x = lat_windows,
        .f = function(x) {x %>%
                dplyr::summarise(
                    long_min = ifelse(rlang::is_empty(long_int), NA, min(x$long_int)),
                    long_max = ifelse(rlang::is_empty(long_int), NA, max(x$long_int))
                )}
    )

    # smooth the minimums
    av_range_rows <- purrr::map_dfr(.x = nlat_list, .f = function(x, rows = range_rows) {
        rows[x[1]:min(x[2], NROW(rows)),] %>%
            dplyr::summarise(mean_long_min = mean(long_min, na.rm=T), mean_long_max = mean(long_max, na.rm=T))
    }) %>%
        bind_cols(lat_id = c(seq(1:nlat) +lat_size), .)

    # LONGITUDE COLS FILTER
    long_windows <- purrr::map(.x = nlong_list, .f = long_window, centroids, nlong)

    # find the min and max longitude for each latitude
    range_cols <- purrr::map_dfr(.x = long_windows, .f = function(x) { x %>%
            dplyr::summarise(
                lat_min = ifelse(rlang::is_empty(lat_int), NA, min(x$lat_int)),
                lat_max = ifelse(rlang::is_empty(lat_int), NA, max(x$lat_int))
            )}
    )

    # smooth the minimums
    av_range_cols <- purrr::map_dfr(.x = nlong_list, .f = function(x, cols = range_cols) {
        cols[x[1]:min(x[2], NROW(cols)),] %>%
            dplyr::summarise(mean_lat_min = mean(lat_min, na.rm=T), mean_lat_max = mean(lat_max, na.rm=T))
    }) %>%
        bind_cols(long_id = c(seq(1:nlong) + round(long_size/2)), .)


    # APPLY A BUFFER
    # change buffer to amount of hexagons (ints) either side
    hex_buffer <- floor(buffer_dist/hex_size)

    buff_grid <- grid %>%
        left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
        left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
        rowwise %>%
        mutate(long_buffer = ifelse(between(hex_long_int,mean_long_min - hex_buffer,
            mean_long_max + hex_buffer), "in", "out")) %>%
        mutate(lat_buffer = ifelse(between(hex_lat_int,mean_lat_min - hex_buffer,
            mean_lat_max + hex_buffer), "in", "out")) %>%
        filter(lat_buffer =="in" | long_buffer == "in")


    return(buff_grid)
}
