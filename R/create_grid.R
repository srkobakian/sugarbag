
#' create_grid
#' This function takes the bounding box of a group of polygons, or a specific
#' table of minimum and maximum longitudes and latitudes to create points
#' for each polygon to be allocated to that will tesselate into hexagons.
#'
#' @param bbox table with columns minimum and maximum and rows
#' containing longitude and latitude values
#' @param radius a float value in degrees for the diameter of the hexagons
#' @param expand_grid a float value to expand the grid beyond the minimum
#'  and maximum longitudes and latitudes
#'
#' @return grid
#' @export
#'
#' @import dplyr
#'
#' @examples
#'

create_grid <- function(bbox, radius, buffer) {

    # filter grid points to be within buffer

    grid <- tibble::as.tibble(expand.grid(hex_long = seq(bbox$min[1] - buffer,
                                                 bbox$max[1] + buffer,
                                                 radius),
                                  hex_lat = seq(bbox$min[2] - buffer,
                                                bbox$max[2] + buffer,
                                                radius)))

    # Shift the longitude of every second latitude - to make hex structure
    shift_lat <- grid %>% dplyr::select(hex_lat) %>%
        dplyr::distinct() %>%
        dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()

    return_grid <- grid %>%
        dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
                                 hex_long + (radius / 2))) %>%
        # TODO why isn't assigned added as a column???
        dplyr::mutate(id=1:NROW(.)) %>%
        dplyr::mutate(assigned=FALSE)

    return(return_grid)
}
