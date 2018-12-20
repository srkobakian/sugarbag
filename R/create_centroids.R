#' Create a data frame of longitude and latitude centroids of each polygon
#'
#' @param shp_sf an sf object, a data set with a simple feature list column
#' @param sf_id a string to indicate the column to identify individual polygons
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return a tibble containing longitude and latitude
#' @export
#'
#'
create_centroids <- function(shp_sf, sf_id = NULL, verbose = FALSE) {

    if (verbose) {
        message("Deriving polygon centroids")
    }

    # have an option to pass id column

    ids <- shp_sf %>% sf::st_set_geometry(NULL) %>% dplyr::select(.data[[sf_id]])

    centroids <- shp_sf %>% sf::st_centroid() %>%
        sf::st_transform(., '+init=epsg:3112 +proj=longlat +ellps=GRS80') %>%
        sf::st_coordinates() %>%
        tibble::as.tibble()

    # return with id column as specified
    if (is.null(sf_id)) {

        centroids <- centroids %>%
            mutate(sf_id = as.factor(dplyr::row_number())) %>%
            dplyr::select(sf_id, longitude = X, latitude = Y)
    } else {

        centroids <- bind_cols(ids, centroids %>%
                dplyr::select(longitude = X, latitude = Y))
    }

    return(centroids)
}
