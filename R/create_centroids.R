#' Create a data frame of longitude and latitude centroids of each polygon
#'
#' @param shp_sf an sf object, a data set with a simple feature list column
#'
#' @return a tibble containing longitude and latitude
#' @export
#'
#' @examples
create_centroids <- function(shp_sf) {
    # have an option to pass id column

    centroids <- shp_sf %>% sf::st_centroid() %>%
        sf::st_transform(., '+init=epsg:3112 +proj=longlat +ellps=GRS80') %>%
        sf::st_coordinates() %>%
        tibble::as.tibble() %>%
        mutate(sf_id = dplyr::row_number()) %>%
        dplyr::select(sf_id, longitude = X, latitude = Y)

    return(centroids)
}
