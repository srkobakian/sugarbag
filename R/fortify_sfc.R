#' Convert a simple features tibble to tibble for plotting. This will contain
#' individual points for plotting the polygon, indicating the longitude and
#' latitude, order of points, if a hole is present, the piece, id and group.
#'
#' @param sfc a simple features table with a geometry column
#'
#' @return a tibble point of long lat points used to plot polygons
#' @export
#'
fortify_sfc <- function(sfc_df) {

    sfc_df <- sf::st_as_sf(sfc_df)
    sf_tbl <- sf::as_Spatial(sfc_df)
    sf_tbl@data[["row"]] <- rownames(sf_tbl@data)
    # Australian Projection for Long Lat
    sf_tbl <- sp::spTransform(sf_tbl, sp::CRS('+init=epsg:3112 +proj=longlat +ellps=GRS80'))
    sf_tbl <- fortify(sf_tbl) %>% left_join(sf_tbl@data, by =c("id" = "row")) %>% dplyr::select(-id)

    return(sf_tbl)

}
