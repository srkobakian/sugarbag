#' Convert a simple features tibble to tibble for plotting.
#'
#' This will contain individual points for plotting the polygon, indicating the
#' longitude and latitude, order of points, if a hole is present, the piece, id
#' and group.
#'
#'
#' @param sfc_df a simples features data set
#' @param keep ratio of points to keep
#'
#' @return a tibble point of long lat points used to plot polygons
#' @export
#'
fortify_sfc <- function(sfc_df, keep = NULL) {


    sfc_df <- sf::st_as_sf(sfc_df)
    if (!is.null(keep)) {
    sfc_df <- sfc_df %>% mutate(geometry = rmapshaper::ms_simplify(sfc_df$geometry, keep = keep, keep_shapes = TRUE))
    }
    sf_tbl <- sf::as_Spatial(sfc_df)
    sf_tbl@data[["row"]] <- rownames(sf_tbl@data)
    # Australian Projection for Long Lat
    sf_tbl <- ggplot2::fortify(sf_tbl) %>% left_join(sf_tbl@data, by =c("id" = "row")) %>% dplyr::select(-id)

    return(sf_tbl)

}
