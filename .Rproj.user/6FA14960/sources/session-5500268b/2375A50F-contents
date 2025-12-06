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
  #if (!is.null(keep)) {
  #  sfc_df <- sfc_df %>% mutate(geometry = rmapshaper::ms_simplify(sfc_df$geometry, keep = keep, keep_shapes = TRUE))
  #}
  # keep will not be used now

  sf_tbl <- sfc_df %>%
    mutate(geom = purrr::map(!!sym("geometry"), function(x) {
      purrr::map_dfr(x, function(y) {
        colnames(y[[1]]) <- c("long", "lat")
        as_tibble(y[[1]])
        }, 
      .id = "polygon")
    })) %>%
    unnest("geom") %>%
    mutate(poly_type = "geo")

  sf::st_geometry(sf_tbl) <- NULL

  return(tibble::as_tibble(sf_tbl))
}
