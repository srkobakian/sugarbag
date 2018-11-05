#' Convert a simple features tibble to tibble for plotting. This will contain
#' individual points for plotting the polygon, indicating the longitude and
#' latitude, order of points, if a hole is present, the piece, id and group.
#'
#' @param sfc a simple features table with a geometry column
#'
#' @return a tibble point of long lat points used to plot polygons
#' @export
#'
sfc_to_tibble <- function(sfc_df) {

    sf_tbl = as(sfc_df,'Spatial')
    sf_tbl@data[["row"]] <- rownames(sf_tbl@data)
    sf_tbl <- fortify(sf_tbl) %>% left_join(sf_tbl@data, by =c("id" = "row")) %>% dplyr::select(-id)

    return(sfc_tbl)

}
