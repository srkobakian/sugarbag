#' Create a sugarbag hex map
#' @description 
#' `geom_sugarbag()` provides a convenient way to create tesselated
#' hexagon maps using the sugarbag algorithm. 
#' 
#' @examples 
#' \donttest{
#' library(ggplot2)
#' # Map of Tasmanian local govt areas using built-in data
#'  tas_lga %>%
#'   ggplot(aes(fill = lga_name_2016)) +
#'   geom_sf(alpha = 0.1) +
#'   geom_sugarbag(aes(geometry = geometry)) +
#'   theme(legend.position = "none")
#'   
#' # Map of SIDS data from North Carolina
#' if (requireNamespace("sf", quietly = TRUE)) {
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' 
#' ggplot(nc,
#'   aes(fill = AREA)) +
#'   geom_sf(alpha = 0.1) +
#'   geom_sugarbag(aes(geometry = geometry))
#' }
#' }
#' @import ggplot2
#' @export
#' @inheritParams ggplot2::geom_polygon
#' @param hex_size Default is 0.2. Units are degrees, corresponding to
#' the diameter of the hexagons. See `?allocate`.
#' @seealso allocate, ggplot2::geom_polygon
#' @title geom_sugarbag
#' @details 
#' The sugarbag algorithm creates a hexagon tile map from spatial 
#' polygons. It represents each polygon with a hexagon, which is placed
#' close to the polygon's centroid while also maintaining its spatial
#' relationship to a focal point.
#' 
#' If `geom_sugarbag()` is used to make a map of Australia, the capital cities
#' will be used as focal points. For non-Australian maps, a single focal point
#' will be inferred from the data, as the centroid with the smallest total 
#' distance to its three nearest neighbours. To specify focal points manually, 
#' construct your hexagon grid manually -- see `?allocate`.
#' 

geom_sugarbag <- function(mapping = NULL,
                          data = NULL,
                          stat = "sugarbag",
                          position = "identity",
                          hex_size = 0.2,
                          na.rm = FALSE,
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSugarbag,
    position = position,
    params = list(
      hex_size = hex_size,
      na.rm = na.rm
    ),
    ...
  )
}


StatSugarbag <- ggproto("StatSugarbag",
                        Stat,
                        compute_group = function(data, scales) {

                        },
                        compute_panel = function(data, 
                                                 scales, 
                                                 hex_size) {
                          data <- sf::st_as_sf(data)
                          out <- make_sugarbag_df(data, hex_size)
                          out %>%
                            select(-group) %>%
                            rename(x = long,
                                   y = lat,
                                   group = sf_id)
                        },
                        required_aes = c(
                          "geometry"
                        ),
                        default_aes = aes(
                          geometry = geometry,
                          fill = NA,
                          size = 0.2,
                          linewidth = 0.2,
                          linetype = 1,
                          alpha = NA
                        ))

stat_sugarbag <- function(mapping = NULL,
                          data = NULL,
                          geom = "polygon",
                          position = "identity",
                          na.rm = FALSE,
                          hex_size = 0.2,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatSugarbag,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, 
                  hex_size = hex_size,
                  ...)
  )
}

GeomSugarbag <- ggproto(
  "GeomSugarbag",
  Geom,
  extra.params = c("hex_size"),
  setup_data = function(data, params) {
    GeomPolygon$setup_data(data, params)
  },
  draw_group = function(data,
                        panel_params,
                        coord) {
    GeomPolygon$draw_panel(
      data,
      panel_params,
      coord
    )
  },
  draw_key = ggplot2::draw_key_polygon,
  default_aes = aes(
    fill = GeomPolygon$default_aes$fill,
    colour = GeomPolygon$default_aes$colour,
    hex_size = 0.2,
    linewidth = 0.2,
    linetype = 1,
    alpha = NA
  ),
  required_aes = c("geometry")
)

#' Create a dataframe of hexagon locations for use in 
#' geom_sugarbag(). This function is an opinionated wrapper around
#' various other functions in the sugarbag package.
#' 
#' @param shp A shapefile of class `"sf"`
#' @param hex_size Default 0.2. See `?allocate`
#' @param hex_width Default 50. See `?allocate` (argument `width`)
#' @keywords internal
make_sugarbag_df <- function(shp,
                             hex_size = 0.2,
                             hex_width = 50) {
  shp <- shp %>%
    mutate(sf_id = as.character(row_number()))

  centroids <- create_centroids(shp, sf_id = "sf_id")

  grid <- create_grid(centroids = centroids, hex_size = hex_size, buffer_dist = 1.2)

  # NEED TO REPLACE WITH infer_focal_points()
  aus_caps <- tibble::tribble(
                        ~points,  ~longitude,    ~latitude,
                    "Melbourne", 144.9750162, -37.82003131,
                     "Canberra", 149.1290262, -35.28302855,
                       "Sydney", 151.1851798, -33.92001097,
                       "Darwin", 130.8500386, -12.42535398,
                     "Brisbane", 153.0350927, -27.45503091,
                     "Adelaide", 138.6000048, -34.93498777,
                       "Hobart", 147.2950297, -42.85000853,
                        "Perth", 115.8399987, -31.95501463
                    )
  
  
  is_aus <- any(min(aus_caps$longitude) <= centroids$longitude &
    max(aus_caps$longitude) >= centroids$longitude & 
    min(aus_caps$latitude) <= centroids$latitude &
    max(aus_caps$latitude) >= centroids$latitude)

  if (is_aus) {
    focal_points <- aus_caps
  } else {
    focal_points <- infer_focal_points(centroids)
  }

  hex_allocated <- allocate(centroids = centroids,
                            sf_id = "sf_id",
                            hex_grid = grid,
                            hex_size = hex_size,
                            hex_filter = 10,
                            focal_points = focal_points,
                            width = hex_width,
                            verbose = FALSE)

  hexagons <- hex_allocated %>%
    fortify_hexagon(hex_size = hex_size, sf_id = "sf_id") %>%
    left_join(., shp, by = "sf_id") %>%
    mutate(poly_type = "hex")

  hexagons
}

#' From a given set of (long, lat) coordinates, return the 
#' coordinate that is the 'focal point'.
#' 
#' @param centroids A data frame with three columns: `sf_id` (character),
#' `longitude` (numeric) and `latitude` (numeric).
#' @return A dataframe with `longitude` and `latitude` columns.
#' The output dataframe has one row, corresponding to the 'focal point' row.
#' @details 
#' The 'focal point' coordinate is defined as the
#' coordinate with the smallest total distance between it and its
#' three closest other coordinates.
#' @keywords internal
#' @examples 
#' \donttest{
#' centroids <- tibble::tribble(
#'                   ~sf_id,       ~longitude,         ~latitude,
#'                       1L, 143.733882205026, -37.3749816911681,
#'                       2L, 144.092833347828, -36.5994048331978,
#'                       3L, 144.126090989656,  -38.110623159732,
#'                       4L, 146.418433326656, -36.7767887044158,
#'                       5L, 147.436575203924, -37.7245209900909,
#'                       6L, 144.964502395924, -37.8001310712579,
#'                       7L, 145.104879082048, -37.8047905740117,
#'                       8L, 145.063768728334, -37.9503253274278,
#'                       9L, 145.169211500636, -37.5206187637749,
#'                      10L, 144.745689372755, -37.4757934330461,
#'                      11L, 145.505990121285, -37.7596168112236,
#'                      12L, 145.468024958887, -38.0731739302227,
#'                      13L, 144.624846316441,  -37.776375592293,
#'                      14L, 145.056546632905,  -38.310243349317,
#'                      15L, 142.235170269284, -35.8606656849638,
#'                    16L, 145.168411414665, -36.2522733040558,
#'                      17L, 142.377470204542, -37.9751565676334
#'                   )
#' # infer_focal_points(centroids)
#' }
infer_focal_points <- function(centroids) {
  
  nested_centroids <- centroids %>% 
    mutate(coords = cbind(longitude, latitude)) %>% 
    select(-longitude, -latitude)
  
  wide_centroids <- nested_centroids  %>% 
    tidyr::pivot_wider(names_from = sf_id,
                values_from = coords) %>% 
    bind_cols(nested_centroids) 
  
  ids <- unique(centroids$sf_id)
  
  dists <- purrr::imap(.x = ids,
                       .f = ~geosphere::distVincentyEllipsoid(wide_centroids$coords,
                                                              wide_centroids[[.x]]),
                       a = 6378160, b = 6356774.719, f = 1 / 298.257222101
  ) %>% 
    purrr::set_names(nm = ids) %>% 
    bind_cols() %>% 
    mutate(sf_id = centroids$sf_id,
           .before = dplyr::everything()) %>% 
    mutate(across(everything(), ~na_if(.x, 0))) %>% 
    tidyr::pivot_longer(cols = !sf_id,
                 names_to = "other_sf_id",
                 values_to = "dist")
  
  chosen_focal <- dists %>% 
    filter(!is.na(dist)) %>% 
    group_by(sf_id) %>% 
    arrange(sf_id, dist) %>% 
    filter(row_number() <= 3) %>% 
    summarise(sum_closest = sum(dist)) %>% 
    ungroup() %>% 
    filter(sum_closest == min(sum_closest))
  
  centroids[centroids$sf_id == chosen_focal$sf_id, ]
  
  centroids %>% 
    filter(sf_id == chosen_focal$sf_id) %>% 
    select(longitude, latitude)
  
}

utils::globalVariables(c("coords",
                         "dist",
                         "sum_closest"))
