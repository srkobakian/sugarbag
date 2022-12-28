#' @examples 
#' \donttest{
#' library(ggplot2)
#'  tas_lga %>%
#'   ggplot(aes(fill = lga_name_2016)) +
#'   geom_sf(alpha = 0.1) +
#'   geom_sugarbag(aes(geometry = geometry)) +
#'   theme(legend.position = "none")
#' }
#' @import ggplot2
#' @export
#' @title geom_sugarbag

geom_sugarbag <- function(mapping = NULL,
                          data = NULL,
                          stat = "sugarbag",
                          position = "identity",
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSugarbag,
    position = position,
    ...
  )
}


StatSugarbag <- ggproto("StatSugarbag",
                        Stat,
                        compute_group = function(data, scales) {

                        },
                        compute_panel = function(data, scales) {
                          data <- sf::st_as_sf(data)
                          out <- make_sugarbag_df(data)
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
    params = list(na.rm = na.rm, ...)
  )
}

GeomSugarbag <- ggproto(
  "GeomSugarbag",
  Geom,
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
    size = 0.2,
    linewidth = 0.2,
    linetype = 1,
    alpha = NA
  ),
  required_aes = c("geometry"),
  non_missing_aes = c("fill")
)


make_sugarbag_df <- function(shp,
                             hex_size = 0.2) {
  shp <- shp %>%
    mutate(sf_id = as.character(row_number()))

  centroids <- create_centroids(shp, sf_id = "sf_id")

  grid <- create_grid(centroids = centroids, hex_size = hex_size, buffer_dist = 1.2)

  # NEED TO REPLACE WITH infer_focal_points()
  focal_points <- tibble::tribble(
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

  # focal_points <- tibble(longitude = stats::median(centroids$longitude),
  #                        latitude = stats::median(centroids$latitude))

  hex_allocated <- allocate(centroids = centroids,
                            sf_id = "sf_id",
                            hex_grid = grid,
                            hex_size = hex_size,
                            hex_filter = 10,
                            # focal_points = capital_cities,
                            focal_points = focal_points,
                            # width = 30,
                            width = 85,
                            verbose = F)

  hexagons <- hex_allocated %>%
    fortify_hexagon(hex_size = hex_size, sf_id = "sf_id") %>%
    left_join(., shp) %>%
    mutate(poly_type = "hex")

  hexagons
}
