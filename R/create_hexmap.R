#' Create a tesselated hexagon map from a set of polygons
#'
#' Allocates each polygon in a shape file to a grid point to create a map of
#' tesselated hexagons. The spatial relationships of areas are preserved while
#' the geographic chape of each area is lost.
#'
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param buffer a float distance in degrees to exapnd hexagon grid
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param export_shp export the simple features set
#'
#' @return a data set containing longitude and latitude of allocated hexagon
#' points for each non null geometry passed in the shape file
#' @export
#'
#' @import sf
#'
#' @examples
#' \dontrun{
#' # change to sa2_2011 on github\
#' shp_path <- system.file("data","sa2_2011.Rda", package = "sugaRbag")
#' load(system.file("data","capital_cities.Rda", package = "sugaRbag"))
#'
#' hexmap <- create_hexmap(shp_path = shp_path, buffer_dist = NULL, hex_size = "auto", export_shp = FALSE,
#' focal_points = capital_cities)
#' }
#'
create_hexmap <- function(shp_path, buffer_dist = NULL, hex_size = "auto", filter_dist = 1000, focal_points = NULL, export_shp = FALSE) {

    # Read in ESRI shape file, remove null geometries, transform projection
    shp_sf <- read_shape(shp_path, simplify = TRUE)

    ###########################################################################
    # Derive centroids from geometry column, do something about warning message
    centroids <- create_centroids(shp_sf)

    # create a buffer distance if not supplied
    if (is.null(buffer_dist)){
        #bounds <- sf::st_bbox(shp_sf, crs = 3112) #unnecessry
        bbox <- tibble::tibble(min = c(min(centroids$longitude),
            min(centroids$latitude)),
            max = c(max(centroids$longitude),
                max(centroids$latitude)))
        buffer_dist <- (bbox$max[1] - bbox$min[1]) / 10
    }

    # if matrix, convert to tibble
    if (!("tbl" %in% class(bbox))){
        bbox <- tibble::as.tibble(bbox)
    }

    # if hex_size TODO: tune this
    if (hex_size == "auto"){
        hex_size <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2]) / 10
    }

    ###########################################################################
    # Create grid for hexagons
    hex_grid <- create_grid(centroids, bbox, hex_size, buffer_dist)


    if (export_shp) {
        suppressWarnings(shp <<- shp_sf)
    }

    # consider focal point distance if they were provided
    if (!is.null(focal_points)) {

        # distance between centroids and all focal points
        message("Finding closest point in focal_points data set.")
        s_centroids <- split(x = centroids, f = centroids$sf_id)

        centroids <- bind_cols(centroids,
            purrr::map_dfr(.x = s_centroids,
                .f = closest_focal_point,
                focal_points = focal_points))

        message("Closest points found.")
    }

    ###########################################################################
    # Allocate polygons to a hexagon

    s_centroids <- centroids %>% split(.$sf_id)

    hexmap_allocation <- purrr::map_dfr(.x = s_centroids, .f = allocate, hex_grid = hex_grid, hex_size = hex_size, filter_dist = filter_dist)

    return(hexmap_allocation)
}
