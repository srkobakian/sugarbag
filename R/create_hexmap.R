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
#' @importFrom sf st_read
#'
#' @examples
#' \dontrun{
#' # change to sa2_2011 on github\
#' shp_path <- system.file("data","sa2_2011.Rda", package = "sugaRbag")
#' focal_points <- system.file("data","capital_cities.Rda", package = "sugaRbag")
#'
#' create_hexmap(shp_path = shp_path, buffer_dist = NULL, hex_size = "auto", export_shp = FALSE,
#' focal_points = focal_points)
#' }
#'
create_hexmap <- function(shp_path, buffer_dist = NULL, hex_size = "auto", focal_points = NULL, export_shp = FALSE) {

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

    if (!is.null(focal_points)) {
        # distance between centroids and all focal points
        centroids <- shp_sf %>% rowwise %>%
            plyr::adply(., .fun = closest_focal_point, focal_points, .margins = 1)
    }


    ###########################################################################
    # Allocate polygons to a hexagon

    hexmap_allocation <- plyr::adply(centroids, .fun = allocate, .margins = 1, grid = hex_grid)

    return(hexmap_allocation)
}
