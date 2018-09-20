#' create_hexmap
#'
#' Allocates each polygon in a shape file to a grid point to create a map of
#' tesselated hexagons. The spatial relationships of areas are preserved while
#' the geographic chape of each area is lost.
#'
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param buffer a float distance in degrees to exapnd hexagon grid
#' @param radius a float value in degrees for the diameter of the hexagons
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param export_shp export the simple features set including centroids
#'
#' @return
#' @export
#'
#' @importFrom sf st_read
#'
#' @examples
#' \dontrun{
#' shp <- "C:/Users/steff/Documents/atlas/hexmap/data/LGA_2011_AUST/LGA_2011_AUST.shp"
#'
#' create_hexmap(shp, buffer = NULL, radius = "auto", export_shp = FALSE,
#' focal_points = system.file("data","capital_cities.Rda", package = "sugaRbag"))
#' }
#'
create_hexmap <- function(shp_path, buffer = NULL, radius = "auto", focal_points = NULL, export_shp = FALSE) {

    # Read in ESRI shape file, remove null geometries, transform projection
    if (class(shp_path) == "character") {
        shp <- tryCatch(expr = sf::st_read(shp),
            error=function(e) print("Argument for shp could not be read as sf object."))
    }

    shp_polys <- shp %>%
        dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
        sf::st_transform(shp_polys, crs = 3112, '+init=epsg:3112 +proj=longlat +ellps=GRS80')
# add message that polygons were dropped?

    # find the bbox
    if (is.null(buffer)){
        bounds <- sf::st_bbox(shp, crs = 3112)
        bbox <- tibble::tibble(min = c(bounds[1], bounds[2]),
            max = c(bounds[3], bounds[4]))
    }

    # if matrix, convert to tibble
    if (!("tbl" %in% class(bbox))){
        bbox <- tibble::as.tibble(bbox)
    }

    # if there was no buffer distance, make it 10% of bounds
    if (is.null(buffer)){
        buffer <- max(c(bbox$max[1] - bbox$min[1]), (bbox$max[2] - bbox$min[2])) * 0.1
    }

    # create buffer zone for grid points
    # takes so long!
    full <- sf::st_combine(shp$geometry)
    buffer_zone <- sf::st_buffer(full, buffer)

    # if radius is not supplied, choose radius TODO: tune this
    if (radius == "auto"){
        radius <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2])/5
    }

    ###########################################################################
    # Derive centroids from geometry column, do something about warning message
    centroids <- shp_polys %>% sf::st_centroid() %>%
        sf::st_transform(., '+init=epsg:3112 +proj=longlat +ellps=GRS80') %>%
        sf::st_coordinates() %>%
        tibble::as.tibble()

    # Add centroids to shapes set
    shp_polys <- shp_polys %>% mutate(long_c = centroids$X, lat_c = centroids$Y)

    if (export_shp) {
        suppressWarnings(shp <<- shp_polys)
    }

    if (!is.null(focal_points)) {
        # distance between centroids and all focal points
        centroids <- shp_polys %>% rowwise %>%
            plyr::adply(., .fun = closest_focal_point, focal_points, .margins = 1)
    }

    ###########################################################################
    # Create grid for hexagons
    hex_grid <- create_grid(bbox = bbox, radius = radius)

    ###########################################################################
    # Allocate polygons to a hexagon

    hexmap_allocation <- plyr::adply(centroids, .fun = allocate, .margins = 1, grid = hex_grid)

    return(hexmap_allocation)
}
