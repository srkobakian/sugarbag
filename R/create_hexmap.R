#' Create a tesselated hexagon map from a set of polygons
#'
#' Allocates each polygon in a shape file to a grid point to create a map of
#' tesselated hexagons. The spatial relationships of areas are preserved while
#' the geographic chape of each area is lost.
#'
#' @param shp a shape file
#' @param shp_path character string location of shape file
#' @param sf_id name of a unique column that distinguishes areas
#' @param buffer_dist distance in degrees to extend beyond the geometry provided
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param filter_dist amount of hexagons around centroid to consider for allocation
#' @param width the angle used to filter the grid points around a centroid
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param export_shp export the simple features set
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return a data set containing longitude and latitude of allocated hexagon
#' points for each non null geometry passed in the shape file
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(tas_sa2)
#' hexmap <- create_hexmap(shp = tas_sa2, sf_id = "SA2_5DIG16",
#' filter_dist = 10, export_shp = FALSE,
#' focal_points = sugaRbag::capital_cities, verbose = TRUE)
#' }
#'
create_hexmap <- function(shp = NULL, shp_path = NULL, sf_id = NULL, buffer_dist = NULL, hex_size = NULL, filter_dist = NULL, width = 15, focal_points = NULL, export_shp = FALSE, verbose = FALSE) {


    if (!is.null(shp)){
        if ("SpatialPolygonsDataFrame" %in% class(shp)){
            shp_sf <- sf::st_as_sf(shp)
        }
        else if ("geometry" %in% colnames(shp))  {
            shp_sf <- sf::st_as_sf(shp)
        }

        else {shp_sf <- shp}
    }
    else {
        if (!is.null(shp_path)) {
            # Read in ESRI shape file, remove null geometries, transform projection
            shp_sf <- read_shape(shp_path, simplify = TRUE)
        }
        else {
            return(message(paste0(shp_path," cannot be found.")))

        }
    }

    sf::st_agr(shp_sf) <- "constant"


    ###########################################################################
    # First make sure all levels have been dropped if not being used
    # Check to be sure column exists
    if (!(sf_id %in% colnames(shp_sf))){
        return(message("sf_id does not exist in this data set."))
    } else {
    shp_sf[[sf_id]] <- droplevels(as.factor(shp_sf[[sf_id]]))
    }

    # check for duplicated id values
    if (any(duplicated(shp_sf[[sf_id]]))) {
        message("The id variable chosen contains duplicates.\nThe shape file has been returned, please choose an identifying variable without duplicates.")
        return(shp_sf)
    }


    # Derive centroids from geometry column, do something about warning message
    centroids <- create_centroids(shp_sf = shp_sf, sf_id = sf_id, verbose = FALSE)

    # Creating a bounding box around all centroids
    bbox <- tibble::tibble(min = c(min(centroids$longitude),
        min(centroids$latitude)),
        max = c(max(centroids$longitude),
            max(centroids$latitude)))

    # create a buffer distance if not supplied
    if (is.null(buffer_dist)){
        buffer_dist <- max((bbox$max[1] - bbox$min[1]), (bbox$max[2] - bbox$min[2]))*0.3
        message(paste0("Buffer set to ", round(buffer_dist,4), " degrees."))
    }

    # Consider a buffer distance above 5 to be a mistake
    if (buffer_dist > 100) {
        # convert metres to degrees
        buffer_dist = buffer_dist/111139
        message(paste0("Converted buffer distance to ", round(buffer_dist, 4), "metres"))
    }


    # if hex_size was not provided
    if (is.null(hex_size)){
        hex_size <- (bbox$max[1] - bbox$min[1])/(bbox$max[2] - bbox$min[2]) / 10
        message(paste0("Converted hexagon size to ", round(hex_size, 4), " degrees."))
    }


    # filter according to amount of hexagons
    if (is.null(filter_dist)){
        filter_dist <- (hex_size)*10
    }
    else {
        if (filter_dist < 10){
        filter_dist <- (hex_size)*10
        }
        else {filter_dist <- (hex_size)*filter_dist}
    }

    message(paste0("Filter set to ", round(filter_dist,4), " degrees."))

    # if matrix, convert to tibble
    #if (!("tbl" %in% class(bbox))){
    #    bbox <- tibble::as.tibble(bbox)
    #}

    ###########################################################################
    # Create grid for hexagons
    hex_grid <- create_grid(centroids, bbox, hex_size, buffer_dist)

    # consider focal point distance if they were provided
    if (!is.null(focal_points)) {

        # distance between centroids and all focal points
        if (verbose) {message("Finding closest point in focal_points data set.")}

        centroids <- centroids %>%
            nest(longitude, latitude) %>%
            mutate(closest = purrr::map(data, closest_focal_point, focal_points = focal_points)) %>%
            unnest(data, closest) %>%
            arrange(focal_distance)

        if (verbose) {message("Closest points found.")}

    }

    ###########################################################################
    # Allocate polygons to a hexagon
    #browser()
    hexmap_allocation <- allocate(centroids = centroids,
        hex_grid = hex_grid,
        hex_size = hex_size,
        filter_dist = filter_dist,
        width = width,
        focal_points = focal_points,
        verbose = verbose,
        id = sf_id)

    if (export_shp) {
        return(list(hexmap_allocation, shp_sf))
    } else {
        return(hexmap_allocation)

    }

}
