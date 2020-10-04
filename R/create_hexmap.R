#' Create a tessellated hexagon map from a set of polygons
#'
#' Allocates each polygon in a shape file to a grid point to create a map of
#' tessellated hexagons. The spatial relationships of areas are preserved while
#' the geographic shape of each area is lost.
#'
#' @param shp a shape file, if class is SPDF, will be converted to sf
#' @param sf_id name of a unique column that distinguishes areas
#' @param buffer_dist distance in degrees to extend beyond the geometry provided
#' @param hex_size a float value in degrees for the diameter of the hexagons
#' @param hex_filter amount of hexagons around centroid to consider
#' @param f_width the angle used to filter the grid points around a centroid
#' @param focal_points a data frame of reference locations when allocating
#' hexagons, capital cities of Australia are used in the example
#' @param order_sf_id a string name of a column to order by for allocating
#' @param export_shp export the simple features set
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return a data set containing longitude and latitude of allocated hexagon
#' points for each non null geometry passed in the shape file
#' @export
#'
#' @examples
#' 
#' data(tas_sa2)
#' data(capital_cities)
#' hexmap <- create_hexmap(
#'   shp = tas_lga,
#'   sf_id = "LGA_CODE16",
#'   focal_points = capital_cities, verbose = TRUE)
#' 
create_hexmap <- function(shp, sf_id, hex_size = NULL, buffer_dist = NULL, hex_filter = 10, f_width = 30, focal_points = NULL, order_sf_id = NULL, export_shp = FALSE, verbose = FALSE) {
  if (!is.null(shp)) {
    if ("SpatialPolygonsDataFrame" %in% class(shp)) {
      shp_sf <- sf::st_as_sf(shp)
    }
    else if ("geometry" %in% colnames(shp)) {
      shp_sf <- sf::st_as_sf(shp)
    }

    else {
      shp_sf <- shp
    }
  }
  else {
    return(message("Provide an sf object, see read_shape()."))
  }


  sf::st_agr(shp_sf) <- "constant"


  ###########################################################################
  # First make sure all levels have been dropped if not being used
  # Check to be sure column exists
  if (!(sf_id %in% colnames(shp_sf))) {
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
  bbox <- tibble::tibble(
    min = c(
      min(centroids$longitude),
      min(centroids$latitude)
    ),
    max = c(
      max(centroids$longitude),
      max(centroids$latitude)
    )
  )

  # create a buffer distance if not supplied
  if (is.null(buffer_dist)) {
    buffer_dist <- max((bbox$max[1] - bbox$min[1]), (bbox$max[2] - bbox$min[2])) * 0.3
    message(paste0("Buffer set to ", round(buffer_dist, 4), " degrees."))
  }

  # Consider a buffer distance above 5 to be a mistake
  if (buffer_dist > 100) {
    # convert metres to degrees
    buffer_dist <- buffer_dist / 111139
    message(paste0("Converted buffer distance to ", round(buffer_dist, 4), "metres"))
  }


  # if hex_size was not provided
  if (is.null(hex_size)) {
    hex_size <- (bbox$max[1] - bbox$min[1]) / (bbox$max[2] - bbox$min[2]) / 10
    message(paste0("Converted hexagon size to ", round(hex_size, 4), " degrees."))
  }


  # filter according to amount of hexagons
  if (is.null(hex_filter)) {
    hex_filter <- (hex_size) * 10
  }
  else {
    if (hex_filter < hex_size) {
      hex_filter <- (hex_size) * 10
    }
    else {
      hex_filter <- (hex_size) * hex_filter
    }
  }

  message(paste0("Filter set to ", round(hex_filter, 4), " degrees."))


  ###########################################################################
  # Create grid for hexagons
  hex_grid <- create_grid(
    centroids = centroids, hex_size = hex_size,
    buffer_dist = buffer_dist
  )

  
  ###########################################################################
  # Allocate polygons to a hexagon
  hexmap_allocation <- allocate(
    centroids = centroids,
    sf_id = sf_id,
    hex_grid = hex_grid,
    hex_size = hex_size,
    hex_filter = hex_filter,
    width = f_width,
    focal_points = focal_points,
    verbose = verbose
  )

  if (export_shp) {
    return(list(hexmap_allocation, shp_sf))
  } else {
    return(hexmap_allocation)
  }
}
