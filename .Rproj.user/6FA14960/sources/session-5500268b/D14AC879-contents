#' Read in the shape file as sf object
#'
#' read_shape
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param simplify a boolean to decide whether to simplify the shape file
#' using rmapshaper, keeping all shapes.
#' @param keep ratio of points to keep
#'
#' @return an sf data frame, with a column of non null geometries
#' @export
#'
#' @examples
#' \donttest{
#' # Example of how a shape file is read
#' if (interactive()) {
#' shape <- read_shape(shp_path = file.choose())
#' }
#' }
#' 
read_shape <- function(shp_path, simplify = TRUE, keep = 0.1) {

  # Check if file or folder has been input
  extn <- tools::file_ext(shp_path)

  if (extn == "") {
    shp_path <- paste0(file.path(shp_path), "/", basename(shp_path), ".", "shp")
    extn <- tools::file_ext(shp_path)
  }

  if (!file.exists(shp_path)) {
    message("The shape file provided cannot be found")
  } else {
    # When it is a shape file
    if (extn == "shp") {
      shp <- tryCatch(
        expr = sf::st_read(shp_path),
        error = function(e) message("Argument for shp could not be read as sf object.")
      )
      shp_obs <- nrow(shp)
      # Simplify polygons to have less detail
      if (simplify) {
        if (!is.null(keep)) {
          shp <- rmapshaper::ms_simplify(shp, 
            keep = keep, keep_shapes = TRUE)
          } else {
          shp <- rmapshaper::ms_simplify(shp, 
            keep = 0.5, keep_shapes = TRUE)
        }
      }
    }

    # When it is a previously imported shape file
    if (extn == "Rda" | extn == "rda") {
      shp <- get(load(file = shp_path, verbose = TRUE))
    }
  }
  
  shp_polys <- shp %>%
    sf::st_as_sf() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.)))

  if (NROW(shp_obs) - NROW(shp_polys) > 0) {
  message(paste0("Shape file now has ", (NROW(shp_obs) - NROW(shp_polys)), " less rows. The rows that were dropped during simplifying."))
  }

  return(shp_polys)
}
