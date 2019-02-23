#' Read in the shape file as sf object
#'
#' read_shape
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param simplify a boolean to decide whether to simplify the shape file
#' using rmapshaper
#' @param keep ratio of points to keep
#'
#' @return an sf data frame, with a column of non null geometries
#' @export
#'
#' @examples
#' \dontrun{
#' # Download resource from sugarbag site
#' # https://srkobakian.github.io/sugarbag/articles/abs-data.html
#' # Find the location of extracted shape data
#' shape <- read_shape(shp_path = file.choose())
#' }
#'
read_shape <- function(shp_path, simplify = TRUE, keep = 0.1) {

    # Check if file or folder has been input
    extn <- tools::file_ext(shp_path)

    if (extn == ""){
        shp_path <- paste0(file.path(shp_path), "/", basename(shp_path), ".", "shp")
        extn <- tools::file_ext(shp_path)
    }

    if (!file.exists(shp_path)) {
        message("The shape file provided cannot be found")
    } else {
        # When it is a shape file
        if (extn == "shp") {
            shp <- tryCatch(expr = sf::st_read(shp_path),
                error=function(e) print("Argument for shp could not be read as sf object."))
            # Simplify polygons to have less detail
            if (simplify) {
                if (!is.null(keep)) {
                    shp <- rmapshaper::ms_simplify(shp, keep = keep)
                } else {
                    shp <- rmapshaper::ms_simplify(shp, keep = 0.5)
                }
            }

        }

        # When it is a previously imported shape file
        if  (extn == "Rda" | extn == "rda") {
            shp <- get(load(file = shp_path, verbose = TRUE))
        }
    }


    shp_polys <- shp %>% sf::st_as_sf() %>%
        dplyr::filter(!sf::st_is_empty(sf::st_geometry(.)))

    # add message that polygons were dropped?
    if (NROW(shp_polys) != NROW(shp)) {
        message(paste0("Shape file now has ", (NROW(shp) - NROW(shp_polys)) , " less rows. The rows that were dropped were likely null geometries."))
    }

    return(shp_polys)
}
