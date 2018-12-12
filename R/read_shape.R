#' Read in the shape file as sf object
#'
#' read_shape
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param simplify boolean to determine whether to simplify the shape file
#' using rmapshaper
#'
#' @return an sf data frame, with a column of non null geometries
#' @export
#'
#'
read_shape <- function(shp_path, simplify) {

    # Check if file or folder has been input
    extn <- tools::file_ext(shp_path)

    if (extn == ""){
        shp_path <- paste0(file.path(shp_path), "/", basename(shp_path), ".", "shp")
        extn <- tools::file_ext(shp_path)
        }

    if (!file.exists(shp_path)) {
        message("The shape file provided cannot be found")
    }    else {
        # When it is a shape file
        if (extn == "shp") {
            shp <- tryCatch(expr = sf::st_read(shp_path),
                error=function(e) print("Argument for shp could not be read as sf object."))
            # Simplify polygons to have less detail
            if (simplify) {
                shp <- rmapshaper::ms_simplify(shp, keep = 0.1)
            }
        }

        # When it is a previously imported shape file
        if  (extn == "Rda") {
            # When sa2 files were saved, they were named shp
            load(file = shp_path, verbose = TRUE)
        }
    }

    shp_polys <- shp %>% sf::st_as_sf() %>%
        dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
        sf::st_transform(shp_polys, crs = 3112, '+init=epsg:3112 +proj=longlat +ellps=GRS80'
        )

    # add message that polygons were dropped?
    if (NROW(shp_polys) != NROW(shp)) {
        message(paste0("Shape file now has ", (NROW(shp) - NROW(shp_polys)) , " less rows. The rows that were dropped were likely null geometries."))
    }

    return(shp_polys)
}
