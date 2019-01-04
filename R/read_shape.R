#' Read in the shape file as sf object
#'
#' read_shape
#'
#' @param shp_path character vector location of shape file, extension .shp
#' @param simplify boolean to determine whether to simplify the shape file
#' using rmapshaper
#' @param epsg the four character string to indicate the CRS
#' @param projstring a string to indicate the projection and epsg
#'
#' @return an sf data frame, with a column of non null geometries
#' @export
#'
#'
read_shape <- function(shp_path = NULL, simplify = NULL, epsg = NULL, projstring = NULL) {

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
                shp <- rmapshaper::ms_simplify(shp, keep = 0.1)
            }
        }

        # When it is a previously imported shape file
        if  (extn == "Rda" | extn == "rda") {
            # When sa2 files were saved, they were named shp
            shp <- get(load(file = shp_path, verbose = TRUE))
        }
    }

        # Set projection and crs
        crs_info <- sf::st_crs(shp)

        if (is.null(projstring)){
            proj4string <- crs_info$proj4string
            message(paste0("Using proj4string: ", proj4string))
        }
        if (is.null(epsg)){
            epsg <- crs_info$epsg
            message(paste0("Using epsg: ", epsg))
        }


    shp_polys <- shp %>% sf::st_as_sf() %>%
        dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
        sf::st_transform(., epsg, projstring
        )

    # add message that polygons were dropped?
    if (NROW(shp_polys) != NROW(shp)) {
        message(paste0("Shape file now has ", (NROW(shp) - NROW(shp_polys)) , " less rows. The rows that were dropped were likely null geometries."))
    }

    return(shp_polys)
}

