#' The point locations of Asutralian capital cities.
#'
#' A dataset containing the longitude and latitude values of Australian
#' capital cities.
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{points}{name of cities}
#'   \item{longitude}{location of point in longitude degrees}
#'   \item{latitude}{location of point in latitude degrees}
#' }
"capital_cities"

#' The polygons of Australian Local Government Areas in 2011.
#'
#' A simple features dataset containing the polygons for all Australian LGAs in
#' 2011.
#'
#' @format A simple features data frame with 559 rows and 6 variables:
#' \describe{
#'   \item{LGA_CODE11}{code for the Local Government Area}
#'   \item{LGA_NAME11}{name of the Local Government Area}
#'   \item{STE_CODE11}{code for the state containing the Local Government Area}
#'   \item{STE_NAME11}{name of the state containing the Local Government Area}
#'   \item{AREA_SQKM}{area contained in the polygon}
#'   \item{geometry}{describes where on Earth the polygon is located}
#' }
"lga_2011"

#' A hexagon map data set of Australia, with hexagons of size 0.3 degrees.
#'
#' A dataset containing information regarding the closest capital city, and the
#' allocated hexagon grid point for each Statistical Area (Level 2) in
#' Australia.
#'
#' @format A data frame with 2192 rows and 11 variables:
#' \describe{
#'   \item{SA2_NAME11}{name of the Statistical Area (Level 2)}
#'   \item{longitude}{location of SA2 in longitude degrees}
#'   \item{latitude}{location of SA2 in latitude degrees}
#'   \item{focal_point}{name of the closest Australian capital city}
#'   \item{longitude1}{location of the closest Australian capital city in
#'    longitude degrees}
#'   \item{latitude1}{location of the closest Australian capital city in
#'    latitude degrees}
#'   \item{focal_dist}{distance in metres between the polygon centroid and closest capital city}
#'   \item{focal_angle}{angle between the closest capital city and polygon centroid}
#'   \item{hex_long}{location of the hexagon grid point in    longitude degrees}
#'   \item{hex_lat}{location of the hexagon grid point in latitude degrees}
#'   \item{hex_id}{id of the hexagon grid point}
#' }
"hexmap_aus_3"

#' The polygons of Australian Local Government Areas in 2011.
#'
#' A simple features dataset containing the polygons for all Australian LGAs in
#' 2011.
#'
#' @format A simple features data frame with 559 rows and 6 variables:
#' \describe{
#'   \item{LGA_CODE11}{code for the Local Government Area}
#'   \item{LGA_NAME11}{name of the Local Government Area}
#'   \item{STE_CODE11}{code for the state containing the Local Government Area}
#'   \item{STE_NAME11}{name of the state containing the Local Government Area}
#'   \item{AREA_SQKM}{area contained in the polygon}
#'   \item{geometry}{describes where on Earth the polygon is located}
#' }
#'
#' @name lga_2011
#' @usage lga_2011
"lga_2011"
