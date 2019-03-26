#' The point locations of Australian capital cities.
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


#' The polygons of Tasmanian Statistical Areas in 2016.
#'
#' A simple features dataset containing the polygons for all Tasmanian SA2s in
#' 2016.
#'
#' @format A simple features data frame with 99 rows and 15 variables:
#' \describe{
#'   \item{SA2_MAIN16}{complete code of the Statistical Area}
#'   \item{SA2_5DIG16}{simple code for the Statistical Area}
#'   \item{SA2_NAME16}{name of the Statistical Area}
#'   \item{SA3_CODE16}{code for the SA3 containing the Statistical Area}
#'   \item{SA3_NAME16}{name of the SA3 containing the Statistical Area}
#'   \item{SA4_CODE16}{code for the SA4 containing the Statistical Area}
#'   \item{SA4_NAME16}{name of the SA4 containing the Statistical Area}
#'   \item{GCC_CODE16}{code for the Greater Capital City region containing
#'    the Statistical Area}
#'   \item{GCC_NAME16}{name of the Greater Capital City region containing
#'   the Statistical Area}
#'   \item{STE_CODE16}{code for the state containing the Statistical Area}
#'   \item{STE_NAME16}{name of the state containing the Statistical Area}
#'   \item{AREASQKM16}{area contained in the polygon}
#'   \item{id}{distinguishes SA2 regions}
#'   \item{population}{amount of people living within the region}
#'   \item{SA2_CODE16}{code of the Statistical Area}
#' }
#'
#' @name tas_sa2
#' @usage tas_sa2
"tas_sa2"

#' The polygons of Tasmanian Local Government Areas in 2016.
#'
#' A simple features dataset containing the polygons for all Australian LGAs in
#' 2016.
#'
#' @format A simple features data frame with 39 rows and 6 variables:
#' \describe{
#'   \item{LGA_CODE16}{code for the Local Government Area}
#'   \item{LGA_NAME16}{name of the Local Government Area}
#'   \item{STE_CODE16}{code for the state containing the Local Government Area}
#'   \item{STE_NAME16}{name of the state containing the Local Government Area}
#'   \item{AREA_SQKM}{area contained in the polygon}
#'   \item{geometry}{describes where on Earth the polygon is located}
#' }
"tas_lga"


#' The amount of homeless people in each Statistical Area at Level 2 in 2016.
#'
#' A data frame of the Statistical Area at Level 2 names and amount of homeless
#'
#' @format A data frame with 545 rows and 2 variables:
#' \describe{
#'   \item{homeless}{amount of homeless people}
#'   \item{SA2_NAME16}{name of the Statistical Area at Level 2}
#' }
"homeless"
