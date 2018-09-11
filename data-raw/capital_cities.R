#' Location of capital cities of Australia
#'
#' A table containing the names, latitudes and longitudes of Australian capital
#' cities
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{city}{name of city}
#'   \item{longitude}{longitude of city}
#'   \item{latitude}{latitude of city}
#' }
#'
capital_cities <- tibble::tibble(
    city = c("Melbourne","Canberra","Sydney","Darwin","Brisbane","Adelaide","Hobart","Perth"),
    long = c(144.9750162,149.1290262,151.1851798,130.8500386,153.0350927,138.6000048,147.2950297,115.8399987),
    lat = c(-37.82003131,-35.28302855,-33.92001097,-12.42535398,-27.45503091,-34.93498777,-42.85000853,-31.95501463))
