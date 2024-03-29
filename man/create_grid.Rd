% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_grid.R
\name{create_grid}
\alias{create_grid}
\title{Create a grid of evenly spaced points to allow hexagons to tessellate}
\usage{
create_grid(
  centroids,
  hex_size,
  buffer_dist,
  latitude = "latitude",
  longitude = "longitude",
  verbose = FALSE
)
}
\arguments{
\item{centroids}{data frame of centroids to be allocated, this should have
columns for longitude and latitude value of centroids, as}

\item{hex_size}{a float value in degrees for the diameter of the hexagons}

\item{buffer_dist}{distance to extend beyond the geometry provided}

\item{latitude}{the column name for the latitude values of the centroids}

\item{longitude}{the column name for the longitude values of the centroids}

\item{verbose}{a boolean to indicate whether to show function progress}
}
\value{
grid
}
\description{
This function takes the output from the create_centroids function, or a
set of centroids in a table with the columns latitude and longitude
}
\examples{
# Create a set of centroids for grid to overlay
centroids <- create_centroids(tas_lga, "lga_code_2016")
# Create the grid
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2, verbose = FALSE)

}
