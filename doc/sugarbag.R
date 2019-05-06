## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE, 
  warning = FALSE,
  fig.height = 4,
  fig.width = 7)

## ------------------------------------------------------------------------
#devtools::install_github("srkobakian/sugarbag")
library(sugarbag)
library(dplyr)
library(tidyr)
library(ggplot2)

## ------------------------------------------------------------------------
centroids <- create_centroids(tas_sa2, sf_id = "SA2_5DIG16")

## ------------------------------------------------------------------------
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)

## ------------------------------------------------------------------------
hex_allocated <- allocate(centroids = centroids,
  sf_id = "SA2_5DIG16",
  hex_grid = grid,
  hex_size = 0.2, # same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 30, verbose = TRUE) # same column used in create_centroids

## ------------------------------------------------------------------------
h1 <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2) %>%
  left_join(., tas_sa2) %>% mutate(poly_type = "hex")

p1 <- fortify_sfc(tas_sa2) %>% mutate(poly_type = "geo")

ggplot(mapping = aes(fill = SA4_NAME16)) +
  geom_polygon(data = p1, aes(x=long, lat, group = group), alpha = 0.1) +
  geom_polygon(data = h1, aes(x=long, lat, group = SA2_5DIG16))

