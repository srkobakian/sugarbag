# using create_hexmap only for VIC


library(tidyverse)
library(purrr)
library(sf)
library(viridis)
library(ggthemes)
library(sugaRbag)
library(plotly)
library(geofacet)


load(system.file("data","sa2_2011.Rda", package = "sugaRbag"))
load("data/capital_cities.Rda")

sf::st_as_sf(shp) ->shp_sfc
shp_sfc %>% dplyr::filter(STE_NAME11=="Queensland")->qld_sfc

hexmap <- create_hexmap(shp = qld_sfc,
    sf_id = "SA2_NAME11",
    buffer_dist = NULL,
    filter_dist = 500,
    hex_size = 1,
    export_shp = FALSE,
    focal_points = capital_cities, verbose = TRUE)


# for VIC
hexmap_df <- left_join(vic_sf, hexmap_allocation, by = c("SA2_NAME11"))

# converting to fortified tibble

hexmap2 <- sfc_to_tibble(hexmap_df)

# Facet by SA4
ggplot(qld_sfc) +
    geom_sf(aes(fill = SA4_NAME11, label = SA2_NAME11)) +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, grid = facet_grid) +
    guides(fill = FALSE)


ggplot(data=hexmap2) +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
        label = SA2_NAME11), position = "identity", stat = "identity") +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), colour = "grey", fill = NA) +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, scales = "free", grid = qld_grid) +
    guides(fill = FALSE) +
    theme_minimal()

# Full Vic map
ggplot(data=hexmap2) +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
        label = SA2_NAME11), position = "identity", stat = "identity") +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), colour = "grey", fill = NA) +
    scale_fill_viridis_d() +
    guides(fill = FALSE) +
    theme_minimal()

