# using create_hexmap only for VIC

library(tidyverse)
library(purrr)
library(sf)
library(viridis)
library(ggthemes)
#library(sugaRbag)
library(plotly)
library(geofacet)


load(system.file("data","sa2_2011.Rda", package = "sugaRbag"))
load("data/capital_cities.Rda")

shp_sfc <- sf::st_as_sf(shp)
qld_sfc <- shp_sfc %>% dplyr::filter(STE_NAME11=="Queensland")

hexmap <- create_hexmap(shp = qld_sfc,
    sf_id = "SA2_NAME11",
    hex_size = 0.3,
    export_shp = FALSE,
    focal_points = capital_cities[5,], verbose = TRUE)

# join to geometry data
hexmap_qld_3 <- left_join(hexmap, qld_sfc)

# converting to fortified tibble
fortified_qld_3<- sfc_to_tibble(hexmap_qld_3)

# Facet by SA4
load("data/qld_grid.rda")
ggplot(qld_sfc) +
    geom_sf(aes(fill = SA4_NAME11, label = SA2_NAME11)) +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, grid = qld_grid) +
    guides(fill = FALSE)


ggplot(data=hexmap_qld_3) +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
        label = SA2_NAME11), position = "identity", stat = "identity") +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), colour = "grey", fill = NA) +
    scale_fill_viridis_d() +
    facet_geo(~SA4_NAME11, scales = "free", grid = qld_grid) +
    guides(fill = FALSE) +
    theme(strip.text.x = element_text(size = 5),
        strip.background = element_rect(fill = NA, size = 1),
        axis.text = element_blank(),
        aspect.ratio = 1)

# Full Qld map
ggplot(data=hexmap_qld_3) +
    geom_hex(aes(x = hex_long, y = hex_lat, fill=SA4_NAME11,
        label = SA2_NAME11), position = "identity", stat = "identity") +
    geom_polygon(aes(x=long,y=lat,order=order,group=group), colour = "grey", fill = NA) +
    scale_fill_viridis_d() +
    guides(fill = FALSE) +
    theme(aspect.ratio = 1)

