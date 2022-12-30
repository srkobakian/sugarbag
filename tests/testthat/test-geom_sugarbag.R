library(ggplot2)
test_that("geom_sugarbag() works", {
  expect_s3_class(geom_sugarbag(), "ggproto")
  
  
  p <- tas_lga %>%
    ggplot(aes(fill = lga_code_2016)) 

  expect_s3_class(p + 
                    geom_sugarbag(aes(geometry = geometry)), "gg")  
  
  expect_s3_class(p + 
                    geom_sugarbag(aes(geometry = geometry),
                                  hex_size = 1,
                                  fill = "purple"), "gg")
  
  # Try with a non-Australian example
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_s3_class(
    ggplot(nc,
    aes(fill = AREA)) +
    geom_sf(alpha = 0.1) +
    geom_sugarbag(aes(geometry = geometry),
                  hex_size = 0.3),
    "gg")

})
