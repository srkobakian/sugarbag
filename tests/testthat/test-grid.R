test_that('grid amount correct', {
  
  lga_centroids <- sugarbag::create_centroids(sugarbag::tas_lga, "LGA_CODE16")
  lga_grid <- create_grid(lga_centroids, hex_size = 0.2, buffer_dist = 1.2, verbose = FALSE)

  
  sa2_centroids <- sugarbag::create_centroids(sugarbag::tas_sa2, "SA2_NAME16")
  sa2_grid <- create_grid(sa2_centroids, hex_size = 0.2, buffer_dist = 1.2, verbose = FALSE)
  
  # check projection returned correct latitude and longitudes
  # will need to be changed when extended for nothings and eastings
  expect_success(expect_length(lga_grid$hex_long, 451))
  expect_success(expect_length(sa2_grid$hex_long, 477))
  
})
