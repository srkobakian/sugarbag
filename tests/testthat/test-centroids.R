test_that('centroid dimensions correct', {
  
  expect_warning(lga_centroids <- sugarbag::create_centroids(sugarbag::tas_lga, "LGA_CODE16"))
  expect_warning(sa2_centroids <- sugarbag::create_centroids(sugarbag::tas_sa2, "SA2_NAME16"))
  
  # check projection returned correct latitude and longitudes
  # will need to be changed when extended for nothings and eastings
  expect_success(expect_length(lga_centroids$longitude, 29))
  expect_success(expect_length(sa2_centroids$longitude, 99))
  
})
