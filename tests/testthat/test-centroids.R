test_that('centroid dimensions correct', {
  
    lga_centroids <- sugarbag::create_centroids(sugarbag::tas_lga, "lga_code_2016")
    sa2_centroids <- sugarbag::create_centroids(sugarbag::tas_sa2, "sa2_name_2016")
  
  # check projection returned correct latitude and longitudes
  # will need to be changed when extended for northings and eastings
  expect_success(expect_length(lga_centroids$longitude, 29))
  expect_success(expect_length(sa2_centroids$longitude, 99))
  
})
