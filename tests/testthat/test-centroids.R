test_that('centroid dimensions correct', {
  expect_equal(expect_warning(
    ncol(
    sugarbag::create_centroids(sugarbag::tas_lga, "LGA_CODE16")
    ), "attributes are constant"), 3)
  
  expect_equal(expect_warning(nrow(sugarbag::create_centroids(sugarbag::tas_lga, "LGA_CODE16")), "correct centroids for longitude"), 29)
})
