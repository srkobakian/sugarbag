context("test-grid")

test_that("grid creation", {
    bbox <- tibble::tibble(min = c(96.81695, -43.65856),
                   max = c(159.105425, -9.219937))

    new_grid <- create_grid(bbox = bbox, radius = "auto", expand_grid = 0.1)

    expect_equal(NROW(new_grid), 26910)

    bbox <- tibble::tibble(min = c(96.81695, -43.65856),
        max = c(159.105425, -9.219937))

    new_grid <- create_grid(bbox = bbox, radius = 0.5, expand_grid = 0.1)

    expect_equal(NROW(new_grid), 14100)
})
