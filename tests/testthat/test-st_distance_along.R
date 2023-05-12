test_that("test-st_distance_along", {
  library(sf)
  library(units)

  expect_error(st_distance_along(NULL))

  expect_equal(
    st_distance_along(st_geometrycollection(list())),
    NA_real_
  )

  expect_equal(
    st_distance_along(st_point(c(100, 100))) %>% unname(),
    0
  )

  pts <- st_as_sf(data.frame(x = c(0, 3, 3, 10), y = c(0, 0, -3, -3)), coords = c("x", "y"))

  expect_equal(
    st_distance_along(pts) %>% unname(),
    c(0, 3, 6, 13)
  )

  expect_equal(
    st_distance_along(pts[1:5, ]) %>% unname(),
    c(0, 3, 6, 13, NA)
  )

  expect_equal(
    st_distance_along(pts[c(1, 2, 5, 3, 4), ]) %>% unname(),
    c(0, 3, rep(NA, 3))
  )

  expect_equal(
    class(st_distance_along(pts)),
    "numeric"
  )

  pts_2056 <- st_set_crs(pts, 2056)
  expect_equal(
    class(st_distance_along(pts_2056)),
    "units"
  )

  expect_equal(
    st_distance_along(pts_2056),
    st_distance_along(pts) * as_units(st_crs(pts_2056)$units)
  )

})
