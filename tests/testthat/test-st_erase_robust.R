test_that("test-st_erase_robust", {
  library(sf)
  st_agr(poly_1) <- "constant"
  st_agr(poly_2) <- "constant"

  st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
  if (packageVersion("sf") < "1.0.1") {
    expect_error(st_erase(poly_1, poly_2))
  }

  expect_equal(nrow(st_erase_robust(poly_1, poly_2)), 3)
  expect_equal(nrow(st_erase_robust(poly_2, poly_1)), 3)

  expect_equal(lengths(st_intersects(poly_1, poly_2)), c(4, 2, 2, 1))
  # at least 1 overlap for each polygon of x, with those of y
  expect_equal(lengths(st_intersects(poly_1[4, ], poly_2[1:3, ])), 0)
  # no overlap of the 4th polygon of x, with polygons 1 to 3 of y

  expect_true(all(st_erase_robust(poly_1[4, ], poly_2[1:3, ]) == poly_1[4, ]))
  # if there's no overlap erasing doesn't change anything

  expect_error(st_erase_robust(poly_1, NULL))
  expect_error(st_erase_robust(NULL, poly_2))
  expect_error(st_erase_robust(poly_1, NA))
  expect_error(st_erase_robust(NA, poly_2))

  p1 <- st_point(c(0, 0))
  p2 <- st_point(c(1, 0))
  p3 <- st_point(c(1, 1))
  p4 <- st_point(c(0, 1))

  sfg_1 <- st_polygon(list(c(p1, p2, p3, p4, p1)))
  sfg_2 <- sfg_1 + c(0.5, 0.5)

  sfc_1 <- st_sfc(sfg_1)
  sfc_2 <- st_sfc(sfg_2)

  # return of st_erase_robust() has the same class(es) as argument x:
  expect_equal(class(st_erase_robust(x = sfg_1, y = sfg_2)), class(sfg_1))
  expect_equal(class(st_erase_robust(x = sfg_1, y = sfc_2)), class(sfg_1))
  expect_equal(class(st_erase_robust(x = sfc_1, y = sfc_2)), class(sfc_1))
  expect_equal(class(st_erase_robust(x = sfc_1, y = sfg_2)), class(sfc_1))

  expect_true(is.na(st_crs(sfg_1)))
  # st_crs(sfg_1) is NA!, note: st_crs() / st_set_crs() can't be applied to objects of the class "sfg"

  expect_true(is.na(st_crs(sfc_1)))
  # st_crs(sfc_1) is NA!

  expect_false(is.na(st_crs(poly_1)))
  # st_crs(poly_1) is not NA!

  # st_erase_robust() only works if st_crs(x) == st_crs(y)
  expect_error(st_erase_robust(poly_1, sfg_1))
  expect_error(st_erase_robust(poly_1, sfc_1))
})

