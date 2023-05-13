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
  # ----------------------------------------------------------------------------
  # argument check_overlap
  expect_error(
    st_erase_robust(poly_1, poly_2, check_overlap = c(TRUE, FALSE)),
    "check_overlap must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_erase_robust(poly_1, poly_2, check_overlap = NA),
    "check_overlap must be a single logical value: TRUE or FALSE"
  )

  mpt  <- st_multipoint(c(p1, p2, p3, p4))
  pl1  <- st_cast(mpt, "POLYGON")
  pl2  <- pl1 * 3
  pl1  <- pl1 + 1.5
  pl3  <- st_buffer(p3, 0.25)
  sfc1 <- st_make_grid(pl2, n = 3)
  sfc2 <- st_sfc(pl1, pl3)

  expect_equal(
    st_erase_robust(pl1, pl2, check_overlap = TRUE), # pl1 is totally cover by pl2 (both of class sfg)
    st_geometrycollection(list())
  )

  eq <- st_equals(
    st_erase_robust(sfc1, sfc2),
    st_erase_robust(sfc1, sfc2, check_overlap = TRUE)
  )
  expect_true(all(seq_along(eq) == sort(unlist(eq))))

  # ----------------------------------------------------------------------------
  # issues of sfhelpers version 0.0.0.9000 with st_erase_robust() /
  # sf::st_combine() / st::st_make_valid() / sf::sf_use_s2() fixed for
  # version >= 0.0.0.9001:
  grid_n3 <- st_make_grid(poly_2, n = 3)

  library(tmap)
  tmap_mode("plot")
  tm_shape(poly_1[1, ], bbox = grid_n3) + tm_polygons() +
  tm_shape(grid_n3) + tm_borders(col = "red")

  # total erase by y works, if it includes surfaces touched on all side by other
  # surfaces:
  expected_total_erase <- st_erase_robust(poly_1[1, ], grid_n3)
  expect_true(nrow(expected_total_erase) == 0)

  nc         <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
  st_agr(nc) <- "constant"
  expect_true(st_is_longlat(nc))
  ext        <- st_bbox(nc) + rep(c(-0.1, 0.1), each = 2)
  grid       <- st_make_grid(ext) %>% st_sf(id = seq_along(.), geom = ., agr = "constant")

  sf_use_s2(TRUE)
  erased_robust_s2_true <- st_erase_robust(grid, nc) # doesn't throw an error any more!
  # plot(erased_robust_s2_true) # erased as expected!
  area_ratio <- sum(st_area(erased_robust_s2_true)) / (sum(st_area(grid)) - sum(st_area(nc)))
  expect_equal(as.numeric(round(area_ratio, 6)), 1) # erase by area close to 1
  # reminder for common issue of st_erase() & st_erase_robust() (sfhelpers version 0.0.0.9000):
  # st_erase(grid, nc)
  expect_error(st_erase(grid, nc))
  # st_union(st_combine(nc)) # cause of error!
  expect_error(st_union(st_combine(nc)))
  # st_combine(nc) # works!
  expect_false(st_is_valid(st_combine(nc))) # but returns invalid geometry (but this isn't causing a stop/error)

  sf_use_s2(FALSE)
  erased_robust_s2_false <- st_erase_robust(grid, nc)
  # plot(erased_robust_s2_true) # erased as expected!
  area_ratio <- sum(st_area(erased_robust_s2_false)) / (sum(st_area(grid)) - sum(st_area(nc)))
  expect_equal(as.numeric(round(area_ratio, 6)), 0.999985) # erase by area close to 1
  # if  sf_use_s2() == FALSE st_erase_robust() gets the same result as st_erase():
  erased_s2_false <- st_erase(grid, nc)
  eq = st_equals(erased_robust_s2_false, erased_s2_false)
  # enable:
  # rhub::check(platform = "ubuntu-gcc-release")
  # rhub::check(platform = "fedora-clang-devel")
  skip_on_os("linux")
  expect_equal(
    seq_along(eq),
    unlist(eq)
  )
})
