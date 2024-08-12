test_that("test-geos_stats", {
  library(sf)
  library(lwgeom)
  library(units)

  expect_error(st_geometry_summary(NA), "the argument x must be of the class sf, sfc or sfg")
  expect_error(st_area_sum(NA), "the argument x must be of the class sf, sfc or sfg")
  expect_error(st_length_sum(NULL), "the argument x must be of the class sf, sfc or sfg")
  expect_error(st_perimeter_sum("XXXX"), "the argument x must be of the class sf, sfc or sfg")
  expect_error(st_perimeter_2d_sum(1:3), "the argument x must be of the class sf, sfc or sfg")

  # some demo data
  mat      <- rbind(0:3, log(4:1))*1000 + rep(c(26, 12), 4)*10^5
  points   <- st_sfc(lapply(data.frame(mat), st_point)) %>% st_set_crs(2056)
  polygons <- points %>% st_buffer(., seq_along(.)*150)

  mix <- c(points, polygons)

  expect_equal(
    st_geometry_summary(mix),
    st_geometry_type(mix) %>% summary() %>% .[. > 0]
  )

  expect_equal(
    st_perimeter_sum(st_union(polygons)[[1]]),
    # sfg: multi-geometry having 4 sub-geometries
    set_units(st_perimeter_sum(polygons), NULL)
    # sfc: 4 geometries
    )

  expect_equal(
    st_geometry_summary(mix),
    st_geometry_summary(st_sf(mix))
  )

  expect_equal(
    st_area_sum(mix),
    st_area_sum(polygons)
  )

  expect_equal(
    st_perimeter_sum(mix),
    st_length_sum(st_cast(polygons, "LINESTRING"))
  )

  expect_equal(
    st_perimeter_sum(mix),
    st_perimeter_2d_sum(mix)
  )

  expect_equal(
    units(st_area_sum(polygons, "km^2")),
    units(st_area_sum(polygons, "km2"))
  )

  expect_equal(
    st_area_sum(polygons, "ha"),
    st_area_sum(polygons, make_units(ha))
  )

  expect_equal(
    st_area_sum(polygons, "cm^2"),
    st_area_sum(polygons, as_units("cm")^2)
  )

  expect_true(
    st_perimeter_sum(polygons, "km", 0) == st_perimeter_sum(polygons, "cm", -5)
  )

  expect_true(
    st_area_sum(polygons, "km^2", 0) == st_area_sum(polygons, "ha", -2)
  )

  nc   <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
  expect_true(st_is_longlat(nc))

  expect_error(
    st_perimeter_2d_sum(st_geometry(nc))
  )
})
