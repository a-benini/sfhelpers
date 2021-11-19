test_that("test-st_rename_geometry", {
  library(sf)
  expect_error(
    st_rename_geometry(st_geometry(poly_1), "renamed_geom"),
    "the argument obj must be of the class sf"
  )

  expect_error(
    st_rename_geometry(poly_1, factor("renamed_geom")),
    "the argument geometry_name must be a single character string"
  )

  expect_error(
    st_rename_geometry(poly_1, c("A", "B")),
    "the argument geometry_name must be a single character string"
  )

  expect_equal(attr(poly_1, "sf_column"), "geometry")

  poly_1_renamed <- st_rename_geometry(poly_1, "renamed_geom")

  expect_equal(
    attr(poly_1_renamed, "sf_column"),
    setdiff(names(poly_1_renamed), names(poly_1))
  )
})
