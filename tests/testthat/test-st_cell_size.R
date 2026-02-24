test_that("test-st_cell_size", {
  library(terra)
  library(stars)
  library(sf)

  # projected coordinates
  r_epsg_2056 <- rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:2056",
    val = 1L
  )

  # geographic coordinates
  r_epsg_4326 <- rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:4326",
    val = 1L
  )

  # no CRS
  r_epsg_NA <- rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = NA,
    val = 1L
  )

  # check error messages
  expect_error(
    st_cell_size(NA),
    "the argument x must be of the class SpatRaster, stars or stars_proxy"
  )

  expect_error(
    st_cell_size(list(r_epsg_2056)),
    "the argument x must be of the class SpatRaster, stars or stars_proxy"
  )

  expect_error(
    st_cell_size(r_epsg_2056, warn = NULL),
    "argument warn must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_cell_size(r_epsg_2056, drop_units = NULL),
    "argument drop_units must be a single logical value: TRUE or FALSE"
  )

  # check with geographic coordinates
  expect_warning(
    st_cell_size(r_epsg_4326),
    "x has geographic coordinates, thus raster cell size is returned without units"
  )

  expect_warning(
    st_cell_size(r_epsg_4326, drop_units = TRUE),
    "x has geographic coordinates"
  )

  expect_no_warning(st_cell_size(r_epsg_4326, warn = FALSE))

  expect_equal(
    st_cell_size(r_epsg_4326, warn = FALSE, drop_units = TRUE),
    prod(res(r_epsg_4326))
  )

  # check with no CRS
  expect_warning(
    st_cell_size(r_epsg_NA),
    "x has no CRS, thus raster cell size is returned without units"
  )

  expect_warning(st_cell_size(r_epsg_NA, drop_units = TRUE), "x has no CRS")

  expect_no_warning(st_cell_size(r_epsg_NA, warn = FALSE))

  expect_equal(
    st_cell_size(r_epsg_NA, warn = FALSE, drop_units = TRUE),
    prod(res(r_epsg_NA))
  )

  # check with projected coordinates
  cell_size        <- prod(res(r_epsg_2056))
  units(cell_size) <- paste0(st_crs(r_epsg_2056)$units, "^2")
  expect_equal(st_cell_size(r_epsg_2056), cell_size)

  expect_equal(
    st_cell_size(r_epsg_2056, drop_units = TRUE),
    as.numeric(cell_size)
  )

  expect_no_warning(st_cell_size(r_epsg_2056))

  # check with stars obj.
  s_epsg_2056 <- st_as_stars(r_epsg_2056)
  expect_equal(
    st_cell_size(r_epsg_2056),
    st_cell_size(s_epsg_2056)
  )

  expect_equal(
    st_cell_size(s_epsg_2056, drop_units = TRUE),
    prod(st_res(s_epsg_2056))
  )

  # check with stars_proxy obj.
  path_tmp_tif <- file.path(tempdir(), "tmp.tif")
  writeRaster(r_epsg_2056, path_tmp_tif, overwrite = file.exists(path_tmp_tif))
  expect_equal(
    st_cell_size(r_epsg_2056),
    st_cell_size(read_stars(path_tmp_tif, proxy = TRUE))
  )
})
