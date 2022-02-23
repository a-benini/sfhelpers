test_that("test-st_bbox_common", {
  library(sf)
  library(sp)
  library(raster)
  library(stars)
  library(terra)
  library(dplyr)
  nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)

  sf    <- nc[4:8, ]
  sp    <- sf::as_Spatial(nc[96:100, ])
  logo  <- raster(system.file("external/rlogo.grd", package = "raster"))
  ext   <- extent(-77.5, -76, 33, 34.5)
  r     <- raster(nrows = nrow(logo), ncols = ncol(logo), ext = ext)
  r[]   <- logo[]
  stars <- st_as_stars(r) %>% st_set_crs(st_crs(sf))
  r     <- as(stars, "Raster") %>% setExtent(ext = ext + rep(1.5, 4))

  l <- list(sf, sp, r, stars)
  check_crs <- sapply(l, function(x) all.equal(st_crs(x), st_crs(l[[1]])))
  expect_true(all(check_crs))

  bb_common <- st_bbox_common(sf, sp, r, stars)
  expect_equal(class(bb_common), "bbox")

  expect_equal(st_crs(bb_common), st_crs(sf))

  expect_equal(st_crs(bb_common), st_crs(stars))

  num <- c(-79.0745010375977, 33, -74.5, 36.5571632385254)
  expect_true(all(near(bb_common, num, tol = 1.490116e-08)))

  expect_equal(unname(st_bbox(sf) == bb_common), c(FALSE, FALSE, FALSE, TRUE))

  expect_equal(st_bbox_common(sf), st_bbox(sf))

  sf_3857 <- st_transform(sf, 3857)

  expect_error(st_bbox_common(sf_3857, sp, r, stars), "arguments have different crs")

  expect_error(st_bbox_common(NA, sp, r, stars))

  expect_error(st_bbox_common(NULL, sp, r, stars))

  expect_error(
    st_bbox_list(list(nc[NULL, ], sp, r, stars)),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )

  l <- lapply(1:nrow(nc), function(x) nc[x, ])

  expect_equal(st_bbox_list(l), st_bbox(nc))

  # with classes from terra pkg
  rast <- rast(r)  # class SpatRaster
  vect <- vect(sf) # class SpatVector
  expect_equal(
    st_bbox_common(r, sf),
    st_bbox_common(rast, vect)
  )
})
