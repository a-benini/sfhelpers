test_that("test-st_bbox_list", {
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

  bb_list <- st_bbox_list(list(sf, sp, r, stars))
  expect_equal(class(bb_list), "bbox")

  expect_equal(st_crs(bb_list), st_crs(sf))

  expect_equal(st_crs(bb_list), st_crs(stars))

  num <- c(-79.0745010375977, 33, -74.5, 36.5571632385254)
  expect_true(all(near(bb_list, num, tol = 1.490116e-08)))

  expect_equal(unname(st_bbox(sf) == bb_list), c(FALSE, FALSE, FALSE, TRUE))

  bbox_A <- st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs = st_crs(nc))
  bbox_B <- st_bbox(c(xmin = -1, ymin = 2, xmax = 3, ymax = 2), crs = st_crs(nc))
  expect_equal(as.numeric(st_bbox_list(list(bbox_A, bbox_B))), c(-1, 2, 3, 4))

  expect_equal(st_bbox_list(list(sf)), st_bbox(sf))

  sf_3857 <- st_transform(sf, 3857)

  expect_error(st_bbox_list(list(sf_3857, sp, r, stars)), "arguments have different crs")

  expect_error(st_bbox_list(list(NA, sp, r, stars)))

  expect_error(st_bbox_list(list(NULL, sp, r, stars)))

  expect_error(
    st_bbox_list(list(sf, sf[NULL, ])),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )

  bbox_with_NA <- st_bbox(c(xmin = NA, ymin = 2, xmax = 3, ymax = 4), crs = st_crs(nc))
  expect_error(
    st_bbox_list(list(nc, bbox_with_NA)),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )

  # with classes from terra pkg
  rast <- rast(r)  # class SpatRaster
  vect <- vect(sf) # class SpatVector
  expect_equal(
    st_bbox_list(list(r, sf)),
    st_bbox_list(list(rast, vect))
  )
})
