test_that("test-st_bbox_common", {
  library(sf)
  library(stars)
  library(terra)
  nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)

  sf   <- nc[4:8, ]
  vect <- vect(nc[96:100, ])
  logo <- rast(system.file("ex/logo.tif", package = "terra"))
  rast <- rast(
    val    = as.vector(logo$red),
    nrows  = nrow(logo),
    ncols  = ncol(logo),
    extent = c(-77, -75, 33, 35),
    crs    = st_crs(sf)$wkt
  )
  stars <- st_as_stars(rast) %>% st_set_bbox(., st_bbox(.) + rep(2, 4))
  l <- list(sf, vect, rast, stars)
  check_crs <- sapply(l, function(x) st_crs(x) == st_crs(l[[1]]))
  expect_true(all(check_crs))

  bb_common <- st_bbox_common(sf, vect, rast, stars)
  expect_equal(class(bb_common), "bbox")

  check_crs <- sapply(l, function(x) st_crs(x) == st_crs(bb_common))
  expect_true(all(check_crs))

  expect_equal(unname(st_bbox(stars) == bb_common), c(FALSE, FALSE, TRUE, TRUE))

  expect_equal(st_bbox_common(sf), st_bbox(sf))

  sf_3857 <- st_transform(sf, 3857)

  expect_error(st_bbox_common(sf_3857, rast, vect, stars), "arguments have different crs")

  expect_error(st_bbox_common(NA, rast, vect, stars))

  expect_error(st_bbox_common(NULL, rast, vect, stars))

  expect_error(
    st_bbox_list(list(nc[NULL, ], rast, vect, stars)),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )

  l <- lapply(1:nrow(nc), function(x) nc[x, ])

  expect_equal(st_bbox_list(l), st_bbox(nc))

  # with classes from terra pkg
  rast <- rast(stars)  # class SpatRaster
  vect <- vect(sf) # class SpatVector
  expect_equal( # some issue with not exactly equal CRS
    st_bbox_common(stars, sf),
    st_bbox_common(rast, vect)
  )

  expect_equal(
    as.numeric(st_bbox_common(rast, sf)),
    as.numeric(st_bbox_common(stars, vect))
  )

  expect_true(
    st_crs(st_bbox_common(rast, sf)) == st_crs(st_bbox_common(stars, vect))
  )

  l <- st_linestring(cbind(0:4, 5:1))
  expect_equal(
    st_bbox_common(st_sfc(l)),
    st_bbox_common(l)
  )
})
