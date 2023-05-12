test_that("test-st_bbox_common", {
  library(sf)
  library(sp)
  library(raster)
  library(stars)
  library(terra)
  nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)

  sf    <- nc[4:8, ]
  sp    <- sf::as_Spatial(nc[96:100, ])
  vect  <- vect(nc[53, ])
  logo  <- raster(system.file("external/rlogo.grd", package = "raster"))
  rast  <- rast(vals = logo[], nrow = nrow(logo), ncol = ncol(logo), extent = c(-77.5, -76, 33, 34.5), crs = st_crs(sf)$wkt)
  stars <- st_as_stars(rast) %>% st_set_bbox(., st_bbox(.) + rep(c(1.5, 0), 2)) %>% st_flip()
  r     <- raster(rast) %>% setExtent(., extent(.) + rep(1.5, 4))

  l <- list(sf, sp, vect, rast, stars, r)
  check_crs <- sapply(l, function(x) st_crs(x) == st_crs(l[[1]]))
  expect_true(all(check_crs))

  bb_common <- st_bbox_common(sf, sp, vect, rast, stars, r)
  expect_equal(class(bb_common), "bbox")

  check_crs <- sapply(l, function(x) st_crs(x) == st_crs(bb_common))
  expect_true(all(check_crs))

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
  # expect_equal( # some issue with not exactly equal CRS
  #   st_bbox_common(r, sf),
  #   st_bbox_common(rast, vect)
  # )

  expect_equal(
    as.numeric(st_bbox_common(r, sf)),
    as.numeric(st_bbox_common(rast, vect))
  )

  expect_true(
    st_crs(st_bbox_common(r, sf)) == st_crs(st_bbox_common(rast, vect))
  )

  l <- st_linestring(cbind(0:4, 5:1))
  expect_equal(
    st_bbox_common(st_sfc(l)),
    st_bbox_common(l)
  )
})
