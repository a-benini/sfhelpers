test_that("test-st_bbox_list", {
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

  bb_list <- st_bbox_list(l)
  expect_equal(class(bb_list), "bbox")

  check_crs <- sapply(l, function(x) st_crs(x) == st_crs(bb_list))
  expect_true(all(check_crs))

  expect_equal(unname(st_bbox(stars) == bb_list), c(FALSE, FALSE, TRUE, TRUE))

  bbox_A <- st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs = st_crs(nc))
  bbox_B <- st_bbox(c(xmin = -1, ymin = 2, xmax = 3, ymax = 2), crs = st_crs(nc))
  expect_equal(as.numeric(st_bbox_list(list(bbox_A, bbox_B))), c(-1, 2, 3, 4))

  expect_equal(st_bbox_list(list(sf)), st_bbox(sf))

  sf_3857 <- st_transform(sf, 3857)

  expect_error(st_bbox_list(list(sf_3857, rast, vect, stars)), "arguments have different crs")

  expect_error(st_bbox_list(list(NA, rast, vect, stars)))

  expect_error(st_bbox_list(list(NULL, rast, vect, stars)))

  expect_error(
    st_bbox_list(list(sf, sf[NULL, ])),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )

  bbox_with_NA <- st_bbox(c(xmin = NA, ymin = 2, xmax = 3, ymax = 4), crs = st_crs(nc))
  expect_error(
    st_bbox_list(list(nc, bbox_with_NA)),
    "at least one of the listed arguments has one or more missing values for its bbox object"
  )
})
