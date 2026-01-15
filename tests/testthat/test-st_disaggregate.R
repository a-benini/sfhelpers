test_that("test-st_disaggregate", {
  library(sf)

  expect_error(
    st_disaggregate(c(0, 0)),
    "the argument x must be of the class sf, sfc or sfg"
  )

  expect_error(
    st_disaggregate(NULL),
    "the argument x must be of the class sf, sfc or sfg"
  )

  expect_error(
    st_disaggregate(list()),
    "the argument x must be of the class sf, sfc or sfg"
  )

  pt1 <- st_point(c(0, 0))
  pt2 <- st_point(c(0, 1))
  pt3 <- st_point(c(1, 1))
  mpt <- st_multipoint(c(pt2, pt3))
  expect_equal(
    st_disaggregate(st_sfc(pt1, mpt)),
    st_sfc(pt1, pt2, pt3)
  )

  expect_error(
    st_disaggregate(mpt, only_geometrycollection = "false"),
    "only_geometrycollection must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_disaggregate(mpt, only_geometrycollection = c(TRUE, FALSE)),
    "only_geometrycollection must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_disaggregate(mpt, keep_empty = "false"),
    "keep_empty must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_disaggregate(mpt, keep_empty = c(TRUE, FALSE)),
    "keep_empty must be a single logical value: TRUE or FALSE"
  )

  expect_equal(
    st_disaggregate(pt1),
    st_sfc(pt1)
  )

  gc_empty <- st_geometrycollection(list())

  expect_equal(
    length(suppressWarnings((st_disaggregate(gc_empty)))),
    0
  )
  expect_equal(
    suppressWarnings(st_disaggregate(gc_empty, keep_empty = TRUE)),
    st_sfc(gc_empty)
  )

  expect_equal(
    st_disaggregate(st_sfc(pt1, mpt), only_geometrycollection = TRUE),
    st_sfc(pt1, mpt)
  )

  expect_equal(
    st_disaggregate(st_sfc(pt1, mpt, gc_empty), only_geometrycollection = TRUE),
    st_sfc(pt1, mpt)
  )

  expect_equal(
    st_disaggregate(st_sfc(pt1, mpt, gc_empty), only_geometrycollection = TRUE, keep_empty = TRUE),
    st_sfc(pt1, mpt, gc_empty)
  )

  expect_equal(
    st_disaggregate(st_sfc(pt1, mpt, gc_empty), keep_empty = TRUE),
    st_sfc(pt1, pt2, pt3, gc_empty)
  )

  gc <- st_geometrycollection(list(pt1, mpt))
  expect_equal(
    st_disaggregate(gc),
    st_sfc(pt2, pt3, pt1)
  )

  expect_equal(
    st_disaggregate(st_sfc(gc, gc_empty)),
    st_sfc(pt2, pt3, pt1)
  )

  expect_equal(
    st_disaggregate(st_sfc(gc_empty, gc, gc_empty), keep_empty = TRUE),
    st_sfc(gc_empty, pt2, pt3, pt1, gc_empty)
  )

  sf_mpt <- st_sf(id = 1, a = "A", b = "B", c = "C", geom = st_sfc(mpt))
  st_agr(sf_mpt) <- c("constant", "aggregate", "identity", NA_agr_)

  expect_error(
    st_disaggregate(sf_mpt, warn = "true"),
    "warn must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_disaggregate(sf_mpt, warn = c(TRUE, FALSE)),
    "warn must be a single logical value: TRUE or FALSE"
  )

  expect_equal(
    all.equal(
      sf::st_agr(st_disaggregate(sf_mpt, warn = FALSE)),
      st_agr(sf_mpt)
    ),
    "1 string mismatch"
  )

  expect_equal(
    sf::st_agr(st_disaggregate(sf_mpt, only_geometrycollection = TRUE, warn = FALSE)),
    st_agr(sf_mpt)
  )

  expect_warning(
    st_disaggregate(sf_mpt),
    "repeating attributes for all sub-geometries for which they may not be constant"
  )

  expect_warning(
    st_cast(sf_mpt, "POINT"),
    "repeating attributes for all sub-geometries for which they may not be constant"
  )

  expect_equal(
    suppressWarnings(st_cast(sf_mpt, "POINT")),
    suppressWarnings(st_disaggregate(sf_mpt))
  )

  expect_equal(
    st_cast(sf_mpt, "POINT", warn = FALSE),
    st_disaggregate(sf_mpt, warn = FALSE)
  )

  st_agr(sf_mpt) <- "constant"
  expect_equal(
    st_cast(sf_mpt, "POINT"),
    st_disaggregate(sf_mpt)
  )

  sf_pt <- st_sf(id = 1:3, a = "A", b = "B", c = "C", geom = st_sfc(pt1, pt2, pt3))
  st_agr(sf_pt) <- c("constant", "aggregate", "identity", NA_agr_)

  expect_equal(
    st_agr(st_disaggregate(sf_pt, warn = FALSE)),
    st_agr(sf_pt)
  )

  pl1 <- st_point(c(0, 0)) %>% st_buffer(1)
  pl2 <- st_point(c(2, 1)) %>% st_buffer(0.5)
  pl3 <- st_point(c(5, 1)) %>% st_buffer(2)
  pl4 <- st_point(c(0, 4)) %>% st_buffer(1.5)
  pl5 <- st_point(c(0, 4.5)) %>% st_buffer(0.6)
  pl4 <- st_erase_robust(pl4, pl5)
  mpl <- st_multipolygon(list(pl3, pl4))
  sfc <- st_sfc(pl1, pl2, mpl)

  eq <- st_equals(
    st_disaggregate(sfc),
    st_cast(sfc, "MULTIPOLYGON") %>% st_cast("POLYGON")
  )

  expect_equal(seq_along(eq), unlist(eq))

  sf  <- st_sf(id = seq_along(sfc), a = "A", b = "B", c = "C", geom = sfc)
  st_agr(sf) <- c("constant", "aggregate", "identity", NA_agr_)
  rownames(sf) <- LETTERS[1:3]

  expect_equal(
    st_disaggregate(sf, warn = FALSE),
    st_cast(sf, "MULTIPOLYGON") %>% st_cast("POLYGON", warn = FALSE)
  )

  ls1        <- st_cast(pl1, "LINESTRING")
  gc         <- st_geometrycollection(list(ls1, pl2, mpl))
  pl_disagg  <- st_disaggregate(gc) %>% .[st_dimension(.) == 2]
  pl_ex_cast <- st_collection_extract(gc, "POLYGON") %>% st_cast("POLYGON")
  eq         <- st_equals(pl_disagg, pl_ex_cast)

  expect_true(
    all(seq_along(eq) %in% unlist(eq))
  )

  xyzm_1 <- st_point(rep(0, 4))
  xyzm_2 <- st_point(1:4)
  xyzm_3 <- st_point(rep(NA_integer_, 4), dim = "XYZM")
  xyzm_gc <- st_geometrycollection(list(xyzm_1, xyzm_2, xyzm_3))

  expect_equal(
    st_disaggregate(xyzm_gc),
    st_sfc(xyzm_1, xyzm_2, xyzm_3)
  )

  xyzm_sfc <- st_sfc(st_multipoint(c(xyzm_1, xyzm_2)), xyzm_3)
  # due to changes in sf-package version 1.0-13 this test doesn't work any more ...
  expect_equal(
    st_disaggregate(xyzm_sfc),
    st_sfc(xyzm_1, xyzm_2)
  )
  # ... because now an error associated with sf::cast() is returned
  # "Error: GEOS does not support XYM or XYZM geometries; use st_zm() to drop M"
  try_1 <- try(st_disaggregate(xyzm_sfc), silent = TRUE) %>% as.character()
  try_2 <- try(st_cast(xyzm_sfc, "MULTIPOINT"), silent = TRUE) %>% as.character()
  # expect_equal(try_1, try_2)

  # the M-dimension is clearly the issue fore if it is no there everything works fine
  xyz_1 <- st_point(rep(0, 3))
  xyz_2 <- st_point(1:3)
  xyz_3 <- st_point(rep(NA_integer_, 3), dim = "XYZ")

  xyz_sfc <- st_sfc(st_multipoint(c(xyz_1, xyz_2)), xyz_3)
  expect_equal(
    st_disaggregate(xyz_sfc),
    st_sfc(xyz_1, xyz_2)
  )
})
