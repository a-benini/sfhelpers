test_that("test-st_or", {
  library(sf)

  expect_error(st_or(NA, poly_2), "the argument x must be of the class sf, sfc or sfg")
  expect_error(st_or(NULL, poly_2), "the argument x must be of the class sf, sfc or sfg")

  expect_error(st_or(poly_1, NA), "the argument y must be of the class sf, sfc or sfg")
  expect_error(st_or(poly_1, NULL), "the argument y must be of the class sf, sfc or sfg")

  sfg <- st_buffer(st_point(c(0, 0)), 1)
  sfc <- st_make_grid(sfg * 2, n = 3)
  sf  <- sfc %>% st_sf(id = seq_along(.), geometry = ., agr = "constant")
  # plot(sfc, col = "gray")
  # plot(sfg, add = TRUE, border = "red", lwd = 2)
  # st_or(sf, sfg) %>% plot()
  eq <- st_equals(
    st_or(sf, sfg),
    st_or(sfg, sf)
  )
  expect_true(all(seq_along(eq) == unlist(eq)))
  expect_equal(
    st_or(sf, sfg)[,-1], # without only attribute (id)
    st_or(sfc, sfg)
  )

  poly_1_epsg_21781 <- st_transform(poly_1, crs = 21781)
  expect_error(st_or(poly_1_epsg_21781, poly_2))
  # "sf::st_crs(x) == sf::st_crs(y) is not TRUE"

  expect_error(st_or(poly_1, poly_2, dim = 3), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = 0:3), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = NA_integer_), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = TRUE), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = matrix(1)), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = list(1)), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")
  expect_error(st_or(poly_1, poly_2, dim = factor(1)), "dim must be a single integer or vector of integers consisting of 0, 1 and/or 2")

  expect_error(st_or(poly_1, poly_2, suffix = c("same_name", "same_name")))
  # The 1st and 2nd element of the argument ‘suffix’ are both specified as "same_name". But they need to be specified differently.

  expect_error(
    st_or(poly_1, poly_2, suffix = 1),
    "the argument suffix must be a length 2 character vector"
  )

  expect_error(
    st_or(poly_1, poly_2, suffix = list("A", "B")),
    "the argument suffix must be a length 2 character vector"
  )

  expect_error(
    st_or(poly_1, poly_2, suffix.all = c(TRUE, FALSE)),
    "suffix.all must be a single logical value: TRUE or FALSE"
  )
  expect_error(
    st_or(poly_1, poly_2, suffix.all = NA),
    "suffix.all must be a single logical value: TRUE or FALSE"
  )

  expect_warning(st_or(poly_1, poly_2), "attribute variables are assumed to be spatially constant throughout all geometries")

  # avoid above warning:
  st_agr(poly_1) <- "constant"
  st_agr(poly_2) <- "constant"

  # if one x or y is sfc only attributes of the sf are returned:
  expect_equal(names(st_or(poly_1, st_geometry(poly_2))), names(poly_1))
  expect_equal(length(st_or(poly_1, st_geometry(poly_2))), length(poly_1))

  # if one x and y are both sfc only a sf with geometry column is returned:
  expect_equal(names(st_or(st_geometry(poly_1), st_geometry(poly_2))), "geometry")

  or_names       <- names(st_or(poly_1, poly_2))
  poly_names     <- c(names(poly_1), names(poly_2))
  or_names_diff  <- setdiff(or_names, poly_names)
  # "A.x", "A.y"
  poly_names_diff <- setdiff(poly_names, or_names)
  #  "A"
  expect_equal(paste0(poly_names_diff, c(".x", ".y")), or_names_diff)

  all_names_suffixed <- st_or(x = poly_1, y = poly_2, suffix.all = TRUE) %>% st_drop_geometry() %>% names()
  x_names_suffixed   <- poly_1 %>% st_drop_geometry() %>% names() %>% paste0(".x")
  y_names_suffixed   <- poly_2 %>% st_drop_geometry() %>% names() %>% paste0(".y")
  expect_equal(all_names_suffixed, c(x_names_suffixed, y_names_suffixed))

  # total overlap of input layer x by input layer y ...
  expect_equal(nrow(st_erase_robust(poly_1[1, ], poly_2)), 0) # check total overlap
  expect_equal(
    st_bbox(st_or(poly_1[1, ], poly_2)), # ... works with pkg version >= 0.0.0.9001
    st_bbox(poly_2)
  )

  # poly_1 and poly_2 have identically named geometry columns:
  expect_equal(attr(poly_1, "sf_column"), attr(poly_2, "sf_column"))
  # create a version of poly_1 with differently named geometry column:
  poly_1_renamed_geom <- st_set_geometry(poly_1, "renamed_geometry")
  expect_false(attr(poly_1_renamed_geom, "sf_column") == attr(poly_2, "sf_column"))
  # input layers x and y with differently named geometry columns works with pkg
  # version >= 0.0.0.9001 and the name the output's geometry column is inherited
  # from input layer x*:
  expect_equal(
    attr(poly_1_renamed_geom, "sf_column"),                       # input layer x
    attr(st_or(x = poly_1_renamed_geom, y = poly_2), "sf_column") # st_or(x = x, y = y)
  )
  # * behavior as sf::st_intersection()

  # create two layers with overlapping linestrings:
  ls1 <- st_linestring(cbind(c(0, 1, 1, 0), c(0:3)))
  ls2 <- st_linestring(cbind(c(2, 1, 1), c(0, 0, 3)))
  ls3 <- st_linestring(cbind(c(0, 0.5, 0.5, 0), c(0, 0, 2.5, 2)))
  A <- st_sf(id_A = 1, A = "A", geom = st_sfc(ls1), agr = "constant")
  B <- st_sf(id_B = 1:2, B = "B", geom = st_sfc(ls2, ls3), agr = "constant")

  # if both input layers consisting of linestings using the default specification ...
  # ... dim = 2 (for surfaces / (multi)polygons) will return a sf-object with zero rows
  expect_equal(
    nrow(st_or(A, B)),
    0
  )

  # to get lines returned set dim = 1:
  expect_true(
    all(
      st_or(A, B, dim = 1) %>% st_dimension() == 1
    )
  )

  # when both input layers consists of linestings, and if the default specification ...
  # ... dim = 2 (for surfaces / (multi)polygons) is used, a sf-object with zero ...
  # ... rows will be returned:
  expect_true(
    all(
      st_or(A, B, dim = c(0, 1)) %>% st_dimension() %in% c(0, 1)
    )
  )

  expect_equal(
    st_or(A, B, dim = c(0, 1)), # returns points & lines
    st_or(A, B, dim = c(0, 1, 2)) # returns points, lines (& if available surfaces)
  )
})
