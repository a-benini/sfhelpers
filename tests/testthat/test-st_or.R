test_that("test-st_or", {
  library(sf)

  expect_error(st_or(NA, poly_2))
  expect_error(st_or(NULL, poly_2))
  # the argument ‘x’ must be of the class “sf” or “sfc”

  expect_error(st_or(poly_1, NA))
  expect_error(st_or(poly_1, NULL))
  # the argument ‘y’ must be of the class “sf” or “sfc”

  poly_1_epsg_21781 <- st_transform(poly_1, crs = 21781)
  expect_error(st_or(poly_1_epsg_21781, poly_2))
  # "sf::st_crs(x) == sf::st_crs(y) is not TRUE"

  expect_error(st_or(poly_1, poly_2, x.suffix = "same_name", y.suffix = "same_name"))
  # The arguments ‘x.suffix’ and ‘y.suffix’ are specified both with "same_name". But they need to be specified differently.

  expect_error(
    st_or(poly_1, poly_2, x.suffix = 1),
    "the argument x.suffix must be a single character string"
  )
  expect_error(
    st_or(poly_1, poly_2, y.suffix = c("A", "B")),
    "the argument y.suffix must be a single character string"
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
  poly_1_renamed_geom <- st_rename_geometry(poly_1, "renamed_geometry")
  expect_false(attr(poly_1_renamed_geom, "sf_column") == attr(poly_2, "sf_column"))
  # input layers x and y with differently named geometry columns works with pkg
  # version >= 0.0.0.9001 and the name the output's geometry column is inherited
  # from input layer x*:
  expect_equal(
    attr(poly_1_renamed_geom, "sf_column"),                       # input layer x
    attr(st_or(x = poly_1_renamed_geom, y = poly_2), "sf_column") # st_or(x = x, y = y)
  )
  # * behavior as sf::st_intersection()
})
