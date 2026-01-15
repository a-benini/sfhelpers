test_that("test-st_layers_as_df", {
  library(sf)

  expect_error(st_layers_as_df(NULL))

  dns <- system.file("shape", package = "sf")

  sf_layers <- st_layers(dns)

  expect_equal(
    st_layers_as_df(dns),
    st_layers_as_df(sf_layers)
  )

  expect_equal(
    st_layers_as_df(dns, drop_driver = FALSE),
    st_layers_as_df(sf_layers, drop_driver = FALSE)
  )

  expect_equal(
    st_layers_as_df(sf_layers, arg_to_ignore = "TRUE"),
    st_layers_as_df(sf_layers)
  )

  expect_error(
    st_layers_as_df(dsn, wrong_arg = "TRUE")
  )

  sf_layers$name <- NULL
  expect_warning(st_layers_as_df(sf_layers))
  expect_null(st_layers_as_df(sf_layers, warn = FALSE))

  expect_error(
    st_layers_as_df(dns, warn = "TRUE"),
    "warn must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    st_layers_as_df(dns, drop_driver = "FALSE"),
    "drop_driver must be a single logical value: TRUE or FALSE"
  )
})

