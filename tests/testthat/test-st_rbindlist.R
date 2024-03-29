test_that("test-st_rbindlist", {
  library(sf)
  nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
  list_of_sf <- lapply(seq_along(nc[[1]]), function(x) nc[x, ])

  expect_equal(st_rbindlist(list_of_sf), nc)
  expect_equal(st_rbindlist(list(nc, NULL)), nc)

  expect_error(
    st_rbindlist(nc),
    "input is sf but should be a plain list of sf objects to be stacked"
  )

  expect_error(
    st_rbindlist(list_of_sf, geometry_name = factor("new_geometry_name")),
    "geometry_name must be either NULL or a single character string"
  )

  expect_error(
    st_rbindlist(list_of_sf, geometry_name = c("A", "B")),
    "geometry_name must be either NULL or a single character string"
  )

  expect_error(
    st_rbindlist(list(nc, NULL, "not_sf_nor_NULL")),
    "not all listed objects are of the class sf"
  )

  expect_error(
    st_rbindlist(list(NULL, NULL, NULL)),
    "no sf objects included in input list"
  )

  expect_error(
    st_rbindlist(list(nc, poly_1)),
    "arguments have different crs"
  )

  # use.names = "check" (default) -> use.names = FALSE -> message
  expect_message(matched_by_position <- st_rbindlist(list(poly_1, poly_2)))
  # names of 1st listed sf object used!
  expect_equal(names(matched_by_position), names(poly_1))

  matched_by_names <- st_rbindlist(list(poly_1, poly_2), use.names = TRUE, fill = TRUE)
  expect_true(c(names(poly_1), names(poly_2)) %in% names(matched_by_names) %>% all())

  poly_1_new_geom <- st_set_geometry(poly_1, "new_geom")
  newly_stacked <- st_rbindlist(list(poly_1_new_geom, poly_2, poly_1), use.names = TRUE, fill = TRUE)
  # by default the name of the frist listed active geometry column is passed on
  expect_equal(
    attr(poly_1_new_geom, "sf_column"),
    attr(newly_stacked, "sf_column")
  )

  l_different_geometry_types <- list(nc[1:3, ], st_cast(nc[4, ], "POLYGON", warn = FALSE), nc[NULL, ])

  n_geom_type <- vapply(l_different_geometry_types, st_geometry_type, by_geometry = FALSE, factor(1)) %>%
    unique() %>%
    length()

  expect_true(n_geom_type > 1)

  sf_helpers <- st_rbindlist(l_different_geometry_types)
  do_call    <- do.call(rbind, l_different_geometry_types)
  row.names(sf_helpers) <- row.names(do_call)
  expect_equal(sf_helpers, do_call)

  l <- list(nc[1:3, ], NULL, nc[4, ], nc[NULL, ], nc[nrow(nc) + 1, ])

  with_id <- st_rbindlist(l, idcol = "id")

  get_id <- function(x) {if (is.null(l[[x]])) {integer(0)} else {rep(x, nrow(l[[x]]))} }

  id <- lapply(seq_along(l), get_id) %>% unlist()

  expect_equal(with_id$id, id)
})
