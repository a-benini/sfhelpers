test_that("test-st_bbox_aligned", {
  library(sf)

  pt <- st_point(c(3, 11))

  expect_error(st_bbox_aligned(pt))

  expect_error(st_bbox_aligned(NULL, 10))

  expect_equal(
    st_bbox_aligned(pt, 10),
    st_bbox(c(xmin = 0, ymin = 10, xmax = 10, ymax = 20))
  )

  expect_error(
    st_bbox_aligned(pt, 1:3),
    "alignment muss be a singel positive numeric value"
  )

  expect_error(
    st_bbox_aligned(pt, list(10)),
    "alignment muss be a singel positive numeric value"
  )

  expect_error(
    st_bbox_aligned(pt, "123"),
    "alignment muss be a singel positive numeric value"
  )

  expect_error(
    st_bbox_aligned(pt, -10),
    "alignment muss be a singel positive numeric value"
  )
})
