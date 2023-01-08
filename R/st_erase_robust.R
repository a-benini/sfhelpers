#' Erasing from geometry set \code{x} all parts overlapped by geometry set \code{y}
#'
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param check_overlap \code{TRUE} detects those geometries included in
#' \code{x} and \code{y} which overlap with the other input layer and applies
#' geometric operations only on these; \code{FALSE} (default) applies geometric
#' operations on all geometries (s. Details).
#' @param use_st_combine \code{TRUE} (default) applies internally
#' \code{st_union(st_combine())} to \code{y}; \code{FALSE} leaves out
#' \code{\link[sf]{st_combine}} and only uses \code{\link[sf]{st_union}}, which
#' might be slower, but more reliable (s. Details).
#' @param ... arguments passed on to \code{\link[s2]{s2_options}}
#'
#' @return Returns all parts of geometry set \code{x} not overlapped by
#' geometry set \code{y}
#'
#' @importFrom sf st_combine st_union st_difference st_crs st_geometry
#' st_intersects
#'
#' @details The example section of the \code{sf}-package help page on geometric
#' operations on pairs of simple feature geometry sets
#' (\code{\link[sf]{geos_binary_ops}}) presents code for a helper function that
#' erases all parts from geometry set \code{x} overlapped by geometry set \code{y}.
#' This function sometimes works as expected, sometimes it doesn't. (s. examples
#' below).
#'
#' Even when both input layers \code{x} and \code{y} consist of valid
#' geometries (which can be checked with \code{\link[sf]{st_is_valid}},
#' respectively fixed with \code{\link[sf]{st_make_valid}}), the a. m. helper
#' function can still throw an error. This is often due to internally applying
#' \code{st_union(st_combine())} to \code{y}. So, currently
#' \code{st_erase_robust()} uses in case of failure as a second option
#' \code{\link[sf]{st_union}} only. Leaving out \code{\link[sf]{st_combine}}
#' may very well add to the complexity of the involved geometries; thus the
#' second option may be rather slow. For this reason, if the coordinates of the
#' input are longlat degrees, setting \code{\link[sf]{sf_use_s2}} to
#' \code{FALSE} can help to speed up \code{st_erase_robust()} (s.examples below).
#'
#' Note that with the recent versions of \code{sf} (>= 1.0-1), although
#' \code{st_union(st_combine())} may very well return invalid geometries,
#' experience made so far shows that this has become less of an obstacle to
#' further prepossessing with \code{\link[sf]{st_difference}}. Nonetheless, sometimes
#' the default internal use of \code{st_union(st_combine())} (\code{use_st_combine}
#' = \code{TRUE}) results in an improper erase. By setting \code{use_st_combine}
#' = \code{FALSE} applying only \code{st_union()} to \code{y} can be enforced,
#' which erases more reliably, but at the cost of longer run time.
#'
#' In case of a significant proportion of non-overlapping geometries, setting
#' \code{check_overlap} to \code{TRUE} can shorten run time. As the internal
#' distinction of overlapping and non-overlapping geometries relies on
#' \code{\link[sf]{st_intersects}}, which itself is time consuming,
#' \code{check_overlap = TRUE} involves a trade off: If all geometries overlap,
#' \code{check_overlap = TRUE} will only increase the run time. On the other
#' hand, the higher the proportion of non-overlapping geometries, the more this
#' argument specification shortens the processing time by leaving out
#' unnecessary geometric operations. Ideally the user has previous knowledge
#' about how little or how much the input layers \code{x} and \code{y} overlap
#' with each other in order to apply \code{check_overlap} in a informed way. In
#' general, setting \code{check_overlap} to \code{TRUE} is advantageous when
#' \code{x} or \code{y} are large geometry sets and include more complex
#' geometries.
#'
#' @examples
#' library(sf)
#'
#' # find code of helper function st_erase():
#' \dontrun{
#' ?geos_binary_ops
#' }
#'
#' # copy function code:
#' st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
#'
#' # get some demo data:
#' nc   <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#' ext  <- st_bbox(nc) + rep(c(-0.1, 0.1), each = 2)
#' grid <- st_make_grid(ext) %>% st_sf(id = seq_along(.), geom = ., agr = "constant")
#'
#' st_is_longlat(nc) # demo data has a longlat degrees crs
#'
#' sf_use_s2(TRUE)
#' # check if helper function works with demo data:
#' \dontrun{
#' st_erase(grid, nc)
#' }
#'
#' # internal processing of input y (nc) returns the same error as st_erase():
#' \dontrun{
#' st_union(st_combine(nc))
#' }
#'
#' # st_erase_robust() can handle this:
#' st_erase_robust(grid, nc) %>% plot()
#'
#' sf_use_s2(FALSE) # if spherical geometry (s2) is switched off,
#' # sfhelpers::st_erase_robust() & helper function st_erase() should work and
#' # return the same (st_erase() might still not work on some Linux operation
#' # systems):
#' \dontrun{
#' all.equal(
#'  st_erase_robust(grid, nc),
#'  st_erase(grid, nc)
#' )
#'
#' # because internal handling of input y (nc) won't throw an error:
#' st_union(st_combine(nc))
#' }
#' @export
st_erase_robust <- function(x, y, check_overlap = FALSE, use_st_combine = TRUE, ...) {
  # if x or y are not of the class "sf", "sfc" or "sfg" throw a corresponding error message
  if (!inherits(x, c("sf", "sfc", "sfg"))) {
    stop("the argument x must be of the class sf, sfc or sfg", call. = TRUE)
  }
  if (!inherits(y, c("sf", "sfc", "sfg"))) {
    stop("the argument y must be of the class sf, sfc or sfg", call. = TRUE)
  }
  # if x and y don't have the same CRS, throw an error message
  if (sf::st_crs(x) != sf::st_crs(y)) {
    stop("sf::st_crs(x) == sf::st_crs(y) is not TRUE", call. = TRUE)
  }

  if (!isTRUE(check_overlap) & !isFALSE(check_overlap)) {
    stop("check_overlap must be a single logical value: TRUE or FALSE", call. = FALSE)
  }

  if (!isTRUE(use_st_combine) & !isFALSE(use_st_combine)) {
    stop("use_st_combine must be a single logical value: TRUE or FALSE", call. = FALSE)
  }

  if (check_overlap) { # apply geometric operations only to overlapping geometries
    y     <- sf::st_geometry(y)
    int   <- sf::st_intersects(x, y, ...)
    int_x <- lengths(int) > 0
    int_y <- unique(unlist(int))
    if (inherits(x, "sf") & !all(int_x)) {
      rbind(st_erase(x[int_x, ], y[int_y], use_st_combine, ...), x[!int_x, ])
    } else if (inherits(x, "sfc")) {
      c(st_erase(x[int_x], y[int_y], use_st_combine, ...), x[!int_x])
    } else { # if x is of class sfg or x is of class sf and all its geometries overlap with y
      st_erase(x, y[int_y], use_st_combine, ...)
    }
  } else { # apply geometric operations to all geometries
    st_erase(x, y, use_st_combine, ...)
  }
}
# helper function for pkg-internal use
st_erase <- function(x, y, use_st_combine, ...) {
  if (use_st_combine) {
    default <- try(sf::st_difference(x, sf::st_union(sf::st_combine(y)), ...), silent = TRUE)
  }
  if(!exists("default")){ default <- NULL }
  if (inherits(default, "try-error") | !use_st_combine) {
    sf::st_difference(x, sf::st_union(y), ...)
  } else {
    default
  }
}
