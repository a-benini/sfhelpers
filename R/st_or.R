#' \code{sf} equivalent of QGIS Union
#'
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param dim integer: A combination of 0, 1, and/or 2 (default) that constrains
#' the dimension(s) of the returned geometries. 0 for points, 1 for lines, 2 for
#' surfaces.
#' @param suffix length 2 character vector (default \code{c(".x", ".y")})
#' attached as suffix to attribute headings inherited from argument \code{x}
#' resp. from \code{y}
#' @param suffix.all \code{TRUE} suffixes all attribute headings inherited from
#' arguments \code{x} and \code{y} according to \code{suffix}; \code{FALSE}
#' (default) suffixes only homonymous attribute headings.
#' @param ... arguments passed on to \code{\link[s2]{s2_options}}
#'
#' @return geometry set containing the intersection of \code{x} and \code{y} and
#' the non-overlapping parts of \code{x} and \code{y}. The attribute table is
#' filled with attribute values from the respective original geometry set for
#' non-overlapping features, and attribute values from both geometry sets for
#' overlapping features. The dimension(s) of the returned geometries is/are
#' determined with the argument \code{dim}.
#'
#' @details
#' \code{st_or()} consists at its core of code presented by
#' \href{https://gis.stackexchange.com/questions/251440/ogr2ogr-equivalent-of-qgis-union/251575#251575}{TimSalbim on gis.stackexchange}
#' . In addition to its precursor this version of \code{st_or()} includes:
#' \itemize{
#'   \item a more robust version of the internal function \code{st_erase()}
#'   equivalent to \code{\link{st_erase_robust}}
#'   \item the ability to handle homonymous attribute variables of both input
#'   geometry sets (s. below examples)
#'   \item the possibility to give customized suffixes to attribute variables
#'   corresponding to the geometry set they originated from (s. below examples)
#'   \item handling of input layers with differently named geometry columns
#'   and/or being totally overlapped by the other input layer.
#'   \item restricts geometric operations to geometries of the input layers
#'   \code{x} and \code{y} overlapping those of the other input
#'   layer (same as \code{\link{st_erase_robust}}).
#' }
#'
#' @importFrom data.table as.data.table rbindlist
#' @importFrom sf st_crs st_sf st_agr st_dimension st_intersection st_geometry
#' st_drop_geometry st_is st_sfc st_intersects
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#' library(dplyr)
#' library(sf)
#'
#' st_or(poly_1, poly_2) %>% plot()
#'
#' st_or(poly_1, poly_2) %>% st_drop_geometry()
#'
#' # to avoid repeated warning messages triggered by non-spatially constant ...
#' # ... attribute variables, set them all to "constant":
#' st_agr(poly_1) <- "constant"
#' st_agr(poly_2) <- "constant"
#'
#' # Give customized suffixes to homonymous attributes of layers x and y:
#' st_or(poly_1, poly_2, suffix = c("_poly_1", "_poly_2")) %>% plot()
#'
#' # If only homonymous attributes from one layer should get a suffix, set ...
#' # ... the suffix for the other layer to an empty string:
#' st_or(poly_1, poly_2, suffix = c("", ".y")) %>% names()
#'
#' # If all attributes attributes of both layers x and y should get a
#' # ... layer-specific suffix, set suffix.all = TRUE:
#' st_or(poly_1, poly_2, suffix.all = TRUE) %>% names()
#'
#' # If only all attributes from one layer should get a suffix, set the suffix ...
#' # ... for the other layer to an empty string and set suffix.all = TRUE:
#' st_or(poly_1, poly_2, suffix = c("", ".y"), suffix.all = TRUE) %>% names()
#'
#' # create two layers with overlapping linestrings:
#' ls1 <- st_linestring(cbind(c(0, 1, 1, 0), c(0:3)))
#' ls2 <- st_linestring(cbind(c(2, 1, 1), c(0, 0, 3)))
#' ls3 <- st_linestring(cbind(c(0, 0.5, 0.5, 0), c(0, 0, 2.5, 2)))
#' A <- st_sf(id_A = 1, A = "A", geom = st_sfc(ls1), agr = "constant")
#' B <- st_sf(id_B = 1:2, B = "B", geom = st_sfc(ls2, ls3), agr = "constant")
#'
#' plot(st_geometry(A), col = "gray", lwd = 4, extent = st_bbox_common(A, B))
#' plot(st_geometry(B), col = "red", lty = 2, add = TRUE)
#' legend("right", legend = c("A", "B"), col = c("gray", "red"), lwd = c(4, 1), lty = c(1, 2))
#'
#' # when both input layers consist of linestings, and if the default specification ...
#' # ... dim = 2 (for surfaces / (multi)polygons) is used, a sf-object with zero ...
#' # ... rows will be returned:
#' st_or(A, B)
#'
#' # to get lines returned set dim = 1:
#' comb_dim_1 <- st_or(A, B, dim = 1) %>%
#'   mutate(comb = ifelse(is.na(A), "B", ifelse(is.na(B), "A", "A+B")))
#' plot(comb_dim_1[, "comb"], lwd = 3, key.pos = 1, main = "dim = 1: only lines")
#'
#' # for returning lines and points where lines cross or touch lines of the other ...
#' # ... input layer, set dim = c(0, 1):
#' comb_dim_0_1 <- st_or(A, B, dim = c(0, 1)) %>%
#'   mutate(comb = ifelse(is.na(A), "B", ifelse(is.na(B), "A", "A+B")))
#' plot(comb_dim_0_1[, "comb"], lwd = 3, cex = 2, key.pos = 1, main = "dim = c(0, 1): points & lines")
#'
#' all.equal(
#'   st_or(A, B, dim = c(0, 1)), # returns points & lines
#'   st_or(A, B, dim = c(0, 1, 2)) # returns points, lines (& if available surfaces)
#' )
#' @export
st_or <- function(x, y, dim = 2, suffix = c(".x", ".y"), suffix.all = FALSE, ...) {
  # if x or y are not of the class "sf", "sfc" or "sfg" throw a corresponding error message
  if (!inherits(x, c("sf", "sfc", "sfg"))) {
    stop("the argument x must be of the class sf, sfc or sfg", call. = TRUE)
  }
  if (!inherits(y, c("sf", "sfc", "sfg"))) {
    stop("the argument y must be of the class sf, sfc or sfg", call. = TRUE)
  }

  # if x and y don't have the same CRS, throw an error message
  if (st_crs(x) != st_crs(y)) {
    stop("sf::st_crs(x) == sf::st_crs(y) is not TRUE", call. = TRUE)
  }

  # check if dim = 0, 1 and/or 2 (default)
  if(!is.vector(dim) | !is.numeric(dim) | any(!dim %in% c(0, 1, 2))){
    stop("dim must be a single integer or vector of integers consisting of 0, 1 and/or 2", call. = FALSE)
  }

  # check if suffix is a length 2 character vector
  if (!(is.character(suffix) & length(suffix) == 2)) {
    stop("the argument suffix must be a length 2 character vector", call. = FALSE)
  }

  # if both suffix-element are the same stop and throw an error message
  if (suffix[1] == suffix[2]) {
    stop(
      paste0(
        'The 1st and 2nd element of the argument ', sQuote("suffix"),
        ' are both specified as "', suffix[1],
        '". But they need to be specified differently.'
      ),
      call. = TRUE
    )
  }

  if(!isTRUE(suffix.all) & !isFALSE(suffix.all)){
    stop("suffix.all must be a single logical value: TRUE or FALSE", call. = FALSE)
  }

  # in case x or y are ar of class sfc or sfg turn them into sf objects
  if (!inherits(x, "sf")) {
    x <- sf::st_sf(geometry = sf::st_geometry(x))
  }
  if (!inherits(y, "sf")) {
    y <- sf::st_sf(geometry = sf::st_geometry(y))
  }

  # trigger warning independently from sf::st_intersection() and sfhelpers:::st_erase()
  if (!isTRUE(all(c(sf::st_agr(x), sf::st_agr(y)) == "constant"))) {
    warning(
      "attribute variables are assumed to be spatially constant throughout all geometries",
      call. = FALSE
    )
  }

  # make sure that (homonymous) attributes of x and y both get a distinct suffix
  x_agr <- names(attr(x, "agr"))
  y_agr <- names(attr(y, "agr"))
  if (suffix.all == FALSE) { # if only homonymous attributes get a distinct suffix
    x_agr <- base::intersect(x_agr, y_agr)
    y_agr <- x_agr
  }
  names(x) <- ifelse(names(x) %in% x_agr, paste0(names(x), suffix[1]), names(x))
  names(y) <- ifelse(names(y) %in% y_agr, paste0(names(y), suffix[2]), names(y))

  # avoid getting repented warnings about non-constant attribute variables
  # assumed to be constant
  sf::st_agr(x) <- "constant"
  sf::st_agr(y) <- "constant"

  # to get the remainder of the intersection sfhelpers:::st_erase()* is used (s. below) instead of the formerly here coded st_erase()
  # (*improved version of st_erase() found under ?st_difference)

  # separate intersecting and non-intersecting geometries
  int      <- sf::st_intersects(x, y, ...)
  int_x    <- lengths(int) > 0
  int_y    <- seq_len(nrow(y)) %in% unique(unlist(int))
  x_no_int <- x[!int_x, ]
  x        <- x[int_x, ]
  y_no_int <- y[!int_y, ]
  y        <- y[int_y, ]

  # get overlap via intersection
  overlap <- sf::st_intersection(x, y, ...)

  # get the non-intersecting parts with sfhelpers:::st_erase()
  x_diff <- st_erase(x, y, ...)
  y_diff <- st_erase(y, x, ...)

  # stack the intersecting and non-intersecting parts and set missing attributes to NA
  l                <- list(overlap, x_diff, y_diff, x_no_int, y_no_int)
  geometry         <- do.call(c, lapply(l, sf::st_geometry))
  tmp_col          <- uuid::UUIDgenerate()
  get_non_geometry <- function(x) { data.table::as.data.table(sf::st_drop_geometry(x), keep.rownames = tmp_col) }
  non_geometry     <- data.table::rbindlist(lapply(l, get_non_geometry), use.names = TRUE, fill = TRUE)
  output           <- sf::st_sf(non_geometry, geometry)

  # geometry collections included in output?
  is_gc <- sf::st_is(output, "GEOMETRYCOLLECTION")
  if (any(is_gc)) { # if so disaggregate these into sub-geometries
    output_gc    <- output[is_gc, ]
    l_sfg        <- lapply(sf::st_geometry(output_gc), `[`)
    geometry     <- do.call(c, lapply(l_sfg, sf::st_sfc, crs = sf::st_crs(output)))
    rep_row      <- rep(seq_len(nrow(output_gc)), lengths(l_sfg))
    non_geometry <- sf::st_drop_geometry(output_gc)[rep_row, , drop = FALSE]
    output       <- rbind(output[!is_gc, ], sf::st_sf(non_geometry, geometry))
  }

  # filter output by desired dimension(s) and remove tmp. col
  output <- output[sf::st_dimension(output) %in% unique(dim), names(output) != tmp_col]

  # name of geometry column is inherited from input layer x*
  # *same behavior as sf::st_intersection():
  sf::st_geometry(output) <- attr(x, "sf_column")

  return(output)
}
