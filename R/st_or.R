#' \code{sf} equivalent of QGIS Union
#'
#' @param x object of class \code{sf} or \code{sfc}
#' @param y object of class \code{sf} or \code{sfc}
#' @param dim integer 0, 1, or 2 (default) for dimension of simple feature
#' @param x.suffix a single character strings (default \code{".x"}) attached as
#' suffix to attribute headings inherited from argument \code{x}
#' @param y.suffix a single character strings (default \code{".y"}) attached as
#' suffix to attribute headings inherited from argument \code{y}
#' @param suffix.all \code{TRUE} suffixes all attribute headings inherited from
#' arguments \code{x} and \code{y} according to \code{x.suffix}, resp.
#' \code{y.suffix}; \code{FALSE} (default) suffixes only homonymous attribute
#' headings.
#'
#' @return geometry set containing the intersection of \code{x} and \code{y} and
#' the non-overlapping parts of \code{x} and \code{y}. The attribute table is
#' filled with attribute values from the respective original geometry set for
#' non-overlapping features, and attribute values from both geometry sets for
#' overlapping features.
#'
#' @details
#' \code{st_or()} consists mainly of code presented by
#' \href{https://gis.stackexchange.com/questions/251440/ogr2ogr-equivalent-of-qgis-union/251575#251575}{TimSalbim on gis.stackexchange}
#' and some improvements put out by
#' \href{https://atriplex.info/blog/index.php/2018/07/12/full-spatial-polygon-union-intersection-with-r-sf/}{Jimbob on his blog}.
#' In addition to these two precursors this version of \code{st_or()} includes:
#' \itemize{
#'   \item a more robust version of the internal function \code{st_erase()}
#'   equivalent to \code{\link{st_erase_robust}}
#'   \item the ability to handle homonymous attribute variables of both input
#'   geometry sets (s. below examples)
#'   \item the possibly to give customized suffixes to attribute variables
#'   corresponding to the geometry set they originated from (s. below examples)
#' }
#'
#' @importFrom sf st_crs st_sf st_agr st_combine st_union st_make_valid
#' st_difference st_dimension st_intersection
#'
#' @examples
#' library(sf)
#' library(magrittr)
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
#' # Give customized suffixes to homonymous attributes of layer x and y:
#' st_or(poly_1, poly_2, x.suffix = "_poly_1", y.suffix = "_poly_2") %>% plot()
#'
#' # If only homonymous attributes from one layer should get a suffix, set ...
#' # ... the suffix for the other layer to an empty string:
#' st_or(poly_1, poly_2, x.suffix = "") %>% names()
#'
#' # If all attributes attributes of both layer x and y should get a
#' # ... layer-specific suffix, set suffix.all = TRUE:
#' st_or(poly_1, poly_2, suffix.all = TRUE) %>% names()
#'
#' # If only all attributes from one layer should get a suffix, set the suffix ...
#' # ... for the other layer to an empty string and set suffix.all = TRUE:
#' st_or(poly_1, poly_2, x.suffix = "", suffix.all = TRUE) %>% names()
#'
#' @export
st_or <- function(x, y, dim = 2, x.suffix = ".x", y.suffix = ".y", suffix.all = FALSE) {
  # if x or y are not of the class "sf" or "sfc" throw a corresponding error message
  if (!any(c("sf", "sfc") %in% class(x))) {
    stop(
      paste0("the argument ", sQuote("x"), " must be of the class ", dQuote("sf"), " or ", dQuote("sfc")),
      call. = TRUE
    )
  }
  if (!any(c("sf", "sfc") %in% class(y))) {
    stop(
      paste0("the argument ", sQuote("y"), " must be of the class ", dQuote("sf"), " or ", dQuote("sfc")),
      call. = TRUE
    )
  }

  # if x and y don't have the same CRS, throw an error message
  if (st_crs(x) != st_crs(y)) {
    stop("sf::st_crs(x) == sf::st_crs(y) is not TRUE", call. = TRUE)
  }

  # check if x.suffix and y.suffix are single character strings
  if (!(is.character(x.suffix) & length(x.suffix) == 1)) {
    stop("the argument x.suffix must be a single character string", call. = FALSE)
  }

  if (!(is.character(y.suffix) & length(y.suffix) == 1)) {
    stop("the argument y.suffix must be a single character string", call. = FALSE)
  }

  # if x.suffix and y.suffix are the same stop and throw an error message
  if (x.suffix == y.suffix) {
    stop(
      paste0(
        "The arguments ", sQuote("x.suffix"), " and ", sQuote("y.suffix"),
        ' are specified both with "', x.suffix,
        '". But they need to be specified differently.'
      ),
      call. = TRUE
    )
  }

  if(!isTRUE(suffix.all) & !isFALSE(suffix.all)){
    stop("suffix.all must be a single logical value: TRUE or FALSE", call. = FALSE)
  }

  # in case x or y are simple feature geometry list columns turn them into sf objects
  if ("sfc" %in% class(x)) {
    x <- sf::st_sf(geometry = x)
  }
  if ("sfc" %in% class(y)) {
    y <- sf::st_sf(geometry = y)
  }

  # make sure that homonymous attributes of x and y both get a distinct suffix
  x_names <- names(x)
  y_names <- names(y)
  x_agr   <- names(attr(x, "agr"))
  y_agr   <- names(attr(y, "agr"))
  if (suffix.all) {
    names(x) <- ifelse(x_names %in% x_agr, paste0(x_names, x.suffix), x_names)
    names(y) <- ifelse(y_names %in% y_agr, paste0(y_names, y.suffix), y_names)
  } else {
    names(x) <- ifelse(x_names %in% y_agr, paste0(x_names, x.suffix), x_names)
    names(y) <- ifelse(y_names %in% x_agr, paste0(y_names, y.suffix), y_names)
  }

  # st_erase to get the remainder of the intersection
  # (improved version of code found under ?st_difference)
  st_erase <- function(x, y) {
    sf::st_difference(x, sf::st_make_valid(sf::st_union(sf::st_combine(y))))
  }

  # we need st_dump to extract polygons from a potential GEOMETRYCOLLECTION
  st_dump <- function(x, dim) {
    dims <- sapply(x, sf::st_dimension)
    x[dims == dim, ]
  }

  # get overlap via intersection
  overlap <- sf::st_intersection(x, y)

  # extract polygons (if dimm = 2)
  overlap <- overlap[sf::st_dimension(overlap) == dim, ]

  gc <- which(sapply(seq(nrow(overlap)), function(i) {
    inherits(overlap[i, ], "GEOMETRYCOLLECTION")
  }))

  if (length(gc) > 0) {
    dmp <- st_dump(overlap, dim = dim)
    overlap <- rbind(overlap[-gc, ], dmp)
  }

  # set all arguments of the input layers to "constant" to avoid getting ...
  # ... repetitions of the warning message saying: ...
  # ... "attribute variables are assumed to be spatially constant throughout all geometries"
  # ... each time st_st_erase() is used / sf::st_difference() called internally
  # if there's a reason for that very warning, it would have been triggered by ...
  # ... the above use of st::intersection()!
  sf::st_agr(x) <- "constant"
  sf::st_agr(y) <- "constant"

  # get the non-intersecting parts and set missing attributes to NA
  #### I have modified this to the erasure and insertion of NA
  #### values before the rbind to ensure they have the same columns
  diff1 <- st_erase(x, y)
  diff2 <- st_erase(y, x)

  diff1[, setdiff(names(overlap), names(diff1))] <- NA
  diff2[, setdiff(names(overlap), names(diff2))] <- NA

  cb <- rbind(diff1, diff2)

  # return combined geometry set with attributes
  return(rbind(overlap, cb))
}
