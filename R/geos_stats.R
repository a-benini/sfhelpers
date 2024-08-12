#' Summary statistical functions for \code{sfg}, \code{sfc} and \code{sf} objects
#'
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#'
#' @return \code{st_geometry_summary()} returns a vector with counts of the
#' geometry types include in \code{x}, which is named by the occurring geometry
#' type(s). If a function of the type \code{st_measure_sum()} is executed, the
#' \code{\link[base]{sum}} of these very measures from all geometries
#' included in \code{x} is returned, if specified in a certain unit and rounded
#' by given digits (see
#' \code{\link[sf]{st_area}}, \code{\link[sf]{st_length}},
#' \code{\link[sf]{st_perimeter}} resp.
#' \code{\link[lwgeom]{st_perimeter_2d}}).
#'
#' @importFrom lwgeom  st_perimeter_2d
#' @importFrom sf st_area st_geometry_type st_length st_is_longlat st_perimeter
#'
#' @examples
#' library(sf)
#' # some demo data
#' mat      <- rbind(0:3, log(4:1))*1000 + rep(c(26, 12), 4)*10^5
#' points   <- st_sfc(lapply(data.frame(mat), st_point)) %>% st_set_crs(2056)
#' polygons <- points %>% st_buffer(., seq_along(.)*150)
#'
#' mix <- c(points, polygons)
#' st_geometry_summary(mix)
#'
#' st_area_sum(polygons)
#' st_area_sum(polygons, "ha", 2)
#' st_area_sum(polygons, "km^2", 3)
#'
#' # if geometries' dimension and measure don't fit:
#' st_length_sum(polygons)
#'
#' st_perimeter_sum(polygons, "km", 1)
#'
#' # specifying only units doesn't effect comparison:
#' st_area_sum(polygons, "km^2") == st_area_sum(polygons, "ha")
#' # but setting units and rounding by digits argument can mess up comparison:
#' st_area_sum(polygons, "km^2") == st_area_sum(polygons, "ha", 0)
#' # if equivalent units-digits-specifying is done, comparison is feasible:
#' st_area_sum(polygons, "km^2", 2) == st_area_sum(polygons, "ha", 0)
#' # but to avoid a mess don't specify the digits before comparing
#'
#' # similarly if returns are used in further calculations then don't specify
#' # the digits to avoid passing on rounding errors
#' (ratio <- st_perimeter_sum(polygons) / st_area_sum(polygons, "ha"))
#' # rounding can be done later on
#' round(ratio)
#' @name geos_stats
#' @export
#' @param value object of class \code{units} or \code{symbolic_units} (see
#' examples section of \code{\link[units]{units}}\code{() <- value}),
#' optional: if unspecified, the default unit of the output is returned
#' @param digits integer indicating the number of decimal places to be used (see
#' \code{\link[base]{round}}), optional: if unspecified, the output is not
#' rounded
#' @param ... passed on to \code{\link[s2]{s2_distance}} or \code{\link[s2]{s2_distance_matrix}}
#' @name geos_stats
#' @export
st_geometry_summary <- function(x){
  check_sf_sfc_sfg(x)
  y <- summary(sf::st_geometry_type(x))
  y[y > 0]
}
#' @name geos_stats
#' @export
st_area_sum <- function(x, value, digits, ...){
  st_measures_sum(fun = sf::st_area, x = x, value = value, digits = digits, ...)
}
#' @name geos_stats
#' @export
st_length_sum <- function(x, value, digits, ...){
  st_measures_sum(fun = sf::st_length, x = x, value = value, digits = digits, ...)
}
#' @name geos_stats
#' @export
st_perimeter_sum <- function(x, value, digits, ...){
  st_measures_sum(fun = sf::st_perimeter, x = x, value = value, digits = digits, ...)
}
#' @name geos_stats
#' @export
st_perimeter_2d_sum <- function(x, value, digits){
  check_4_st_perimeter_2d_sum(x)
  st_measures_sum(fun = lwgeom::st_perimeter_2d, x = x, value = value, digits = digits)
}
# helper functions for pkg-internal use
st_measures_sum <- function(fun, x, value, digits, ...){
  check_sf_sfc_sfg(x)
  sum <- sum(fun(x, ...))
  if(!missing(value)){
    units(sum) <- value
  }
  if(!missing(digits)){
    sum <- round(sum, digits = digits)
  }
  sum
}
check_sf_sfc_sfg <- function(x){
  if (!inherits(x, c("sf", "sfc", "sfg"))) {
    stop("the argument x must be of the class sf, sfc or sfg", call. = TRUE)
  }
}
check_4_st_perimeter_2d_sum <- function(x){
  if(inherits(x, c("sf", "sfc", "sfg"))){
    if (isTRUE(sf::st_is_longlat(x))){
      stop("for perimeter of longlat geometry, cast to LINESTRING and use st_length_sum")
    }
  }
}


