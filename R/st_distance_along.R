#' Compute Euclidian or great circle distance along a sequence of geometries
#'
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param ... passed on to \code{\link[s2]{s2_distance}} or \code{\link[s2]{s2_distance_matrix}}
#' @param which character; for Cartesian coordinates only: one of \code{Euclidean}, \code{Hausdorff} or \code{Frechet}; for geodetic coordinates, great circle distances are computed; see details
#' @param par for \code{which} equal to \code{Hausdorff} or \code{Frechet}, optionally use a value between 0 and 1 to densify the geometry
#' @param tolerance ignored if \code{sf::st_is_longlat(x)} is \code{FALSE}; otherwise, if set to a positive value, the first distance smaller than \code{tolerance} will be returned, and true distance may be smaller; this may speed up computation. In meters, or a \code{units} object convertible to meters.
#'
#' @details For details see \code{\link[sf]{st_distance}} since
#' \code{st_distance_along()} is built-on that function.
#'
#' @return Returns a numeric vector of length \code{x} starting with 0 for the
#' first geometry, followed by the cumulative sums of distances between the
#' geometries along the sequence of \code{x}. If a geometry is empty the
#' corresponding value and any following value in vector are \code{NA}.
#'
#' If the coordinate reference system of \code{x} was set, the
#' distances are returned with corresponding unit of measure; see
#' \code{\link[units]{set_units}}.
#'
#' @importFrom sf st_is_longlat st_geometry st_distance
#'
#' @examples library(sf)
#' library(tmap)
#'
#' storms <- st_read(system.file("shape", package = "sf"), "storms_xyz", quiet = TRUE)
#' storms <- st_set_crs(storms, 4326)
#' st_storm_30 <- storms[30, ]
#' st_storm_30_pt <- st_cast(st_storm_30, "POINT", warn = FALSE)
#' dist_along <- st_distance_along(st_storm_30_pt)
#' st_storm_30_pt$dist_along_km <- units::set_units(dist_along, "km") %>% round() %>% paste("km")
#'
#' tm_shape(st_storm_30) + tm_lines() +
#'   tm_shape(st_storm_30_pt) + tm_bubbles(size = 0.5, col = "red") +
#'   tm_text("dist_along_km", xmod = 2, ymod = 1) +
#'   tm_layout(inner.margins = 0.1)
#'
#' @export
st_distance_along <- function(x,
                              ...,
                              which = ifelse(isTRUE(sf::st_is_longlat(x)), "Great Circle", "Euclidean"),
                              par = 0.0, tolerance = 0.0) {
  x <- sf::st_geometry(x)
  if (length(x) == 0) {
    sf::st_distance(x, ..., by_element = TRUE, which = which, par = par, tolerance = tolerance)
  } else {
    i <- seq_len(length(x) - 1)
    cumsum(sf::st_distance(x[c(1, i)], x[c(1, i + 1)], ..., by_element = TRUE, which = which, par = par, tolerance = tolerance))
  }
}
