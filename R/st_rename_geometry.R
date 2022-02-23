#' Rename geometry column of an sf object
#'
#' @param obj object of class \code{sf}
#' @param geometry_name a single character string renaming the active list-column with simple feature geometries
#'
#' @return a \code{sf} object with renamed geometry list-column.
#'
#' @export
#'
#' @importFrom sf st_geometry
#'
#' @details \code{st_rename_geometry()} is inspired by code found on
#' \href{https://gis.stackexchange.com/questions/386584/sf-geometry-column-naming-differences-r}{gis.stackexchange}
#' and particularly useful to homogenize \code{sf} objects with differently
#' named geometry columns or \code{list}s of such before binding them to a single
#' \code{sf} object by
#' \itemize{
#'   \item \code{rbind} (\code{\link[sf]{bind}})
#'   \item \code{do.call(rbind, <list_of_sf>)}
#'   \item \code{sf::st_as_sf(data.table::rbindlist(<list_of_sf>))}
#' }
#'
#' @examples
#' # current active list-column with simple feature geometries:
#' attr(poly_1, "sf_column")
#' st_rename_geometry(poly_1, "renamed_geometry")
st_rename_geometry <- function(obj, geometry_name) {
  if (!inherits(obj, "sf")) {
    stop("the argument obj must be of the class sf", call. = FALSE)
  }
  if (!(is.character(geometry_name) & length(geometry_name) == 1)) {
    stop("the argument geometry_name must be a single character string", call. = FALSE)
  }
  st_rename_geom(obj, geometry_name)
}
# helper function for pkg-internal use
st_rename_geom <- function(obj, geometry_name) {
  names(obj)[names(obj) == attr(obj, "sf_column")] <- geometry_name
  sf::st_geometry(obj) <- geometry_name
  return(obj)
}
