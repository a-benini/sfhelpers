#' Convert a list of sf objects to a single sf object
#'
#' @description A fast alternative to \code{do.call(rbind, <list_of_sf>)}
#'
#' @param l \code{list} of objects of the class \code{sf}
#' @param ... arguments of the function \code{\link[data.table]{rbindlist}}
#' except \code{l}
#' @param use_geometry \code{TRUE} detects and binds the active list-columns
#' with simple feature geometries separately if all of them have the same
#' geometry type, thus enabling matching of geometry columns differing by
#' name as well as by position across the \code{list}ed sf objects.
#' @param use_any_geometry \code{TRUE} does the same as
#' \code{use_geometry = TRUE}, but allows different geometry types among the
#' \code{list}ed sf objects. If \code{use_any_geometry = TRUE} then
#' \code{use_geometry} is ignored.
#' @param geometry_name  a single character string giving a user-defined name to
#' the returned active list-column with simple feature geometries.
#'
#' @return a \code{sf} object
#'
#' @export
#'
#' @importFrom data.table as.data.table rbindlist
#' @importFrom sf st_as_sf st_crs st_geometry_type st_geometry st_drop_geometry st_sf
#' @importFrom uuid UUIDgenerate
#'
#' @details \code{st_rbindlist()} is basically a wrapper for
#' \code{sf::st_as_sf(data.table::rbindlist(<list_of_sf>))}, which is a fast
#' alternative to \code{do.call(rbind, <list_of_sf>)}. It comes with some
#' properties which \code{rbind} has, but \code{rbindlist} misses:
#' \itemize{
#'   \item checks if all \code{sf} objects included in the input share the same
#'   CRS
#'   \item returns a \code{sf} object with a correct \code{bbox} (more
#'   information on how to handle this \code{rbindlist} issue can be found
#'   \href{https://github.com/Rdatatable/data.table/issues/4681}{here}).
#'   \item allows binding \code{sf} objects with different geometry types, which
#'   can be achieved by setting \code{st_rbindlist}'s argument
#'   \code{use_any_geometry} to \code{TRUE}.
#' }
#' Remarks about matching geometry columns of input \code{sf} objects:
#' \itemize{
#'   \item If the geometry columns are identically named, but differ by position
#'   within
#'   each \code{sf} object set \code{use.names = TRUE} and \code{fill = TRUE}.
#'   \item If the geometry (and other) columns are differently named, but have
#'   same position within every \code{sf} object set \code{use.names = FALSE}.
#'   \item If the geometry columns are differently named and at the same time
#'   differently positioned set \code{use_geometry = TRUE} or
#'   \code{use_any_geometry = TRUE}. This detects the geometry columns and
#'   then matches them separately while the other columns can be still treated
#'   collectively by the arguments \code{use.names} and \code{fill}. Note that
#'   matching geometry columns separately requires more processing time.
#' }
#' If the argument \code{geometry_name} is unspecified (default \code{NULL})
#' the geometry column name of the returned \code{sf} object depends on the
#' following:
#' \itemize{
#'   \item If separately matched geometry columns (\code{use_geometry = TRUE} or
#'   \code{use_any_geometry = TRUE}) have a common name, this name persists,
#'   else \code{"geometry"} is used.
#'   \item If geometry (and other) columns are differently named and matched by
#'   their same position (\code{use.names = FALSE}), the geometry column name
#'   is inherited from the first \code{sf} object \code{list}ed in the input.
#' }
#'
#' @seealso \href{../doc/rbindlist_issues.html}{\code{vignette("rbindlist_issues")}}
#'
#' @examples library(magrittr)
#' library(dplyr)
#' library(sf)
#'
#' # a set of geometries to work with
#' nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#'
#' # proof of concept with a list of sf-objects having identically named and
#' # positioned attribute and geometry columns:
#' list_of_sf <- lapply(1:nrow(nc), function(x) nc[x, ])
#' nc_new <- st_rbindlist(list_of_sf)
#' all.equal(nc, nc_new)
#'
#' # list of sf-objects having differently named, but identically positioned
#' # geometry columns (attribute columns are also same-positioned and still
#' # same-named):
#' list_of_sf <- lapply(
#'   1:nrow(nc),
#'   function(x) st_rename_geometry(nc[x, ], paste0("geom_", x))
#'   )
#' # such a list of sf-objects is easy to stack ...
#' nc_new_2 <- st_rbindlist(list_of_sf, use.name = FALSE)
#' # ... but by default the name of the returned geometry column ...
#' attr(nc_new_2, "sf_column")
#' # ... is inherited from the first listed sf-object.
#' attr(nc_new_2, "sf_column") == attr(list_of_sf[[1]], "sf_column")
#' # user defined name of the stacked geometry column:
#' nc_new_3 <- st_rbindlist(list_of_sf, use.name = FALSE, geometry_name = "geom")
#' attr(nc_new_3, "sf_column")
#'
#' # from which listed element / sf-object do the stacked features originate?
#' l <- list(A = nc[1, 1:3], B = NULL, C = nc[2:3, 1:3])
#' st_rbindlist(l, idcol = "ID")
#'
#' # list of sf-objects with identically named, but differently positioned
#' # geometry and attribute columns (the latter might not be included in
#' # each listed sf-object) ...
#' l <- list(nc[1, NULL], nc[2, 1], nc[3, 2:3])
#' # ... requires use.name = TRUE and fill = TRUE
#' st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE)
#'
#' # list of sf-objects with differently named and positioned geometry columns,
#' # and (if existing) identically named attribute columns ...
#' l <- lapply(
#'   seq_along(l),
#'   function(x) st_rename_geometry(l[[x]], paste0("geom_", x))
#'   )
#' sapply(l, attr, "sf_column") # geometry column names of listed sf-objects
#' # ... requires use_geometry = TRUE for detecting and binding the geometry
#' # columns (separately from the attribute columns)
#' st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE, use_geometry = TRUE)
#'
#' # list of sf-objects having different geometry types,
#' #  all columns identically named and positioned ...
#' l <- list(
#'   multi_poly = nc[1:2, ],
#'   no_row     = nc[NULL, ],
#'   poly       = st_cast(nc[4, ], "POLYGON", warn = FALSE),
#'   empty_row  = nc[nrow(nc) + 1, ]
#'   )
#' sapply(l, st_geometry_type, by_geometry = FALSE)
#' # ... requires use_any_geometry = TRUE
#' st_obj <- st_rbindlist(l, idcol = TRUE, use_any_geometry = TRUE)
#' st_obj[, ".id"] %>% mutate(geometry_type = st_geometry_type(.))
#'
#' # list of sf-objects having different geometry types as well as having
#' # differently named and positioned geometry columns, and (if existing)
#' # identically named attribute columns ...
#' l <- lapply(
#'   seq_along(l),
#'   function(x) {st_rename_geometry(l[[x]], paste0("geom_", x)) %>% .[,1:x]}
#'   ) %>%
#'   setNames(names(l))
#' # ... requires use.name, fill and use_any_geometry all being set to TRUE
#' st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE, use_any_geometry = TRUE)
st_rbindlist <- function(l, ..., use_geometry = FALSE, use_any_geometry = FALSE, geometry_name = NULL) {
  if (!is.list(l) || is.data.frame(l)) {
    stop("input is ", class(l)[1L], " but should be a plain list of sf objects to be stacked", call. = FALSE)
  }
  if(!(isTRUE(use_geometry) | isFALSE(use_geometry))){
    stop("use_geometry must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if(!(isTRUE(use_any_geometry) | isFALSE(use_any_geometry))){
    stop("use_any_geometry must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if(!(is.null(geometry_name) | (is.character(geometry_name) & length(geometry_name) == 1))){
    stop("geometry_name must be either NULL or a single character string", call. = FALSE)
  }
  if (!all(vapply(l, function(x) { inherits(x, "sf") | is.null(x) }, logical(1)))) {
    stop("not all listed objects are of the class sf", call. = FALSE)
  }
  is_not_null <- !vapply(l, is.null, logical(1))
  if(!any(is_not_null)) {
    stop("no sf objects included in input list", call. = FALSE)
  }
  l_crs <- lapply(l[is_not_null], sf::st_crs)
  if (!all(vapply(l_crs, function(x) { x == l_crs[[1]] }, logical(1)))) {
    stop("arguments have different crs", call. = FALSE)
  }
  if(use_geometry == FALSE & use_any_geometry == FALSE){
    sf <- sf::st_as_sf(data.table::rbindlist(l, ...))
    sf <- sf[seq_len(nrow(sf)), ]
    class(sf) <- c("sf", "data.frame")
  } else {
    if(use_any_geometry == FALSE){
      if(length(unique(vapply(l[is_not_null], st_geometry_type, by_geometry = FALSE, factor(1)))) > 1 ){
        stop("use_geometry = TRUE requires all sf objects of the input to have the same geometry type. if intending to bind sf objects with different geometry types set: use_any_geometry = TRUE", call. = FALSE)
      }
    }
    geometry <- do.call(c, lapply(l[is_not_null], sf::st_geometry))
    tmp_col <- uuid::UUIDgenerate()
    get_non_geometry <- function(x) { if(is.null(x)) {x} else {data.table::as.data.table(sf::st_drop_geometry(x), keep.rownames = tmp_col)} }
    non_geometry <- data.table::rbindlist(lapply(l, get_non_geometry), ...)
    sf <- sf::st_sf(non_geometry, geometry)
    sf <- sf[ ,names(sf) != tmp_col]
  }
  if(!is.null(geometry_name)){
    names(sf)[names(sf) == attr(sf, "sf_column")] <- geometry_name
    sf::st_geometry(sf) <- geometry_name
  }
  if(is.null(geometry_name) & (use_geometry | use_any_geometry)){
    geometry_names_unique <- unique(vapply(l[is_not_null], attr, which = "sf_column", character(1)))
    if(length(geometry_names_unique) == 1){
      names(sf)[names(sf) == "geometry"] <- geometry_names_unique
      sf::st_geometry(sf) <- geometry_names_unique
    }
  }
  return(sf)
}
