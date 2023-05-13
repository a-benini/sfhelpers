#' Convert a list of sf objects to a single sf object
#'
#' @description A fast alternative to \code{do.call(rbind, <list_of_sf>)}
#'
#' @param l \code{list} of objects of the class \code{sf}
#' @param ... arguments of the function \code{\link[data.table]{rbindlist}}
#' except \code{l}
#' @param geometry_name  a single character string giving a user-defined name to
#' the returned active list-column with simple feature geometries. If
#' unspecified / by default (\code{NULL}), the geometry column name is inherited
#' from the first \code{sf} object \code{list}ed in the input.
#'
#' @return a \code{sf} object
#'
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom sf st_as_sf st_crs st_geometry_type
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
#'   \item allows binding \code{sf} objects with different geometry types
#' }
#' Remarks about matching geometry columns of input \code{sf} objects:
#' \itemize{
#'   \item If the attribute columns are identically named, but differ by
#'   position within
#'   each \code{sf} object, set \code{use.names = TRUE} and \code{fill = TRUE}.
#'   \item If the geometry and/or attribute columns are differently named, but
#'   have
#'   same position within every \code{sf} object, set \code{use.names = FALSE}.
#'   \item In any case the active geometry columns of the listed
#'   \code{sf}-objects are matched due to the identical internal naming of
#'   these columns.
#' }
#'
#' @seealso \href{../doc/rbindlist_issues.html}{\code{vignette("rbindlist_issues")}}
#'
#' @examples library(sf)
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
#' # a list of sf-objects having ...
#' l <- lapply(
#'   1:3,
#'   function(x){data.frame(nc[x, 1:3]) %>% setNames(., c(paste0(names(.), "_", x))) %>% st_sf()}
#' )
#' # ... differently named, but identically positioned geometry & attribute columns
#' sapply(l, names) %>% t()
#' # by setting use.name = FALSE such a list of sf-objects is easy to stack:
#' st_rbindlist(l, use.name = FALSE)
#'
#' # from which listed element / sf-object do the stacked features originate?
#' l <- list(A = nc[1, 1:3], B = NULL, C = nc[2:3, 1:3])
#' st_rbindlist(l, idcol = "ID")
#'
#' # list of sf-objects with identically named, but differently positioned
#' # geometry and attribute columns (the latter might not be included in
#' # each listed sf-object) ...
#' l <- list(nc[1, NULL], nc[2, 1], nc[3, 2:3])
#' # ... requires use.name = TRUE and fill = TRUE:
#' st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE)
#'
#' # list of sf-objects with differently named and positioned geometry columns,
#' # and (if existing) identically named attribute columns ...
#' l <- lapply(
#'   seq_along(l),
#'   function(x) st_set_geometry(l[[x]], paste0("geom_", x))
#'   )
#' sapply(l, attr, "sf_column") # geometry column names of listed sf-objects
#'
#' # ... can be handled also by setting use.name = TRUE and fill = TRUE:
#' st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE)
#' # this works because st_rbindlist() internally renames all active geometry columns identically!
#'
#'# note: as shown above the name of the returned geometry column ...
#'# ... is inherited by default from the first listed sf-object. The user ...
#'# ... can set an alternative geometry column name:
#'st_rbindlist(l, use.name = TRUE, fill = TRUE, idcol = TRUE,
#'             geometry_name = "geom_named_by_user")
st_rbindlist <- function(l, ..., geometry_name = NULL) {
  if (!is.list(l) || is.data.frame(l)) {
    stop("input is ", class(l)[1L], " but should be a plain list of sf objects to be stacked", call. = FALSE)
  }
  if(!(is.null(geometry_name) | (is.character(geometry_name) & length(geometry_name) == 1))){
    stop("geometry_name must be either NULL or a single character string", call. = FALSE)
  }
  is_not_null <- !vapply(l, is.null, logical(1))
  if (!all(vapply(l[is_not_null], inherits, what = "sf", logical(1)))) {
    stop("not all listed objects are of the class sf", call. = FALSE)
  }
  if(!any(is_not_null)) {
    stop("no sf objects included in input list", call. = FALSE)
  }
  if (length(unique(lapply(l[is_not_null], sf::st_crs))) > 1) {
    stop("arguments have different crs", call. = FALSE)
  }

  # name of geometry column of the first listed sf-object
  first_geometry_name <- attr(l[is_not_null][[1]], "sf_column")

  # homogenize name of active geometry columns
  if(length(unique(vapply(l[is_not_null], attr, which = "sf_column", character(1)))) > 1){
    tmp_geometry_name <- uuid::UUIDgenerate()
    for (i in seq_along(l)[is_not_null]) {
      sf::st_geometry(l[[i]]) <- tmp_geometry_name
    }
  }

  # homogenize geometry types
  if (length(unique(vapply(l[is_not_null], sf::st_geometry_type, by_geometry = FALSE, factor(1)))) > 1) {
    for (i in seq_along(l)[is_not_null]) {
      class(l[[i]][[attr(l[[i]], "sf_column")]])[[1]] <- "sfc_GEOMETRY"
    }
  }

  sf <- sf::st_as_sf(data.table::rbindlist(l, ...))
  sf <- sf[seq_len(nrow(sf)), ]
  class(sf) <- c("sf", "data.frame")

  if(is.null(geometry_name)){
    sf::st_geometry(sf) <- first_geometry_name
  }else{
    sf::st_geometry(sf) <- geometry_name
  }

  return(sf)
}
