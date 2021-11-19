#' Return common bounding of multiple spatial objects
#'
#' @return object of the class \code{bbox} equal to the return of
#' \code{\link[sf]{st_bbox}} but representing the common bounding of several
#' input (\code{list}ed) objects
#'
#' @param ... objects that can be coerced to bounding boxes with
#' \code{\link[tmaptools]{bb}}
#'
#' @importFrom tmaptools bb
#' @importFrom sf st_crs st_bbox
#'
#' @examples
#' library(sf)
#' library(sp)
#' library(raster)
#' library(stars)
#' nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#'
#' # create spatial objects of different classes and extent:
#' sf    <- nc[4:8, ]
#' sp    <- sf::as_Spatial(nc[96:100, ])
#' logo  <- raster(system.file("external/rlogo.grd", package = "raster"))
#' ext   <- extent(-77.5, -76, 33, 34.5)
#' r     <- raster(nrows = nrow(logo), ncols = ncol(logo), ext = ext)
#' r[]   <- logo[]
#' stars <- st_as_stars(r) %>% st_set_crs(st_crs(sf))
#' r     <- as(stars, "Raster") %>% setExtent(ext = ext + rep(1.5, 4))
#'
#' # common extent / bbox:
#' (bbox_common <- st_bbox_common(sf, sp, r, stars))
#'
#' # map objects within their common extent:
#' library(tmap)
#' tm_shape(r, bbox = bbox_common) + tm_raster(title = "r", style = "cont") +
#'   tm_shape(stars) + tm_raster(title = "stars", style = "cont", palette = "viridis") +
#'   tm_shape(sf) + tm_borders() +
#'   tm_shape(sp) + tm_borders(col = "red")
#'
#' l <- lapply(1:nrow(nc), function(x) nc[x, ])
#'
#' st_bbox_list(l)
#'
#' # the bbox of the original geometry set (nc) and the bbox of its listed objects are identical:
#' all.equal(st_bbox_list(l), st_bbox(nc))
#'
#' @export st_bbox_common
st_bbox_common <- function(...) {
  l <- list(...)
  st_bbox_list(l)
  }
#'
#' @export
#' @rdname st_bbox_common
#' @param l \code{list} of objects that can be coerced to bounding boxes with
#' \code{\link[tmaptools]{bb}}
st_bbox_list <- function(l) {
  l_bb <- lapply(l, tmaptools::bb)
  l_crs <- lapply(l_bb, sf::st_crs)
  if (!all(vapply(l_crs, function(x) {x == l_crs[[1]]}, logical(1)))) {
    stop("arguments have different crs", call. = TRUE)
  }
  mat_bb <- vapply(l_bb, invisible, numeric(4))
  if (anyNA(mat_bb)) {
    stop("at least one of the listed arguments has one or more missing values for its bbox object", call. = TRUE)
  }
  xmin <- min(mat_bb["xmin", ])
  ymin <- min(mat_bb["ymin", ])
  xmax <- max(mat_bb["xmax", ])
  ymax <- max(mat_bb["ymax", ])
  sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = l_crs[[1]])
  }
