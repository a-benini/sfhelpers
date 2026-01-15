#' Return common bounding of multiple spatial objects or aligned bounding of
#' single spatial object
#'
#' @return object of the class \code{bbox} equal to the return of
#' \code{\link[sf]{st_bbox}} representing in the case of
#' \itemize{
#'   \item \code{st_bbox_common()} or \code{st_bbox_list()} the common bounding
#'   of several (\code{list}ed) input objects
#'   \item \code{st_bbox_aligned()} the aligned bounding of a single input object.
#'   Returned values \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax} match
#'   \eqn{k} \eqn{\cdot} \code{alignment}-argument, where \eqn{k} is an integer.
#'   The input \code{obj}ect is positioned within these four coordinates.
#' }
#'
#' @param ... objects that can be coerced to bounding boxes with
#' \code{\link[tmaptools]{bb}} or with \code{\link[sf]{st_bbox}}
#'
#' @importFrom tmaptools bb
#' @importFrom sf st_crs st_bbox
#'
#' @examples
#' library(sf)
#' library(stars)
#' library(terra)
#'
#' # get /create spatial objects of different classes and extent:
#' sf <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#' logo <- rast(system.file("ex/logo.tif", package = "terra"))
#' rast <- rast(
#'   val    = as.vector(logo$red),
#'   nrows  = nrow(logo),
#'   ncols  = ncol(logo),
#'   extent = c(-77, -75, 33, 35),
#'   crs    = st_crs(sf)$wkt
#'   )
#' stars <- st_as_stars(rast) %>% st_set_bbox(., st_bbox(.) + rep(2, 4)) %>% st_flip()
#'
#' # common extent / bbox:
#' (bbox_common <- st_bbox_common(sf, rast, stars))
#'
#' # map objects within their common extent:
#' library(tmap)
#' tm_shape(stars, bbox = bbox_common) +
#'   tm_raster(title = "stars", style = "cont", palette = "Spectral") +
#'   tm_shape(rast) +
#'   tm_raster(title = "SpatRaster", style = "cont", palette = "viridis") +
#'   tm_shape(sf) + tm_borders(col = "magenta") +
#'   tm_layout(legend.stack = "horizontal", legend.position = c(0.05, 0.05))
#'
#' # list of sf objects
#' l <- lapply(1:nrow(sf), function(x) sf[x, ])
#'
#' # bbox of all listed sf objects
#' st_bbox_list(l)
#'
#' # the bbox of the original geometry set (sf) and the bbox of its listed objects are identical:
#' all.equal(st_bbox_list(l), st_bbox(sf))
#'
#' # get pretty / aligned bbox of an object
#' (bb_aligned <- st_bbox_aligned(sf, alignment = 2.5))
#'
#' # dividing the aligned bbox by the alignment value results in integer values
#' bb_aligned / 2.5
#'
#' # plot used object within extent of aligned bbox
#' plot(st_geometry(sf), extent = bb_aligned, col = "gray")
#'
#' # add to plot grid matching the alignment (= cellsize)
#' grid_aligned <- st_make_grid(bb_aligned, cellsize = 2.5)
#' plot(grid_aligned, border = "red", lwd = 2, add = TRUE)
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
#' \code{\link[tmaptools]{bb}} or with \code{\link[sf]{st_bbox}}
st_bbox_list <- function(l) {
  bb_or_st_bbox <- function(x) {
    suppressWarnings(default <- try(tmaptools::bb(x), silent = TRUE))
    if (inherits(default, "try-error")) {
      suppressWarnings(default <- try(tmaptools::bb(sf::st_bbox(x)), silent = TRUE))
    }
    if (inherits(default, "try-error")) {
      stop("at least one of the listed arguments can not be coerced to bounding boxes with tmaptools::bb() or with sf::st_bbox()", call. = FALSE)
    } else {
      default
    }
  }
  l_bb <- lapply(l, bb_or_st_bbox)
  if (any(vapply(l_bb, function(x) { sf::st_crs(x) != sf::st_crs(l_bb[[1]]) }, logical(1)))) {
    stop("arguments have different crs", call. = FALSE)
  }
  mat_bb <- vapply(l_bb, invisible, numeric(4))
  if (anyNA(mat_bb)) {
    stop("at least one of the listed arguments has one or more missing values for its bbox object", call. = FALSE)
  }
  xmin <- min(mat_bb["xmin", ])
  ymin <- min(mat_bb["ymin", ])
  xmax <- max(mat_bb["xmax", ])
  ymax <- max(mat_bb["ymax", ])
  sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = sf::st_crs(l_bb[[1]]))
}
#'
#' @export
#' @rdname st_bbox_common
#' @param obj any object that can be coerced to a bounding box with
#' \code{\link[sf]{st_bbox}}
#' @param alignment single positive numeric value, corresponding to the units of
#' the CRS, which the input \code{obj}ect has.
st_bbox_aligned <- function(obj, alignment){
  if (!(is.numeric(alignment) & length(alignment) == 1L)) {
    stop("alignment muss be a singel positive numeric value", call. = FALSE)
  }
  if (alignment <= 0) {
    stop("alignment muss be a singel positive numeric value", call. = FALSE)
  }
  bb <- sf::st_bbox(obj) / alignment
  bb <- sf::st_bbox(c(floor(bb$xmin), floor(bb$ymin), ceiling(bb$xmax), ceiling(bb$ymax)), crs = sf::st_crs(obj)) * alignment
  return(bb)
}
