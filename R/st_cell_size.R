#' Return cell size of a \code{SpatRaster} or \code{stars} object
#'
#' @param x object of class \code{SpatRaster}, \code{stars} or \code{stars_proxy}
#' @param warn logical; if \code{TRUE} (default) warn if \code{x} has no CRS or
#' geographic coordinates
#' @param drop_units logical; if \code{TRUE} returns output without units,
#' if \code{FALSE} (default) returns output with units if \code{x} has projected coordinates
#'
#' @importFrom sf st_crs st_is_longlat
#' @importFrom stars st_res
#' @importFrom terra res
#'
#' @returns A single numeric value representing the cell size of input \code{x}
#' equivalent to the product of the input's x and y raster resolution. By default
#' and if \code{x} has projected coordinates, the cell size is returned with
#' the units according to input \code{x}'s CRS.
#'
#' @export
#'
#' @examples library(terra)
#' library(sf)
#'
#' # a raster with projected coordinates
#' r_projected <- rast(
#'   nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
#'   crs = "EPSG:2026"
#' )
#' values(r_projected) <- sample(5, ncell(r_projected), replace = TRUE)
#' # plot(r_projected)
#'
#' # units of projected coordinates
#' st_crs(r_projected)$units
#' # pixel resolution of raster
#' res(r_projected)
#' # cell size of raster-pixel
#' st_cell_size(r_projected)
#' # cell size without units
#' st_cell_size(r_projected, drop_units = TRUE)
#'
#' # a raster with geographic coordinates
#' r_geographic <- rast(
#'   nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
#'   crs = "EPSG:4326"
#' )
#' values(r_geographic) <- sample(5, ncell(r_geographic), replace = TRUE)
#' # plot(r_geographic)
#'
#' # unavailable units of geographic coordinates
#' st_crs(r_geographic)$units
#' # pixel resolution of raster
#' res(r_geographic)
#' # if raster has geographic coordinates
#' # cell size is return by default without units and with a warning
#' st_cell_size(r_geographic)
#' # warning can be avoided
#' st_cell_size(r_geographic, warn = FALSE)
st_cell_size <- function(x, warn = TRUE, drop_units = FALSE) {
  # if x is not of the class "SpatRaster", "stars" or "stars_proxy" throw a corresponding error message
  if (!inherits(x, c("SpatRaster", "stars", "stars_proxy"))) {
    stop("the argument x must be of the class SpatRaster, stars or stars_proxy", call. = TRUE)
  }
  if (!isTRUE(warn) & !isFALSE(warn)) {
    stop("argument warn must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(drop_units) & !isFALSE(drop_units)) {
    stop("argument drop_units must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (inherits(x, "SpatRaster")) {
    cs <- prod(terra::res(x))
  } else {
    cs <- prod(stars::st_res(x))
  }
  if (warn) {
    warning_text <- ""
    if (isTRUE(sf::st_is_longlat(x))) {
      warning_text <- "x has geographic coordinates"
    }
    if (is.na(sf::st_crs(x))) {
      warning_text <- "x has no CRS"
    }
    if (!drop_units & nchar(warning_text) > 0) {
      warning_text <- paste0(warning_text, ", thus raster cell size is returned without units")
    }
    if (nchar(warning_text) > 0) {
      warning(warning_text, call. = FALSE, immediate. = TRUE)
    }
  }
  crs_units <- sf::st_crs(x)$units
  if (!(isTRUE(is.na(crs_units)) | isTRUE(is.null(crs_units))) & !drop_units) {
    units(cs) <- paste0(crs_units, "^2")
  }
  return(cs)
}
