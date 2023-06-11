#' Return properties of layers in a datasource as \code{data.frame}
#'
#' @param x any input that works for \code{\link[sf]{st_layers}}'s argument
#' \code{dsn} or an object of class \code{sf_layers}
#' @param ... arguments of the function \code{\link[sf]{st_layers}} except
#' \code{dsn} if \code{x} is equivalent to \code{dsn} else ignored
#' @param drop_driver logical; \code{TRUE} (default) drops information about
#' the driver, \code{FALSE} adds the driver as column to the returned
#' \code{data.frame}
#' @param warn logical; \code{TRUE} (default) warns if no layer is available,
#' \code{FALSE} does not warn in such a case
#'
#' @return if no layer is available \code{NULL}, else a \code{data.frame}
#' resembling the \code{print}ed return of \code{\link[sf]{st_layers}} with
#' columns
#' \describe{
#'   \item{layer_name}{name of the layer}
#'   \item{geometry_type or geometry_types}{for each layer a string listing its
#'   geometry type(s)}
#'   \item{features}{number of features (if reported; see argument
#'   \code{do_count} of \code{\link[sf]{st_layers}})}
#'   \item{fields}{number of fields}
#'   \item{crs}{short name of each layer's \code{crs}}
#'   \item{driver (optional)}{name of driver, if argument \code{drop_driver} has
#'   been set to \code{FALSE}}
#' }
#'
#' @importFrom sf st_layers
#'
#' @export
#'
#' @examples library(sf)
#' dsn <- system.file("shape", package = "sf")
#' st_layers_as_df(x = dsn, drop_driver = FALSE)
#'
#' # properties of layers of multiple datasources in one data.frame
#' some_dsn <- list.files(system.file("gpkg", package = "sf"), full.names = TRUE)
#' multiple_dsn <- c(dsn, some_dsn)
#' df <-
#'   as.list(multiple_dsn) %>%
#'   setNames(multiple_dsn) %>%
#'   lapply(st_layers_as_df, drop_driver = FALSE) %>%
#'   data.table::rbindlist(use.names = FALSE, idcol = "dsn")
st_layers_as_df <- function(x, ..., drop_driver = TRUE, warn = TRUE) {
  if (!isTRUE(drop_driver) & !isFALSE(drop_driver)) {
    stop("drop_driver must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (!(isTRUE(warn) | isFALSE(warn))) {
    stop("warn must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (!inherits(x, "sf_layers")) {
    x <- sf::st_layers(dsn = x, ...)
  }
  n_gt <- max(sapply(x$geomtype, length))
  x$geomtype <- vapply(x$geomtype, function(x) paste(x, collapse = ", "), "")
  driver <- x$driver
  x$driver <- NULL
  x$features[x$features < 0] <- NA
  if (length(x$name) == 0) {
    if (warn) {
      warning("\nAvailable layers:\n<none>\n")
    }
    return(NULL)
  } else {
    crs <- sapply(x$crs, function(crs) crs$input)
    x$crs <- crs
    df <- data.frame(unclass(x))
    gt <- if (n_gt > 1) {
      "geometry_types"
    } else {
      "geometry_type"
    }
    names(df) <- c("layer_name", gt, "features", "fields", "crs_name")
    if (!drop_driver) {
      df$driver <- rep(driver, nrow(df))
    }
    return(df)
  }
}
