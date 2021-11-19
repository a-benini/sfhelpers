#' The Second of Two Overlapping Polygon Layers for Demonstration Purposes
#'
#' This polygon layer (\code{poly_2}) overlaps spatially with an other one
#' \code{\link{poly_1}}. They have one homonymous attribute \code{A} in common
#' and each of them is composed of four adjacent squares. These layers serve the
#' purpose of demonstrating the workings of the functions
#' \code{\link{st_erase_robust}} and  \code{\link{st_or}}.
#'
#' @format Attributes:
#' \describe{
#'   \item{A}{character}
#'   \item{D}{integer}
#'   \item{E}{year}
#' }
"poly_2"
