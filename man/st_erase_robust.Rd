% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_erase_robust.R
\name{st_erase_robust}
\alias{st_erase_robust}
\title{Erasing from geometry set \code{x} all parts overlapped by geometry set \code{y}}
\usage{
st_erase_robust(x, y, ...)
}
\arguments{
\item{x}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{y}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{...}{arguments passed on to \code{\link[s2]{s2_options}}}
}
\value{
Returns all parts of geometry set \code{x} not overlapped by
geometry set \code{y}
}
\description{
Erasing from geometry set \code{x} all parts overlapped by geometry set \code{y}
}
\details{
The example section of the \code{sf}-package help page on geometric
operations on pairs of simple feature geometry sets
(\code{\link[sf]{geos_binary_ops}}) presents code for a helper function that
erases all parts from geometry set \code{x} overlapped by geometry set \code{y}.
This function sometimes works as expected, sometimes it doesn't. (s. examples
below).

Even when both input layers \code{x} and \code{y} consist of valid geometries
(which can be checked with \code{\link[sf]{st_is_valid}}, respectively fixed
with \code{\link[sf]{st_make_valid}}), the a. m. helper function can still
throw an error. This is often due to internally applying
\code{st_union(st_combine())} to \code{y}. \code{\link[sf]{st_combine}} is
sometimes also the cause for returning incorrectly erased geometry. To avoid
these issues, \code{st_erase_robust()} uses \code{\link[sf]{st_union}} only.
Leaving out \code{st_combine()} may add to the complexity of the
involved geometries, thus potentially increasing the run time. To counteract
this, \code{st_erase_robust()} detects with the help of
\code{\link[sf]{st_intersects}} which geometries from the input \code{x} and
\code{y} overlap with those of the other layer and applies geometric
operations only to such geometries.

When using the helper function from the a.m. \code{sf} help page with input
having longlat degrees CRS, switching off spherical geometry (s2) by setting
\code{\link[sf]{sf_use_s2}} to \code{FALSE} can help to overcome issues.
But this isn't a safe workaround for all issues caused by \code{st_combine()}
when it comes to erasing.
}
\examples{
library(sf)

# find code of helper function st_erase():
# ?geos_binary_ops

# copy function code:
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

# get some demo data:
nc   <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
ext  <- st_bbox(nc) + rep(c(-0.1, 0.1), each = 2)
grid <- st_make_grid(ext) \%>\% st_sf(id = seq_along(.), geom = ., agr = "constant")

st_is_longlat(nc) # demo data has a longlat degrees crs

sf_use_s2(TRUE)
# check if helper function works with demo data:
try(st_erase(grid, nc))

# internal processing of input y (nc) returns the same error as st_erase():
try(st_union(st_combine(nc)))

# st_erase_robust() can handle this:
st_erase_robust(grid, nc) \%>\% plot()
}
