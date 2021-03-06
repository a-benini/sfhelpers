% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_erase_robust.R
\name{st_erase_robust}
\alias{st_erase_robust}
\title{Erasing from geometry set \code{x} all parts overlapped by geometry set \code{y}}
\usage{
st_erase_robust(x, y, check_overlap = FALSE, ...)
}
\arguments{
\item{x}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{y}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{check_overlap}{\code{TRUE} detects those geometries included in
\code{x} and \code{y} which overlap with the other input layer and applies
geometric operations only on these; \code{FALSE} (default) applies geometric
operations on all geometries (s. Details).}

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

Even when both input layers \code{x} and \code{y} consist of valid
geometries (which can be checked with \code{\link[sf]{st_is_valid}},
respectively fixed with \code{\link[sf]{st_make_valid}}), the a. m. helper
function can still throw an error. This is often due to internally applying
\code{st_union(st_combine())} to \code{y}. So, currently
\code{st_erase_robust()} uses in case of failure as a second option
\code{\link[sf]{st_union}} only. Leaving out \code{\link[sf]{st_combine}}
may very well add to the complexity of the involved geometries; thus the
second option may be rather slow. For this reason, if the coordinates of the
input are longlat degrees, setting \code{\link[sf]{sf_use_s2}} to
\code{FALSE} can help to speed up \code{st_erase_robust()} (s.examples below).

Note that with the recent versions of \code{sf} (>= 1.0-1), although
\code{st_union(st_combine())} might very well return an invalid geometry,
experience made so far shows that this has become less of an obstacle for
further prepossessing with \code{\link[sf]{st_difference}}.

In case of a significant proportion of non-overlapping geometries, setting
\code{check_overlap} to \code{TRUE} can shorten run time. As the internal
distinction of overlapping and non-overlapping geometries relies on
\code{\link[sf]{st_intersects}}, which itself is time consuming,
\code{check_overlap = TRUE} involves a trade off: If all geometries overlap,
\code{check_overlap = TRUE} will only increase the run time. On the other
hand, the higher the proportion of non-overlapping geometries, the more this
argument specification shortens the processing time by leaving out
unnecessary geometric operations. Ideally the user has previous knowledge
about how little or how much the input layers \code{x} and \code{y} overlap
with each other in order to apply \code{check_overlap} in a informed way. In
general, setting \code{check_overlap} to \code{TRUE} is advantageous when
\code{x} or \code{y} are large geometry sets and include more complex
geometries.
}
\examples{
library(sf)

# find code of helper function st_erase():
\dontrun{
?geos_binary_ops
}

# copy function code:
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

# get some demo data:
nc   <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
ext  <- st_bbox(nc) + rep(c(-0.1, 0.1), each = 2)
grid <- st_make_grid(ext) \%>\% st_sf(id = seq_along(.), geom = ., agr = "constant")

st_is_longlat(nc) # demo data has a longlat degrees crs

sf_use_s2(TRUE)
# check if helper function works with demo data:
\dontrun{
st_erase(grid, nc)
}

# internal processing of input y (nc) returns the same error as st_erase():
\dontrun{
st_union(st_combine(nc))
}

# st_erase_robust() can handle this:
st_erase_robust(grid, nc) \%>\% plot()

sf_use_s2(FALSE) # if spherical geometry (s2) is switched off,
# sfhelpers::st_erase_robust() & helper function st_erase() should work and
# return the same (st_erase() might still not work on some Linux operation
# systems):
\dontrun{
all.equal(
 st_erase_robust(grid, nc),
 st_erase(grid, nc)
)

# because internal handling of input y (nc) won't throw an error:
st_union(st_combine(nc))
}
}
