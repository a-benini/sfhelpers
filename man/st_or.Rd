% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_or.R
\name{st_or}
\alias{st_or}
\title{\code{sf} equivalent of QGIS Union}
\usage{
st_or(x, y, dim = 2, suffix = c(".x", ".y"), suffix.all = FALSE, ...)
}
\arguments{
\item{x}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{y}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{dim}{integer: A combination of 0, 1, and/or 2 (default) that constrains
the dimension(s) of the returned geometries. 0 for points, 1 for lines, 2 for
surfaces.}

\item{suffix}{length 2 character vector (default \code{c(".x", ".y")})
attached as suffix to attribute headings inherited from argument \code{x}
resp. from \code{y}}

\item{suffix.all}{\code{TRUE} suffixes all attribute headings inherited from
arguments \code{x} and \code{y} according to \code{suffix}; \code{FALSE}
(default) suffixes only homonymous attribute headings.}

\item{...}{arguments passed on to \code{\link[s2]{s2_options}}}
}
\value{
geometry set containing the intersection of \code{x} and \code{y} and
the non-overlapping parts of \code{x} and \code{y}. The attribute table is
filled with attribute values from the respective original geometry set for
non-overlapping features, and attribute values from both geometry sets for
overlapping features. The dimension(s) of the returned geometries is/are
determined with the argument \code{dim}.
}
\description{
\code{sf} equivalent of QGIS Union
}
\details{
\code{st_or()} consists at its core of code presented by
\href{https://gis.stackexchange.com/questions/251440/ogr2ogr-equivalent-of-qgis-union/251575#251575}{TimSalbim on gis.stackexchange}
. In addition to its precursor this version of \code{st_or()} includes:
\itemize{
\item a more robust version of the internal function \code{st_erase()}
equivalent to \code{\link{st_erase_robust}}
\item the ability to handle homonymous attribute variables of both input
geometry sets (s. below examples)
\item the possibility to give customized suffixes to attribute variables
corresponding to the geometry set they originated from (s. below examples)
\item handling of input layers with differently named geometry columns
and/or being totally overlapped by the other input layer.
\item restricts geometric operations to geometries of the input layers
\code{x} and \code{y} overlapping those of the other input
layer (same as \code{\link{st_erase_robust}}).
}
}
\examples{
library(dplyr)
library(sf)

st_or(poly_1, poly_2) \%>\% plot()

st_or(poly_1, poly_2) \%>\% st_drop_geometry()

# to avoid repeated warning messages triggered by non-spatially constant ...
# ... attribute variables, set them all to "constant":
st_agr(poly_1) <- "constant"
st_agr(poly_2) <- "constant"

# Give customized suffixes to homonymous attributes of layers x and y:
st_or(poly_1, poly_2, suffix = c("_poly_1", "_poly_2")) \%>\% plot()

# If only homonymous attributes from one layer should get a suffix, set ...
# ... the suffix for the other layer to an empty string:
st_or(poly_1, poly_2, suffix = c("", ".y")) \%>\% names()

# If all attributes attributes of both layers x and y should get a
# ... layer-specific suffix, set suffix.all = TRUE:
st_or(poly_1, poly_2, suffix.all = TRUE) \%>\% names()

# If only all attributes from one layer should get a suffix, set the suffix ...
# ... for the other layer to an empty string and set suffix.all = TRUE:
st_or(poly_1, poly_2, suffix = c("", ".y"), suffix.all = TRUE) \%>\% names()

# create two layers with overlapping linestrings:
ls1 <- st_linestring(cbind(c(0, 1, 1, 0), c(0:3)))
ls2 <- st_linestring(cbind(c(2, 1, 1), c(0, 0, 3)))
ls3 <- st_linestring(cbind(c(0, 0.5, 0.5, 0), c(0, 0, 2.5, 2)))
A <- st_sf(id_A = 1, A = "A", geom = st_sfc(ls1), agr = "constant")
B <- st_sf(id_B = 1:2, B = "B", geom = st_sfc(ls2, ls3), agr = "constant")

plot(st_geometry(A), col = "gray", lwd = 4, extent = st_bbox_common(A, B))
plot(st_geometry(B), col = "red", lty = 2, add = TRUE)
legend("right", legend = c("A", "B"), col = c("gray", "red"), lwd = c(4, 1), lty = c(1, 2))

# when both input layers consist of linestings, and if the default specification ...
# ... dim = 2 (for surfaces / (multi)polygons) is used, a sf-object with zero ...
# ... rows will be returned:
st_or(A, B)

# to get lines returned set dim = 1:
comb_dim_1 <- st_or(A, B, dim = 1) \%>\%
  mutate(comb = ifelse(is.na(A), "B", ifelse(is.na(B), "A", "A+B")))
plot(comb_dim_1[, "comb"], lwd = 3, key.pos = 1, main = "dim = 1: only lines")

# for returning lines and points where lines cross or touch lines of the other ...
# ... input layer, set dim = c(0, 1):
comb_dim_0_1 <- st_or(A, B, dim = c(0, 1)) \%>\%
  mutate(comb = ifelse(is.na(A), "B", ifelse(is.na(B), "A", "A+B")))
plot(comb_dim_0_1[, "comb"], lwd = 3, cex = 2, key.pos = 1, main = "dim = c(0, 1): points & lines")

all.equal(
  st_or(A, B, dim = c(0, 1)), # returns points & lines
  st_or(A, B, dim = c(0, 1, 2)) # returns points, lines (& if available surfaces)
)
}
