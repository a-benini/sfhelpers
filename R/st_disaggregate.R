#' Disaggregate GEOMETRYCOLLECTION, MULTIPOINT, MULTILINESTRING, MULTIPOLYGON
#' into sub-geometries
#'
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param only_geometrycollection logical; if \code{TRUE} only
#' \code{GEOMETRYCOLLECTION}s are disaggregated into their components,
#' which can include \code{MULTI}-part geometries. If \code{FALSE} (default)
#' \code{MULTI}-part geometries originating from \code{GEOMETRYCOLLECTION}s or
#' included as such in \code{x} are further disaggregated into their
#' sub-geometries.
#' @param keep_empty logical; if \code{TRUE} empty geometries are kept, if
#' \code{FALSE} (default) they are dropped.
#' @param warn logical; if \code{TRUE}, warn if attributes are assigned to sub-geometries
#'
#' @importFrom sf st_agr st_geometry st_is_empty st_is st_sfc st_drop_geometry
#' st_crs st_sf st_geometry_type st_cast st_set_geometry
#' @importFrom uuid UUIDgenerate
#'
#' @return an object of class \code{sf}, if the input \code{x} itself is an
#' \code{sf}-object, else a object of class \code{sfc}, containing sub-geometries
#' of the disaggregated input geometries. The level of disaggregation is
#' determined by the argument \code{only_geometrycollection}.
#'
#' @details When \code{GEOMETRYCOLLECTION}s, \code{MULTIPOINT}s,
#' \code{MULTILINESTRING}s or \code{MULTIPOLYGON}s are combined in a geometry
#' set, possibly together with single part geometries, then disaggregating them
#' can be a cumbersome task. In most situations, some combination of filtering
#' by geometry type, \code{\link[sf]{st_collection_extract}} and/or
#' \code{\link[sf]{st_cast}}, will do the job. However, \code{st_disaggregate}
#' achieves that much more smoothly (for comparison s. examples below).
#'
#' In particulate, extracting single and multi-part geometries as such when
#' contained in the very same \code{GEOMETRYCOLLECTION} (i.e., without
#' disaggregating the multi-part geometries into their sub-geometries) can be
#' done with \code{st_disaggregate}, but neither with
#' \code{st_collection_extract} nor with \code{st_cast}. Furthermore,
#' \code{st_disaggregate} can dissolve a \code{sfg} object consisting of
#' a multi-part geometry into all its sub-geometries, while \code{st_cast} would
#' only return the first of the sub-geometry.
#'
#' @export
#'
#' @examples library(sf)
#'
#' pt1 <- st_point(c(-10, -10))
#' pt2 <- st_point(c(10, -10))
#' pt3 <- st_point(c(10, 10))
#' pt4 <- st_point(c(-10, 10))
#' pt5 <- st_point(c(6, -5))
#' mpt <- st_multipoint(c(pt1, pt2, pt5))
#' ls1 <- st_linestring(c(pt1, pt2, pt3)) * 0.7 + c(1, 0)
#' ls2 <- st_linestring(c(pt1, pt4)) * 0.9 - c(1, 0)
#' ls3 <- st_linestring(c(pt1, pt3, pt4)) * 0.2 + c(-5, 6)
#' mls <- st_multilinestring(list(ls2, ls3))
#' pl1 <- st_polygon(list(rbind(pt1, pt3, pt4, pt1))) * 0.2 + c(-2, 3)
#' pl2 <- st_polygon(list(rbind(pt1, pt2, pt3, pt4, pt1) * 0.3,
#'                        rbind(pt1, pt2, pt3, pt1) * 0.1)) - c(2, 3)
#' pl3 <- st_polygon(list(rbind(pt1, pt5, pt3, pt4, pt1))) * 0.2 + c(3, 3)
#' mpl <- st_multipolygon(list(pl2, pl3))
#' gc1 <- st_geometrycollection(list(mpt, ls1, mpl))
#' gc2 <- st_geometrycollection(list()) # empty geometry
#'
#' # function plotting sfc with distinct color for each geometry
#' plot_sfc <- function(sfc) {
#'   sfc %>% plot(., col = seq_along(.), border = seq_along(.), lwd = 2, cex = 1.5)
#' }
#'
#' plot(gc1)
#' st_disaggregate(gc1) %>% summary()
#' st_disaggregate(gc1) %>% plot_sfc()
#' st_disaggregate(gc1, only_geometrycollection = TRUE) %>% summary()
#' st_disaggregate(gc1, only_geometrycollection = TRUE) %>% plot_sfc()
#'
#' mixed_bag <- st_sfc(pt3, pt4, mls, pl1, gc1, gc2)
#' mixed_bag %>% plot_sfc()
#' mixed_bag %>% summary()
#' st_disaggregate(mixed_bag) %>% summary()
#' st_disaggregate(mixed_bag, only_geometrycollection = TRUE) %>% summary()
#' st_disaggregate(mixed_bag, keep_empty = TRUE) %>% summary()
#' st_disaggregate(mixed_bag, keep_empty = TRUE) %>% st_is_empty() %>% summary()
#'
#' mixed_bag_without_gc <- st_sfc(pt3, pt4, mpt, ls1, mls, pl1, mpl)
#' mixed_bag_without_gc %>% plot_sfc()
#' mixed_bag_without_gc %>% summary()
#' st_disaggregate(mixed_bag_without_gc) %>% summary()
#' # if no geometry collection is involved and only_geometrycollection = TRUE ...
#' # ... then no multi-part geometry is disaggregated:
#' all.equal(
#'   mixed_bag_without_gc,
#'   st_disaggregate(mixed_bag_without_gc, only_geometrycollection = TRUE)
#' )
#'
#' # compare st_disaggregate() to st_cast() on mixed sets of single and multi-part-geometries:
#' mixed_ls_mls <- st_sfc(ls1, mls)
#' mixed_ls_mls %>% summary()
#' st_disaggregate(mixed_ls_mls) %>% summary()
#' st_cast(mixed_ls_mls, "LINESTRING") %>% summary() # only 1st sub-geometry of each multi-part kept.
#' st_cast(mixed_ls_mls, "MULTILINESTRING") %>% st_cast("LINESTRING") %>% summary() # this trick works!
#' # but st_disaggregate() is more elegant:
#' st_equals(
#'   st_disaggregate(mixed_ls_mls),
#'   st_cast(mixed_ls_mls, "MULTILINESTRING") %>% st_cast("LINESTRING")
#' )
#'
#' # compare usage of st_disaggregate() and st_collection_extract() for extracting
#' # single geometries of the same dimension from a geometry collection containing
#' # single as well as multi-part geometries of that very dimension:
#' gc <- st_geometrycollection(list(pt1, pt2, ls1, mls))
#' st_disaggregate(gc) %>% summary()
#' st_collection_extract(gc, "LINESTRING") %>% summary()
#' st_disaggregate(gc) %>% .[st_dimension(.) == 1] %>% summary()
#' st_collection_extract(gc, "LINESTRING") %>% st_cast("LINESTRING") %>% summary()
#' st_equals(
#'   st_disaggregate(gc) %>% .[st_dimension(.) == 1],
#'   st_collection_extract(gc, "LINESTRING") %>% st_cast("LINESTRING")
#' )
#'
#' # extracting single and multi-part geometries (of the same dimension) as such
#' # from a geometry collection only works with st_disaggregate():
#' st_disaggregate(gc, only_geometrycollection = TRUE) %>% .[st_dimension(.) == 1] %>% summary()
st_disaggregate <- function(x, only_geometrycollection = FALSE, keep_empty = FALSE, warn = TRUE) {
  if (!inherits(x, c("sf", "sfc", "sfg"))) {
    stop("the argument x must be of the class sf, sfc or sfg", call. = TRUE)
  }
  if (!(isTRUE(only_geometrycollection) | isFALSE(only_geometrycollection))) {
    stop("only_geometrycollection must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (!(isTRUE(keep_empty) | isFALSE(keep_empty))) {
    stop("keep_empty must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  if (!(isTRUE(warn) | isFALSE(warn))) {
    stop("warn must be a single logical value: TRUE or FALSE", call. = FALSE)
  }

  # warn if not all attribute variables are constant
  if (warn & inherits(x, "sf")) {
    if (!isTRUE(all(sf::st_agr(x) == "constant"))) {
      warning(
        "repeating attributes for all sub-geometries for which they may not be constant",
        call. = TRUE
      )
    }
  }

  # get class of input
  x_class <- class(x)
  # non sf-obj-input as geometry column
  if (!inherits(x, "sf")) {
    x <- sf::st_sf(geometry = sf::st_geometry(x))
  }

  # tmp id for below ordering of output
  tmp_id      <- uuid::UUIDgenerate()
  x[, tmp_id] <- seq_len(nrow(x))

  # keep input row names as column
  row_names      <- uuid::UUIDgenerate()
  x[ ,row_names] <- rownames(x)

  # preserve attributes of sf object
  x_agr <- sf::st_agr(x)

  # keep or drop empty geometries
  is_empty <- sf::st_is_empty(x)
  if (keep_empty) {
    x_empty <- x[is_empty, ]
  } else {
    x_empty <- NULL
  }
  x <- x[!is_empty, ]

  # disaggregate geometry collections into their sub-geometries
  is_gc <- sf::st_is(x, "GEOMETRYCOLLECTION")
  if (any(is_gc)) {
    x_gc         <- x[is_gc, ]
    l_sfg        <- lapply(sf::st_geometry(x_gc), `[`)
    geometry     <- do.call(c, lapply(l_sfg, sf::st_sfc, crs = sf::st_crs(x)))
    rep_row      <- rep(seq_len(nrow(x_gc)), lengths(l_sfg))
    non_geometry <- sf::st_drop_geometry(x_gc)[rep_row, , drop = FALSE]
    x_gc_former  <- sf::st_set_geometry(sf::st_sf(non_geometry, geometry), attr(x, "sf_column"))
    x            <- rbind(x[!is_gc, ], x_gc_former)
  }

  # dissagregate not only geometry collections, but also multi-geometries?
  if (only_geometrycollection == FALSE) {
    geometry_type <- sf::st_geometry_type(x)
    x <- rbind(
      sf::st_cast(x[geometry_type == "MULTIPOINT", ], "POINT", warn = FALSE),
      sf::st_cast(x[geometry_type == "MULTILINESTRING", ], "LINESTRING", warn = FALSE),
      sf::st_cast(x[geometry_type == "MULTIPOLYGON", ], "POLYGON", warn = FALSE),
      x[!geometry_type %in% c("MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON"), ]
    )
  } else {
    geometry_type <- NULL # placeholder for below update of attributes of sf-output
  }

  # keep empty geometries (if they haven't been dropped)
  x <- rbind(x, x_empty)

  # order output by (1) sequence of input geometries and (2) geometry type
  x <- x[order(x[[tmp_id]], sf::st_geometry_type(x)), ]

  # return output
  if (!"sf" %in% x_class) { # if input is not a sf-obj
    sf::st_geometry(x)      # only geometry
  } else {                  # else if input is a sf-obj
    if (any(c(is_gc, geometry_type %in% c("MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON")))) { # any geometries spitted?
      x_agr[x_agr == "identity" & !is.na(x_agr)] <- "constant" # if so, adjust attributes of sf-obj
    }
    sf::st_agr(x) <- x_agr # update attributes of sf-obj
    # add row names
    suffix        <- unlist(lapply(rle(x[[tmp_id]])$lengths, seq_len)) - 1
    rownames(x)   <- paste0(x[[row_names]], ifelse(suffix == 0, "", paste0(".", suffix)))
    x[, !names(x) %in% c(tmp_id, row_names)] # drop tmp. id & col. with input row names
  }
}
