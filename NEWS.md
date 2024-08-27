# sfhelpers 0.0.0.9006
* replace `lwgeom::st_perimeter()` by `sf::st_perimeter()` / update to `sf` >= 1.0.15.
* minor improvements.

# sfhelpers 0.0.0.9005
* improve README and help pages.
* remove `raster` and `sp` from the suggested packages.

# sfhelpers 0.0.0.9004
* add summary statistical functions for `sfg`, `sfc` and `sf` objects: 
  - `st_geometry_summary()`
  - `st_area_sum()`
  - `st_length_sum()`
  - `st_perimeter_sum()`
  - `st_perimeter_2d_sum()`
* add function `st_layers_as_df()`
* make returns of `st_erase_robust()` and `st_or()` reliable by abandoning
internal use of `st_union(st_combine())` in any case. And therefore also
abandoning argument `use_st_combine`. Instead, only `st_union()` is used, which
may slow down `st_erase_robust()` and `st_or()`.
* abandon argument `check_overlap` in `st_or()` and `st_erase_robust()`. These
functions now detect automatically which geometries from the input `x` and `y`
overlap with those of the other layer and apply geometric operations only to
such geometries. This change makes for shorter run times.
* fixed bug: `st_rbindlist()` now always stacks the active geometry columns of
the `list`ed `sf`-inputs. --> abandon argument `use_geometry`.
* fixed bug: `st_bbox_common()` / `st_bbox_list()` CRS check works.
* improve help pages.
* `st_disaggregate()` might fail if geometries of input have M-dimension. This due to changes in `sf`-package version 1.0-13 associated with `st_cast()`, on which `st_disaggregate()` is built. No quick fix yet.
  
# sfhelpers 0.0.0.9003
* add function `st_distance_along()`.
* add argument `use_st_combine` (default `TRUE`) to functions `st_erase_robust()`
and `st_or()`. Setting `use_st_combine` to `FALSE` enforces internal use of 
`st_union()` instead of `st_union(st_combine())`. Leaving out `st_combine()` may
make the erasing more reliable but also slower.
* replace arguments `x.suffix` and `y.suffix` by argument `suffix` in function `st_or()`.
* minor improvements of functions.
* improved vignette and help pages.

# sfhelpers 0.0.0.9002
* `st_rename_geometry()` function has been superseded because with `sf` package
version >= 1.0-6 `st_set_geometry()` offers the same functionality.
* dependency of imported `sf` package set to version >= 1.0-6. This made it
possible to replace package-internal dependency on functions
`sfhelpers::st_rename_geometry()` and `sfhelpers:::st_rename_geom()` with
`sf::st_set_geometry()` and `sf::st_geometry()<-`.
* more efficient homogenizing of active geometry columns of `st_rbindlist()` input.
* vignette `rbindlist_issues` updated.
* more efficient CRS check used within functions `st_rbindlist()`, `st_bbox_list()`
and `st_bbox_common()`.

# sfhelpers 0.0.0.9001
* fixed bug: `st_or()` and `st_erase_robust()` return expected geometries.
* fixed bug: `st_or()` works with input layers having differently named geometry columns.
* fixed bug: `st_or()` works with input layers being totally covered by the other one.
* new argument `check_overlap` added to functions `st_or()` and `st_erase_robust()`.
* redesign of argument `dim` and of internal handling of `GEOMETRYCOLLECTION` of
function `st_or()` (s. examples of the function's help page).
* enable passing arguments on to `s2::s2_options()` in functions `st_or()` and
`st_erase_robust()`.
* function `st_or()` accepts `sfg` as input.
* include classes `SpatExtent`, `SpatRaster` and `SpatVector` from `terra`
package as input options for functions `st_bbox_common()` and `st_bbox_list()`.
* added `st_disaggregate()` function.
* abandon `use_any_geometry` argument in function `st_rbindlist()`: The function
can now handle `list`s of `sf`-objects having different `geometry_type`s without
any need to specify this.
* if the argument `geometry_name` is unspecified (default `NULL`), the function
`st_rbindlist()` will always return a `sf` object with a geometry column
inheriting its name from the 1st `sf` object listed in the input.
* vignette `rbindlist_issues` updated.

# sfhelpers 0.0.0.9000
* 1st release on GitHub
* 6 functions
  - `st_bbox_common()`
  - `st_bbox_list()`
  - `st_erase_robust()`
  - `st_or()`
  - `st_rbindlist()`
  - `st_rename_geometry()`
* 2 demo `sf` objects with partly identically named attribute columns and party overlapping geometries / polygons
  - `poly_1`
  - `poly_2`
* 1 vignette
  - `rbindlist_issues` detailing some issues and workarounds related to `sf::st_as_sf(data.table::rbindllist(<list_of_sf>))`
