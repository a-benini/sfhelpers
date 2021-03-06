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
