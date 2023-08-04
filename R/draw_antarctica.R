draw_antarctica <- function(extent = "",
                            sbcg = "cg",
                            exactExtents = TRUE,
                            simplify = 0,
                            crs = "racmo",
                            crsIn = NULL,
                            ...) {
  #' Draw MEaSURES Antarctic outlines on maps
  #'
  #' @description Used after plotting a raster to add MEaSURES outlines. This
  #'   function is essentialy a wrapper around `terra::lines()`, but lets us
  #'   easily decide on the outlines to add, their extent, and with which crs.
  #'
  #' @param extent Define the area within which to draw Antarctica. This
  #'   argument is fed into `get_extent()`; see there for details.
  #' @param sbcg "string": Which parts of Antarctica should be drawn? Options
  #'   are:
  #'    * shelves          "s"
  #'    * basins           "b"  # uses IMBIE; use "bb" for MEaSURES
  #'    * coasts           "c"
  #'    * grounding line   "g"
  #' @param exactExtents BINARY: Should only the defined shelf/shelves be
  #'   included (TRUE), or can the outlines of all shelves within the bounding
  #'   box be included too (FALSE)?
  #' @param simplify numeric: Should the outline be simplified? Uses the
  #'   `terra::simplifyGeom()` function, so this value is the tolerance - i.e.
  #'   nodes must be at least this far apart, defined in crs units. Larger
  #'   numbers are coarser; 0 (default) is no simplifying.
  #' @param crs "string": Which crs should the lines be drawn in? See the
  #'   `set_crs()` function
  #' @param ... Any arguments that can be used in `terra::lines()`
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  sbcg   <- strsplit(sbcg, "")[[1]] # separate out what we want
  crs    <- use_crs(crs)

  # Add grounding lines
  if ("g" %in% sbcg) {
    ground <- get_grounding_line(extent, crs)
    if (simplify != 0) ground <- terra::simplifyGeom(ground, simplify)
    terra::lines(ground, ...)
  }

  # Add coastlines
  if ("c" %in% sbcg) {
    coast <- get_coastline(extent, crs)
    if (simplify != 0) coast <- terra::simplifyGeom(coast, simplify)
    terra::lines(coast, ...)
  }

  # Add shelf outlines
  if ("s" %in% sbcg) {
    shelves <- get_shelf_outline(extent, exactExtents, crs)
    # shelves <- get_extent(extent = extent,
                          # rectangularExtent = !exactExtents,
                          # returnOnly = "shelves", crs = crs)

    if (simplify != 0) shelves <- terra::simplifyGeom(shelves, simplify)
    terra::lines(shelves, ...)
  }

  # Add basin outlines
  if ("b" %in% sbcg) {
    if (sum(sbcg == "b") > 1) {
      basins <- get_basin_outline(extent, exactExtents, imbieBasins = FALSE, crs)
    } else {
      basins <- get_basin_outline(extent, exactExtents, imbieBasins = TRUE, crs)
    }
    if (simplify != 0) basins <- terra::simplifyGeom(basins, simplify)
    terra::lines(basins, ...)
  }
}
