draw_antarctica <- function(extent = "",
                            sbcg = "cg",
                            rectangularExtent = FALSE,
                            simplify = 0,
                            crs = "racmo",
                            extentArgs = list(),
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
  #' @param rectangularExtent BINARY: Should only the defined shelf/shelves be
  #'   included (TRUE), or can the outlines of all shelves within the bounding
  #'   box be included too (FALSE)?
  #' @param simplify numeric: Should the outline be simplified? Uses the
  #'   `terra::simplifyGeom()` function, so this value is the tolerance - i.e.
  #'   nodes must be at least this far apart, defined in crs units. Larger
  #'   numbers are coarser; 0 (default) is no simplifying.
  #' @param crs "string": Which crs should the lines be drawn in? See the
  #'   `set_crs()` function
  #' @param extentArgs A list of arguments to feed into `get_extent()`. Only
  #'   necessary if
  #' @param ... Any arguments that can be used in `terra::lines()`
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  sbcg   <- strsplit(sbcg, "")[[1]] # separate out what we want
  crs    <- use_crs(crs)

  # Handle necessary arguments for the inner get_extent() function calls
  extentArgs$crs    <- crs
  extentArgs$extent <- extent

  # Add grounding lines
  if ("g" %in% sbcg) {
    ground <- do.call(get_grounding_line, extentArgs)

    # Only plot if there is a grounding line to plot!
    if (nrow(ground) > 0) {
      if (simplify != 0) ground <- terra::simplifyGeom(ground, simplify)
      terra::lines(ground, ...)
    }
  }

  # Add coastlines
  if ("c" %in% sbcg) {
    coast <- do.call(get_coastline, extentArgs)

    # Only plot if there is a coastline to plot!
    if (nrow(coast) > 0) {
      if (simplify != 0) coast <- terra::simplifyGeom(coast, simplify)
      terra::lines(coast, ...)
    }
  }

  # shelves/basins can be explicitly defined by name
  extentArgs$rectangularExtent <- rectangularExtent

  # Add shelf outlines
  if ("s" %in% sbcg) {
    shelves <- do.call(get_shelf_outline, extentArgs)

    # Only plot if there is a shelf to plot!
    if (nrow(shelves) > 0) {
      if (simplify != 0) shelves <- terra::simplifyGeom(shelves, simplify)
        terra::lines(shelves, ...)
    }
  }

  # Add basin outlines
  if ("b" %in% sbcg) {
    if (sum(sbcg == "b") > 1) {
      extentArgs$returnImbie <- FALSE
      basins <- do.call(get_basin_outline, extentArgs)
    } else {
      extentArgs$returnImbie <- TRUE
      basins <- do.call(get_basin_outline, extentArgs)
    }

    # Only plot if there is a basin to plot!
    if (nrow(basins) > 0) {
      if (simplify != 0) basins <- terra::simplifyGeom(basins, simplify)
      terra::lines(basins, ...)
    }
  }
}
