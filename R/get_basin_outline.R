get_basin_outline <- function(extent = "",
                              exactExtents = TRUE,
                              imbieBasins = NULL,
                              crs = "racmo",
                              crsIn = NULL){
  #' Return the MEaSURES basin outlines for a given extent
  #'
  #' @description This function is useful for plotting basins in Antarctica. It
  #'   is mainly used in the `draw_antarctica()` function, but works separately
  #'   as well. The basins can be reprojected using the "crs" argument,
  #'   defaulting to "racmo".
  #'
  #' @param extent Define the area within which to get the basin outlines. Exact
  #'   basin names can be used, as can SpatRasters, SpatVectors, or SpatExtents,
  #'   or it can be left empty, and all basins are returned. For exactExtents =
  #'   TRUE to work, this argument must be a string with the MEaSURES names of
  #'   the required basins. Fed into `get_extent()`; see there for details.
  #' @param exactExtents BINARY: Should only the defined basin/s be included
  #'   (TRUE), or can the outlines of all basins within the bounding box be
  #'   included too (FALSE)?
  #' @param imbieBasins BINARY: Should the IMBIE basins (TRUE, the default) or
  #'   the MEaSURES basins (FALSE) be returned? The IMBIE basins have codes
  #'   (e.g. "A-Ap") and are larger regions; the MEaSURES basins have names
  #'   (that can overlap with the ice shelf names), and are small "sub" regions.
  #'   If NULL, both are returned.
  #' @param crs "string": Which projection should the grounding line be returned
  #'   in?
  #' @param crsIn "string": Which projection is the extent given in? Needs
  #'   defining if the extent is a SpatExtent, as they do not have crs value
  #'   attached.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Which basins?
  if (isTRUE(imbieBasins)) {
    basins <- define_racmo_globals()$imbie$basins
  } else if (isFALSE(imbieBasins)) {
    basins <- define_racmo_globals()$measures$basins
  } else if (is.null(imbieBasins)) {
    meas <- define_racmo_globals()$measures$basins
    immB <- define_racmo_globals()$imbie$basins
  }

  extents   <- get_extent(extent,
                          rectangularExtent = !exactExtents,
                          returnOnly = "basins",
                          imbieBasins = imbieBasins,
                          crs = use_crs("stereo"), crsIn = crsIn)

  basins    <- terra::intersect(basins, extents)
  basins    <- terra::project(basins, use_crs(crs))

  return(basins)
}
