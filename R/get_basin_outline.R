get_basin_outline <- function(extent = "",
                              exactExtents = TRUE,
                              subBasins = FALSE,
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
  #'   shelf names can be used, as can SpatRasters, SpatVectors, or SpatExtents,
  #'   or it can be left empty, and all basins are returned. For exactExtents =
  #'   TRUE to work, this argument must be a string with the MEaSURES names of
  #'   the required shelves. Fed into `get_extent()`; see there for details.
  #' @param crs "string": Which projection should the grounding line be returned
  #'   in?
  #' @param exactExtents BINARY: Should only the defined shelf/shelves be
  #'   included (TRUE), or can the outlines of all shelves within the bounding
  #'   box be included too (FALSE)?
  #' @param crsIn "string": Which projection is the extent given in? Needs
  #'   defining if the extent is a SpatExtent, as they do not have crs value
  #'   attached.
  #'
  #' @param subBasins BINARY: Should the MEaSURES subregion basins be returned?
  #'   Default is FALSE, which means that the IMBIE basins are returned. If
  #'   exactExtents is TRUE, make sure that the extent argument uses the correct
  #'   name for the correct dataset: IMBIE basins (subBasins = FALSE) have codes
  #'   (e.g. "A-Ap"), whereas MEaSURES basins (subBasins = TRUE) have names,
  #'   such as "Vincennes_Bay".
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Which basins?
  if (isTRUE(subBasins)) {
    basins <- define_racmo_globals()$measures$basins
    print("sub basins")
  } else {
    basins <- define_racmo_globals()$imbie$basins
    print("full basins")
  }

  extents   <- get_extent(extent,
                          rectangularExtent = !exactExtents,
                          returnOnly = "basins",
                          crs = use_crs("stereo"), crsIn = crsIn)

  basins    <- terra::intersect(basins, extents)
  basins    <- terra::project(basins, use_crs(crs))

  return(basins)
}
