get_shelf_outline <- function(extent = "",
                              exactExtents = TRUE,
                              crs = "racmo",
                              crsIn = NULL){
  #' Return the MEaSURES shelf outlines for a given extent
  #'
  #' @description This function is useful for plotting ice shelves in
  #'   Antarctica. It is mainly used in the `draw_antarctica()` function, but
  #'   works separately as well. The ice shelves can be reprojected using the
  #'   "crs" argument, defaulting to "racmo".
  #'
  #' @param extent Define the area within which to get the shelf outlines. Exact
  #'   shelf names can be used, as can SpatRasters, SpatVectors, or SpatExtents,
  #'   or it can be left empty, and all shelves are returned. For exactExtents =
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
  #' @examples
  #'   # All ice shelves in Antarctica
  #'   t1 <- get_shelf_outline()
  #'   terra::plot(t1)
  #'
  #'   # Just on shelf  t2 <- get_shelf_outline("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Multiple shelves; only the named shelves
  #'   t3 <- get_shelf_outline(c("Amery", "West"), exactExtents = TRUE)
  #'   terra::plot(t3)
  #'
  #'   # Multiple shelves; includes intermediary shelves
  #'   t4 <- get_shelf_outline(c("Amery", "West"), exactExtents = FALSE)
  #'   terra::plot(t4)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  shelves <- define_racmo_globals()$measures$shelves
  extents <- get_extent(extent,
                        rectangularExtent = !exactExtents,
                        returnOnly = "shelves",
                        crs = use_crs("stereo"), crsIn = crsIn)

  shelves   <- terra::intersect(shelves, extents)
  shelves   <- terra::project(shelves, use_crs(crs))

  return(shelves)
}

