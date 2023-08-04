get_grounding_line <- function(extent = "",
                               crs = "racmo",
                               preferType = NULL,
                               crsIn = NULL) {
  #' Return the MEaSURES grounding line for a given extent
  #'
  #' @description This function is useful for plotting the grounding line (GL)
  #'   of Antarctica. It is mainly used in the `draw_antarctica()` function, but
  #'   works separately as well. The GL can be reprojected using the "crs"
  #'   argument, defaulting to "racmo".
  #'
  #' @param extent Define the area within which to get the grounding line. This
  #'   argument is fed into `get_extent()`; see there for details.
  #' @param crs "string": Which projection should the grounding line be returned
  #'   in?
  #' @param preferType "string": Use ice shelves or basins if there is a clash
  #'   in the extent definition? See `get_extent()` for a full explanation.
  #' @param crsIn "string": Which projection is the extent given in? Needs
  #'   defining if the extent is a SpatExtent, as they do not have crs value
  #'   attached.
  #'
  #' @examples
  #'   # Full grounding line
  #'   t1 <- get_grounding_line()
  #'   terra::plot(t1)
  #'
  #'   # Single shelf
  #'   t2 <- get_grounding_line("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Multiple shelves
  #'   t3 <- get_grounding_line(c("Amery", "West"))
  #'   terra::plot(t3)
  #'
  #'   # Compare preferType options
  #'   t4 <- get_grounding_line("Drygalski", preferType = NULL)
  #'   terra::plot(t4)
  #'
  #'   t5 <- get_grounding_line("Drygalski", preferType = "shelves")
  #'   terra::plot(t5)
  #'
  #'   t6 <- get_grounding_line("Drygalski", preferType = "basins")
  #'   terra::plot(t6)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare full grounding line data
  groundingLine <- define_racmo_globals()$measures$groundingLine
  groundingLine <- terra::vect(terra::geom(groundingLine), type = "lines")
  terra::crs(groundingLine) <- use_crs("stereo")   # MEaSURES is EPSG:3031

  # Define the required extent
  extent     <- get_extent(extent = extent,
                           rectangularExtent = TRUE,
                           preferType = preferType,
                           crsIn = crsIn,
                           crs = use_crs("stereo")) # return in 3031 to match GL

  # Crop the full grounding line to the required extent
  groundingLine <- terra::intersect(x = groundingLine,
                                    y = extent)

  # Reproject after intersect cropping
  groundingLine <- terra::project(groundingLine, use_crs(crs))

  return(groundingLine)
}
