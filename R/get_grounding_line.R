get_grounding_line <- function(extent = "",
                               rectangularExtent = TRUE,
                               preferType = NULL,
                               returnOnly = NULL,
                               imbieBasins = NULL,
                               crs = "racmo",
                               crsIn = NULL) {
  #' Return the MEaSURES grounding line for a given extent
  #'
  #' @description This function is useful for plotting the Antarctic grounding
  #'   line (i.e. where ice shelves become afloat). An extent is defined
  #'   according to `get_extent()`, and the grounding line that intersects with
  #'   the extent is returned. This function is mainly for use in the
  #'   `draw_antarctica()` function.
  #'
  #' @param extent Define the extent used to crop the grounding line. This
  #'   argument is fed into `get_extent()`; see there for details.
  #' @param crs "string": Which projection should the grounding line be returned
  #'   in?
  #' @inheritParams get_extent
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
  terra::crs(groundingLine) <- use_crs("stereo")    # MEaSURES is EPSG:3031

  # Define the required extent
  extent <- get_extent(extent = extent,
                       rectangularExtent = rectangularExtent,
                       preferType = preferType,
                       returnOnly = returnOnly,
                       imbieBasins = imbieBasins,
                       crs = use_crs("stereo"),     # return in 3031 to match GL
                       crsIn = crsIn)

  # Crop the full grounding line to the required extent
  groundingLine <- terra::intersect(x = groundingLine,
                                    y = extent)

  # Reproject after intersect cropping
  groundingLine <- terra::project(groundingLine, use_crs(crs))

  return(groundingLine)
}
