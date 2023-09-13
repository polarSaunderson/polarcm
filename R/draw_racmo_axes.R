draw_racmo_axes <- function(x = "",
                            crs = "racmo",
                            tickKula = kulaL(4),
                            lineKula = kulaL(4),
                            interval = NULL,
                            tickLength = -0.25) {
  #' Add a tighter border of axes around Antarctic RACMO plots
  #'
  #' @description The default terra borders for Antarctica leave a lot of white
  #'   space around the continent. This function draws axes that are tighter to
  #'   the coastline of the raster. This function should be called straight
  #'   after using terra::plot(x, axes = FALSE).
  #'
  #' @param x SpatRaster: Which data has been plotted? Supply the SpatRaster to
  #'   correctly align the axes with the extent; if an empty string is supplied,
  #'   which is the default, it assumes that the whole continent has been
  #'   plotted, and will add the appropriate axes.
  #' @param crs Which crs are the axes being added to?
  #' @param tickKula Which colour should the tick marks be?
  #' @param axisKula Which colour should the axis lines be?
  #' @param interval numeric: The interval between tick marks, in the ?
  #' @param tck numeric: How long should the tick marks be?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Define defaults for tick mark interval
  if (crs == "racmo") {
    interval <- domR::set_if_null(interval, 10)
  } else if (crs == "3031") {
    interval <- domR::set_if_null(interval, 1000000)
  }

  # The approach to determining the ticks depends on which argument is supplied
  if (methods::is(x) %in% c("SpatExtent", "SpatVector", "SpatRaster")) {
    if (methods::is(x) == "SpatExtent") {
      bbox <- x
    } else {
      bbox   <- terra::ext(x)
    }

    # Separate out bounding box (easier to track the left than bbox[1])
    left   <- bbox[1]
    right  <- bbox[2]
    bottom <- bbox[3]
    top    <- bbox[4]

    # If there is a 0 on the x-axis, we want a tick on it!
    if (left < 0 & right > 0) {
      xTicks <- c(rev(seq(0, left, -interval)), seq(interval, right, interval))
    } else {
      xTicks <- seq(left, right, interval)
    }
    # And also on the y-axis!
    if (bottom < 0 & top > 0) {
      yTicks <- c(rev(seq(0, bottom, -interval)), seq(interval, top, interval))
    } else {
      yTicks <- seq(bottom, top, interval)
    }

    # Add lines to the plot ---!
    # line with ticks
    axis(1, at = xTicks, labels = NA, col.ticks = tickKula, col = lineKula, tcl = tickLength, pos = bottom)
    axis(2, at = yTicks, labels = NA, col.ticks = tickKula, col = lineKula, tcl = tickLength, pos = left)
    axis(3, at = xTicks, labels = NA, col.ticks = tickKula, col = lineKula, tcl = tickLength, pos = top)
    axis(4, at = yTicks, labels = NA, col.ticks = tickKula, col = lineKula, tcl = tickLength, pos = right)

    # additional line without ticks (it goes all the way across, beyond ticks)
    axis(1, at = c(left, right), labels = NA, col.ticks = tickKula, col = lineKula, tcl = 0, pos = bottom)
    axis(2, at = c(top, bottom), labels = NA, col.ticks = tickKula, col = lineKula, tcl = 0, pos = left)
    axis(3, at = c(left, right), labels = NA, col.ticks = tickKula, col = lineKula, tcl = 0, pos = top)
    axis(4, at = c(top, bottom), labels = NA, col.ticks = tickKula, col = lineKula, tcl = 0, pos = right)
  } else if (x == "") {
    if (crs == "racmo") {
      # line with ticks
      axis(1, at = seq(-30, 30, interval), labels = NA, tcl = tickLength, pos = -30.125, col.ticks = tickKula)
      axis(2, at = seq(-20, 20, interval), labels = NA, tcl = tickLength, pos = -32.875, col.ticks = tickKula)
      axis(3, at = seq(-30, 30, interval), labels = NA, tcl = tickLength, pos = 29.875, col.ticks = tickKula)
      axis(4, at = seq(-20, 20, interval), labels = NA, tcl = tickLength, pos = 32.625, col.ticks = tickKula)

      # additional line without ticks (it goes all the way across, beyond ticks)
      axis(1, at = c(-32.875, 32.625), labels = NA, tcl = 0, pos = -30.125, col = lineKula)
      axis(2, at = c(-30.125, 29.875), labels = NA, tcl = 0, pos = -32.875, col = lineKula)
      axis(3, at = c(-32.875, 32.625), labels = NA, tcl = 0, pos = 29.875, col = lineKula)
      axis(4, at = c(-30.125, 29.875), labels = NA, tcl = 0, pos = 32.625, col = lineKula)
    } else {
      warning("Supply a raster to base the axes on!")
    }
  }
  # Add south pole
  southPole <- terra::vect(matrix(c(0, 0), nrow = 1), type  = "points")
  terra::points(southPole, col = tickKula, pch = 3, cex = 0.5)
}
