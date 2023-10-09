# get_coastline <- function(extent = "",
#                           crs = "racmo",
#                           preferType = NULL,
#                           crsIn = NULL) {
  #' Return the MEaSURES coastline for a given extent
  #' #'
  #' #' @description This function is useful for plotting the Antarctic coastline
  #' #'   (i.e. the most seaward part of the ice, grounded or shelf). It is mainly
  #' #'   used in the `draw_antarctica()` function, but works separately as well.
  #' #'   The coastlines can be reprojected using the "crs" argument, defaulting to
  #' #'   "racmo".
  #' #'
  #' #' @param extent Define the area within which to get the coastline. This
  #' #'   argument is fed into `get_extent()`; see there for details.
  #' #' @param crs "string": Which projection should the coastline be returned in?
  #' #' @param preferType "string": Use ice shelves or basins if there is a clash?
  #' #'   See `get_extent()` for a full explanation.
  #' #' @param crsIn "string": Which projection is the extent given in? Needs
  #' #'   defining if the extent is a SpatExtent, as they do not have crs value
  #' #'   attached.
  #' #'
  #' #' @examples
  #' #'   # Full coastline
  #' #'   t1 <- get_coastline()
  #' #'   terra::plot(t1)
  #' #'
  #' #'   # Single shelf
  #' #'   t2 <- get_coastline("Amery")
  #' #'   terra::plot(t2)
  #' #'
  #' #'   # Multiple shelves
  #' #'   t3 <- get_coastline(c("Amery", "West"))
  #' #'   terra::plot(t3)
  #' #'
  #' #'   # Compare preferType options
  #' #'   t4 <- get_coastline("Drygalski", preferType = NULL)
  #' #'   terra::plot(t4)
  #' #'
  #' #'   t5 <- get_coastline("Drygalski", preferType = "shelves")
  #' #'   terra::plot(t5)
  #' #'
  #' #'   t6 <- get_coastline("Drygalski", preferType = "basins")
  #' #'   terra::plot(t6)
  #' #'
  #' #' @export
  #'
  # # Code -----------------------------------------------------------------------
  # # Prepare full coastline data
  # fullCoast  <- define_racmo_globals()$measures$coasts
  # fullCoast  <- terra::vect(terra::geom(fullCoast), type = "lines")
  # terra::crs(fullCoast) <- use_crs("stereo")  # MEaSURES is EPSG:3031
  #
  # # Define the required extent
  # extent     <- get_extent(extent = extent,
  #                          rectangularExtent = TRUE,
  #                          preferType = preferType,
  #                          crsIn = crsIn,
  #                          crs = use_crs("stereo"))
  #
  # # Creop the full coastline to the required extent
  # fullCoast  <- terra::intersect(x = fullCoast, y = extent)
  #
  # # Reproject
  # fullCoast <- terra::project(fullCoast, use_crs(crs))
  #
#   return(fullCoast)
# }
