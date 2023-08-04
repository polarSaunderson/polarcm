get_coastline <- function(extent = "",
                          rectangularExtent = TRUE,
                          preferType = NULL,
                          useOnly = NULL,
                          imbieBasins = NULL,
                          crs = "racmo",
                          crsIn = NULL) {
  #' Return the MEaSURES coastline for a given extent
  #'
  #' @description This function is useful for plotting the Antarctic coastline
  #'   (i.e. the most seaward part of the ice, either grounded or ice shelf). An
  #'   extent is defined according to `get_extent()`, and the coastline that
  #'   intersects with the extent is returned. This function is mainly for use
  #'   in the `draw_antarctica()` function.
  #'
  #' @param extent Define the extent used to crop the coastline. This argument
  #'   is fed into `get_extent()`; see there for details of valid input.
  #' @param crs "string": Which projection should the coastline be returned in?
  #' @inheritParams get_extent
  #'
  #' @examples -----------------------------------------------------------------
  #'   # Full coastline
  #'   t1 <- get_coastline()
  #'   terra::plot(t1)
  #'
  #'   # Coastline of a single shelf
  #'   t2 <- get_coastline("Amery")
  #'   terra::plot(t2)
  #'
  #'   # Coastline of separate shelves
  #'   t3 <- get_coastline(c("Amery", "West"), rectangularExtent = FALSE)
  #'   terra::plot(t3)
  #'
  #'   # All coastlines in the bounding box around multiple shelves
  #'   t4 <- get_coastline(c("Amery", "West"), rectangularExtent = TRUE)
  #'   terra::plot(t4)
  #'
  #'   # Compare preferType options
  #'   t4 <- get_coastline("Drygalski", preferType = NULL)
  #'   terra::plot(t4)
  #'
  #'   t5 <- get_coastline("Drygalski", preferType = "shelves")
  #'   terra::plot(t5)
  #'
  #'   t6 <- get_coastline("Drygalski", preferType = "basins")
  #'   terra::plot(t6)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare full coastline data
  fullCoast  <- define_racmo_globals()$measures$coasts
  fullCoast  <- terra::vect(terra::geom(fullCoast), type = "lines")
  terra::crs(fullCoast) <- use_crs("stereo")     # MEaSURES is EPSG:3031

  # Define the required extent
  extent <- get_extent(extent = extent,
                       rectangularExtent = rectangularExtent,
                       preferType = preferType,
                       useOnly = useOnly,
                       imbieBasins = imbieBasins,
                       crs = use_crs("stereo"),  # return in 3031 to match coast
                       crsIn = crsIn)

  # Crop the full coastline data to the required extent
  fullCoast  <- terra::intersect(x = fullCoast,
                                 y = extent)

  # Reproject
  fullCoast <- terra::project(fullCoast, use_crs(crs))

  return(fullCoast)
}
