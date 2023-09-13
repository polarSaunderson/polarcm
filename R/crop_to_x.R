crop_to_shelf <- function(x, shelf) {
  #' Crop RCM data to an Antarctic ice shelf
  #'
  #' @description Spatially crop RACMO or MAR SpatRasters. The 'shelf' must be
  #'   an exact match for a name in the MEaSURES dataset. Multiple shelves can
  #'   be used in a vector. The data returned is for a bounding box around the
  #'   shelves; use `mask_to_shelf()` for "cropping" tightly to the actual shelf
  #'   outline.
  #'
  #' @param x SpatRaster: The RACMO or MAR data to crop. It must be an existing
  #'   SpatRaster.
  #' @param shelf "string": The EXACT name of the ice shelves to crop 'x' to. If
  #'   a vector with multiple shelves is input, non-shelf pixels in the bounding
  #'   box between those shelves are also included.
  #'
  #' @seealso mask_to_shelf()
  #' @seealso crop_to_basin()
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
  #'   x <- subset_racmoM_by_summer("precip", 1991) |>
  #'     subset_racmoM_by_month(12)
  #'
  #'   # Single shelf
  #'   xCrop <- crop_to_shelf(x, "Amery")
  #'   terra::plot(xCrop)
  #'
  #'   # Multiple shelves
  #'   xCrop2 <- crop_to_shelf(x, c("Amery", "Shackleton"))
  #'   terra::plot(xCrop2)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify shelf
  dataCrs <- terra::crs(x)
  shelf   <- get_shelf_outline(extent = shelf,
                               rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031;
                                               # reprojects to match input data

  # Crop
  croppedX <- terra::crop(x, shelf, snap = "out")

  # Add data back that is lost in cropping
  terra::varnames(croppedX)  <- terra::varnames(x)[[1]]
  terra::longnames(croppedX) <- terra::longnames(x)[[1]]
  terra::units(croppedX)     <- terra::units(x)[[1]]
  names(croppedX)            <- names(x)

  return(croppedX)
}

crop_to_basin <- function(x, basin) {
  #' Crop RACMO or MAR data to an Antarctic ice shelf
  #'
  #' @description Spatially crop RACMO or MAR SpatRasters. The 'basin' must be
  #'   an exact match for a name in the MEaSURES dataset, and can refer to
  #'   either the IMBIE basins (e.g. "A-Ap") or the refined MEaSURES basins
  #'   (e.g. "Dry Valleys"). Multiple basins can be used in a vector. The data
  #'   returned is for a bounding box around the shelves; use `mask_to_basin()`
  #'   for "cropping" tightly to the actual basin outline.
  #'
  #' @param x SpatRaster: The RACMO or MAR data to crop. It must be an existing
  #'   SpatRaster.
  #' @param basin "string": The EXACT name of the basins to crop the 'x' to. If
  #'   a vector with multiple basins is input, non-shelf pixels in the bounding
  #'   box between those basin are also included.
  #'
  #' @seealso mask_to_basin()
  #' @seealso crop_to_shelf()
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
  #'   x <- subset_racmoM_by_summer("precip", 1991) |>
  #'     subset_racmoM_by_month(12)
  #'
  #'   # Single basin
  #'   xCrop <- crop_to_basin(x, "A-Ap")
  #'   terra::plot(xCrop)
  #'
  #'   # Multiple basins
  #'   xCrop2 <- crop_to_basin(x, c("A-Ap", "Dry Valleys"))
  #'   terra::plot(xCrop2)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify basin
  dataCrs <- terra::crs(x)
  basin   <- get_basin_outline(extent = basin, rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031; reprojects

  # Crop
  croppedX <- terra::crop(x, basin, snap = "out")

  # Add data back that is lost in cropping
  terra::varnames(croppedX)  <- terra::varnames(x)[[1]]
  terra::longnames(croppedX) <- terra::longnames(x)[[1]]
  terra::units(croppedX)     <- terra::units(x)[[1]]
  names(croppedX)            <- names(x)

  return(croppedX)
}
