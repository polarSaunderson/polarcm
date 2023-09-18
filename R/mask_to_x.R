mask_to_shelf <- function(x, shelf) {
  #' Mask RACMO or MAR data to an Antarctic ice shelf
  #'
  #' @description Spatially mask RACMO or MAR SpatRasters. The 'shelf' must be
  #'   an exact match for a name in the MEaSURES dataset. Multiple shelves can
  #'   be used in a vector.
  #'
  #'   The data returned is "masked" to the actual shelf outline, meaning the
  #'   surrounding pixels are given a value of NA; use `crop_to_shelf()` for the
  #'   rectangular bounding box.
  #'
  #' @param x SpatRaster: The RACMO or MAR data to mask. It must be an existing
  #'   SpatRaster.
  #' @param shelf "string": The EXACT name of the ice shelves to mask 'x' to. If
  #'   multiple are used, non-shelf pixels in the bounding box are masked out.
  #'
  #' @seealso crop_to_shelf()
  #' @seealso mask_to_basin()
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
  #'   x <- subset_racmoM_by_summer("precip", 1991) |>
  #'     subset_racmoM_by_month(12)
  #'
  #'   # Single shelf
  #'   xMask <- mask_to_shelf(x, "Amery")
  #'   terra::plot(xMask)
  #'
  #'   # Multiple shelves
  #'   xMask2 <- mask_to_shelf(x, c("Amery", "Shackleton"))
  #'   terra::plot(xMask2)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify shelf
  dataCrs <- terra::crs(x)
  shelf   <- get_shelf_outline(extent = shelf,
                               rectangularExtent = FALSE,
                               crs = dataCrs)  # shelves are in 3031; reprojects

  # Crop
  croppedX <- terra::crop(x = x, y = shelf, snap = "out")
  maskedX  <- terra::mask(x = croppedX, mask = shelf, touches = TRUE)

  # Add data back that is lost in cropping
  terra::varnames(maskedX)  <- terra::varnames(x)[[1]]
  terra::longnames(maskedX) <- terra::longnames(x)[[1]]
  terra::units(maskedX)     <- terra::units(x)[[1]]
  names(maskedX)            <- names(x)

  return(maskedX)
}


mask_to_basin <- function(x, basin) {
  #' Mask RACMO or MAR data to an Antarctic basin
  #'
  #' @description Spatially mask RACMO or MAR SpatRasters. The 'basin' must be
  #'   an exact match for a basin name in the MEaSURES dataset, and can refer to
  #'   either the IMBIE basins (e.g. "A-Ap") or the refined MEaSURES basins
  #'   (e.g. "Dry Valleys"). Multiple basins can be used in a vector.
  #'
  #'   The data returned is "masked" to the actual basin outline, meaning the
  #'   surrounding pixels are given a value of NA; use `crop_to_basin()` for the
  #'   rectangular bounding box.
  #'
  #' @param x SpatRaster: The RACMO or MAR data to mask. It must be an existing
  #'   SpatRaster.
  #' @param basin "string": The EXACT name of the basins to mask 'x' to. If
  #'   multiple are used, non-basins pixels in the bounding box are also masked
  #'   out.
  #'
  #' @seealso crop_to_basin()
  #' @seealso mask_to_shelf()
  #'
  #' @examples -----------------------------------------------------------------
  #' \dontrun{
  #'   x <- subset_racmoM_by_summer("precip", 1991) |>
  #'     subset_racmoM_by_month(12)
  #'
  #'   # Single basin
  #'   xMask <- mask_to_basin(x, "A-Ap")
  #'   terra::plot(xMask)
  #'
  #'   # Multiple basins
  #'   xMask2 <- mask_to_basin(x, c("A-Ap", "Dry Valleys"))
  #'   terra::plot(xMask2)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Identify basin
  dataCrs <- terra::crs(x)
  basin   <- get_basin_outline(extent = basin,
                               rectangularExtent = FALSE,
                               crs = dataCrs)  # basins are in 3031; reprojects

  # Crop
  croppedX <- terra::crop(x = x, y = basin, snap = "out")
  maskedX  <- terra::mask(x = croppedX, mask = basin, touches = TRUE)

  # Add data back that is lost in cropping
  terra::varnames(maskedX)  <- terra::varnames(x)[[1]]
  terra::longnames(maskedX) <- terra::longnames(x)[[1]]
  terra::units(maskedX)     <- terra::units(x)[[1]]
  names(maskedX)            <- names(x)

  return(maskedX)
}
