use_crs <- function(crs = NULL) {
  #' Return a crs string
  #'
  #' @description RCM projections do not have standard EPSG codes, and usually
  #'   work on a rotated grid. This function returns the correct proj4 string
  #'   for the RACMO or MAR data in Antarctica. A couple of other common
  #'   projections for use in Antarctica are also available. These projection
  #'   definitions are stored in `.polarEnv$crs`, which is configured by
  #'
  #' @param crs "string": Which crs to return? See the "Options" section below
  #'   for a list of available projections. If none of those names are matched,
  #'   the string will be fed in as is, and assumed to be an alternative valid
  #'   crs definition. Defaults to match whichever RCM data was listed first
  #'   in the ".Rprofile" file (matches using the dataset's 'src').
  #'
  #' @details # Projections
  #'
  #'   ## RACMO
  #'   The RACMO projections are taken directly from their respective NetCDFs.
  #'   These can be viewed with a command such as
  #'   `ncdf4::ncatt_get(nc, "rotated_pole")`.
  #'
  #'   ## MAR
  #'   The default MAR crs is based on EPSG:3031 (taken from the description of
  #'   this [Zenodo dataset](https://doi.org/10.5281/zenodo.2547638), but it
  #'   appears that it is in kilometres, not metres as a standard EPSG:3031
  #'   would be. This is very much to be finished off with the correct datasets!
  #'
  #'   ## Projection Options
  #'   Options are:
  #'
  #'       - "racmo"             Antarctic wide
  #'       - "racmoAA"           Antarctic wide (as above, just more explicit!)
  #'       - "racmoAP"           Antarctic Peninsula
  #'       - "mar"               Who knows at the moment?!
  #'       - "stereo"            Antarctic Polar Stereographic EPSG:[3031](https://epsg.io/3031)
  #'       - "lambertEqual"      Lambert Equal Area Equidistant (EPSG: [102020](https://epsg.io/102020))
  #'       - "ortho"             Orthographic South Pole (EPSG: [102037](https://epsg.io/102037))
  #'       - "WGS84" / "ERA5"    WGS84 GPS System (EPSG: [4326](https://epsg.io/4326))
  #'       - "stereoAus"         Australian Antarctic Polar Stereographic (EPSG: [3032](https://epsg.io/3032))
  #'       - "lambertAus"        Australian Antarctic Lambert (EPSG: [3033](https://epsg.io/3033))
  #'       - "ease2"             NSIDC EASE-Grid 2.0 South (EPSG: [6932](https://epsg.io.6932))
  #'
  #'   These crs definitions are stored in `.polarEnv$grids$crs`.
  #'   For more information on RCM projections, see here:
  #'    [https://gitlab.tudelft.nl/slhermitte/manuals/blob/master/RACMO_reproject.md]()
  #'   For more information on the non-RCM EPSG codes, follow the links; those
  #'   pages are particularly good for showing the extent where the projection
  #'   has been created for.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Prepare
  token <- configure_polarcm()
  crs   <- domR::set_if_null(crs, token$defaults$grid$crs)

  # Switch and return crs
  crs <- switch(tolower(crs),       # make case insensitivite
                "racmo"                      = ,
                "10.5281/zenodo.5512076"     = ,           # RACMO2.3p3 AA
                "10.5281/zenodo.7760490"     = ,           # RACMO2.3p2 AA
                "racmoaa"                    = token$grids$crs$racmoCrs,
                "10.5281/zenodo.7961732"     = ,           # RACMO2.3p2 AP
                "racmoap"                    = token$grids$crs$racmoApCrs,
                "10.5281/zenodo.6347190"     = ,
                "mar"                        = token$grids$crs$marCrs,
                "lambert"                    = token$grids$crs$lambertCrs,
                "ortho"  = , "orthographic"  = token$grids$crs$orthoCrs,
                "stereo" = , "stereographic" = ,
                "3031"   = , "epsg:3031"     = "EPSG:3031",
                "wgs84"  = , "wgs 84"        = ,
                "era5"   = , "ecmwf"         = ,
                "4326"   = , "epsg:4326"     = "EPSG:4326",
                crs)
  return(crs)
}
