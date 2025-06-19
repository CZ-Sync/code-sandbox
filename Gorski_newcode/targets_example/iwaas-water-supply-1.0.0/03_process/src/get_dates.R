################################################################################
#                               Get gridMet dates                              #
################################################################################
#' Get list of dates from multi-layer gridMET raster
#'
#' Returns a `Date` vector of dates corresponding to gridmet `SpatRaster`
#'   layers. Used as input for for monthly aggregation.
#'
#' @param in_rast `SpatRaster` of gridMET rasters (single- or multi-layer)
#'
#' @return `Date` vector corresponding to the the date of each gridMET raster
#'   layer from `in_rast`
#'
get_gridmet_dates <- function(in_rast) {
  out <- in_rast |>
    names() |>
    stringr::str_split("=") |>
    unlist() |>
    stringr::str_extract(pattern = "^\\d{5}")
  out[!is.na(out)] |>
    as.numeric() |>
    as.Date(origin = "1900-01-01")
}

################################################################################
#                               Get SSEBop dates                               #
################################################################################
#' Get list of dates from SSEBop filenames
#'
#' @param in_files character vector of input SSEBop files (.zip or .tif)
#'
#' @return POSIXct vector of daily dates of SSEBop data
#'
get_ssebop_date <- function(in_files) {
  in_files |>
    stringr::str_extract("\\d{7}") |>
    strptime("%Y%j") |>
    as.POSIXct(tz = "GMT")
}

################################################################################
#                               Get SNODAS dates                               #
################################################################################
#' Get SNODAS dates
#'
#' @param in_files character; filenames of snodas files
#'
#' @return vector of dates associated with `in_files`
#'
get_snodas_dates <- function(in_files) {
  in_files |>
    stringr::str_extract("\\d{8}") |>
    as.Date(format = "%Y%m%d")
}
