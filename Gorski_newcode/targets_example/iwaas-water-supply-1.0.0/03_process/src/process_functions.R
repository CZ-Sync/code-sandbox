################################################################################
#                                  Unzip data                                  #
################################################################################
#' Unzip data
#'
#' Unzip zip files and return file paths
#'
#' @param zip_files character; vector of zip file paths to unzip
#' @param path_out character; string of the path to unzip files to
#' @param unzip_file_pattern character; REGEX pattern used to identify the files
#'   in each zip file that should be unzipped. If NULL, all files returned.
#' @param file_out_pattern character; REGEX pattern used to identify which
#'   unzipped files should be listed in the output vector. To return the parent
#'   directory of the output files (useful for .gdb geodatabase files), use
#'    `".."`.
#'
#' @return A character vector of  files paths (or a subset of them if
#'   file_out_pattern is specified) of unzipped files
#'
unzip_files <- function(zip_files, path_out, unzip_file_pattern = NULL,
                        file_out_pattern = "|") {
  # List files that should be unzipped
  files_to_unzip <- purrr::map(
    zip_files,
    \(.x) index_zipped_files(.x, unzip_file_pattern)
  )

  # Unzip files
  files_out <- purrr::map2(
    zip_files,
    files_to_unzip,
    \(.x, .y) {
      archive::archive_extract(
        archive = .x,
        dir = path_out,
        files = .y
      )
    }
  )

  # Output only the those unzipped files that are specified with
  #   file_out_pattern argument
  if (file_out_pattern == "..") {
    out <- file.path(
      path_out,
      stringr::str_remove(files_out[[1]][[1]], "/[^ ]+$")
    )
  } else {
    out <- file.path(
      path_out,
      stringr::str_subset(unlist(files_out), file_out_pattern)
    )
  }

  return(out)
}

#' Get index files matching pattern within archive
#'
#' Helper for `unzip_files`
#'
#' @param zip_files character; vector of zip file paths to unzip
#' @param unzip_file_pattern character; REGEX pattern used to identify the files
#'   in each zip file that should be unzipped. If NULL, NULL is returned.
#'
#' @return num; index of files matching pattern within archive
#'
index_zipped_files <- function(zip_files, unzip_file_pattern = NULL) {
  if (is.null(unzip_file_pattern)) {
    return(NULL)
  }

  archive::archive(zip_files)[["path"]] |>
    grepl(pattern = unzip_file_pattern, x = _) |>
    which()
}

################################################################################
#            Read delimited file and write to other location as CSV            #
################################################################################
#' Read delimited file and write to other location as CSV
#'
#' @param in_file character; delimited file to be read in
#' @param out_file character; path and name of file to write the CSV
#' @param ... Additional arguments passed to `readr::read_delim`
#'
#' @return character of output CSV
#'
#' @seealso `readr::read_delim`
#'
write_to_csv <- function(in_file, out_file, ...) {
  out <- readr::read_delim(
    file = in_file,
    # Convert column name format from "201005" to "2010_05"; "huc..." to "HUC"
    name_repair = ~ dplyr::if_else(
      startsWith(.x, "huc"),
      "HUC",
      sub("(.*)(\\d{2})", "\\1_\\2", .x)
    ),
    ...
  )

  readr::write_csv(out, file = out_file)
  return(out_file)
}

################################################################################
#             Unzip and combine monthly rasters to multilayer tif              #
################################################################################
#' Unzip many zip files that contain tifs and merge them into a multi-layer tif
#'
#' @param zip_files chr; a vector of zip files containing tifs
#' @param out_path chr; the path and file name of the output tif
#' @param layer_name_date_pattern chr; regex pattern used to identify the date
#'   from the layer names of the rasters contained in `zip_files`
#' @param name_replacement_pattern chr nave-value pair; lhs: pattern to replace,
#'   rhs: pattern to use as replacement; passed to `stringr::str_replace_all`
#'   pattern argument `layer_name_date_pattern` to construct raster layer names
#' @param crop_latlon num; vector of length four (xmin, xmax, ymin, ymax).
#'   Min/max latitudes and longitudes to crop raster by. Default is no cropping.
#' @param memfrac num (0, 0.9); fraction of availible RAM to allow for writing.
#'   Passed to terra::writeRaster.
#'
#' @return chr; `out_path` where combined raster is written
#'
unzip_monthly_rasters <- function(zip_files, out_path, layer_name_date_pattern,
                                  name_replacement_pattern, crop_latlon = NULL,
                                  memfrac = NA) {
  # Load data ----
  # Unzip tif files to temp directory
  tif_files <- unzip_files(
    zip_files = zip_files,
    path_out = tempdir(),
    unzip_file_pattern = "|",
    file_out_pattern = ".tif$"
  )

  # Load rasters as multi-layer SpatRaster
  combined_rast <- terra::rast(tif_files)

  # Crop raster if crop_latlon is specified
  if (!is.null(crop_latlon)) {
    # Re-project extent to match combined_rast
    wndw <- terra::rast(extent = terra::ext(crop_latlon), crs = "EPSG: 4326") |>
      terra::project(terra::crs(combined_rast)) |>
      terra::ext()

    # Set analysis window
    terra::window(combined_rast) <- wndw
  }

  # Change layer names ----
  # Get input raster layer names
  default_layer_names <- names(combined_rast)

  # Extract 6 digits (%Y%m) and insert an _ after the 4th character
  corrected_layer_names <- default_layer_names |>
    stringr::str_extract(layer_name_date_pattern) |>
    stringr::str_replace_all(pattern = name_replacement_pattern)

  # Notify user of name change
  message(sprintf(
    "Changing the layer names.\nExample: '%s' --> '%s'",
    default_layer_names[[1]],
    corrected_layer_names[[1]]
  ))

  # Rename rasters
  names(combined_rast) <- corrected_layer_names

  # Write raster ----
  terra::writeRaster(
    combined_rast,
    out_path,
    overwrite = TRUE,
    memfrac = memfrac
  )

  return(out_path)
}


################################################################################
#                               Aggregate monthly                              #
################################################################################
#' Aggregate daily rasters to monthly rasters
#'
#' Wrapper for `terra::tapp()` to aggregate daily rasters to monthly rasters.
#'  Creates a raster time series dataset, performs monthly aggregation, and
#'  outputs a multi-layer `SpatRaster` where each layer is an aggregated month.
#'
#' @param in_data Multi-layer `SpatRaster` where each layer contains daily data
#'   or object that can be read by `terra::rast()`, such as a filename
#'   (character) to a raster file.
#' @param out_path character; path to output raster location; if NULL (default),
#'   raster will not be written and SpatRaster will be returned
#' @param in_dates a vector holding date/time data with a length equal to
#'   nlyr(`in_data`). Passed to `terra::tapp()`.
#' @param date_subset Date or character vector that can be coerced into a Date;
#'   vector of dates (days) to subset raster by
#' @param .fun a function (unquoted) to aggregate by. Passed to
#'   `terra::tapp()`.
#'
#' @return If out_path is NULL, a `SpatRaster` where each layer is a monthly
#'   aggregation of the daily input rasters; if `out_path` is not NULL, the path
#'   of the location where the raster was written
#'
aggregate_monthly <- function(in_data, out_path = NULL, in_dates,
                              date_subset = NULL, .fun = sum) {
  # Ensure data in correct format
  in_dates <- as.Date(in_dates)

  # Define date subset layer index
  lyr_idx <- NULL

  if (!is.null(date_subset)) {
    # Ensure date subset is in correct format
    date_subset <- as.Date(date_subset)

    # List of layer indices for date subset
    lyr_idx <- in_dates %within% lubridate::interval(
      min(date_subset),
      max(date_subset)
    ) |>
      which()

    # Subset in_dates
    in_dates <- in_dates[lyr_idx]
  }

  # Read in raster file as spatraster and assign time attribute
  if (inherits(in_data, "SpatRaster")) {
    rt <- in_data
  } else {
    rt <- terra::rast(in_data, lyrs = lyr_idx)
  }

  terra::time(rt, tstep = "days") <- in_dates

  # Perform summary function (.fun) and aggregate by yearmonth
  rast_out <- terra::tapp(rt, index = "yearmon", fun = .fun, na.rm = TRUE)

  # Convert decimal month to year_month (e.g., 2018.083 -> 2018_02) and name
  #  layers in YYYY_mm format
  yrmon_out <- sprintf(
    "%.0f_%02.f", # {integer}_{two digit integer, pad with 0}
    floor(terra::time(rast_out)), # round down year
    ((terra::time(rast_out) %% 1) * 12) + 1 # decimal month (e.g., 0.83 = Feb)
  )
  terra::set.names(rast_out, yrmon_out)


  # If out.path is defined, write raster; otherwise, return spatraster
  if (is.null(out_path)) {
    return(rast_out)
  } else {
    terra::writeRaster(rast_out, filename = out_path, overwrite = TRUE)
    return(out_path)
  }
}

################################################################################
#                    Extract soil moisture data from ESA-CCI                   #
################################################################################
#' Extract soil moisture variable from ESA-CCI data
#'
#' @param in_file
#' @param vars Variable to extract (sm = soil moisture) More info available in
#'   section 5.3.2.2 of the ESA-CCI Product User Guide (PUG) available at:
#'   https://www.esa-soilmoisture-cci.org/data
#'
#' @return a multi-layer `SpatRaster` with only the selected variable
#'
subset_cci <- function(in_file, vars = "sm") {
  out_rast <- terra::rast(in_file) |>
    terra::subset(subset = "sm")
  terra::set.names(out_rast, terra::time(out_rast))

  return(out_rast)
}

################################################################################
#               Combine daily CCI netCDFs into single raster file              #
################################################################################
#' Combine daily CCI netCDFs into single raster file
#'
#' @param in_rast_files character; vector of cci raster (netCDF) paths
#' @param out_path character; path and filename of daily cci raster
#'
#' @return A file path to the daily cci raster (passed to `terra::writeRaster`)
#'
combine_rasters <- function(in_rast_files, out_path) {
  in_rast_files |>
    purrr::map(subset_cci) |>
    terra::rast() |>
    terra::crop(terra::ext(-128, -64, 22, 56)) |>
    terra::writeRaster(filename = out_path, overwrite = TRUE)

  return(out_path)
}

################################################################################
#             Decompress SNODAS SWE data from tarball to spatraster            #
################################################################################
#' Decompress SNODAS SWE data from tarball (.dat.gz.tar) to spatraster
#'
#' @param in_files character; vector of paths to SNODAS .tar files
#' @param out_dir character; directory to extract files to
#'
#' @return spatraster
#'
decompress_snodas <- function(in_files, out_dir = tempdir()) {
  # Identify files to unzip
  swe_files <- sprintf(
    "us_ssmv11034tS__T0001TTNATS%s05HP001.dat.gz",
    stringr::str_extract(in_files, "\\d{8}")
  )

  gz_files <- file.path(out_dir, swe_files)

  # Delete intermediate files on exit
  on.exit(unlink(gz_files, force = TRUE), add = TRUE)

  purrr::walk2(
    in_files,
    swe_files,
    \(.x, .y) archive::archive_extract(archive = .x, dir = out_dir, files = .y)
  )

  daily_tifs <- purrr::map_chr(
    seq_along(gz_files),
    \(.x) tempfile(pattern = "_daily_", fileext = ".tif")
  )

  purrr::walk2(gz_files, daily_tifs, gz_to_tif)

  # Build output raster
  out_ras <- terra::rast(daily_tifs)

  return(out_ras)
}


#' Convert SNODAS gz files to tifs
#'
#' Information on how to convert the SNODAS binary files to projected GeoTIFFs
#' can be found on the SNODAS website:
#' https://nsidc.org/data/user-resources/help-center/how-do-i-convert-snodas-binary-files-geotiff-or-netcdf
#'
#' @param gz_file chr; file path to SNODAS gz file
#' @param out_file chr; file path for output GeoTIFF file
#'
#' @return `out_file`
#'
gz_to_tif <- function(gz_file, out_file) {
  arch <- archive::file_read(gz_file, mode = "rb")
  on.exit(close(arch), add = TRUE)

  out_rast <- arch |>
    readBin(n = 3351 * 6935, what = "integer", size = 2, endian = "big") |>
    matrix(nrow = 3351, ncol = 6935, byrow = TRUE) |>
    terra::rast()

  # Set attributes (mostly in-place)
  terra::NAflag(out_rast) <- -9999
  terra::time(out_rast) <- get_snodas_dates(gz_file)
  terra::set.crs(out_rast, "+proj=longlat +datum=WGS84")
  terra::set.names(out_rast, get_snodas_dates(gz_file))

  # Set extent
  terra::set.ext(
    out_rast,
    terra::ext(
      -124.733749999995013,
      -66.942083333330658,
      24.949583333332331,
      52.874583333331216
    )
  )

  terra::writeRaster(out_rast, filename = out_file)
  return(out_file)
}

################################################################################
#                   Unzip and aggregate daily data to monthly                  #
################################################################################
#' Unzip and aggregate daily data to monthly
#'
#' This is used in cases of daily zipped rasters which, when unzipped, may use
#'   too much storage space and/or memory.It  unzips and aggregates rasters,
#'   then deletes unzipped files.
#'
#' @param in_files character; vector of file paths/names to zip files
#' @param out_path character; path to output raster location; if NULL (default),
#'   raster will not be written and SpatRaster will be returned
#' @param method either "SNODAS" or "SSEBop", which describes the unzipping and
#'   date retrieval method to be used
#' @param .fun a function (unquoted) to aggregate by. Passed to `terra::tapp()`.
#'
#' @return a multi-layer `SpatRaster` of monthly data aggregated from daily; if
#'   `out_path` is not NULL, the path of the location where the raster was
#'   written input rasters
#'
unzip_aggregate <- function(in_files, out_path, method = NULL, .fun) {
  # create temp directory
  tmp <- tempdir(check = TRUE)
  on.exit(tempdir(check = TRUE), add = TRUE)


  # Unzip to temp directory and read in data based on data method
  if (method == "SSEBop") {
    # Unzip files to temp directory
    unzipped_files <- unzip_files(
      zip_files = in_files,
      path_out = tmp,
      unzip_file_pattern = ".tif$",
      file_out_pattern = ".tif$"
    )
    # Read in unzipped rasters
    annual_rast <- terra::rast(unzipped_files)
    # Get dates
    dates <- get_ssebop_date(in_files = in_files)
  } else if (method == "SNODAS") {
    annual_rast <- decompress_snodas(in_files = in_files, out_dir = tempdir())
    dates <- get_snodas_dates(in_files = in_files)
  } else {
    stop("Method must be one of: 'SSEBop' or 'SNODAS'.")
  }

  # Sum daily data by month
  monthly_rast <- aggregate_monthly(
    annual_rast,
    out_path = out_path,
    in_dates = dates,
    .fun = .fun
  )

  # Delete temp files
  all_files <- list.files(tempdir(), recursive = TRUE, full.names = TRUE) |>
    stringr::str_subset(stringr::fixed("_outraster_"), negate = TRUE)
  on.exit(unlink(all_files, force = TRUE), add = TRUE)

  return(monthly_rast)
}

################################################################################
#            Loop unzip and aggregate (`unzip_aggregate`) over years           #
################################################################################
#' Loop unzip and aggregate (`unzip_aggregate`) over years
#'
#'  This function splits a list of filenames by year and loops `unzip_aggregate`
#'    over years. This is used to reduce the amount of storage and memory
#'    consumed during the unzip and aggregate process at the expense of time.
#'
#' @param in_zip a character vector of file paths/names to zip files
#' @param out_path character; path to output raster location
#' @param method either "SNODAS" or "SSEBop", which describes the unzipping and
#'   date retrieval method to be used
#' @param dates Date; vector of dates corresponding to the files in `in_zip`
#' @param .fun a function (unquoted) to aggregate by. Passed to
#'   `terra::tapp()`.
#'
#' @return character; path of a multi-layer raster of monthly data aggregated
#'   from daily input rasters
#'
daily_zip_to_monthly_rast <- function(in_zip, out_path, method = NULL, dates,
                                      .fun) {
  # Split files by year
  files_split <- tibble::tibble(files = in_zip, dates = dates) |>
    dplyr::mutate(year = format(dates, "%Y"))

  files_split <- split(files_split[["files"]], files_split[["year"]])

  # Temporary annual output tif files
  annual_tif <- purrr::map_chr(
    seq_along(files_split),
    \(.x) tempfile(pattern = "_outraster_", fileext = ".tif")
  )
  on.exit(unlink(annual_tif, force = TRUE), add = TRUE)

  # Unzip and aggregate files by month
  rast_monthly <- purrr::map2(
    files_split,
    annual_tif,
    \(.x, .y) {
      unzip_aggregate(
        in_files = .x,
        out_path = .y,
        method = method,
        .fun = .fun
      )
    }
  )

  # Convert from a list of rasters to a single `SpatRaster`
  out_rast <- purrr::map(rast_monthly, terra::rast) |>
    terra::rast()

  # Set names of output raster to YYYY_mm
  lyr_names <- purrr::map(rast_monthly, ~ names(terra::rast(.x))) |>
    unlist(use.names = FALSE)
  terra::set.names(out_rast, lyr_names)
  terra::writeRaster(out_rast, filename = out_path, overwrite = TRUE)

  return(out_path)
}

################################################################################
#               Extract area-weighted mean of raster by polygons               #
################################################################################
#' Extract area-weighted mean of raster by polygons
#'
#' Extract area-weighted mean of raster by polygons. Then set column names to
#'   the date of the data and remove rows that contain no data (the raster data
#'   did not overlap the polygon)
#'
#' @param raster_data raster (`RasterLayer`, `RasterStack`, `RasterBrick`,
#'   or `SpatRaster`). All layers will be extracted as additional columns.
#' @param polygon_data named list; options of polygons (`sf`, `sfc`, or
#'   `SpatialPolygonsDataFrame`) to extract raster data to.
#' @param spatial_scale character; name of `polygon_data` element to use
#' @param id_col Character. Quoted name of column from `polygon_data` to be used
#'   as ID column
#' @param out_path_pattern character; pattern passed to `sprinf` to build output
#'   file path. File name is same as `raster_data` with "csv" replacing "csv"
#' @param fun A function to aggregate by (passed to `exact_extract`)
#' @param max_cells_in_memory numeric; the maximum number of cells that
#'   exact_extract should hold in memory when running the mean calculation
#' @param scale_factors named list where names are var_names and values are
#'   numbers to multiply the output values by
#'
#' @return A tibble in a wide format with with a length of `nrow(polygon_data)`
#'   that contain data and a width of `nlayers(raster_data)`.
#'
extract_awm <- function(raster_data, polygon_data, spatial_scale, id_col,
                        out_path_pattern, fun = "mean",
                        max_cells_in_memory = 3e+07,
                        scale_factors = NULL) {
  # Get output path
  raster_file <- stringr::str_remove(basename(raster_data), ".tif")

  out_path <- paste0(raster_file, ".csv") |>
    sprintf(fmt = out_path_pattern)

  # Scale factor
  if (is.null(scale_factors)) {
    scale_factor <- 1
  } else if (any(purrr::map_lgl(
    names(scale_factors),
    \(.x) stringr::str_detect(basename(raster_data), .x)
  ))) {
    scale_factor <- purrr::pluck(
      scale_factors,
      purrr::map_lgl(
        names(scale_factors),
        \(.x) stringr::str_detect(basename(raster_data), .x)
      ) |>
        subset(names(scale_factors), subset = _)
    )
  } else {
    scale_factor <- 1
  }

  # Read raster data
  raster_data <- terra::rast(raster_data)

  # Read in correct polygon at correct spatial scale
  polygon_data <- polygon_data[[spatial_scale]]

  # Remove HUCS with EMPTY geometries (very small areas which get dropped)
  polygon_data <- dplyr::filter(polygon_data, !sf::st_is_empty(polygon_data))

  # Transform polygon_data to match the projection of raster_data
  if (sf::st_crs(raster_data) != sf::st_crs(polygon_data)) {
    message("Reprojecting polygon data to match the projection of raster_data")
    polygon_data <- sf::st_transform(polygon_data, sf::st_crs(raster_data))
  }

  # Get dates
  dates <- names(raster_data)

  # Extract raster data by polygon data
  out <- exactextractr::exact_extract(
    x = raster_data,
    y = polygon_data,
    fun = fun,
    max_cells_in_memory = max_cells_in_memory
  ) |>
    # convert to tibble for console print ease
    tibble::as_tibble() |>
    # set column names (i.e., layer names) as dates
    setNames(dates) |>
    # Add ID column
    tibble::add_column(sf::st_drop_geometry(polygon_data[id_col]), .before = 1)

  # Multiply numeric columns by scale factor
  if (scale_factor != 1) {
    message(sprintf(
      "Scaling output: multiplying %s by %s",
      raster_file,
      scale_factor
    ))

    out <- dplyr::mutate(
      out,
      dplyr::across(dplyr::where(is.numeric), \(.x) .x * scale_factor)
    )
  }

  # Write data as CSV
  readr::write_csv(out, file = out_path)

  return(out_path)
}
