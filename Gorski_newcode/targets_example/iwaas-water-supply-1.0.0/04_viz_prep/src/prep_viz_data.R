#' Rasterize an sf object
#'
#' @param sf an sf object
#' @param var_name character, passed to `fasterize::fasterize` `field` argument.
#'   `sf` column name that should be used to provide the value for the raster
#'   cells.
#' @param res Numeric. Raster resolution (in meters)
#'
#' @return a spatraster
#'
rasterize_sf <- function(sf, var_name, res) {
  # Ensure projection has meters as linear unit
  if (sf::st_crs(sf, parameters = TRUE)[["units_gdal"]] != "metre") {
    stop("sf object must be in projected with meters as the linear unit")
  }

  out_rast <- fasterize::fasterize(
    sf = sf,
    field = var_name,
    raster = raster::raster(
      ext = raster::extent(sf),
      resolution = res
    )
  ) |>
    terra::rast()

  terra::crs(out_rast) <- sf::st_crs(sf)[["wkt"]]

  return(out_rast)
}


#' Rasterize model (NHM/WRF-Hydro) HUC-wise tabular summaries
#'
#' @param sf an sf object
#' @param summary_csvs character vector of file paths to input summary csvs
#' @param var_names character or (preferably) named list
#'   (e.g., `list(Precipitation = "Ppt`, ...), of `sf` column names that should
#'   be used to provide the value for the raster
#'   cells.
#' @param res numeric, raster resolution (in meters)
#' @param out_path_pattern character, pattern to build output file paths from
#'   containing 1 "%s" to insert var_names
#' @param sf_override a named list where the name is the variable name and the
#'   value is the name of the target that should replace sf in the case of that
#'   variable.
#'
#' @return character, file paths to rasters, each raster having one layer for
#'   each var_name
#'
rasterize_summaries <- function(sf, summary_csvs, var_names, res = 1000,
                                out_path_pattern, sf_override = NULL) {
  # Extract Derived variable name from equations
  var_names <- dplyr::if_else(
    stringr::str_detect(var_names, stringr::fixed("=")),
    setNames(
      stringr::str_extract(var_names, "^.*(?=(\\s=))"),
      names(var_names)
    ),
    var_names
  )

  assertthat::assert_that(
    !any(
      duplicated(names(var_names)[nzchar(names(var_names))]),
      duplicated(var_names)
    ),
    msg = "There must not be duplicate values or names in var_names"
  )

  assertthat::assert_that(
    stringr::str_count(out_path_pattern, stringr::fixed("%s")) == 1,
    msg = "`out_path_pattern` must contain exactly one instance of '%s'"
  )

  # Prep output paths
  out_rast <- sprintf(out_path_pattern, var_names)

  # Read  in data as list of data frames
  # Get the correct file path for the correct var_name
  in_csvs <- purrr::map_chr(
    setNames(var_names, var_names),
    \(.x) stringr::str_subset(summary_csvs, sprintf("_%s", .x))
  )

  model_df <- purrr::map(
    in_csvs,
    \(.x) {
      readr::read_csv(
        .x,
        col_types = readr::cols(HUC = "c", .default = "n")
      )
    }
  )

  # For each variable in var_names, rasterize using HUC12 spatial data
  # Output a vector of file paths (by variable) to multilayer (seasonal) rasters
  purrr::pwalk(
    list(
      mapped_model_df = model_df,
      mapped_out_rast = out_rast,
      mapped_var_name = var_names
    ),
    function(mapped_model_df, mapped_out_rast, mapped_var_name) {
      # Override sf if necessary
      if (!is.null(sf_override)) {
        if (mapped_var_name %in% names(sf_override)) {
          sf_current <- sf_override[[mapped_var_name]]
        } else {
          sf_current <- sf
        }
      } else {
        sf_current <- sf
      }

      # Join HUC spatial data to dataframe of summarized variables
      huc_sf <- sf_current |>
        dplyr::select(HUC) |>
        dplyr::left_join(mapped_model_df, by = "HUC") |>
        sf::st_cast("MULTIPOLYGON")

      # Remove unnecessary columns
      group_names <- stringr::str_subset(
        colnames(huc_sf),
        "HUC|SHAPE|geometry",
        negate = TRUE
      )

      # Rasterize data
      # Make a raster layer for each season
      purrr::map(
        group_names,
        \(mapped_var_name) {
          rasterize_sf(
            sf = huc_sf,
            var_name = mapped_var_name,
            res = res
          )
        }
      ) |>
        terra::rast() |>
        setNames(group_names) |>
        terra::writeRaster(filename = mapped_out_rast, overwrite = TRUE)
    }
  )
  return(out_rast)
}



#' Rasterize OCONUS model (NHM/WRF-Hydro) HUC-wise tabular summaries
#'
#' @param sf an sf object
#' @param summary_csvs character vector of file paths to input summary csvs
#' @param crs_list list of values that can be read by `terra::crs()`.
#' @param var_name character, passed to `fasterize::fasterize` `field` argument.
#'   `sf` column name that should be used to provide the value for the raster
#'   cells.
#' @param res Numeric. Raster resolution (in meters)
#' @param out_path_pattern character, pattern to build output file paths from
#'   containing 1 "%s" to insert var_names
#' @param sf_override a named list where the name is the variable name and the
#'   value is the name of the target that should replace sf in the case of that
#'   variable.
#'
#' @return character, file paths to rasters, each raster having one layer for
#'   each var_name
#'
rasterize_oconus_summaries <- function(sf, summary_csvs, crs_list = NULL,
                                       var_names, res = 1000, out_path_pattern,
                                       sf_override = NULL) {
  # Get region_group
  region_group <- sf::st_drop_geometry(sf["region_group"]) |>
    dplyr::distinct() |>
    dplyr::pull(region_group)

  assertthat::assert_that(
    length(region_group) == 1,
    msg = "There must only be a single value in `sf$region_group`"
  )

  if (region_group %in% names(crs_list)) {
    sf <- terra::vect(sf) |>
      terra::project(y = crs_list[[region_group]]) |>
      sf::st_as_sf()

    if (!is.null(sf_override)) {
      sf_override <- purrr::modify(
        sf_override,
        \(.x) {
          terra::vect(.x) |>
            terra::project(y = crs_list[[region_group]]) |>
            sf::st_as_sf()
        }
      )
    }
  } else {
    message("No entry in `crs_list` matching `sf$region_group`.")
  }

  out_path_pattern <- sprintf(out_path_pattern, "%s", region_group)

  out <- rasterize_summaries(
    sf = sf,
    summary_csvs = summary_csvs,
    var_names = var_names,
    res = res,
    out_path_pattern = out_path_pattern,
    sf_override = sf_override
  )

  return(out)
}
