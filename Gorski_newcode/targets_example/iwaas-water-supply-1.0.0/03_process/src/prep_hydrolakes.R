#' Prepare hydrolakes data
#'
#' Because this function is specific to hydrolakes, it doesn't seem important to
#' generalize it much further (i.e., dynamic volume column name)
#'
#' @param in_df chr; hydrolakes data frame. Note, column names and
#'   units are hardcoded into the function (i.e., vol_m3 = "m3").
#' @param var_name chr; named value pair, where name is variable and the value
#'   is what that variable is named in the dataset.
#' @param out_pattern chr; path to output csv with one '%s' which will become
#'   the variable name.
#' @param xwalk data frame; HUC12 to hydrologic region crosswalk. Note, column
#'   namesand units are hardcoded into the function (i.e., Area_sqkm = "km2").
#' @param huc_col chr; HUC12 column name in hydrolakes CSVs (`in_csvs`)
#' @param xwalk_huc_col chr; HUC12 column names in `xwalk`
#'
#' @return `out_csv`, where a single CSV is stored with the cobined hydrolakes
#'   data
#'
prep_hydrolakes <- function(in_df, var_name, out_pattern, xwalk, huc_col,
                            xwalk_huc_col) {
  assertthat::assert_that(
    stringr::str_count(out_pattern, "%s") == 1,
    msg = "`out_pattern` must contain exactly one instance of '%s'."
  )

  out_path <- sprintf(out_pattern, var_name)

  in_df |>
    # Join xwalk
    dplyr::left_join(
      xwalk,
      by = dplyr::join_by(!!huc_col == !!xwalk_huc_col),
      relationship = "many-to-many"
    ) |>
    # Replace NA in hydrolakes volume with 0
    tidyr::replace_na(list(vol_m3 = 0)) |>
    # Convert lake storage to depth in mm
    dplyr::mutate(lake_storage_mm = vol_to_depth(
      volume = vol_m3,
      area = Area_sqkm,
      units_volume = "m3",
      units_area = "km2",
      units_depth = "mm",
      drop_units = TRUE
    )) |>
    dplyr::select(HUC = !!huc_col, lake_storage_mm) |>
    readr::write_csv(out_path)

  return(out_path)
}

#' Unit-aware method to convert a volume and area to depth
#'
#' Depth = volume/area. Units are retained and converted appropriately.
#'
#' @param volume num; a volume
#' @param area  num; an area to divide the volume over
#' @param units_volume,units_area,units_depth chr; unit symbol for inputs
#'   (`volume`, `area`) and output (`depth`). A list of valid unit symbols can
#'   be viewed with `?units::units_depth`.
#' @param drop_units lgl; Should units attribute be dropped in the output
#'   (TRUE) or retained (FALSE)
#'
#' @return a numeric vector (or units if `drop_units = FALSE`) of depth
#'
vol_to_depth <- function(volume, area, units_volume = "m3", units_area = "km2",
                         units_depth = "mm", drop_units = TRUE) {
  # Descriptive error if units don't make sense
  output_unit <- paste(units_volume, units_area, sep = "/")
  if (!units::ud_are_convertible(output_unit, units_depth)) {
    cli::cli_abort(c(
      "The output cannot be defined in the units selected.",
      "i" = "The output unit of `{units_volume}/{units_area}` is
      '{output_unit}'",
      "x" = "'{output_unit}' is not convertable to '{units_depth}'"
    ))
  }

  # Calculate depth and convert units
  depth <- units::as_units(volume, units_volume) / units::as_units(area, units_area)
  units(depth) <- units_depth

  # Remove units attribute of output if requested
  if (drop_units) {
    depth <- units::drop_units(depth)
  }

  return(depth)
}
