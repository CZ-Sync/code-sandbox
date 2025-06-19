#' Build table with variables and paths of model data to ensemble
#'
#' @param nhm_vars list (chr); named list of NHM variables
#' @param nhm_files chr; vector of paths to NHM data to ensemble
#' @param wrfhydro_vars list (chr); named list of WRF-Hydro variables
#' @param wrfhydro_files chr; vector of paths to WRF-Hydro data to ensemble
#'
#' @return tibble
#'
build_ensemble_tbl <- function(nhm_vars, nhm_files, wrfhydro_vars,
                               wrfhydro_files) {
  # Ensure that variable name (left of equal sign) is present within
  # the corresponding file name
  assertthat::assert_that(
    # NHM
    all(purrr::map2_lgl(
      basename(nhm_files),
      stringr::str_remove(nhm_vars, " =.*"),
      stringr::str_detect
    )),
    # WRF-Hydro
    all(purrr::map2_lgl(
      basename(wrfhydro_files),
      stringr::str_remove(wrfhydro_vars, " =.*"),
      stringr::str_detect
    )),
    msg = "The values in nhm_vars and wrfhydro_vars (left of the equal sign, if applicable) must be present in the corresponding file.
    They must be in the same order."
  )

  # Subset to only variables that are present in both models
  vars_to_ensemble <- intersect(names(nhm_vars), names(wrfhydro_vars))

  cli::cli_inform(c(
    "i" = "The following variables will be ensembled:",
    "*" = toString(vars_to_ensemble)
  ))

  names(nhm_files) <- names(nhm_vars)
  names(wrfhydro_files) <- names(wrfhydro_vars)

  # Construct tibble with variables and paths to corresponding model data
  var_tbl <- tibble::tibble(
    var_name = vars_to_ensemble,
    nhm_path = nhm_files[vars_to_ensemble],
    wrfhydro_path = wrfhydro_files[vars_to_ensemble]
  )

  return(var_tbl)
}

#' Apply function to identically structured data frames to combine them
#'
#' If using targets, should branch over `var_tbl_row` (input from
#'   `build_ensemble_tbl`). `nhm_files` and `wrf_files` should be upstream file
#'   targets from `tarchetypes::tar_files`. This will ensure that if a file gets
#'   updated, only the relevant branch is rebuilt. It also negates the need for
#'   including a hash table as targets will check hashes via the upstream
#'   `tar_files` call.
#'
#' @param var_tbl_row 1 row tibble; should be row from output of
#'   build_ensemble_tbl
#' @param out_path_pattern chr; string passed to `sprintf` to define the output
#'   path. Should have one instance of "%s" to insert the variable name.
#' @param nhm_file chr; vector of file paths to NHM files matching
#' @param wrf_file chr; vector of file paths to WRF-Hydro files matching
#' @param huc_col chr; name of column used to identify HUCs in NHM and WRF-Hydro
#'   files.
#'
#' @return character; path to output CSV of model ensemble data frame
#'
ensemble_models <- function(var_tbl_row, out_path_pattern, nhm_file, wrf_file,
                            huc_col = "HUC") {
  assertthat::assert_that(
    nrow(var_tbl_row) == 1,
    msg = "var_tbl_row must be a single row.
    If using targets, consider branching across var_tbl_row."
  )

  assertthat::assert_that(
    all(
      var_tbl_row[["nhm_path"]] == nhm_file,
      var_tbl_row[["wrfhydro_path"]] == wrf_file
    ),
    msg = "The paths in var_tbl_row must match nhm_file and wrf_file."
  )

  # Read in CSVs
  #   CSVs are wide format, with a HUC column (chr) and year_month columns (num)
  #   where the values are the values for the given variable (described in file
  #   name) at a given HUC (row) for a given year_month (column)
  nhm_water_budget_df <- data.table::fread(
    var_tbl_row[["nhm_path"]],
    colClasses = c(HUC = "character")
  )
  wrf_water_budget_df <- data.table::fread(
    var_tbl_row[["wrfhydro_path"]],
    colClasses = c(HUC = "character")
  )

  nhm_anti_df <- setdiff(
    nhm_water_budget_df[["HUC"]],
    wrf_water_budget_df[["HUC"]]
  )
  wrf_anti_df <- setdiff(
    wrf_water_budget_df[["HUC"]],
    nhm_water_budget_df[["HUC"]]
  )

  # Add absent rows (with NA values) to each model and arrange data frames
  nhm_water_budget_df <- nhm_water_budget_df |>
    tibble::add_case(HUC = wrf_anti_df) |>
    tidytable::arrange(HUC)

  wrf_water_budget_df <- wrf_water_budget_df |>
    tibble::add_case(HUC = nhm_anti_df) |>
    tidytable::arrange(HUC)

  # # Ensure input data is structured the identically ----
  if (!identical(nhm_water_budget_df[["HUC"]], wrf_water_budget_df[["HUC"]])) {
    cli::cli_abort(c(
      "There is a discrepency between `nhm_file` and `wrf_file` in the `HUC`
      column.",
      "x" = "`nhm_file` and `wrf_file` must be able to be structured
      identically."
    ))
  }

  if (!identical(names(nhm_water_budget_df), names(wrf_water_budget_df))) {
    cli::cli_abort(c(
      "There is a discrepency between column names of `nhm_file` and
      `wrf_file`.",
      "x" = "`nhm_file` and `wrf_file` must be able to be structured
      identically."
    ))
  }

  # Average model values as matrices for faster (vectorized) processing ----
  nhm_long <- nhm_water_budget_df |>
    tidytable::pivot_longer(!HUC, values_to = "nhm")
  wrf_long <- wrf_water_budget_df |>
    tidytable::pivot_longer(!HUC, values_to = "wrf")
  ens_long <- tidytable::full_join(nhm_long, wrf_long, by = c("HUC", "name"))
  ens_long[["ens"]] <- rowMeans(ens_long[, c("nhm", "wrf")], na.rm = TRUE)

  # Reassemble data frame ----
  ensemble_df <- ens_long |>
    tidytable::select(HUC, name, ens) |>
    tidytable::pivot_wider(
      names_from = name,
      values_from = ens
    )

  # Quick unit test to verify results ----
  rand_obs <- sample(seq_len(nrow(ens_long)), size = 100)
  expected <- dplyr::bind_cols(
    wrf_long[rand_obs, "wrf"],
    nhm_long[rand_obs, "nhm"]
  ) |>
    rowMeans(na.rm = TRUE)
  observed <- purrr::map_dbl(
    rand_obs,
    \(obs) {
      row <- which(ensemble_df[["HUC"]] == ens_long[["HUC"]][obs])
      col <- ens_long[["name"]][obs]
      ensemble_df[[col]][row]
    }
  )

  if (!identical(expected, observed)) {
    cli::cli_abort(c(
      "Unit test failed",
      "x" = "Mean values from `nhm_file` and `wrf_file` did not equal output of
      ensemble."
    ))
  }

  # Write CSV
  out_path <- sprintf(out_path_pattern, var_tbl_row[["var_name"]])
  data.table::fwrite(ensemble_df, file = out_path)

  return(out_path)
}
