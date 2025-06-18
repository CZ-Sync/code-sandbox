#' Extract/calculate models and ensemble at HUC12 scale
#'
#' @param var_to_rerun chr vector length 1; name of variable to
#'   re-run. Must match the name of the named a variable in `model_vars`.
#' @param nc_files chr length 2; name-value pairs where names are "nhm" and
#'   "wrfhydro" and values are paths to corresponding netcdf files containing
#'   data to extract
#' @param model_vars chr length 2; name-value pairs where names are "nhm" and
#'   "wrfhydro" and values are all variable names that correspond with
#'   `model_csvs` and the value of `var_to_rerun`
#' @param model_csvs chr length 2; name-value pairs where names are "nhm" and
#'   "wrfhydro" and values are file paths to CSVs that contain the data for
#'   `model_vars`
#' @param out_path_pattern_ind chr; the pattern used to create the paths for the
#'   intermediate outputs from the individual models; should have 3 "%s": model,
#'   variable, unit
#' @param out_path_pattern_ens chr; the pattern used to create the paths for the
#'   ensemble output; should have 2 "%s": variable, unit
#' @param dataset_override (optional) named list, nested named list.
#'    Level 1: name = variable name, value = nested list... Only applied if name
#'      matches variable name specified in `var_to_rerun`
#'    Level 2: name-value pairs for `nc_files` and `method` values to override
#'      for variable name in Level 1
#'
#' @return chr; path to model ensemble of selected variable
#'
get_model_huc12 <- function(var_to_rerun, nc_files, model_vars, model_csvs,
                            month_filter, scale_factors,
                            out_path_pattern_ind, out_path_pattern_ens,
                            dataset_override = NULL) {
  # Replace arguments with overrides if necessary
  method <- list(nhm = "NHM", wrfhydro = "WRF-Hydro")

  if (var_to_rerun %in% names(dataset_override)) {
    dataset_override_sel <- dataset_override[[var_to_rerun]]
    nc_files <- modifyList(nc_files, dataset_override_sel[["nc_files"]])
    method <- modifyList(method, dataset_override_sel[["method"]])
  }

  # Get all inputs in the same order
  model_order <- c("nhm", "wrfhydro")
  nc_files <- nc_files[model_order]
  model_vars <- model_vars[model_order]
  model_csvs <- model_csvs[model_order]
  method <- method[model_order]

  # Get selected variable
  var_selected <- purrr::map(model_vars, \(.x) .x[var_to_rerun])

  # If the variable is a derived variable, compute it, otherwise extract the
  #   data for that variable. This way prevents re-extracting/writing variables
  #   unnecessarily.
  if (all(purrr::map_lgl(var_selected, \(.x) stringr::str_detect(.x, "=")))) {
    # If the variable is a derived variable (contains "=" in name) just re-run
    #   `compute_derived_variables` with `convert_to_huc8 = NULL`. We can safely
    #   skip the extraction phase because the extracted variables would be HUC12
    model_csvs <- purrr::pmap(
      list(
        model_vars_sel = var_selected,
        model_vars_sel_nm = names(var_selected),
        model_csvs = model_csvs,
        model_var_names = model_vars,
        out_pattern = sprintf(out_path_pattern_ind, names(nc_files), "%s")
      ),
      function(model_vars_sel, model_vars_sel_nm, model_csvs, model_var_names,
               out_pattern) {
        compute_derived_variables(
          derived_var_eq = setNames(model_vars_sel, model_vars_sel_nm),
          var_names = model_var_names,
          raw_csvs = model_csvs,
          convert_to_huc8 = NULL,
          out_path_pattern = out_pattern
        )
      }
    )
  } else {
    # If the variable is not a derived variable (no "=") then it is extracted
    #   from the netcdf
    model_csvs <- purrr::pmap(
      list(
        nc_file = nc_files,
        var_names = var_selected,
        out_pattern = sprintf(
          out_path_pattern_ind,
          names(nc_files),
          "%s",
          "%s"
        ),
        method = method
      ),
      function(nc_file, var_names, out_pattern, method) {
        model_nc_extract_vars_to_csv(
          nc_file = nc_file,
          method = method,
          var_names = var_names,
          month_filter = month_filter,
          scale_factors = scale_factors,
          convert_to_huc8 = NULL,
          out_path_pattern = out_pattern # varname then units
        )
      }
    )
  }

  # Ensemble the outputs from the function above
  ensemble_models(
    var_tbl_row = tibble::tibble(
      var_name = var_to_rerun,
      nhm_path = model_csvs[["nhm"]],
      wrfhydro_path = model_csvs[["wrfhydro"]]
    ),
    out_path_pattern = out_path_pattern_ens,
    nhm_file = model_csvs[["nhm"]],
    wrf_file = model_csvs[["wrfhydro"]],
    huc_col = "HUC"
  )
}
