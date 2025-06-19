#' Build tibble used to organize data used for uncertainty metric calculations
#'
#' @param benchmark_names named chr; vector of benchmark dataset names; names
#'   should be variable names that correspond with figs_variables
#' @param c404_vars named chr; vector of conus404 variables; names should be
#'   variable names that correspond with figs_variables
#' @param nhm_vars named chr; vector of nhm variables; names should be variable
#'   names that correspond with figs_variables
#' @param wrfhydro_vars named chr; vector of WRF-Hydro variables; names should
#'   be variable names that correspond with figs_variables
#' @param benchmark_csvs chr; vector of benchmark dataset csvs (huc by month)
#' @param c404_csvs chr; vector of conus404 dataset csvs (huc by month)
#' @param nhm_csvs chr; vector of nhm dataset csvs (huc by month)
#' @param wrfhydro_csvs chr; vector of WRF-Hydro dataset csvs (huc by month)
#'
#' @return a tibble where each row contains the information needed to complete
#'   one iteration of uncertainty metrics calculations
#'
build_uncertainty_tbl <- function(benchmark_names, c404_vars, nhm_vars,
                                  wrfhydro_vars, benchmark_csvs, c404_csvs,
                                  nhm_csvs, wrfhydro_csvs, ensemble_tbl,
                                  ensemble_csvs) {
  # Variables
  figs_variables <- names(benchmark_names)

  # Model groups
  model_groups <- c("CONUS404", "NHM", "WRF-Hydro", "Ensemble")

  # Extract Derived variable name from equations
  nhm_vars <- dplyr::if_else(
    stringr::str_detect(nhm_vars, "="),
    setNames(stringr::str_extract(nhm_vars, "^.*(?=(\\s=))"), names(nhm_vars)),
    nhm_vars
  )

  wrfhydro_vars <- dplyr::if_else(
    stringr::str_detect(wrfhydro_vars, "="),
    setNames(
      stringr::str_extract(
        wrfhydro_vars, "^.*(?=(\\s=))"
      ),
      names(wrfhydro_vars)
    ),
    wrfhydro_vars
  )

  # Subset and name CSVS
  benchmark_csvs <- purrr::map_chr(
    benchmark_names,
    \(.x) stringr::str_subset(benchmark_csvs, .x)
  )[figs_variables]
  nhm_csvs <- purrr::map_chr(
    nhm_vars,
    \(.x) stringr::str_subset(nhm_csvs, .x)
  )[figs_variables]
  wrfhydro_csvs <- purrr::map_chr(
    wrfhydro_vars,
    \(.x) stringr::str_subset(wrfhydro_csvs, .x)
  )[figs_variables]
  c404_csvs <- purrr::map_chr(
    c404_vars,
    \(.x) stringr::str_subset(c404_csvs, .x)
  )[figs_variables]
  ensemble_csvs <- setNames(
    ensemble_csvs,
    ensemble_tbl[["var_name"]]
  )[figs_variables]

  out <- tibble::tibble(
    variable = rep(figs_variables, length(model_groups)),
    benchmark = rep(benchmark_names[figs_variables], length(model_groups)),
    benchmark_data = benchmark_csvs[variable],
    model = rep(model_groups, each = length(figs_variables)),
    model_var_name = c(
      c404_vars[figs_variables],
      nhm_vars[figs_variables],
      wrfhydro_vars[figs_variables],
      names(ensemble_csvs[figs_variables])
    ),
    model_data = c(c404_csvs, nhm_csvs, wrfhydro_csvs, ensemble_csvs)
  ) |>
    dplyr::filter(!is.na(model_var_name)) |>
    dplyr::arrange(variable, benchmark, model)

  return(out)
}

#' Calculate uncertainty metrics for dataframe of predictions and observations
#'
#' @param data data frame; must have a column of prediction (`Preds`) and
#'   a observation (`Obs`) values
#' @param groups unquoted column name; columns to group by; passed to
#'   `dplyr::group_by(...)`
#' @param ... additional unquoted column names to group by; passed to
#'   `dplyr::group_by(...)`
#' @param season (optional) chr; if specified, replaces the Season column with
#'   single string given (probably "Annual")
#'
#' @return data frame with uncertainty calculations performed group-wise by the
#'   specified columns
#'
calc_uncertainty <- function(data, groups, ..., season = NULL) {
  out <- dplyr::group_by(data, {{ groups }}, ...) |>
    dplyr::mutate(dplyr::across(
      c(Obs, Preds),
      \(.x) ifelse(is.na(.x), NA_real_, .x)
    )) |>
    #dplyr::filter(!any(all(Obs == 0), all(Preds == 0))) |>
    dplyr::summarise(
      r = hydroGOF::rPearson(Preds, Obs),
      RMSE = hydroGOF::rmse(Preds, Obs),
      PBIAS = hydroGOF::pbias(Preds, Obs),
      NSE = hydroGOF::NSE(Preds, Obs),
      # If all values in a group are NA (happens in soil moisture data rarely),
      #   KGE returns an error. This catches the error and returns NA.
      KGE =  tryCatch(
        hydroGOF::KGE(Preds, Obs, na.rm = FALSE),
        error = \(x) NA_real_
      ),
      .groups = "drop"
    )

  if (!is.null(season)) {
    out <- dplyr::mutate(out, Season = season)
  }

  return(out)
}

#' Build table that contains uncertainty metrics for a singl variable
#'
#' @param uncert_tbl a date frame (creaeted from `build_uncertainty_tbl`) that
#'   contains rows FOR A SINGLE VARIABLE to calculate uncertainty for
#' @param out_path_pattern chr; a pattern passed to `sprintf` to build output
#'  file path; should contain 1 "%s" for the variable name
#' @param huc_col chr; name of column containing HUCs in input datasets
#' @param xwalk data.frame; Hydrologic region to HUC crosswalk
#' @param xwalk_huc_col chr; column used to identify HUCs in `xwalk`
#' @param xwalk_reg_col chr; column used to identify regions in `xwalk`
#' @param xwalk_huc_col_override named list; name-value pairs where the name is
#'   the variable name where a different huc column should be used and the value
#'   is the name of the huc column to be used (chr)
#'
#' @return chr; path to data frame with summary of uncertainty metrics by
#'   season and region for a single variable
#'
calc_uncertainty_metrics <- function(uncert_tbl, out_path_pattern,
                                     huc_col = "HUC", xwalk,
                                     xwalk_huc_col, xwalk_reg_col,
                                     xwalk_huc_col_override = NULL) {
  # Get variable from table and ensure there is only 1
  variable <- unique(uncert_tbl[["variable"]])

  assertthat::assert_that(
    length(variable) == 1,
    msg = "`uncert_tbl[[\"variable\"]]` must contain only 1 unique value"
  )

  # Build output path
  out_path <- sprintf(out_path_pattern, variable)

  # Change xwalk_huc_col if override is set
  if (variable %in% names(xwalk_huc_col_override)) {
    xwalk_huc_col <- xwalk_huc_col_override[[variable]]
  }

  # Load crosswalk
  xwalk <- xwalk |>
    # Remove unnecessary columns and remove NULL regions
    dplyr::select(
      HUC = !!xwalk_huc_col,
      Region = !!xwalk_reg_col,
      Area = Area_sqkm
    ) |>
    dplyr::filter(Region != "NULL") |>
    # Remove duplicates (i.e., HUC8s that are within more than 1 VM region) by
    #   assigning each HUC to the VM region that covers the majority of it.
    dplyr::group_by(HUC, Region) |>
    dplyr::summarise(Area = sum(Area, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(HUC) |>
    dplyr::slice_max(Area) |>
    # Remove unnecessary columns
    dplyr::select(HUC, Region)


  # Build table of uncertainty metrics
  metric_table <- purrr::pmap(
    uncert_tbl, function(...) {
      # Perform the following calculations for each row in uncert_tbl
      uncert_tbl_row <- tibble::tibble(...)

      # Prep data for uncertainty calculations
      obs_pred_long <- list(
        predictions = uncert_tbl_row[["model_data"]],
        observations = uncert_tbl_row[["benchmark_data"]]
      ) |>
        # Read and pivot observations and predictions longer
        purrr::map2(
          list("Preds", "Obs"),
          ~ readr::read_csv(
            .x,
            col_types = readr::cols(HUC = "c", .default = "n")
          ) |>
            tidyr::pivot_longer(
              cols = !HUC,
              names_to = c("Year", "Month"),
              names_sep = "_", values_to = .y
            )
        ) |>
        # Join observation and predictions there the
        #  `with` allows for calling input list elements by name directly, which
        #  aids in use of the pipe operator see `?with`
        with(dplyr::inner_join(
          observations,
          predictions,
          by = dplyr::join_by(HUC, Year, Month)
        )) |>
        # Add season column
        dplyr::mutate(Season = dplyr::case_when(
          Month %in% c("12", "01", "02") ~ "Winter",
          Month %in% c("03", "04", "05") ~ "Spring",
          Month %in% c("06", "07", "08") ~ "Summer",
          Month %in% c("09", "10", "11") ~ "Fall"
        )) |>
        dplyr::filter(dplyr::if_all(c(Obs, Preds), complete.cases))

      # Perform uncertainty calculations for Annual and Seasonal time periods
      dplyr::bind_rows(
        calc_uncertainty(obs_pred_long, HUC, season = "Annual"),
        calc_uncertainty(obs_pred_long, HUC, Season)
      ) |>
        # Join crosswalk
        dplyr::inner_join(xwalk, by = "HUC") |>
        # Add model name
        dplyr::mutate(Model = uncert_tbl_row[["model"]]) |>
        # Rearrange columns
        dplyr::select(HUC, Season, Region, Model, tidyselect::everything()) |>
        # Calculate grouped mean for each metric column
        dplyr::group_by(Season, Region, Model) |>
        dplyr::summarise(
          dplyr::across(
            dplyr::where(is.numeric),
            ~ mean(.x, na.rm = TRUE)
          ),
          .groups = "drop"
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(Season, Region, Model)

  readr::write_csv(metric_table, out_path)

  return(out_path)
}
