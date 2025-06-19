#' Build tibble for breakdown figures
#'
#' @param figs_variables chr; vector of variable names to build breakdown
#'   figures for
#' @param benchmark_names named chr; vector of benchmark dataset names; names
#'   should be variable names that correspond wit figs_variables
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
#' @param huc_col chr; length 1, column name of huc column for all datasets
#' @param xwalk_huc_col chr; length 1, column name of huc column for xwalk
#'
#' @return tibble
#'
build_breakdown_var_tbl <- function(figs_variables, benchmark_names, c404_vars,
                                    nhm_vars, wrfhydro_vars, benchmark_csvs,
                                    c404_csvs, nhm_csvs, wrfhydro_csvs, huc_col,
                                    xwalk_huc_col) {
  # Extract Derived variable name from equations
  nhm_vars <- dplyr::if_else(
    stringr::str_detect(nhm_vars, stringr::fixed("=")),
    setNames(stringr::str_extract(nhm_vars, "^.*(?=(\\s=))"), names(nhm_vars)),
    nhm_vars
  )

  wrfhydro_vars <- if_else(
    stringr::str_detect(wrfhydro_vars, stringr::fixed("=")),
    setNames(
      stringr::str_extract(wrfhydro_vars, "^.*(?=(\\s=))"),
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

  tibble::tibble(
    variable = figs_variables,
    benchmark = benchmark_names[figs_variables],
    c404 = c404_vars[figs_variables],
    nhm = nhm_vars[figs_variables],
    wrfhydro = wrfhydro_vars[figs_variables]
  ) |>
    # Subset csv lists to return correct data file for each dataset
    dplyr::mutate(
      benchmark_data = benchmark_csvs,
      c404_data = c404_csvs,
      nhm_data = nhm_csvs,
      wrfhydro_data = wrfhydro_csvs
    ) |>
    # Add file hashes for map_data files
    dplyr::mutate(data_hash = pmap(
      dplyr::pick(tidyselect::everything()), # calls current data
      function(benchmark_data, c404_data, nhm_data, wrfhydro_data, ...) {
        purrr::map_chr(
          c(benchmark_data, c404_data, nhm_data, wrfhydro_data),
          ~ ifelse(is.na(.x), NA, digest::digest(.x, file = TRUE))
        )
      }
    )) |>
    # Add names of HUC columns for joining
    dplyr::mutate(
      benchmark_huc_col = huc_col,
      c404_huc_col = huc_col,
      nhm_huc_col = huc_col,
      wrfhydro_huc_col = huc_col,
      xwalk_huc_col = xwalk_huc_col
    )
}
