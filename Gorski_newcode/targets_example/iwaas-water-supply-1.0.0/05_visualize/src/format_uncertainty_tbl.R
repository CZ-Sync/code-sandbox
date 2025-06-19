#' Build formatted gt table
#'
#' @param in_csv chr; path to CSV output from `calc_uncertainty_metrics`
#' @param out_path_pattern chr; pattern passed to sprint with one "%s" (for
#'  variable name)
#' @param variable_name chr; scalar of variable name
#' @param group_col chr; column name of column to group rows by
#' @param ... unquoted column names to use as id columns
#'
#' @return a `gt_tbl` object from gt package
#'
format_uncertainty_table <- function(in_csv, out_path_pattern, variable_name,
                                     group_col, ...) {
  # Construct output path
  out_path <- sprintf(out_path_pattern, variable_name)

  # Read and process data
  data <- readr::read_csv(
    in_csv,
    col_types = readr::cols(
      Season = "c",
      Region = "c",
      Model = "c",
      .default = "n"
    )
  ) |>
    tidyr::pivot_wider(
      id_cols = c(...),
      names_from = Season,
      values_from = tidyselect::where(is.numeric),
      names_glue = "{Season}_{.value}"
    ) |>
    dplyr::select(
      c(...),
      tidyselect::starts_with("Annual"),
      tidyselect::starts_with("Fall"),
      tidyselect::starts_with("Winter"),
      tidyselect::starts_with("Spring"),
      tidyselect::starts_with("Summer")
    )

  # Construct table
  out_gt <- gt::gt(data, groupname_col = group_col) |>
    gt::fmt_number(columns = tidyselect::contains("_")) |>
    gt::tab_spanner_delim("_") |>
    gt::tab_options(row_group.as_column = TRUE) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        weight = gt::px(2),
        color = "gray30"
      ),
      locations = gt::cells_body(columns = ends_with("_r"))
    )

  # Write table and output the path
  gt::gtsave(out_gt, out_path)
  return(out_path)
}
