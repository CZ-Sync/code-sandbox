#' Create data mask based on SWE conditions
#'
#' Outputs a data frame with same dimensions as `df_swe` where, for numeric
#' columns, values to keep are replaced with 1 and values to remove are replaced
#' with NA. This is determined by excluding HUC8s where > `area_threshold` is
#' covered by HUC12s that exceed `swe_threshold`.
#'
#' @param df_swe data frame of swe observations
#' @param df_xwalk data frame containing at least HUC12, HUC8, and area columns
#' @param swe_threshold num(1); depth in mm, above which, HUC12s are considered
#'   snow covered.
#' @param area_threshold num(1); fraction [0,1] of HUC8 snow-covered area, above
#'   which HUC8 should be excluded
#' @param swe_huc12_col chr(1); name of HUC12 column in `df_swe`
#' @param xwalk_cols chr(3); named character vector that specifies the names of
#'   HUC8, HUC12, and area columns. Names must be: "huc8", "huc12", "area".
#'
#' @return a data frame of the same dimensions as `df_swe` where value of 1 is
#' an observation that is valid and NA is an observation that should be excluded
#'
mask_by_swe <- function(csv_swe, df_xwalk, swe_threshold, area_threshold,
                        swe_huc12_col, xwalk_cols) {
  assertthat::assert_that(
    is.character(xwalk_cols),
    length(xwalk_cols) == 3,
    length(setdiff(names(xwalk_cols), c("huc8", "huc12", "area"))) == 0,
    msg = cli::cli_abort(c(
      "xwalk_cols must:",
      "i" = "be a named character vector with a length of 3",
      "i" = "contain the following names: huc8, huc12, area"
    ))
  )

  # Prep xwalk
  df_xwalk <- dplyr::select(df_xwalk, all_of(xwalk_cols))

  # Prep SWE
  df_swe <- readr::read_csv(
    csv_swe,
    col_types = readr::cols(!!swe_huc12_col := "c", .default = "n"),
    col_select = c(
      huc12 = tidyselect::all_of(swe_huc12_col),
      tidyselect::everything()
    )
  )

  # Make SWE mask
  df_swe |>
    # Replace NA observations with values that will end up being masked out
    dplyr::mutate(dplyr::across(
      matches("\\d{4}_\\d{2}"),
      \(.x) dplyr::if_else(is.na(.x), 100, .x)
    )) |>
    # Area is covered by snow (i.e., observations with >= 0.1 mm of SWE)
    dplyr::mutate(dplyr::across(
      tidyselect::matches("\\d{4}_\\d{2}"),
      \(.x) dplyr::if_else(.x >= swe_threshold, 1, 0)
    )) |>
    # Calculate proportion each HUC12 makes op of it's HUC8
    dplyr::left_join(df_xwalk, by = "huc12") |>
    dplyr::group_by(huc8) |>
    dplyr::mutate(area_prop = area / sum(area, na.rm = TRUE)) |>
    # Proportion of each HUC8 covered by snow
    dplyr::summarise(dplyr::across(
      tidyselect::matches("\\d{4}_\\d{2}"),
      \(.x) sum(.x * area_prop)
    )) |>
    # Mask out HUC8s where proportion covered by snow > threshold
    dplyr::mutate(dplyr::across(
      tidyselect::matches("\\d{4}_\\d{2}"),
      \(.x) dplyr::if_else(.x > area_threshold, NA, 1)
    )) |>
    dplyr::rename(!!swe_huc12_col := "huc8")
}
