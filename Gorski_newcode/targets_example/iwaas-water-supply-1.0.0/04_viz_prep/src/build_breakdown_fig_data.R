#' Helper for `prep_breakdown_data`
#'
#' Convert wide huc x yr_mon dataset into long summaries of area weighted mean
#'
#' @param dataframe data.frame
#' @param id_col_data chr; name of id column in `data_path`
#' @param xwalk data frame; HUC to Van Metre region crosswalk
#' @param id_col_xwalk chr; name of id column in `xwalk`
#' @param area_col chr; name of area column in `xwalk`
#' @param data_name chr; name of dataset (i.e., "Benchmark", "NHM", "WRF-Hydro")
#'
#' @return long-format tibble of means by date for single dataset
#'
awm_by_date <- function(dataframe, id_col_data, xwalk, id_col_xwalk, area_col,
                        data_name) {
  dataframe |>
    # Convert to long data - "date" is column for former column names
    tidyr::pivot_longer(
      col = -tidyselect::all_of(id_col_data),
      names_to = "date"
    ) |>
    # Join crosswalk data by HUC
    dplyr::left_join(
      xwalk,
      by = dplyr::join_by(!!id_col_data == !!id_col_xwalk)
    ) |>
    # Remove NAs in values and area columns - can't run weighted.mean with NAs
    dplyr::filter(!is.na(value), !is.na(.data[[area_col]])) |>
    # Weighted mean by date - mean of values weighted by area
    dplyr::group_by(date) |>
    dplyr::summarize(
      !!data_name := weighted.mean(x = value, w = .data[[area_col]]),
      .groups = "drop"
    )
}

#' Helper for `prep_breakdown_data`
#'
#' Convert wide huc x yr_mon dataset into long summaries of area weighted mean
#'
#' @param dataframe data.frame
#' @param id_col_data chr; name of id column in `data_path`
#' @param xwalk data frame; HUC to Van Metre region crosswalk
#' @param id_col_xwalk chr; name of id column in `xwalk`
#' @param area_col chr; name of area column in `xwalk`
#' @param reg_col chr; name of region column in `xwalk`
#' @param agg_reg_col chr; name of aggregated region column in `xwalk`; note
#'   that agg region "Northeast through Midwest" is shortened to "NE/Midwest"
#' @param data_name chr; name of dataset (i.e., "Benchmark", "NHM", "WRF-Hydro")
#'
#' @return long-format tibble of means by region and season for single dataset
#'
awm_by_reg_seas <- function(dataframe, id_col_data, xwalk, id_col_xwalk,
                            area_col, reg_col, agg_reg_col, data_name) {
  out_seas <- dataframe |>
    # Convert to long data - "date" is column for former column names
    tidyr::pivot_longer(
      col = -tidyselect::all_of(
        id_col_data
      ),
      names_to = "date"
    ) |>
    # Add season column
    dplyr::mutate(season = stringr::str_sub(date, 6, 7)) |>
    dplyr::mutate(season = as.character(factor(
      x = season,
      levels = c(sprintf("%02d", c(12, 1:11))),
      labels = rep(c("winter", "spring", "summer", "fall"), each = 3)
    ))) |>
    # Seasonal mean
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(.data[[id_col_data]], season) |>
    dplyr::summarise(value = mean(value), .groups = "drop") |>
    # Join crosswalk data by HUC
    dplyr::left_join(
      xwalk,
      by = dplyr::join_by(!!id_col_data == !!id_col_xwalk)
    ) |>
    # Remove NAs in values and area columns - can't run weighted.mean with NAs
    dplyr::filter(.data[[reg_col]] != "NULL", !is.na(.data[[area_col]])) |>
    # Weighted mean by region - mean of values weighted by area
    dplyr::group_by(.data[[reg_col]], season, .data[[agg_reg_col]]) |>
    dplyr::summarise(
      !!data_name := weighted.mean(x = value, w = .data[[area_col]]),
      .groups = "drop"
    )

  out_ann <- out_seas |>
    dplyr::group_by(.data[[reg_col]], .data[[agg_reg_col]]) |>
    dplyr::summarise(
      !!data_name := mean(.data[[data_name]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(season = "annual") |>
    dplyr::select(colnames(out_seas))

  out <- dplyr::bind_rows(out_seas, out_ann) |>
    dplyr::arrange(!!reg_col, season)

  return(out)
}

#' Prep data for breakdown for single variable
#'
#' @param var_tbl_row 1 row of the variable tibble, output from
#'   `build_breakdown_var_tbl`
#' @param figure_method chr; either "bar" or "line"; prep data for which figure
#' @param vm_xwalk data.frame; HUC to Hydrologic (Van Metre) region crosswalk
#' @param area_col chr; name of area column in `vm_xwalk`
#' @param reg_col hr; name of region column in `vm_xwalk`
#' @param agg_reg_col chr; name of aggregated region column in `vm_xwalk`
#' @param huc8_override named list where the name is the variable name and the
#'   value is the HUC8 column name to use instead of the value in
#'   `var_tbl_row[["xwalk_huc_col"]]`
#'
#' @return long-format tibble of means by date for benchmark, NHM, and WRF-Hydro
#'
prep_breakdown_data <- function(var_tbl_row, figure_method, vm_xwalk,
                                area_col, reg_col, agg_reg_col,
                                huc8_override = NULL) {
  # Load crosswalk
  # Rename huc12 column to "HUC" unless huc8_override, then huc8 -> HUC
  id_col_xwalk <- "HUC"

  if (isTRUE(var_tbl_row[["variable"]] %in% names(huc8_override))) {
    xwalk_name_repair <- ~ if_else(
      str_detect(.x, unlist(huc8_override)[var_tbl_row[["variable"]]]),
      id_col_xwalk,
      .x
    )
  } else {
    xwalk_name_repair <- ~ dplyr::if_else(
      str_detect(.x, var_tbl_row[["xwalk_huc_col"]]),
      id_col_xwalk,
      .x
    )
  }

  vm_xwalk <- vm_xwalk |>
    dplyr::select(tidyselect::starts_with(
      c("huc12", "huc8", !!reg_col, !!agg_reg_col, !!area_col)
    )) |>
    dplyr::rename_with(xwalk_name_repair)

  assertthat::assert_that(
    area_col %in% colnames(vm_xwalk),
    msg = "`area_col` must appear in dataframe at `vm_xwalk_path`"
  )

  # If huc8_override, summarize area by HUC8
  if (isTRUE(var_tbl_row[["variable"]] %in% names(huc8_override))) {
    if (figure_method == "line") {
      vm_xwalk <- vm_xwalk |>
        # Summarize area by HUC and region
        dplyr::group_by(HUC) |>
        dplyr::summarise(!!area_col := sum(.data[[area_col]]), .groups = "drop")
    } else if (figure_method == "bar") {
      vm_xwalk <- vm_xwalk |>
        # Summarize area by HUC and region
        dplyr::group_by(HUC, .data[[reg_col]], .data[[agg_reg_col]]) |>
        dplyr::summarise(
          !!area_col := sum(.data[[area_col]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::group_by(HUC) |>
        # Where there are duplicate HUCs (e.g., part of the HUC12s in a HUC 8
        # are in one Region, and part are in another), return the Region and
        # AggRegion for the Region with the higher proportion of area. Where the
        # highest region is "NULL", use the second highest. Take the sum of the
        # area of all HUCs by region.
        dplyr::summarize(
          !!reg_col := ifelse(
            # If region with the largest area is "NULL"...
            isTRUE(.data[[reg_col]][which.max(.data[[area_col]])] == "NULL"),
            # ... assign the Region with the second highest area...
            .data[[reg_col]][which(.data[[area_col]] == sort(.data[[area_col]], decreasing = TRUE)[2])],
            # ... otherwise, assign the region with the highest area
            .data[[reg_col]][which.max(.data[[area_col]])]
          ),
          # Do the same thing for aggregated regions
          !!agg_reg_col := ifelse(
            isTRUE(.data[[agg_reg_col]][which.max(.data[[area_col]])] == "NULL"),
            .data[[agg_reg_col]][which(.data[[area_col]] == sort(.data[[area_col]], decreasing = TRUE)[2])],
            .data[[agg_reg_col]][which.max(.data[[area_col]])]
          ),
          # Sum of area for each group (HUC8)
          !!area_col := sum(.data[[area_col]], na.rm = TRUE)
        )
    }
  }

  assertthat::assert_that(
    area_col %in% colnames(vm_xwalk),
    msg = "`area_col` must appear in dataframe at `vm_xwalk_path`"
  )

  if (is.na(var_tbl_row[["c404"]])) {
    in_list <- list(
      .data_path = purrr::map_chr(
        c("benchmark_data", "nhm_data", "wrfhydro_data"),
        ~ pull(var_tbl_row, .x)
      ),
      .id_col_data = purrr::map_chr(
        c("benchmark_huc_col", "nhm_huc_col", "wrfhydro_huc_col"),
        ~ pull(var_tbl_row, .x)
      ),
      .data_name = c("Benchmark", "NHM", "WRF-Hydro")
    )
  } else {
    in_list <- list(
      .data_path = purrr::map_chr(
        c("benchmark_data", "c404_data"),
        ~ dplyr::pull(var_tbl_row, .x)
      ),
      .id_col_data = purrr::map_chr(
        c("benchmark_huc_col", "c404_huc_col"),
        ~ dplyr::pull(var_tbl_row, .x)
      ),
      .data_name = c("Benchmark", "CONUS404")
    )
  }

  # Read in data and filter to HUCs common across all datasets
  dfs <- purrr::map2(
    setNames(in_list[[".data_path"]], in_list[[".data_name"]]),
    in_list[[".id_col_data"]],
    \(data_path, id_col_data) {
      readr::read_csv(
        data_path,
        col_types = readr::cols(!!id_col_data := "c", .default = "n")
      )
    }
  )

  common_huc <- purrr::map(dfs, ~ .x[["HUC"]]) |>
    purrr::reduce(intersect)

  in_list[[".dataframe"]] <- purrr::map(
    dfs,
    \(.x) dplyr::filter(.x, HUC %in% common_huc)
  )

  # Iterate awm function over datasets by either date (for line) or season and
  #   region (for bar) and combine
  if (figure_method == "line") {
    # Area weighted mean by date
    out <- purrr::pmap(
      in_list,
      function(.dataframe, .id_col_data, .id_col_xwalk, .data_name, ...) {
        awm_by_date(
          dataframe = .dataframe,
          id_col_data = .id_col_data,
          id_col_xwalk = id_col_xwalk,
          xwalk = vm_xwalk,
          area_col = area_col,
          data_name = .data_name
        )
      }
    ) |>
      # Join list elements by "date" column
      Reduce(f = function(...) merge(..., by = "date", all.x = TRUE), x = _) |>
      # Convert year_month to date
      dplyr::mutate(date = as.Date(paste0(date, "_01"), format = "%Y_%m_%d"))
  }

  if (figure_method == "bar") {
    message('Renaming aggregated region from "Northeast through Midwest" to "NE/Midwest"')
    # Area weighted mean by date
    out <- purrr::pmap(
      in_list,
      function(.dataframe, .id_col_data, .data_name, ...) {
        awm_by_reg_seas(
          dataframe = .dataframe,
          id_col_data = .id_col_data,
          xwalk = vm_xwalk,
          id_col_xwalk = id_col_xwalk,
          area_col = area_col,
          reg_col = reg_col,
          agg_reg_col = agg_reg_col,
          data_name = .data_name
        ) |>
          dplyr::mutate(join_col = paste(.data[[reg_col]], season, sep = "_"))
      }
    ) |>
      # Join list elements by region and season columns
      Reduce(
        f = function(...) {
          merge(...,
            by = c("join_col", reg_col, agg_reg_col, "season"),
            all.x = TRUE
          )
        },
        x = _
      ) |>
      dplyr::select(-join_col) |>
      # Order Regions and Agg regions for plotting
      dplyr::mutate(
        !!reg_col := factor(.data[[reg_col]], levels = as.character(1:18)),
        !!agg_reg_col := dplyr::case_when(
          .data[[agg_reg_col]] == "Northeast through Midwest" ~ "NE/Midwest",
          .default = .data[[agg_reg_col]]
        ),
        !!agg_reg_col := factor(
          .data[[agg_reg_col]],
          levels = c("NE/Midwest", "Southeast", "High Plains", "Western")
        )
      )
  }

  out |>
    # Add row with variable name
    dplyr::mutate(variable = var_tbl_row[["variable"]]) |>
    # Lengthen data by dataset
    tidyr::pivot_longer(cols = in_list[[".data_name"]], names_to = "dataset")
}

#' Iterate `prep_breakdown_data` over entire variable tibble
#'
#' @param var_tbl data frame; output from `build_breakdown_var_tbl`
#' @param vm_xwalk data.frame; HUC to Hydrologic (Van Metre) region crosswalk
#' @param area_col chr; name of area column in `vm_xwalk`
#' @param figure_method chr; either "bar" or "line"; prep data for which figure
#' @param reg_col chr; name of region column in `vm_xwalk`
#' @param agg_reg_col chr; name of aggregated region column in `vm_xwalk`
#' @param huc8_override named list where the name is the variable name and the
#'   value is the HUC8 column to use ubstead of `huc_col`
#'
#' @return list of long-format tibbles of means by date for benchmark, NHM, and
#'   WRF-Hydro; each element is a variable
#'
prep_all_breakdown_data <- function(var_tbl, figure_method, vm_xwalk, area_col,
                                    reg_col = NULL, agg_reg_col = NULL,
                                    huc8_override = NULL) {
  assertthat::assert_that(
    figure_method %in% c("bar", "line"),
    msg = "figure_method must be either 'bar' or 'line'"
  )

  if (figure_method == "bar") {
    assertthat::assert_that(
      !any(is.null(reg_col), is.null(agg_reg_col)),
      msg = "If figure_method = 'bar', reg_col and agg_reg_col must be defined"
    )
  }

  # Iterate prep_breakdown_line_data over rows of var_tbl
  purrr::map(
    # Split var_tbl into list by row
    split(var_tbl, var_tbl[["variable"]]),
    # Iterate prep_breakdown_line_data by row
    \(.x) {
      prep_breakdown_data(
        var_tbl_row = .x,
        figure_method = figure_method,
        vm_xwalk = vm_xwalk,
        area_col = area_col,
        reg_col = reg_col,
        agg_reg_col = agg_reg_col,
        huc8_override = huc8_override
      )
    }
  )
}
