#' Get regional monthly summary for flux geofacet plot
#'
#' @param file character. Path to water supply model (NHM/WRF-Hydro/Ensemble)
#'   output CSV.
#' @param huc_level numeric. `8` or `12`. The HUC level of the data in `file`.
#' @param var_name character. The name of the hydrologic variable of the data in
#'   `file`.
#' @param xwalk data frame. HUC12 to Hydrologic region crosswalk.
#'
#' @return data frame
#'
get_region_monthly_summary <- function(file, huc_level, var_name, xwalk) {
  data <- readr::read_csv(file, col_types = readr::cols())

  assertthat::assert_that(
    huc_level %in% c(8, 12),
    msg = "The specified huc level must be either 8 or 12"
  )
  huc_col <- ifelse(huc_level == 12, "HUC12", "HUC8")

  joined_data <- data |>
    dplyr::left_join(xwalk, by = c(HUC = huc_col)) |>
    dplyr::mutate(Region = factor(
      Region,
      levels = as.character(seq.int(1, 18, by = 1))
    ))

  data_region <- joined_data |>
    # Filter out NA and 'NULL' values for van metre regions
    dplyr::filter(if_all(
      c(Region, Region_nam),
      \(.x) (!is.na(.x) & !(.x == "NULL"))
    )) |>
    dplyr::group_by(Region, Region_nam) |>
    dplyr::summarize(
      dplyr::across(
        tidyselect::matches("^\\d{4}_\\d{2}"),
        \(.x) mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  data_region |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^\\d{4}_\\d{2}"),
      names_to = c("year", "month"),
      names_sep = "_",
      names_transform = as.numeric,
      values_to = "value"
    ) |>
    dplyr::group_by(Region, Region_nam, month) |>
    dplyr::summarize(
      dplyr::across(-year, \(.x) mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(variable = var_name)
}

#' Get regional monthly summaries for flux geofacet plot
#'
#' @param water_budget data frame. Each row is a different water supply
#'   variable. Has columns: "file" (the file path to the water supply CSV),
#'   "huc_level" (the associated HUC-code level 8 or 12), and "var_name" (the
#'   variable name).
#' @param van_metre_xwalk data frame. HUC12 to Hydrologic region crosswalk.
#'
#' @return data frame
#'
van_metre_monthly_means <- function(water_budget, van_metre_xwalk) {
  purrr::pmap(
    water_budget,
    function(...) {
      current_data <- tibble(...)

      get_region_monthly_summary(
        file = current_data[["file"]],
        huc_level = current_data[["huc_level"]],
        var_name = current_data[["var_name"]],
        xwalk = van_metre_xwalk
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      value = ifelse(variable == "Precipitation", value, value * -1),
      plot_year = ifelse(month > 9, 2019, 2020),
      plot_date = lubridate::make_date(plot_year, month, "01"),
      variable = factor(
        variable,
        levels = c("Precipitation", "Streamflow", "Evapotranspiration")
      ),
      label = paste(Region, Region_nam, sep = "_"),
      Region_label = paste(Region_nam, Region)
    )
}

#' Plot flux data in geofacet
#'
#' @param van_metre_means_plot data frame. Regional summary of water supply
#'   data. Output from `van_metre_monthly_means()`.
#' @param van_metre_grid data frame. Passed to `grid` argument of
#'  `geofacet::facet_geo`.
#'
#' @return ggplot
#'
flux_geofacet_plot <- function(van_metre_means_plot, van_metre_grid) {
  van_metre_budget <- van_metre_means_plot |>
    dplyr::group_by(Region, Region_nam, label, month, plot_date) |>
    dplyr::summarize(budget = sum(value, na.rm = TRUE), .groups = "keep") |>
    dplyr::mutate(Region_label = paste(Region_nam, Region))

  flux_ylim <- c(
    -round(max(van_metre_means_plot$value), -1) - 10,
    round(max(van_metre_means_plot$value), -1) + 10
  )

  base_flux_plot <- ggplot2::ggplot(van_metre_means_plot) +
    ggplot2::geom_bar(
      data = dplyr::filter(van_metre_means_plot, variable == "Precipitation"),
      ggplot2::aes(x = plot_date, y = value, fill = variable),
      stat = "identity"
    ) +
    ggplot2::geom_bar(
      data = dplyr::filter(van_metre_means_plot, variable != "Precipitation"),
      ggplot2::aes(x = plot_date, y = value, fill = variable),
      stat = "identity"
    ) +
    ggplot2::geom_line(
      data = van_metre_budget, ggplot2::aes(x = plot_date, y = budget),
      color = "red",
      linewidth = 1.25
    ) +
    ggplot2::scale_x_date(date_labels = "%b", expand = c(0, 0), name = "") +
    ggplot2::scale_y_continuous(limits = flux_ylim, expand = c(0, 0)) +
    ggplot2::ylab("Monthly average depth (millimiters)") +
    ggplot2::scale_fill_manual(
      breaks = c("Precipitation", "Streamflow", "Evapotranspiration"),
      values = c("grey10", "grey45", "grey80"),
      name = ""
    )

  flux_geo <- base_flux_plot +
    ggplot2::geom_rect(
      xmin = min(van_metre_means_plot[["plot_date"]]) - 12,
      xmax = max(van_metre_means_plot[["plot_date"]]) + 12,
      ymin = flux_ylim[[1]],
      ymax = flux_ylim[[2]],
      fill = NA,
      color = "grey10"
    ) +
    # data must be aggregated to van metre region with a column matching the
    # Region column of van_metre_grid. In this column, each van metre region is
    # defined with a number, this is what is used to determine what data goes
    # into which geofacet. van_metre_grid is a tibble that tells facet_geo what
    # column and row to place each van metre region facet.
    geofacet::facet_geo(
      ~Region_label,
      grid = van_metre_grid,
      move_axes = TRUE
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(color = NA, fill = NA),
      strip.background = ggplot2::element_rect(color = NA, fill = NA),
      strip.text = ggplot2::element_text(size = 15),
      plot.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.6, "cm"),
      axis.text = ggplot2::element_text(size = 15, color = "grey10"),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 0.2, unit = "cm")
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.2, unit = "cm")
      ),
      axis.title = ggplot2::element_text(size = 18, color = "grey10"),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.2, unit = "cm")
      ),
      legend.text = ggplot2::element_text(size = 18, color = "grey10"),
      legend.title = ggplot2::element_text(
        size = 18, face = "bold",
        color = "grey10"
      ),
      axis.ticks = ggplot2::element_line(linewidth = 0.18, color = "grey10"),
      axis.ticks.length = grid::unit(0.25, "cm")
    )
}
