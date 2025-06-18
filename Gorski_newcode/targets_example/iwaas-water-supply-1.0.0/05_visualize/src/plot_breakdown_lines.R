#' Prep data for plotting breakdown line charts
#'
#' @param line_data list of dataframes, output from
#'   `prep_all_breakdown_data(..., figure_method = "line")`
#' @param water_yr lgl; should data be converted to water year?
#'
#' @return list of lists of data frames; nested line chart data
#'
prep_breakdown_line_data <- function(line_data, water_yr) {
  line_data_monthly <- purrr::map(
    line_data,
    \(.x) {
      .x |>
        dplyr::mutate(month = format(date, "%m")) |>
        dplyr::group_by(month, dataset, variable) |>
        dplyr::mutate(
          upper = max(value, na.rm = TRUE),
          lower = min(value, na.rm = TRUE)
        ) |>
        dplyr::group_by(upper, lower, .add = TRUE) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
        dplyr::mutate(
          month = as.Date(sprintf("2000-%s-01", month), format = "%Y-%m-%d")
        )
    }
  )

  if (water_yr) {
    line_data_monthly <- purrr::map(
      line_data_monthly,
      ~ dplyr::mutate(.x, month = as.Date(ifelse(
        format(month, "%m") %in% c("10", "11", "12"),
        month - years(1),
        month
      )))
    )
  }

  purrr::map2(line_data, line_data_monthly, ~ list(full = .x, monthly = .y))
}


#' Common theme for breakdown line plots
#'
#' @return ggplot theme
#'
line_theme <- function() {
  ggthemes::theme_clean(base_size = 7) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.line.y = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.ticks.length = grid::unit(0.03, "in"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3),
      plot.background = ggplot2::element_rect(color = NA),
      plot.margin = ggplot2::margin()
    )
}

#' Plot breakdown figure line plots
#'
#' @param line_data list of dataframes, output from
#'   `prep_all_breakdown_data(..., figure_method = "line")`
#' @param pal chr(3); color palette
#' @param water_yr lgl; should data be converted to water year?
#'
#' @return list (length = 2: full timeseries and monthly) of ggplots
#'
plot_breakdown_lines <- function(line_data, pal, water_yr = FALSE) {
  # Prep data, artificially adjusting date to plot by water year if
  #   `water_yr` = `TRUE`
  line_data_prepped <- prep_breakdown_line_data(line_data, water_yr = water_yr)

  # Extract line data from list
  line_data_full <- purrr::map(
    line_data_prepped,
    \(.x) purrr::pluck(.x, "full")
  )
  line_data_monthly <- purrr::map(
    line_data_prepped,
    \(.x) purrr::pluck(.x, "monthly")
  )

  # Plot full timeseries
  line_full_fig <- purrr::map(
    line_data_full,
    \(.x) {
      ggplot2::ggplot(
        data = .x,
        ggplot2::aes(x = date, y = value, color = dataset)
      ) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::scale_x_date(
          breaks = seq(
            as.Date("2009-10-01"),
            as.Date("2020-09-30"),
            by = "2 year"
          ),
          labels = format(
            seq.Date(as.Date("2010-10-01"), as.Date("2021-09-30"), by = "2 year"),
            format = "%Y"
          )
        ) +
        ggplot2::expand_limits(
          y = max(.x[["value"]], na.rm = TRUE),
          x = max(.x[["date"]], na.rm = TRUE)
        ) +
        line_theme()
    }
  )

  # Plot average annual timeseries by month
  line_fig_month <- purrr::map(
    line_data_monthly,
    \(.x) {
      ggplot2::ggplot(data = .x, ggplot2::aes(x = month)) +
        ggplot2::geom_line(ggplot2::aes(y = value, color = dataset)) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = lower, ymax = upper, fill = dataset),
          alpha = 0.35
        ) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_x_date(
          breaks = seq(
            as.Date("2000-01-01"),
            as.Date("2000-12-31"),
            by = "2 months"
          ),
          labels = \(.x) month.abb[as.numeric(strftime(.x, "%m"))]
        ) +
        line_theme()
    }
  )

  if (water_yr) {
    line_full_fig <- purrr::map(
      line_full_fig,
      ~ .x +
        ggplot2::xlab("Water year (Oct - Sep)") +
        ggplot2::theme(axis.title.x = element_text())
    )

    line_fig_month <- purrr::map(
      line_fig_month,
      ~ .x +
        ggplot2::scale_x_date(
          breaks = seq(
            as.Date("1999-10-01"),
            as.Date("2000-09-30"),
            by = "2 months"
          ),
          labels = \(.x) month.abb[as.numeric(strftime(.x, "%m"))]
        )
    )
  }

  # Return list of figures
  purrr::map2(
    line_full_fig,
    line_fig_month,
    \(.x, .y) list(full = .x, monthly = .y)
  )
}
