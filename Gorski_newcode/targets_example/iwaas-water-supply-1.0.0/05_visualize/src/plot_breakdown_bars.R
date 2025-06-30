#' Prepare bar plot data for plotting
#'
#' @param bar_data list of dataframes, output from
#'   `prep_all_breakdown_data(..., figure_method = "bar")`
#' @param season_order chr; values from the following vector in the desired
#'   plotting order, first on the left, the remaining top to bottom on the
#'   right: c("annual", "fall", "winter", "spring", "summer")
#'
#' @return list of lists of data frames; nested barchart data
#'
prep_breakdown_bar_data <- function(bar_data, season_order) {
  purrr::map(
    bar_data,
    \(.x) {
      dplyr::mutate(.x, season = factor(season, levels = season_order)) |>
        split(.x[["season"]])
    }
  ) |>
    setNames(names(bar_data))
}

#' Plot breakdown figure barplots
#'
#' @param bar_data list of dataframes, output from
#'   `prep_all_breakdown_data(..., figure_method = "bar")`
#'   from `prep_breakdown_bar_data`
#' @param season_order chr; values from the following vector in the desired
#'   plotting order, first on the left, the remaining top to bottom on the
#'   right: c("annual", "fall", "winter", "spring", "summer")
#' @param pal chr(3); color palette
#'
#' @return list (by variable) of lists (by season) of barplots
#'
plot_breakdown_bars <- function(bar_data, season_order, pal) {
  # Prep bar data for plotting
  bar_data <- prep_breakdown_bar_data(
    bar_data = bar_data,
    season_order = season_order
  )

  # Get max y value for each variable
  max_y <- purrr::map_depth(bar_data, 2, ~ purrr::pluck(.x, "value")) |>
    purrr::map(~ purrr::reduce(.x, max))

  # Plot bar figures - list of 5 variables, each containing a list of 5 seasons
  out <- purrr::map2(
    .x = bar_data,
    .y = max_y,
    \(.x1, .y1) {
      purrr::map2(
        .x = .x1,
        .y = .y1,
        \(.x2, .y2) plot_one_breakdown_bar(data = .x2, max_y = .y2, pal = pal)
      )
    }
  )

  return(out)
}

#' Plot a single breakdown barplot
#'
#' @param data data frame. Breakdown data to plot
#'
#' @return ggplot
#'
plot_one_breakdown_bar <- function(data, max_y, pal) {
  ggplot2::ggplot(data = data) +
    ggplot2::geom_col(
      ggplot2::aes(x = Region, y = value, fill = dataset),
      position = "dodge"
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(AggRegion_nam),
      scales = "free_x",
      space = "free",
      switch = "x"
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::expand_limits(y = max_y) +
    ggthemes::theme_clean(base_size = 7) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 2, b = -2)
      ),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.line.y = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.ticks = ggplot2::element_line(linewidth = 0.3, color = "gray40"),
      axis.ticks.length = grid::unit(0.03, "in"),
      legend.background = ggplot2::element_rect(color = NA),
      legend.key.size = grid::unit(0.125, "in"),
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      panel.spacing.x = grid::unit(3, "pt"),
      plot.background = ggplot2::element_rect(color = NA),
      plot.margin = ggplot2::margin(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(margin = ggplot2::margin(b = 3)),
      strip.text.x = ggplot2::element_blank()
    )
}
