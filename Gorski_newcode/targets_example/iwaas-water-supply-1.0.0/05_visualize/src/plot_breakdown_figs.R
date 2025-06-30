#' Combine bar and line plots to create final model comparison figure
#'
#' @param bar_figs list of barplot ggplots, output from `plot_breakdown_bars`
#' @param line_figs list of line plot ggplots, output from
#'   `plot_breakdown_lines`
#' @param bar_title_pattern chr; pattern where one `"%s"` is the season
#' @param line_titles chr; vector (length = `line_figs` = 2) of line figs titles
#' @param y_labels_pattern chr; pattern where one `%s` is the variable name
#' @param y_title_override named list where names are variable names and values
#'   are desired y-axis label
#' @param out_png_pattern chr; pattern to save output PNGs to where one `"%s"`
#'   isthe variable name
#'
#' @return
#' @export
#'
#' @examples
plot_breakdown_figs <- function(bar_figs, line_figs, bar_title_pattern,
                                line_titles, y_labels_pattern, y_title_override,
                                out_png_pattern) {
  # Get variable name
  assertthat::assert_that(
    assertthat::are_equal(names(bar_figs), names(line_figs)),
    msg = "The names of bar_figs and line_figs must be the same"
  )
  var_name <- names(bar_figs)

  # Unnest inputs one level
  bar_figs <- purrr::pluck(bar_figs, 1)
  line_figs <- purrr::pluck(line_figs, 1)

  fig_legend <- get_legend_non0(
    bar_figs[[1]] +
      ggplot2::theme(legend.position = "bottom")
  )

  # Build list of titles for panels
  titles <- c(
    line_titles,
    stringr::str_to_title(sprintf(bar_title_pattern, names(bar_figs)))
  ) |>
    purrr::map(
      \(.x) grid::textGrob(.x, x = 0.07, just = "left", gp = grid::gpar(fontsize = 7))
    ) |>
    append(list(NULL), after = 0)

  # Arrange figures in list
  figs <- c(list(fig_legend), line_figs, bar_figs)

  # Interleave titles and figures
  titles_figs_list <- purrr::map2(titles, figs, list) |>
    unlist(recursive = FALSE)

  # Add x axis text to bottom row
  titles_figs_list[[8]] <- titles_figs_list[[8]] +
    ggplot2::theme(strip.text.x = ggplot2::element_text())
  titles_figs_list[[16]] <- titles_figs_list[[16]] +
    ggplot2::theme(strip.text.x = ggplot2::element_text())

  # Y-axis titles
  y_title_label <- sprintf(y_labels_pattern, var_name) |>
    as.list() |>
    setNames(var_name)

  if (var_name %in% names(y_title_override)) {
    y_title_label <- modifyList(y_title_label, y_title_override)
  }

  y_titles <- grid::textGrob(
    unlist(y_title_label),
    vjust = 0.75,
    rot = 90,
    gp = grid::gpar(fontsize = 6)
  )

  # Arrange left side of figure
  fig_left <- cowplot::plot_grid(
    plotlist = titles_figs_list[1:8],
    rel_heights = c(0.01, 0.2, 0.2, 0.8, 0.2, 0.8, 0.2, 0.75),
    ncol = 1,
    align = "v",
    axis = "lr"
  )

  # Arrange right side of figure
  fig_right <- cowplot::plot_grid(
    plotlist = titles_figs_list[9:16],
    rel_heights = c(0.2, 0.8, 0.2, 0.8, 0.2, 0.8, 0.2, 1),
    ncol = 1,
    align = "v",
    axis = "lr"
  )

  # Combine right and left side to arrange final figure
  out_plot <- cowplot::plot_grid(
    # Left y-axis titles
    y_titles,
    # Left column
    fig_left,
    # Right y-axis titles
    y_titles,
    # Right column
    fig_right,
    # Right margin
    NULL,
    # Options
    nrow = 1,
    rel_widths = c(0.05, 1, 0.05, 1, 0.05),
    align = "h",
    axis = "b"
  )

  # Generate output path
  out_path <- sprintf(out_png_pattern, var_name)

  # Write figure
  ggplot2::ggsave(
    filename = out_path,
    plot = out_plot,
    bg = "white",
    width = ggplot2::unit(6, "in"),
    height = ggplot2::unit(3.5, "in")
  )

  # Return output path
  return(out_path)
}
