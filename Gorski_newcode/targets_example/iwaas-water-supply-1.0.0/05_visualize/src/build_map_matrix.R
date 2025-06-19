#' Generate plot matrix with maps and proportional bar plots
#'
#' @param map_attr_tbl tibble of data visualization attributes used to specify
#'   plot styling for each row of maps
#' @param map_data chr; vector of input files of rasters. Each raster should
#'   have 5 layers, named: full, fall, winter, spring, summer
#' @param hr_region_sf sf object of Hydrologic Regions polygons
#' @param matrix_attr_list list of figure-wide data viz attributes. Required
#'   named elements are: column order, maxcell, rel_widths, width, height.
#' @param y_title_override named list where name is y title (var_name) and value
#'   is character of name to be changed into
#' @param out_path_pattern chr, string passed to `sprintf` to build output
#'   filenames for the figures; must have 2 occurrences of `%s` corresponding to
#'   model name and figure type, respectively
#' @param version chr; either "full" or "seasonal". If "full", the output plot
#'   has 5 columns: One for annual and 4 seasons. If "seasonal", just the 4
#'   seasons are included.
#'
#' @return a file path to a png of the combined map/bar plot matrix figure
#'
build_map_matrix <- function(map_attr_tbl, map_data, hr_region_sf,
                             matrix_attr_list, y_title_override = NULL,
                             out_path_pattern,
                             version = c("full", "seasonal")) {
  version <- match.arg(version, c("full", "seasonal"))

  # Build output paths
  figure_names <- unique(map_attr_tbl[["figure"]])
  out_paths <- sprintf(out_path_pattern, figure_names)

  # Define y-axis titles
  y_titles <- as.list(map_attr_tbl[["map_vars"]]) |>
    setNames(map_attr_tbl[["map_vars"]])

  # Override automatic y_titles
  if (!is.null(y_title_override)) {
    assertthat::assert_that(
      all(names(y_title_override) %in% map_attr_tbl[["map_vars"]]),
      msg = paste(
        "y_title_override must be a named list where the names must",
        "be in map_attr_tbl[['map_vars']]."
      )
    )

    y_titles <- modifyList(y_titles, y_title_override)
  }

  # Get column names
  req_seasons <- switch(version,
    full = c("full", "winter", "spring", "summer", "fall"),
    seasonal = c("winter", "spring", "summer", "fall")
  )

  assertthat::assert_that(
    length(c(
      setdiff(matrix_attr_list[["column_order"]], req_seasons),
      setdiff(req_seasons, matrix_attr_list[["column_order"]])
    )) == 0,
    msg = sprintf(
      paste(
        "matrix_attr_list[['column_order']] must contain the all the",
        "following values and only those values:\n%s"
      ),
      toString(req_seasons)
    )
  )

  column_names <- names(matrix_attr_list[["column_order"]])
  assertthat::assert_that(
    !is.null(column_names),
    msg = "matrix_attr_list[['column_order']] must be named"
  )

  # Build map list
  assertthat::assert_that(assertthat::are_equal(
    map_attr_tbl[["map_data"]],
    map_data,
    check.attributes = FALSE
  ))

  # Build annual and seasonal plots for each data variable
  # Returns a nested list of plots
  map_list <- build_maps(
    map_attr_tbl = map_attr_tbl,
    matrix_attr_list = matrix_attr_list,
    hr_region_sf = hr_region_sf
  )

  # Build map legends, using the 'Annual' or 'Winter' plot from each set of maps
  # for each variable, depending on the on the version of the plot created.
  legend_option <- switch(version,
    full = "full",
    seasonal = "winter"
  )

  legend_plots <- map_list |>
    purrr::map(\(.x) .x[[legend_option]]) |>
    purrr::map(
      \(.x) {
        get_legend_non0(
          .x +
            ggplot2::theme(
              legend.position = "top",
              legend.box.margin = ggplot2::margin(t = -2, b = -1, unit = "mm")
            )
        ) |>
          cowplot::plot_grid(nrow = 1)
      }
    ) |>
    split(map_attr_tbl[["figure"]])

  # Build label row
  row_of_labels <- switch(version,
    # For "full", figure is 7 columns: the first column helps provide adequate
    # space for y-axis labels; column 3 helps proved space between the Annual
    # column and the seasonal columns.
    full = {
      c(
        list(NULL),
        as.list(column_names[[1]]),
        list(NULL),
        as.list(column_names[-1])
      ) |>
        purrr::map(labelGrob) |>
        cowplot::plot_grid(
          plotlist = _,
          nrow = 1,
          rel_widths = matrix_attr_list[["rel_widths"]]
        )
    },
    # For "seasonal", figure is 5 columns: the first column helps provide
    # adequate space for y-axis labels.
    seasonal = {
      cowplot::plot_grid(
        plotlist = purrr::map(c(list(NULL), as.list(column_names)), labelGrob),
        nrow = 1,
        rel_widths = matrix_attr_list[["rel_widths"]]
      )
    }
  )

  # Build rows of maps
  row_list <- switch(version,
    full = rlang::expr(c(
      list(labelGrob(.y, rot = 90)),
      .x[1],
      list(grid::nullGrob()),
      .x[2:5]
    )),
    seasonal = rlang::expr(c(
      list(labelGrob(.y, rot = 90)),
      .x[1:4]
    ))
  )


  map_rows <- purrr::map2(
    map_list,
    y_titles,
    \(.x, .y) {
      cowplot::plot_grid(
        plotlist = rlang::eval_bare(row_list),
        nrow = 1,
        rel_widths = matrix_attr_list[["rel_widths"]],
        scale = 0.99
      )
    }
  ) |>
    split(map_attr_tbl[["figure"]])

  # Produce fluxes and figures
  out <- purrr::map2(
    map_rows,
    legend_plots,
    \(.x, .y) {
      cowplot::plot_grid(
        plotlist = c(
          # Row of labels
          list(row_of_labels),
          # Rows of plots and legends
          purrr::map2(.x, .y, ~ c(list(.x), list(.y)))
        ) |>
          purrr::list_flatten(),
        # Matrix_wide definitions
        nrow = sum(1, length(.x), length(.y)),
        rel_heights = c(0.05, rep(c(1, 0.05), length(.x)), vjust = 2)
      ) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "mm")
        )
    }
  )

  # Write figures and return output paths
  purrr::walk2(
    out_paths,
    out,
    \(.x, .y) {
      cowplot::save_plot(
        filename = .x,
        plot = .y,
        base_width = matrix_attr_list[["width"]],
        base_height = matrix_attr_list[["height"]],
        bg = "white",
        dpi = 320
      )
    }
  )
  return(out_paths)
}
