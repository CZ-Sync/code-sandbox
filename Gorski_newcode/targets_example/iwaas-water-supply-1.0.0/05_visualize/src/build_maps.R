#' Generate CONUS maps with state overlay
#'
#' @param map_attr_tbl tibble of data visualization attributes used to specify
#'   plot styling for each row of maps
#' @param matrix_attr_list list of figure-wide data viz attributes. Required
#'   named elements are: column order, maxcell, rel_widths, width, height.
#' @param hr_region_sf sf object of Hydrologic Regions polygons
#'
build_maps <- function(map_attr_tbl, matrix_attr_list, hr_region_sf) {
  # Append map data to map attribute table
  # The following pmap call loops the making of map plots over the map_attr_tbl
  #   for each time period (specified by `column_order`). The output is a nested
  #   list of plots, where the first level is variables, and the second level
  #   is time period.
  ls_plots <- purrr::pmap(map_attr_tbl, function(...) {
    current_plot <- tibble::tibble(...)

    # Define breaks or n_breaks, based on what it available in map_attr_tbl
    breaks <- switch("breaks" %in% colnames(current_plot) + 1,
      NULL,
      unlist(current_plot[["breaks"]])
    )
    n_breaks <- switch("n_breaks" %in% colnames(current_plot) + 1,
      NULL,
      unlist(current_plot[["n_breaks"]])
    )

    # For current variable, read in summarized spatial data for each period
    #   defined in `column_order`
    spatrasters <- current_plot[["map_data"]][[1]] |>
      terra::rast() |>
      terra::subset(matrix_attr_list[["column_order"]]) |>
      split(matrix_attr_list[["column_order"]]) |>
      setNames(matrix_attr_list[["column_order"]])

    # If breaks is NULL, we need to define the breaks here, based on values
    # across all the periods for the current variable
    if (is.null(breaks)) {
      # Get numeric vector of values for all periods, dropping NAs
      values_all <- purrr::map(
        spatrasters,
        \(.x) terra::values(.x, na.rm = TRUE)
      ) |>
        unlist(use.names = FALSE)

      # Get breaks at quantiles
      breaks <- quantile(
        values_all,
        probs = seq(0, 1, 1 / n_breaks),
        na.rm = TRUE
      )

      breaks <- ifelse(breaks > 1, round(breaks), round(breaks, 2))
      breaks[[1]] <- 0
    } else {
      # If breaks are specified, determine n_breaks
      n_breaks <- length(breaks)
    }

    # Generate a plot for each time period defined in `column_order`, using the
    #   summarized raster datasets
    out_plot <- map(
      spatrasters,
      \(.x) {
        plot_map(
          spatraster = .x,
          breaks = breaks,
          n_breaks = n_breaks,
          pal = current_plot[["pal"]],
          pal_dir = current_plot[["pal_dir"]],
          pal_offset = current_plot[["pal_offset"]],
          legend_title = current_plot[["legend_title"]],
          width = matrix_attr_list[["width"]],
          height = matrix_attr_list[["height"]],
          maxcell = matrix_attr_list[["maxcell"]],
          hr_region_sf = hr_region_sf
        ) +
          ggplot2::theme(legend.position = "none")
      }
    )

    return(out_plot)
  })

  # add names for safe filter
  names(ls_plots) <- map_attr_tbl[["map_vars"]]

  return(ls_plots)
}
