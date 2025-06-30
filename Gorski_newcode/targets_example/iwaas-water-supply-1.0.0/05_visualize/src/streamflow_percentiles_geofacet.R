#' Plot hydrograph with focus on single year
#'
#' @param data data frame. Data to plot.
#' @param focal_year numeric. Year to highlight.
#' @param focal_color character. Color to highlight `focal_year`.
#' @param focal_linewidth numeric. Width of line for `focal_year`.
#'
#' @return ggplot `geom_line` layer
#'
plot_focal_hydrograph <- function(data, focal_year, focal_color,
                                  focal_linewidth) {
  if (focal_color %in% c("a", "b", "c")) {
    ggplot2::geom_line(
      data = dplyr::filter(data, water_year == focal_year),
      ggplot2::aes(
        x = plot_date_single_year,
        y = value,
        group = water_year,
        color = focal_color
      ),
      linewidth = focal_linewidth
    )
  } else {
    ggplot2::geom_line(
      data = dplyr::filter(data, water_year == focal_year),
      ggplot2::aes(x = plot_date_single_year, y = value, group = water_year),
      color = focal_color,
      linewidth = focal_linewidth
    )
  }
}

#' Generate hydrograph
#'
#' @param vm_streamflow_aw data frame. Data to plot.
#' @param regions character. Regions to plot.
#' @param focal_value character. One of "min" or "max". Which function should
#'  be used to identify focal year.
#' @param focal_color character. Color to highlight `focal_year`.
#' @param focal_linewidth numeric. Width of line for `focal_year`.
#' @param regional_sr_totals data frame. Total streamflow for each water year
#'   for each region.
#'
#' @return ggplot
#'
generate_hydrograph <- function(vm_streamflow_aw, regions, focal_value,
                                focal_color, focal_linewidth,
                                regional_sr_totals) {
  assertthat::assert_that(
    focal_value %in% c("min", "max"),
    msg = "`focal_value` must be one of 'min' or 'max'."
  )

  purrr::map(
    regions,
    \(region) {
      region_data <- filter(vm_streamflow_aw, Region == region)
      region_cumulative_data <- regional_sr_totals |>
        dplyr::filter(Region == region) |>
        dplyr::arrange(dplyr::desc(value))

      focal_year <- switch(focal_value,
        max = head(region_cumulative_data[["water_year"]], 1),
        min = tail(region_cumulative_data[["water_year"]], 1)
      )

      plot_focal_hydrograph(
        region_data,
        focal_year,
        focal_color,
        focal_linewidth
      )
    }
  )
}

#' Generate label for hydrograph
#'
#' @param vm_streamflow_aw data frame. Data to plot.
#' @param vm_streamflow_aw_cumulative
#' @param regions character. Regions to plot.
#' @param focal_value character. One of "min" or "max". Which function should
#'  be used to identify focal year.
#' @param focal_color character. Color to highlight `focal_year`.
#' @param max_sr_value numeric. Maximum sreamflow regional value.
#'
#' @return ggplot `geom_text` layer
#'
generate_label <- function(vm_streamflow_aw, vm_streamflow_aw_cumulative,
                           regions, focal_value, focal_color, max_sr_value) {
  assertthat::assert_that(
    focal_value %in% c("min", "max"),
    msg = "`focal_value` must be one of 'min' or 'max'."
  )

  purrr::map(
    regions,
    \(region) {
      region_data <- dplyr::filter(vm_streamflow_aw, Region == region)
      region_cumulative_data <- vm_streamflow_aw_cumulative |>
        dplyr::filter(Region == region) |>
        dplyr::arrange(dplyr::desc(value))

      if (focal_value == "max") {
        focal_year <- head(region_cumulative_data[["water_year"]], 1)

        label_data <- dplyr::filter(region_data, water_year == focal_year) |>
          dplyr::arrange(dplyr::desc(value))
        focal_month <- head(label_data[["month"]], 1)
        position_factor <- 1.6
      } else if (focal_value == "min") {
        focal_year <- tail(region_cumulative_data[["water_year"]], 1)

        label_data <- dplyr::filter(region_data, water_year == focal_year) |>
          dplyr::arrange(dplyr::desc(value))
        focal_month <- tail(label_data[["month"]], 1)
        position_factor <- 0.65
      }

      plot_data <- dplyr::filter(
        region_data,
        water_year == focal_year,
        month == focal_month
      )

      label_y_value <- plot_data[["value"]]
      label_x_value <- plot_data[["plot_date_single_year"]]
      diff_from_axis_max <- max_sr_value - label_y_value

      label_position_y <- ifelse(
        diff_from_axis_max < 100,
        label_y_value - 130,
        label_y_value
      )
      label_position_y <- ifelse(
        label_y_value < 0.5,
        label_y_value + 0.12,
        label_position_y
      )
      date_adjustment <- ifelse(diff_from_axis_max < 100, 110, 0)
      date_adjustment <- ifelse(label_y_value < 0.5, -60, date_adjustment)
      label_position_x <- label_x_value + date_adjustment

      if (focal_month %in% c(10, 11)) {
        label_position_x <- label_position_x + 45
      } else if (focal_month %in% c(8, 9)) {
        label_position_x <- label_position_x - 45
      }

      ggplot2::geom_text(
        data = plot_data,
        ggplot2::aes(
          x = label_position_x,
          y = label_position_y * position_factor,
          label = water_year
        ),
        color = focal_color,
        linewidth = 1,
        size = 15 / ggplot2::.pt
      )
    }
  )
}

#' Construct and save geofacet plot with regional inset map.
#'
#' @param geofacet ggplot. Geofacet plot.
#' @param region_map ggplot. Regional boundary inset map.
#' @param plot_width numeric. Width of plot in inches.
#' @param plot_height numeric. Height of plot in inches.
#' @param background_color character. Background color of plot.
#' @param outfile character. File path to save plot to.
#'
#' @return character. File path of output plot.
#'
save_geofacet_w_map <- function(geofacet, region_map, legend_height,
                                plot_width, plot_height, background_color,
                                outfile) {
  plot_canvas <- grid::rectGrob(
    x = 0,
    y = 0,
    width = plot_width,
    height = plot_height,
    gp = grid::gpar(fill = background_color, alpha = 1, col = background_color)
  )

  geofacet_grob <- geofacet::get_geofacet_grob(
    geofacet + ggplot2::theme(legend.position = "none")
  )

  plot_legend <- get_legend_non0(geofacet)

  final_plot <- cowplot::ggdraw(ylim = c(0, 1), xlim = c(0, 1)) +
    # a background
    cowplot::draw_grob(
      plot_canvas,
      x = 0,
      y = 1,
      height = plot_width,
      width = plot_height,
      hjust = 0,
      vjust = 1
    ) +
    cowplot::draw_grob(
      geofacet_grob,
      x = 0.03,
      y = 0.05,
      height = 0.92,
      width = 0.94
    ) +
    cowplot::draw_plot(
      plot_legend,
      x = 0.01,
      y = -0.02,
      height = 0.1,
      hjust = 0,
      vjust = 0
    ) +
    cowplot::draw_plot(
      region_map,
      x = 0.15,
      y = 0.05,
      height = 0.26,
      hjust = 0.5
    )

  ggplot2::ggsave(
    outfile,
    plot = final_plot,
    height = 9,
    width = 16,
    dpi = 300,
    bg = background_color
  )

  return(outfile)
}

#' Calculate area weighted percentiles for streamflow data
#'
#' @param percentile_values list of numeric values
#' @param huc12_streamflow_wide data frame. Streamflow data in wide format.
#' @param van_metre_xwalk data frame. HUC12 to hydrologic region crosswalk.
#'
#' @return data frame
#'
vm_AreaWeighted_percentiles <- function(percentile_values,
                                        huc12_streamflow_wide,
                                        van_metre_xwalk) {
  purrr::map(
    percentile_values,
    \(percentile_value) {
      huc12_streamflow_wide |>
        # pivot so single column for year, but column for each month
        tidyr::pivot_longer(
          cols = tidyselect::matches("^\\d{4}_\\d{2}"),
          names_to = c("year", ".value"),
          names_sep = "_"
        ) |>
        # Get percentiles for each HUC, for each month (across years), based on
        # passed percentile value
        dplyr::group_by(HUC) |>
        dplyr::summarize(
          dplyr::across(
            tidyselect::matches("\\d{2}"),
            ~ quantile(., probs = percentile_value, na.rm = TRUE)
          ),
          .groups = "drop"
        ) |>
        # Join VM xwalk
        dplyr::left_join(van_metre_xwalk, by = c("HUC" = "HUC12")) |>
        dplyr::mutate(
          Region = factor(Region, levels = as.character(seq.int(1, 18, by = 1)))
        ) |>
        dplyr::filter(
          dplyr::if_all(c(Region, Region_nam), \(.) !is.na(.) & !(. == "NULL"))
        ) |>
        # Get area-weighted means of monthly percentiles for each region
        dplyr::mutate(Area_sqkm = ifelse(is.na(Area_sqkm), 0, Area_sqkm)) |>
        dplyr::group_by(Region, Region_nam) |>
        dplyr::summarize(
          dplyr::across(
            tidyselect::matches("\\d{2}"),
            \(.) weighted.mean(x = ., w = Area_sqkm, na.rm = TRUE)
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(percentile = percentile_value)
    }
  ) |>
    purrr::list_rbind() |>
    tidyr::pivot_longer(
      tidyselect::matches("\\d{2}"),
      names_to = "month",
      values_to = "value",
      names_transform = list(month = as.integer)
    )
}

#' Calculate area weighted mean streamflow data
#'
#' @param huc12_streamflow_wide data frame. Streamflow data in wide format.
#' @param van_metre_xwalk data frame. HUC12 to hydrologic region crosswalk.
#'
#' @return data frame
#'
vm_AreaWeighted <- function(huc12_streamflow_wide, van_metre_xwalk) {
  huc12_streamflow_wide |>
    # Join Van Metre xwalk
    dplyr::left_join(van_metre_xwalk, by = c("HUC" = "HUC12")) |>
    dplyr::mutate(
      Region = factor(Region, levels = as.character(seq.int(1, 18, by = 1)))
    ) |>
    # Filter out NA and "NULL" values for van metre regions
    dplyr::filter(
      dplyr::if_all(c(Region, Region_nam), \(.) !is.na(.) & !(. == "NULL"))
    ) |>
    # Set area to 0 if NA
    dplyr::mutate(Area_sqkm = ifelse(is.na(Area_sqkm), 0, Area_sqkm)) |>
    # For each region
    dplyr::group_by(Region, Region_nam) |>
    # Get weighted mean of values for each year_month, across all HUCs in that
    # region, using Area as the weight
    dplyr::summarize(
      dplyr::across(
        tidyselect::matches("^\\d{4}_\\d{2}"),
        ~ weighted.mean(x = ., w = Area_sqkm, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    # Pivot longer, so that have columns for year and month
    tidyr::pivot_longer(
      cols = tidyselect::matches("^\\d{4}_\\d{2}"),
      names_to = c("year", "month"),
      names_sep = "_",
      names_transform = as.numeric,
      values_to = "value"
    ) |>
    dplyr::mutate(
      plot_date = lubridate::make_date(year, month, 01),
      water_year = dataRetrieval::calcWaterYear(plot_date),
      Region = factor(Region, levels = as.character(seq(1, 18, 1))),
      plot_year = ifelse(month > 9, 2009, 2010),
      plot_date_single_year = lubridate::make_date(plot_year, month, 01),
      Region_label = paste(Region_nam, Region, sep = " ")
    )
}

#' Create percentile ribon ggplot layer
#'
#' @param vm_percentiles_aw data frame. Data to plot.
#'
#' @return ggplot `geom_ribbon` layer
#'
percentile_ribbons_geom <- function(vm_percentiles_aw) {
  # set up data in single water year, wide format for plotting ribbons
  vm_percentiles_single_year <- vm_percentiles_aw |>
    dplyr::mutate(
      plot_year = ifelse(month > 9, 2009, 2010),
      plot_date = lubridate::make_date(plot_year, month, 01)
    ) |>
    tidyr::pivot_wider(names_from = percentile, values_from = value) |>
    dplyr::mutate(Region_label = paste(Region_nam, Region, sep = " "))

  # set up tibble for plotting streamflow percentiles data
  percentile_ribbons_single_year <- tibble::tibble(
    lower_bound = c("0.05", "0.5"),
    upper_bound = c("0.5", "0.95"),
    fill = RColorBrewer::brewer.pal(4, "BrBG")[c(2, 3)]
  )

  purrr::pmap(percentile_ribbons_single_year, function(...) {
    current_ribbon <- tibble::tibble(...)

    ggplot2::geom_ribbon(
      data = vm_percentiles_single_year,
      ggplot2::aes(
        x = plot_date,
        ymin = get(current_ribbon[["lower_bound"]]),
        ymax = get(current_ribbon[["upper_bound"]]),
        fill = current_ribbon[["upper_bound"]]
      ),
      alpha = 0.5,
      color = "white",
      linewidth = 0.5
    )
  })
}

#' Plot percentile base plot
#'
#' @param vm_streamflow_aw data frame. Data to plot.
#' @param ribbon_layers_single_year ggplot layer. Percentile ribbon plot.
#' @param VM_AW_percentiles data frame. Area weighted streamflow percentiles.
#'
#' @return ggplot
#'
percentile_base_plot <- function(vm_streamflow_aw, ribbon_layers_single_year,
                                 VM_AW_percentiles) {
  max_streamflow_value <- max(VM_AW_percentiles[["value"]])

  # Get total streamflow for each water year for each region
  vm_streamflow_aw_cumulative <- vm_streamflow_aw |>
    dplyr::group_by(Region, Region_nam, water_year) |>
    dplyr::summarize(
      value = sum(value),
      Region_label = paste(Region_nam, Region),
      .groups = "drop"
    ) # add label

  # Construct final plot
  vm_streamflow_aw |>
    ggplot2::ggplot() +
    ribbon_layers_single_year +
    ggplot2::scale_fill_manual(
      values = RColorBrewer::brewer.pal(4, "BrBG")[c(2, 3)],
      labels = c("5th - 50th", "50th - 95th"),
      name = "Percentile bands"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = plot_date_single_year, y = value, group = water_year,
        color = "a"
      ),
      linewidth = 0.3
    ) +
    generate_hydrograph(
      vm_streamflow_aw,
      regions = seq(1, 18, by = 1),
      focal_value = "min",
      focal_color = "white",
      focal_linewidth = 1.5,
      regional_sr_totals = vm_streamflow_aw_cumulative
    ) +
    generate_hydrograph(
      vm_streamflow_aw,
      regions = seq(1, 18, by = 1),
      focal_value = "min",
      focal_color = "c",
      focal_linewidth = 1,
      regional_sr_totals = vm_streamflow_aw_cumulative
    ) +
    generate_hydrograph(
      vm_streamflow_aw,
      regions = seq(1, 18, by = 1),
      focal_value = "max",
      focal_color = "white",
      focal_linewidth = 1.5,
      regional_sr_totals = vm_streamflow_aw_cumulative
    ) +
    generate_hydrograph(
      vm_streamflow_aw,
      regions = seq(1, 18, by = 1),
      focal_value = "max",
      focal_color = "b",
      focal_linewidth = 1,
      regional_sr_totals = vm_streamflow_aw_cumulative
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "grey70",
        RColorBrewer::brewer.pal(4, "BrBG")[[4]],
        RColorBrewer::brewer.pal(4, "BrBG")[[1]]
      ),
      labels = c("Total streamflow", "Wettest year", "Driest year"),
      name = ""
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(linewidth = c(0.3, 1, 1))
      )
    ) +
    generate_label(
      vm_streamflow_aw,
      vm_streamflow_aw_cumulative,
      regions = seq(1, 18, by = 1),
      focal_value = "min",
      focal_color = RColorBrewer::brewer.pal(4, "BrBG")[[1]],
      max_sr_value = max_streamflow_value
    ) +
    generate_label(
      vm_streamflow_aw,
      vm_streamflow_aw_cumulative,
      regions = seq(1, 18, by = 1),
      focal_value = "max",
      focal_color = RColorBrewer::brewer.pal(4, "BrBG")[[4]],
      max_sr_value = max_streamflow_value
    ) +
    ggplot2::scale_y_continuous(
      name = "Total streamflow in millimeters",
      limits = c(0.2, 300),
      breaks = c(1, 10, 100),
      labels = c(1, 10, 100),
      expand = c(0, 0),
      trans = "log10"
    ) +
    ggplot2::scale_x_date(
      date_labels = "%b",
      expand = c(0, 0),
      name = "",
      limits = c(
        lubridate::make_date(2010 - 1, 10, 01),
        lubridate::make_date(2010, 9, 01)
      )
    ) +
    ggplot2::annotation_logticks(
      color = "grey10",
      outside = TRUE,
      short = grid::unit(0.04, "cm"),
      mid = grid::unit(0.1, "cm"),
      long = grid::unit(0.18, "cm"),
      size = 0.2
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::guides(
      ggplot2::guide_legend(direction = "horizontal")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 15),
      plot.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.6, "cm"),
      panel.border = ggplot2::element_rect(
        color = "grey10",
        fill = NA,
        linewidth = 0.1
      ),
      axis.text = ggplot2::element_text(size = 15, color = "grey10"),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 0.2, unit = "cm")
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.2, unit = "cm")
      ),
      axis.ticks = ggplot2::element_line(color = "grey10", linewidth = 0.2),
      axis.ticks.length = grid::unit(0.18, "cm"),
      axis.title = ggplot2::element_text(size = 18, color = "grey10"),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.2, unit = "cm")
      ),
      legend.text = ggplot2::element_text(size = 18, color = "grey10"),
      legend.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        color = "grey10"
      )
    )
}

#' Plot geofacet streamflow percentiles plot
#'
#' @param base_percentile_plot ggplot. Percentile base plot.
#' @param van_metre_grid data frame. Grid passed to `grid` argument of
#'   `geofacet::facet_geo`.
#'
#' @return ggplot
#'
geofacet_percentiles <- function(base_percentile_plot, van_metre_grid) {
  base_percentile_plot +
    geofacet::facet_geo(
      ~Region_label,
      grid = van_metre_grid,
      move_axes = TRUE,
      drop = TRUE
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(color = "grey10", fill = NA),
      strip.background = ggplot2::element_rect(color = NA, fill = NA),
      plot.background = ggplot2::element_blank()
    )
}
