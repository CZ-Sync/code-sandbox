#' Generate wide-format regional seasonal flow summaries

#'
#' @param ensembled_model_csvs_BF Path to water supply model ensembled output csv
#'   for baseflow
#' @param ensembled_model_csvs_QF Path to water supply model ensembled output csv
#'   for quickflow
#' @param van_metre_xwalk data frame. HUC12 to Hydrologic region crosswalk.
#'
#' @return wide-formatted dataframe
#'
wide_flow_subset <- function(ensembled_model_csvs_BF, ensembled_model_csvs_QF,
                             van_metre_xwalk) {
  water_flow_data <- tibble::tibble(
    file = c(ensembled_model_csvs_BF, ensembled_model_csvs_QF),
    huc_level = c(12, 12),
    var_name = c("Baseflow", "Quickflow")
  )

  van_metre_flow <- purrr::pmap(water_flow_data, function(...) {
    current_data <- tibble::tibble(...)

    get_region_monthly_summary(
      file = current_data[["file"]],
      huc_level = current_data[["huc_level"]],
      var_name = current_data[["var_name"]],
      xwalk = van_metre_xwalk
    )
  }) |>
    dplyr::bind_rows()

  van_metre_flow |>
    dplyr::mutate(season = dplyr::case_when(
      month < 3 ~ "winter",
      month < 6 ~ "spring",
      month < 9 ~ "summer",
      month < 12 ~ "fall",
      TRUE ~ "winter"
    )) |>
    dplyr::mutate(
      season = factor(season, levels = c("fall", "winter", "spring", "summer"))
    ) |>
    # seasonal
    dplyr::group_by(Region, Region_nam, variable, season) |>
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "keep") |>
    # pivot to wide format
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::mutate(
      Streamflow = Baseflow + Quickflow,
      Baseflow_fraction = Baseflow / Streamflow,
      Per_Baseflow = Baseflow / Streamflow * 100
    )
}


#' Generate sf object of regional flow summary data
#'
#' @param van_metre_regions_sf_simp sf object of simplified van metre regions
#' @param flow_subset_wide Regional summary of seasonal baseflow, quickflow,
#'    streamflow, and baseflow fraction. Output from `wide_flow_subset()`.
#'
#' @return sf object with polygon and point geometry
#'
wide_flow_subset_sf <- function(van_metre_regions_sf_simp, flow_subset_wide) {
  flow_subset_sf <- dplyr::left_join(
    van_metre_regions_sf_simp,
    flow_subset_wide,
    by = c("Region", "Region_nam")
  )
  dplyr::mutate(
    flow_subset_sf,
    geometry_point = sf::st_geometry(sf::st_centroid(flow_subset_sf))
  )
}


#' Generate base plot of Van Metre regions for seasonal flow plots
#'
#' @param flow_subset_sf sf object with polygon and point geometry for Van 
#'    Metre regions, along with seasonal flow summaries for each region.
#'    Output of `wide_flow_subset_sf()`
#'
#' @return  ggplot
#'
plot_base <- function(flow_subset_sf) {
  ggplot2::ggplot(flow_subset_sf) +
    ggplot2::geom_sf(fill = "grey80", color = "white", linewidth = 0.15)
}


#' Generate plot with row of maps of selected flow variable
#'
#' @param base_plot ggplot of Van Metre regions. Output of `plot_base()`
#' @param flow_subset_sf  sf object with polygon and point geometry for Van 
#'    Metre regions, along with seasonal flow summaries for each region.
#'    Output of `wide_flow_subset_sf()`
#' @param var `Streamflow` or `Baseflow` - the flow variable to map
#'
#' @return ggplot
#'
flow_plot <- function(base_plot, flow_subset_sf, var) {
  lims <- flow_subset_sf |>
    tidyr::pivot_longer(
      tidyselect::contains("flow"),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::filter(variable %in% c("Streamflow", "Baseflow")) |>
    dplyr::pull("value") |>
    range() |>
    round(-1)

  base_plot +
    ggplot2::geom_sf(
      ggplot2::aes(size = {{ var }}, geometry = geometry_point)
    ) +
    ggplot2::scale_size(
      limits = lims,
      range = c(0.01, 3.2),
      breaks = c(25, 50, 100, 150),
      labels = c(25, 50, 100, 150),
      name = "mm"
    ) +
    ggplot2::facet_wrap(~season, nrow = 1) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      legend.spacing.x = ggplot2::unit(0.8, "mm"),
      legend.text = ggplot2::element_text(color = "#424242", size = 7),
      legend.title = ggplot2::element_text(
        color = "#424242",
        size = 7,
        hjust = 0.25,
        vjust = 0.5
      ),
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(
        colour = "#424242",
        fill = NA,
        linewidth = 0.3
      )
    )
}

#' Generate plot of baseflow fraction, by Van Metre region
#'
#' @param base_plot ggplot of Van Metre regions. Output of `plot_base()`
#'
#' @return ggplot
#'
baseflow_fract_plot <- function(base_plot) {
  base_plot +
    ggplot2::geom_sf(
      ggplot2::aes(fill = Baseflow_fraction),
      color = "white",
      linewidth = 0.15
    ) +
    scico::scale_fill_scico(
      limits = c(0.25, 1),
      palette = "oslo",
      direction = -1,
      end = 0.9,
      name = ""
    ) +
    ggplot2::facet_wrap(~season, nrow = 1) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      legend.spacing.x = grid::unit(0.8, "mm"),
      legend.text = ggplot2::element_text(color = "#424242", size = 7),
      legend.title = ggplot2::element_text(
        color = "#424242",
        size = 7,
        hjust = 0.25,
        vjust = 1.2
      ),
      legend.position = "bottom",
      legend.key.height = grid::unit(0.08, "cm"),
      panel.border = ggplot2::element_rect(
        colour = "#424242",
        fill = NA,
        linewidth = 0.3
      )
    )
}

#' Generate gridded plot of seasonal baseflow, streamflow and baseflow
#'
#' @param fileout file path for output png
#' @param flow_subset_sf  sf object with polygon and point geometry for Van 
#'    Metre regions, along with seasonal flow summaries for each region.
#'    Output of `wide_flow_subset_sf()`
#' @param streamflow_ps_plot  a ggplot object of 4 seasonal proportional symbol maps of
#'     streamflow, by Van Metre region. Output of flow_plot()`
#' @param baseflow_ps_plot a ggplot object of 4 seasonal proportional symbol maps of
#'     baseflow, by Van Metre region. Output of flow_plot()`
#' @param baseflow_fraction_plot a ggplot object of 4 seasonal choropleth maps
#'    of baseflow fraction, by Van Metre region. Output of `baseflow_fract_plot()`
#'
#' @return filepath of output plot
#'
streamflow_baseflow_plot <- function(fileout, flow_subset_sf,
                                     streamflow_ps_plot, baseflow_ps_plot,
                                     baseflow_fraction_plot) {
  y_titles <- list("Streamflow", "Baseflow", "Baseflow fraction")
  column_names <- stringr::str_to_title(unique(flow_subset_sf[["season"]]))

  # Export plot, matching to Anthony"s gridded plot layout and style
  row_of_labels <- cowplot::plot_grid(
    plotlist = purrr::map(c(list(NULL), as.list(column_names)), labelGrob),
    nrow = 1,
    rel_widths = c(0.1, 1, 1, 1, 1)
  )

  map_list <- list(streamflow_ps_plot, baseflow_ps_plot, baseflow_fraction_plot)
  legend_plots <- purrr::map(
    map_list,
    \(.x) {
      cowplot::get_legend(
        .x +
          ggplot2::theme(
            legend.position = "top",
            legend.box.margin = ggplot2::margin(t = -2, b = -1, unit = "mm")
          )
      ) |>
        cowplot::plot_grid(nrow = 1)
    }
  )

  map_rows <- purrr::map2(
    map_list,
    y_titles,
    \(.x, .y) {
      cowplot::plot_grid(
        plotlist = c(
          list(labelGrob(.y, rot = 90)),
          list(.x + ggplot2::theme(legend.position = "None"))
        ),
        nrow = 1,
        rel_widths = c(0.1, 4),
        scale = 0.99
      )
    }
  )

  plot_elements <- c(
    # Row of labels
    list(row_of_labels),
    # Rows of plots and legends
    list(
      map_rows[1], legend_plots[1],
      map_rows[2], legend_plots[2],
      map_rows[3], legend_plots[3]
    )
  ) |>
    purrr::list_flatten()

  out <- cowplot::plot_grid(
    plotlist = plot_elements,
    # Matrix_wide definitions
    nrow = 7,
    rel_heights = c(0.05, rep(c(1, 0.05), length(map_rows)), vjust = 2)
  ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "mm")
    )

  cowplot::save_plot(
    filename = fileout,
    plot = out,
    base_width = 6,
    base_height = 4,
    bg = "white",
    dpi = 320
  )

  return(fileout)
}
