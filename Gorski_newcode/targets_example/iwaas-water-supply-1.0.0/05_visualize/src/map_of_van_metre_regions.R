#' Create map of hydrologic regions (Van Metre regions)
#'
#' @param hr_region_sf sf object of hydrologic regions. Each row must be a
#' multipart polygon for each hydrologic region. It must have the following
#' columns: Region, Region_nam, id, and a spatial geometry column (usually
#' "geometry").
#'
#' @return a ggplot object
#'
van_metre_map <- function(hr_region_sf) {
  # Build region to aggregated region crosswalk
  agg_reg_xwalk <- tibble::tibble(
    Region = as.character(1:18),
    Agg_reg = as.factor(c(2, 3, 3, 2, 2, 3, 3, 3, 2, 1, 1, 1, 3, 4, 4, 4, 4, 4))
  )

  # Prepare hydrologic regions
  hr_sf <- hr_region_sf |>
    # Remove gaps
    sf::st_buffer(1800) |>
    sf::st_difference() |>
    # Correct invalid topology incorrect geometry types
    sf::st_make_valid() |>
    sf::st_cast("MULTIPOLYGON") |>
    nngeo::st_remove_holes() |>
    # Transform spatial projection
    sf::st_transform("EPSG: 5070")

  # Create aggregated regions
  ar_sf <- hr_sf |>
    dplyr::left_join(agg_reg_xwalk, by = "Region") |>
    dplyr::distinct(Agg_reg, geometry)

  # Get inner lines of multipolygons
  ar_inner_sf <- rmapshaper::ms_innerlines(ar_sf)
  hr_inner_sf <- rmapshaper::ms_innerlines(hr_sf)

  # Define labels and placement (in map units for EPSG:5070)
  hr_labels_df <- tibble::tribble(
    ~label, ~x,       ~y,
    "1",    1625000,  2300000,
    "2",    1450000,  1400000,
    "3",    1450000,  600000,
    "4",    500000,   2450000,
    "5",    450000,   2050000,
    "6",    870000,   1520000,
    "7",    500000,   1200000,
    "8",    180000,   910000,
    "9",    -120000,  2600000,
    "10",   -780000,  2600000,
    "11",   -500000,  2050000,
    "12",   -330000,  1430000,
    "13",   -300000,  830000,
    "14",   -1600000, 2600000,
    "15",   -1100000, 1800000,
    "16",   -1200000, 1160000,
    "17",   -2100000, 2700000,
    "18",   -1800000, 1900000
  )

  # Define aggregated region fill colors
  ar_colors <- list("#DCCEB8", "#B6B5A4", "#979793", "#777777")

  # Plot map
  ggplot2::ggplot() +
    # Aggregated region fill
    ggplot2::geom_sf(data = ar_sf, ggplot2::aes(fill = Agg_reg), color = NA) +
    ggplot2::scale_fill_manual(values = ar_colors) +

    # Aggregated region borders
    ggplot2::geom_sf(
      data = ar_inner_sf,
      color = ggplot2::alpha("white", 1),
      linewidth = 0.6
    ) +

    # Hydrologic region borders
    ggplot2::geom_sf(
      data = hr_inner_sf,
      fill = NA, color = ggplot2::alpha("white", 0.8),
      linewidth = 0.4
    ) +

    # Place hydrologic region numeric labels
    ggplot2::geom_label(
      data = hr_labels_df,
      ggplot2::aes(x = x, y = y, label = label),
      size = 6,
      color = "gray15",
      fill = NA,
      label.padding = ggplot2::unit(0.1, "lines"),
      label.size = NA
    ) +

    # Themes
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::unit(c(0, -0.125, -0.1, 0.1), "in")
    )
}
