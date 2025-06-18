#' Plot map
#'
#' @param spatraster A spatraster to map
#' @param out_name Character. Path and name of file to write image file to.
#' @param breaks Numeric vector of values that define color breaks.
#' @param n_breaks Numeric value of the number of color breaks desired. Ignored
#'   if `breaks` is specified.
#' @param pal Character. Name of color palette to use from `scico::scico`. See
#'   `scico_palette_show()` for options.
#' @param pal_dir Either 1 or -1. If -1 the palette will be reversed
#' @param pal_offset integer; number of palette values to remove. Useful in
#'   cases where palette starts with the background color
#' @param legend_title character; legend title.
#' @param width numeric; Width of output figure in inches.
#' @param height numeric; Height of output image in inches
#' @param maxcell integer; Maximum number of cells to use for plot. Passed to
#'   `tidyterra::geom_spatraster`
#' @param hr_region_sf sf object of Hydrologic Regions polygons
#'
#' @return If out_name is specified, the path of the output plot; if not, a
#'   ggplot.
#'
#' @seealso `scico::scico`
#'
plot_map <- function(spatraster, out_name = NULL, breaks = NULL, n_breaks = 10,
                     pal = "lapaz", pal_dir = 1, pal_offset = 0, legend_title,
                     width = 6, height = 4, maxcell = 5e+05, hr_region_sf) {
  # Prepare Hydrologic Region boundaries
  bound <- hr_region_sf |>
    # Remove gaps
    sf::st_buffer(1800) |>
    sf::st_difference() |>
    # Correct invalid topology incorrect geometry types
    sf::st_make_valid() |>
    sf::st_cast("MULTIPOLYGON") |>
    nngeo::st_remove_holes() |>
    # Transform spatial projection
    sf::st_transform("EPSG: 5070")

  # Build plot
  out_plot <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = spatraster, maxcell = maxcell) +
    ggplot2::geom_sf(
      data = bound,
      fill = NA,
      color = ggplot2::alpha("white", 0.8),
      linewidth = 0.2
    ) +
    ggplot2::binned_scale(
      aesthetics = "fill",
      scale_name = "stepsn",
      palette = \(x) {
        tail(
          scico::scico(
            n_breaks + pal_offset,
            palette = pal,
            direction = pal_dir
          ),
          n_breaks
        )
      },
      breaks = breaks,
      labels = \(.x) pretty_labels(.x),
      limits = c(0, tail(breaks, 2)[[1]] + 1),
      guide = ggplot2::guide_colorsteps(
        direction = "horizontal",
        title.position = "left",
        barwidth = grid::unit(width * 3 / 4, units = "in"),
        barheight = grid::unit(1, units = "mm"),
        label.vjust = 2
      )
    ) +
    ggplot2::labs(fill = legend_title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.spacing.x = grid::unit(0.8, "mm"),
      legend.text = ggplot2::element_text(color = "#424242", size = 7),
      legend.title = ggplot2::element_text(
        color = "#424242",
        size = 7,
        hjust = 0.25,
        vjust = 1.09
      ),
      legend.position = c(0.5, 0.99),
      panel.border = ggplot2::element_rect(
        colour = "#424242",
        fill = NA,
        linewidth = 0.3
      )
    )

  # Add state borders, for map type
  out_plot <- out_plot +
    ggplot2::geom_sf(
      data = bound,
      fill = NA,
      color = ggplot2::alpha("white", 0.7),
      linewidth = 0.1
    ) +
    ggplot2::theme(legend.position = "none")


  # Return write file and return output file path or return plot if out_path is
  # NULL
  if (is.null(out_name)) {
    return(out_plot)
  } else {
    ggplot2::ggsave(
      out_name,
      plot = out_plot,
      width = width,
      height = height,
      bg = "white",
      units = "in"
    )
    return(out_name)
  }
}
