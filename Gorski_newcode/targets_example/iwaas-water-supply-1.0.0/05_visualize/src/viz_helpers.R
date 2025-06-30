#' Format numbers for labels
#'
#' Improves the aesthetics of large numbers by formatting them according to the
#'   number of digits. For 4-digit numbers, values are left as is; for numbers
#'   with 5 or 6 digits, commas are added at the thousands mark; for numbers
#'   with 7+ digits, values are converted to scientific notation.
#'
#' @param num A numeric vector
#'
#' @return A character string with formatted numbers.
pretty_labels <- function(num) {
  tidytable::case_when(
    num >= 1000000 ~ format(num, scientific = TRUE),
    num >= 10000 ~ format(num, scientific = FALSE, big.mark = ","),
    num < 10000 ~ as.character(num)
  )
}

#' Pre-styled textGrob for plot labels
#'
#' @param label character.
#' @param ... additional arguments passed to `grid.text::textGrob`
#'
#' @return A text grob
#'
labelGrob <- function(label, fontsize = 8, col = "#424242", ...) {
  grid::textGrob(label, gp = grid::gpar(fontsize = fontsize, col = col), ...)
}

#' Get legend (and avoid returning zeroGrob objects)
#'
#' @param plot a ggplot
#'
#' @return a grob with "guide-box" component name (i.e., a legend)
#'
get_legend_non0 <- function(plot) {
  legends <- cowplot::get_plot_component(plot, "guide-box", return_all = TRUE)
  non0_idx <- which(!purrr::map_lgl(legends, ~ inherits(., what = "zeroGrob")))

  if (length(non0_idx) == 0) {
    cli::cli_warn(c("!" = "No non-zeroGrob legends. Returning `NULL`."))
    return(NULL)
  }

  if (length(non0_idx) > 1) {
    cli::cli_warn(c("!" = "Multiple legends exist. Returning the first one."))
  }

  legends[non0_idx][[1]]
}
