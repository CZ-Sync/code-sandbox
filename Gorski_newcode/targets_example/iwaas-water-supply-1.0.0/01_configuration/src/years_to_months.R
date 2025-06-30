#' Years to months, in specified date format
#'
#' Converts range of `years` to a list of all months within the year range in
#'   a format specified by `format`. This function is used to build filenames.
#'
#' @param water_years numeric; vector of water years
#' @param format character; string used to format the date. See `?strptime`
#'
#' @return A character vector of all dates within range of `years` in the
#'   specified format
#'
#' @seealso  `?strptime`
#'
water_years_to_months <- function(water_years, format) {
  seq(as.Date(sprintf("%s-10-01", min(water_years) - 1)),
    as.Date(sprintf("%s-09-30", max(water_years))),
    by = "1 months"
  ) |>
    format(format = format)
}
