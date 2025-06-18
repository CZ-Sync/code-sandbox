#' Build summaries for map matrices
#'
#' @param model_csv_files character vector, input file names
#' @param var_names character vector, variable names to summarize
#' @param periods character vector, c("annual", "monthly", "seasonal", "full");
#'   the time period to summarize by
#' @param out_path_pattern pattern to build output file paths from containing 1
#'   "%s" to insert var_names
#' @param id_col character, column name in model_csv_files that identified HUC12
#' @param scale_factor numeric, number to multiply input values by - mostly for
#'   unit conversion
#' @param water_yr lgl; should data be converted to water year? This is only
#'   relevant if periods = "annual"
#' @param dataset_override a named list where the name is the variable name and
#'   the value is the path the the CSV that should replace `model_csv_files` in
#'   the case of that variable.
#'
#' @return character, output file paths
#'
map_matrix_model_summaries <- function(model_csv_files, var_names,
                                       periods = c("full", "seasonal"),
                                       out_path_pattern, id_col = "HUC",
                                       scale_factor = 1, water_yr = FALSE,
                                       dataset_override = NULL) {
  # Extract Derived variable name from equations
  var_names <- dplyr::if_else(
    stringr::str_detect(var_names, stringr::fixed("=")),
    setNames(
      stringr::str_extract(var_names, "^.*(?=(\\s=))"),
      names(var_names)
    ),
    var_names
  )

  assertthat::assert_that(
    !any(
      duplicated(names(var_names)[nzchar(names(var_names))]),
      duplicated(var_names)
    ),
    msg = "There must not be duplicate values or names in var_names"
  )

  assertthat::assert_that(
    stringr::str_count(out_path_pattern, stringr::fixed("%s")) == 1,
    msg = "out_path_pattern must contain exactly one instance of '%s'"
  )

  assertthat::assert_that(
    all(periods %in% c("annual", "monthly", "seasonal", "full")),
    msg = "period must be one (or more) of 'annual', 'monthly', 'seasonal', or 'full'"
  )

  # Subset model_csv_files to only the variables of interest, overriding
  # dataset if variable is listed in `dataset_override`
  in_csvs <- purrr::map(var_names, function(var_name) {
    if (var_name %in% names(dataset_override)) {
      dataset_override[[var_name]]
    } else {
      stringr::str_subset(model_csv_files, sprintf("_%s", var_name))
    }
  })

  # Build output file paths with variable in name
  out_csvs <- sprintf(out_path_pattern, var_names)

  # Get summaries for multiple periods and multiple variables
  # Output is a list of data.tables (length == length(var_names))
  # Data tables have columns for huc12 and for each summary group
  dt_list <- purrr::map(
    # Map across input csv files
    in_csvs,
    \(mapped_in_csv) {
      purrr::map(
        # Map across multiple periods
        periods,
        \(mapped_period) {
          model_summary(
            in_csv = mapped_in_csv,
            period = mapped_period,
            chunk_size = 5000,
            id_col = id_col,
            .fun = mean,
            scale_factor = scale_factor,
            water_yr = water_yr,
            colClasses = list(character = 1)
          )
        }
      ) |>
        # Combine into single data.table per input_file
        Reduce(f = merge, x = _)
    }
  ) # list of data.tables

  purrr::walk2(dt_list, out_csvs, fwrite)
  return(out_csvs)
}


#' Summarize NHM or WRFHydro models by time period
#'
#' @param in_csv character, path to input csv
#' @param period "annual", "monthly", "seasonal", or "full"; the time period to
#'   summarize by
#' @param chunk_size numeric, number of columns to split the in_csv into
#' @param id_col character, name of column used to identify rows in in_csv
#' @param .fun an unquoted function to summarize by
#' @param scale_factor numeric, number to multiply input values by - mostly for
#'   unit conversion
#' @param water_yr lgl; should data be converted to water year?
#' @param ... additional arguments passed to data.table::fread()
#'
#' @return character, file path to output csv of in_csv summarized by period
#'   #a data table of observations summarized by groups
#'
model_summary <- function(in_csv, period, chunk_size = 5000, id_col = "HUC",
                          .fun = mean, scale_factor = 1, water_yr = FALSE, ...) {
  assertthat::assert_that(
    period %in% c("annual", "monthly", "seasonal", "full"),
    msg = "period must be one of 'annual', 'monthly', 'seasonal', or 'full'"
  )

  # Read in CSV
  dt <- data.table::fread(in_csv, ...)

  # Rename id_col and set set it as the key
  colnames(dt)[colnames(dt) == id_col] <- "HUC"
  data.table::setkeyv(dt, cols = "HUC")

  # Get dates from column names
  yyyy_mm <- stringr::str_subset(colnames(dt), pattern = "^\\d{4}_\\d{2}$")

  # Convert dates to water years
  if (water_yr && period == "annual") {
    message("Data is being converted from calendar year to water year")

    yyyy_mm <- ifelse(
      # If the last 2 digits (month) are in 10:12...
      test = stringr::str_extract(yyyy_mm, "\\d{2}$") %in% c("10", "11", "12"),
      # ... add 1 to the first 4 digits (year)...
      yes = str_replace(
        yyyy_mm,
        "^\\d{4}",
        as.character(as.numeric(str_extract(yyyy_mm, "^\\d{4}")) + 1)
      ),
      #  ... otherwise return the value as is
      no = yyyy_mm
    )
  }

  # Define group factor
  if (period == "full") {
    group_factor <- rep("full", length(yyyy_mm))
  } else if (period == "annual") {
    group_factor <- as.integer(stringr::str_sub(yyyy_mm, 1, 4))
  } else if (period == "seasonal") {
    group_factor <- str_sub(yyyy_mm, 6, 7) |>
      factor(
        levels = c(sprintf("%02d", c(12, 1:11))),
        labels = rep(c("winter", "spring", "summer", "fall"), each = 3)
      ) |>
      as.character()
  } else if (period == "monthly") {
    group_factor <- as.integer(str_sub(yyyy_mm, 6, 7))
  }

  out_dt <- dt_split_summary(
    dt = dt,
    chunk_size = chunk_size,
    id_col = "HUC",
    group_factor = group_factor,
    .fun = .fun
  )
  data.table::setkeyv(out_dt, "HUC")

  cols <- colnames(out_dt[, lapply(out_dt, is.numeric) == TRUE, with = FALSE])
  out_dt[, (cols) := lapply(.SD, "*", scale_factor), .SDcols = cols]

  return(out_dt)
}

#' Split-Apply-Combine grouped summary for data table
#'
#' Performing the standard `DT[, lapply(.SD, .fun), by = group]` on the
#'   NHM/WRFHYDRO data was taking a long time (>10 min per variable) and was
#'   returning errors. This method speeds up that process without errors by
#'   about 10 times.
#'
#' @param dt a data table in wide format
#' @param chunk_size numeric, number of columns to split dt into
#' @param id_col character, name of column used to identify rows in dt
#' @param group_factor a vector length nrow(dt) to group dt by
#' @param .fun an unquoted function to summarize by
#'
#' @return a data table of observations summarized by groups
#'   (nrows == nrows(dt), ncol == length(unique(group_factor)))
#'
dt_split_summary <- function(dt, chunk_size, id_col, group_factor, .fun) {
  # Create factor for splitting
  dt_nrow <- nrow(dt)
  n_chunks <- ceiling(dt_nrow / chunk_size)
  f <- rep(1:n_chunks, each = chunk_size, length.out = dt_nrow)

  # Split dt into list of smaller data tables
  dt_split <- split(dt, f)

  # Calculate group mean on split data tables and reassemble
  dt_out <- purrr::map(
    dt_split,
    dt_group_summary,
    id_col = id_col, group_factor = group_factor, .fun = .fun
  ) |>
    rbindlist()

  return(dt_out)
}

#' Helper for dt_split_summary
#'
#' Performs group summary on data table in wide format
#'
#' @param in_dt a data table in wide format
#' @param id_col character, name of column used to identify rows
#' @param group_factor a vector length nrow(in_dt) to group in_dt by
#' @param .fun an unquoted function to summarize by
#'
#' @return data.table, summarized by group
#'
dt_group_summary <- function(in_dt, id_col, group_factor, .fun) {
  # Transpose data table and add group factor
  d_t <- data.table::transpose(in_dt, make.names = id_col)
  d_t[, group := group_factor]
  data.table::setkey(d_t, group)

  # Perform group summary
  out <- d_t[, lapply(.SD, .fun), by = group] |>
    data.table::transpose(keep.names = id_col, make.names = "group")

  return(out)
}

#' Build swe summary for map matrices
#'
#' @param model_csv_files character vector, input file names for ensembled csvs
#' @param var_name character vector, SWE variable name to summarize
#' @param out_path_pattern pattern to build output file path from containing 1
#'   "%s" to insert var_name
#'
#' @return character, output file paths
#'
map_matrix_model_summary_max_swe <- function(model_csv_files,
                                             var_name,
                                             out_path_pattern) {
  assertthat::assert_that(
    stringr::str_count(out_path_pattern, stringr::fixed("%s")) == 1,
    msg = "out_path_pattern must contain exactly one instance of '%s'"
  )

  # Subset model_csv_files to only the variables of interest
  in_csv <- stringr::str_subset(model_csv_files, sprintf("_%s", var_name))

  # Build output file path with variable in name
  out_csv <- sprintf(out_path_pattern, var_name)

  # Read in ensembled SWE data
  ensembled_swe <- readr::read_csv(
    in_csv,
    col_types = readr::cols(HUC = "c", .default = "n")
  )

  # Compute maximum monthly SWE for each HUC for each water year
  swe_summary_annual <- ensembled_swe |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("\\d{4}_\\d{2}"),
      names_to = c("Year", "Month"),
      names_sep = "_"
    ) |>
    dplyr::mutate(Water_Year = case_when(
      Month %in% c("10", "11", "12") ~ as.numeric(Year) + 1,
      .default = as.numeric(Year)
    )) |>
    dplyr::group_by(HUC, Water_Year) |>
    dplyr::arrange(value, .by_group = TRUE) |>
    dplyr::summarise(
      max_swe_mm = last(value), Month = last(Month),
      .groups = "drop"
    )

  # Compute average annual maximum monthly swe for each HUC, across water years
  swe_summary <- swe_summary_annual |>
    dplyr::group_by(HUC) |>
    dplyr::summarise(mean_max_swe_mm = mean(max_swe_mm, na.rm = TRUE)) |>
    dplyr::mutate(
      mean_max_swe_mm = ifelse(is.na(mean_max_swe_mm), 0, mean_max_swe_mm)
    )

  readr::write_csv(swe_summary, out_csv)

  return(out_csv)
}
