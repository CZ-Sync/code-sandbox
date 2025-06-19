#' Convert WRF-HYDRO & NHM netCDF array to csv for multiple variables
#'
#' Extracts all data for the user-specified variables of interest and
#' time period of interest. Writes the results to `csv` in a wide format
#' similar to the NHM model output provided by the NHM team (one column of
#' HUC12 ids and the remaining columns as `year-month` summary values).
#'
#' @param nc_file chr, full file path(s) for WRF-HYDRO/NHM netCDF(s)
#' @param method chr, one of ("NHM", "WRF-Hydro", "CONUS404", "Dugger")
#' @param var_names chr, vector of variables of interest
#' @param out_path_pattern chr, a pattern for the file output path. Used with
#'   `sprintf`
#' @param month_filter num, vector of year_months (format = %Y_%m) to subset
#' @param convert_to_huc8 chr, vector of variable names to convert from HUC12 to
#'   HUC8
#' @param scale_factors named list where names are var_names and values are
#'   numbers to multiply the output values by
#' @param dataset_override (optional) named list, nested named list.
#'    Level 1: name = variable name, value = nested list... variable name must
#'      match name of one of variables specified in `var_names`
#'    Level 2: name-value pairs for `nc_file` and `method` values to override
#'      for variable name in Level 1
#'
model_nc_extract_vars_to_csv <- function(nc_file, method, var_names,
                                         out_path_pattern, month_filter,
                                         convert_to_huc8 = NULL,
                                         scale_factors = NULL,
                                         dataset_override = NULL) {
  # Extract non-computed variables ----------------
  # Remove derived variables
  var_names_non_compute <- stringr::str_detect(
    unlist(var_names),
    "=",
    negate = TRUE
  )
  var_names_non_compute <- var_names[var_names_non_compute]

  assertthat::assert_that(
    !isTRUE(duplicated(names(var_names_non_compute))),
    !isTRUE(duplicated(var_names_non_compute)),
    msg = "There must not be duplicate values or names in var_names"
  )

  assertthat::assert_that(
    stringr::str_count(out_path_pattern, "%s") == 1,
    msg = "out_path_pattern must contain exactly 1 instance of '%s'"
  )

  # Construct tibble
  # Doubly mapped: (1) across nc_files (e.g., CONUS, AK), and (2) across
  # variables (e.g., ET, Precip). Output it a nested list. Level 1: length of
  # nc_files, Level 2: length of var_names_non_compute.
  non_compute_tbl_list <- purrr::map(
    nc_file,
    \(nc_file) {
      purrr::map2(
        var_names_non_compute,
        names(var_names_non_compute),
        \(.x, .y) {
          model_nc_to_tbl(
            nc_file = nc_file,
            method = method,
            var_name = setNames(.x, .y),
            month_filter = month_filter,
            scale_factors = scale_factors,
            convert_to_huc8 = convert_to_huc8,
            dataset_override = dataset_override
          )
        }
      )
    }
  )

  # Bind outputs of regions by variable (e.g., row bind all precip data frames
  #   across regions). Output list (length = # of variables) of data frames
  # non_compute_tbl_list
  non_compute_tbl_list <- purrr::map(
    non_compute_tbl_list,
    \(.x) purrr::list_flatten(.x, name_spec = "{inner}")
  ) |>
    purrr::list_transpose() |>
    purrr::map(dplyr::bind_rows)

  # Save output filenames as out. names(non_compute_tbl_list) in format '{var_name}_{data_units}'
  out <- sprintf(out_path_pattern, names(non_compute_tbl_list)) |>
    setNames(names(var_names_non_compute))

  # Write non_compute_tbl_list to CSV
  purrr::walk2(non_compute_tbl_list, out, readr::write_csv)

  # Compute derived variables -------------
  # Get derived variables
  vars_compute_eq <- stringr::str_detect(var_names, "=")
  vars_compute_eq <- var_names[vars_compute_eq]

  if (length(vars_compute_eq) > 0) {
    vars_values_derived <- purrr::map_chr(
      vars_compute_eq,
      \(.x) stringr::str_trim(stringr::str_extract(.x, ".+(?==)"))
    )

    # Inform user of status
    cli::cli_inform(c(
      "There are {length(vars_values_derived)} variables to compute:",
      setNames(vars_values_derived, rep.int("*", length(vars_values_derived)))
    ))

    # Compute derived variables as needed
    out_computed <- purrr::map2_chr(
      vars_compute_eq,
      names(vars_compute_eq),
      \(.x, .y) {
        compute_derived_variables(
          derived_var_eq = setNames(.x, .y),
          var_names = var_names,
          raw_csvs = out,
          scale_factors = scale_factors,
          convert_to_huc8 = convert_to_huc8,
          out_path_pattern = out_path_pattern
        )
      }
    )

    # Combine outputs
    out <- c(out, out_computed)[names(var_names)]
  }

  return(out)
}

#' Convert WRF-HYDRO & NHM netCDF array to csv
#'
#' Extracts all data for the user-specified variable of interest and
#' time period of interest. Writes the results to tibble in a wide format
#' similar to the NHM model output provided by the NHM team (one column of
#' HUC12 ids and the remaining columns as `year-month` summary values).
#'
#' @param nc_file chr, full file path for WRF-HYDRO netCDF
#' @param method chr, one of ("NHM", "WRF-Hydro", "CONUS404", "Dugger")
#' @param var_name chr, variable of interest
#' @param month_filter num, vector of year_months (format = %Y_%m) to subset
#' @param scale_factors named list where names are var_names and values are
#'   numbers to multiply the output values by
#' @param convert_to_huc8 chr, vector of variable names to convert from HUC12 to
#'   HUC8
#' @param dataset_override (optional) named list, nested named list.
#'    Level 1: name = variable name, value = nested list... variable name must
#'      match name of one of variables specified in `var_names`
#'    Level 2: name-value pairs for `nc_file` and `method` values to override
#'      for variable name in Level 1
#'
model_nc_to_tbl <- function(nc_file, method, var_name, month_filter = NULL,
                            scale_factors = NULL, convert_to_huc8,
                            dataset_override) {
  # Replace arguments with overrides if necessary
  if (names(var_name) %in% names(dataset_override)) {
    dataset_override_sel <- dataset_override[[names(var_name)]]
    nc_file <- dataset_override_sel[["nc_file"]]
    method <- dataset_override_sel[["method"]]
  }

  # Get scale factor
  if (is.null(scale_factors)) {
    scale_factor <- 1
  } else if (any(purrr::map_lgl(
    names(scale_factors),
    \(.x) stringr::str_detect(names(var_name), .x)
  ))) {
    scale_factor <- purrr::map_lgl(
      names(scale_factors),
      \(.x) stringr::str_detect(names(var_name), .x)
    ) |>
      subset(names(scale_factors), subset = _) |>
      purrr::pluck(.x = scale_factors)

    cli::cli_inform(c(
      "i" = "Scaling output: multiplying {names(var_name)} by {scale_factor}."
    ))
  } else {
    scale_factor <- 1
  }

  # open netcdf and run a few checks
  open_nc_file <- ncdf4::nc_open(unlist(nc_file))

  if (method %in% c("NHM", "WRF-Hydro")) {
    # run a few checks - if these checks do not pass then check the netcdf
    # metadata for clues
    stopifnot(
      c(var_name, "yrmo") %in% names(open_nc_file[["var"]]),
      c("huc_id", "time") %in% names(open_nc_file[["dim"]])
    )

    # extract variables of interest from netcdf and close file
    yrmo_all <- ncdf4::ncvar_get(open_nc_file, varid = "yrmo")
    # If yrmo_all is 2-D, get first column
    if (length(dim(yrmo_all)) > 1) {
      yrmo_all <- yrmo_all[1, ]
    }
    yrmo_all <- stringr::str_replace(yrmo_all, "-", "_")

    huc_ids <- ncdf4::ncvar_get(open_nc_file, varid = "huc_id") |>
      as.vector()
  } else if (method == "CONUS404") {
    # run a few checks - if these checks do not pass then check the netcdf
    # metadata for clues
    stopifnot(
      c(var_name) %in% names(open_nc_file[["var"]]),
      c("huc_id", "time") %in% names(open_nc_file[["dim"]])
    )

    # extract variables of interest from netcdf and close file
    yrmo_all <- ncdf4::ncvar_get(open_nc_file, varid = "time") |>
      as.vector() |>
      as.Date(origin = "1979-10-01") |>
      format("%Y_%m")

    huc_ids <- ncdf4::ncvar_get(open_nc_file, varid = "huc_id") |>
      as.vector()
  } else if (method == "Dugger") {
    # Run a few checks - if these checks do not pass then check the netcdf
    #   metadata for clues
    stopifnot(
      c(var_name) %in% names(open_nc_file[["var"]]),
      c("WBDHU12", "time") %in% names(open_nc_file[["dim"]])
    )

    # Extract variables of interest from netcdf and close file
    yrmo_all <- ncdf4::ncvar_get(open_nc_file, varid = "time") |>
      as.vector() |>
      as.Date(origin = "2009-10-01") |>
      format("%Y_%m")

    huc_ids <- ncdf4::ncvar_get(open_nc_file, varid = "WBDHU12") |>
      as.vector() |>
      bit64::as.integer64() |>
      as.character()
  } else {
    stop("method must be one of: NHM, WRF-Hydro, CONUS404, or Dugger")
  }

  data_var <- ncdf4::ncvar_get(open_nc_file, varid = var_name)
  data_units <- open_nc_file[["var"]][[var_name]][["units"]]
  data_dim <- dim(data_var) # year_mo, HUC
  ncdf4::nc_close(open_nc_file)

  # Check data dimensions.
  # If rows != length(huc_ids) (and  therefore = length(year_mo) instead),
  # transpose data, so that data is HUC x year_mo
  if (data_dim[[1]] == length(huc_ids)) {
    out <- data_var
  } else {
    out <- t(data_var)
  }

  colnames(out) <- yrmo_all

  out <- out |>
    tibble::as_tibble() |>
    dplyr::mutate(
      HUC = stringr::str_pad(huc_ids, side = "left", pad = "0", width = 12),
      .before = 1
    )

  # subset for months of interest
  if (!is.null(month_filter)) {
    month_subset <- as.character(month_filter)
    out <- dplyr::select(
      out,
      tidyselect::all_of("HUC"),
      tidyselect::starts_with(month_subset)
    )
  }

  # Multiply numeric columns by scale factor
  if (scale_factor != 1) {
    out <- dplyr::mutate(
      out,
      dplyr::across(tidyselect::where(is.numeric), \(.x) .x * scale_factor)
    )
  }

  # Convert datasets from HUC12 to HUC8 as needed
  if (isTRUE(names(var_name) %in% convert_to_huc8)) {
    out <- convert_huc12_data_to_huc8(out)
  }

  return(setNames(list(out), paste(var_name, data_units, sep = "_")))
}

#' Compute variables based on given expression
#'
#' @param derived_var_eq chr; an equation with the new variable name on the lhs
#'   and the simple formula to perform the equation on the rhs; must be able to
#'   able to be applied to numeric data frame and return a data frame (+, -, *)
#' @param var_names chr; named vector of all all variable names that correspond
#'   with `raw_csvs`
#' @param raw_csvs chr; vector of file paths to CSVs that contain extracted data
#'   used as inputs for the equations in
#' @param scale_factors named list where names are var_names and values are
#'   numbers to multiply the output values by `derived_var_eq`
#' @param convert_to_huc8 chr, vector of variable names to convert from HUC12 to
#'   HUC8
#' @param out_path_pattern chr; the pattern used to create the paths for
#'   `raw_csvs`
#'
#' @return chr; path to CSV of computed variable
#'
compute_derived_variables <- function(derived_var_eq, var_names, raw_csvs,
                                      scale_factors = NULL, convert_to_huc8,
                                      out_path_pattern) {
  # Get variables and equation to derive new variable ---------------
  # Name of new variable
  derived_var <- stringr::str_extract(derived_var_eq, ".+(?==)") |>
    stringr::str_trim()

  derived_var_name <- names(derived_var_eq)

  # Name of variables needed to calculate it
  required_vars <- derived_var_eq |>
    stringr::str_split_1("[^\\w]") |>
    stringr::str_subset(".+") |> # remove blanks ("")
    stringr::str_subset(derived_var, negate = TRUE)

  required_vars_data <- dplyr::if_else(
    stringr::str_detect(var_names[required_vars], "="),
    stringr::str_trim(stringr::str_extract(
      var_names[required_vars],
      ".+(?==)"
    )),
    var_names[required_vars]
  )

  all_output_csvs <- list.files(
    dirname(out_path_pattern),
    pattern = stringr::str_remove(basename(out_path_pattern), "%s.*$"),
    full.names = TRUE
  )

  # The equation to calculate new variable
  given_expression <- derived_var_eq |>
    stringr::str_extract("(?<==).*") |>
    stringr::str_trim()

  # Inform user of calculation that is occurring
  cli::cli_inform(c(
    "i" = "Calculating new variable {.var {derived_var}} using expression
    {.code {given_expression}}"
  ))

  # Get scale factor
  if (is.null(scale_factors)) {
    scale_factor <- 1
  } else if (any(purrr::map_lgl(
    names(scale_factors),
    \(.x) stringr::str_detect(derived_var_name, .x)
  ))) {
    scale_factor <- purrr::map_lgl(
      names(scale_factors),
      \(.x) stringr::str_detect(derived_var_name, .x)
    ) |>
      subset(names(scale_factors), subset = _) |>
      purrr::pluck(.x = scale_factors)

    cli::cli_inform(c(
      "i" = "Scaling output: multiplying {derived_var_name} by {scale_factor}."
    ))
  } else {
    scale_factor <- 1
  }

  # Load required data ---------------
  # Get the CSVs for the variables needed to derive new equation
  required_csvs <- purrr::map_chr(
    required_vars_data,
    \(.x) stringr::str_subset(all_output_csvs, .x)
  )

  # Read them in
  required_dfs <- purrr::map(
    required_csvs,
    \(.x) {
      readr::read_csv(
        .x,
        col_types = readr::cols(HUC = "c", .default = "n")
      )
    }
  ) |>
    setNames(required_vars)

  # Get in units by splitting csv paths by out_path_pattern and extracting
  #  second %s value
  units_in <- stringr::str_extract(
    basename(required_csvs),
    ".*_(.*?).csv$",
    group = 1
  )

  # Check assumptions before calculating new variable ----------------
  # Ensure required variables exist in var_names
  assertthat::assert_that(
    all(required_vars %in% names(var_names)),
    msg = sprintf(
      "Variables required to compute new variable (%s) must appear as names in `var_names`",
      toString(required_vars)
    )
  )

  # Ensure equation can be applied to data frames
  assertthat::assert_that(
    stringr::str_replace_all(
      given_expression,
      pattern = paste(required_vars, collapse = "|"),
      replacement = "tibble(c(1,2,3))"
    ) |>
      sprintf(fmt = "class(%s) == 'data.frame'") |>
      parse(text = _) |>
      eval(),
    msg = sprintf(
      "The given equation (`%s`) must be able to be applied to numeric data frames and return a data frame",
      given_expression
    )
  )

  # Ensure column names match
  assertthat::assert_that(
    length(unique(purrr::map(required_dfs, colnames))) == 1,
    msg = sprintf(
      "Column names for required input datasets (%s) must be equal",
      toString(var_names[required_vars])
    )
  )

  # Ensure column classes are equal
  assertthat::assert_that(
    length(unique(purrr::map(required_dfs, \(.x) purrr::map(.x, class)))) == 1,
    msg = sprintf(
      "Column classes required input datasets (%s) must be equal",
      toString(var_names[required_vars])
    )
  )

  # Ensure units are the same
  assertthat::assert_that(
    length(unique(units_in)) == 1,
    msg = sprintf(
      "The units of input datasets must be equal. The units are: %s",
      toString(sprintf("`%s` (%s)", required_vars, units_in))
    )
  )

  # Calculate new variable ------------------
  # Generate expression used to calculate new variable
  calculation_expression <- stringr::str_replace_all(
    given_expression,
    pattern = setNames(
      sprintf("select(required_dfs[['%s']], where(is.numeric))", required_vars),
      required_vars
    )
  ) |>
    sprintf(fmt = "mutate(out_df, %s)")

  # Perform calculation from `calculation_expression` on matrices
  out_df <- required_dfs[[1]]

  out <- eval(parse(text = calculation_expression))

  # Calculate new units for the output path --------------------
  # Apply expression from input to 1 of each unit
  unit_calc_expression <- stringr::str_replace_all(
    given_expression,
    pattern = setNames(
      sprintf("units::as_units(1, '%s')", units_in),
      required_vars
    )
  )

  unit_out <- eval(parse(text = unit_calc_expression)) |>
    units::deparse_unit()

  # Output
  out_path <- sprintf(
    out_path_pattern,
    paste(derived_var, unit_out, sep = "_")
  ) |>
    setNames(derived_var_name)

  # Multiply numeric columns by scale factor
  if (scale_factor != 1) {
    cli::cli_inform(c(
      "i" = "Scaling output: multiplying {derived_var_name} by {scale_factor}."
    ))

    out <- dplyr::mutate(
      out,
      dplyr::across(dplyr::where(is.numeric), \(.x) .x * scale_factor)
    )
  }

  # Convert datasets from HUC12 to HUC8 as needed
  if (isTRUE(derived_var_name %in% convert_to_huc8)) {
    out <- convert_huc12_data_to_huc8(out)
  }

  readr::write_csv(out, file = out_path)

  return(out_path)
}

#' Helper for `model_nc_to_csv` and `compute_derived_variables`
#'
#' Converts Data in HUC12 rows into HUC rows by mean of HUC12s within HUC8
#'
#' @param in_data data frame in HUC x Date format
#'
#' @return HUC8 x Date data frame
#'
convert_huc12_data_to_huc8 <- function(in_data) {
  huc12_col <- stringr::str_subset(colnames(in_data), "(?i)huc")
  assertthat::assert_that(
    length(huc12_col) == 1,
    msg = "When converting from HUC12 to HUC8 data, there must only be 1 column with 'huc' in the name of the input data"
  )

  out <- in_data |>
    dplyr::mutate(
      HUC = stringr::str_sub(.data[[huc12_col]], start = 1, end = 8),
      .after = 1,
      .keep = "unused"
    ) |>
    dplyr::group_by(HUC) |>
    dplyr::summarise(dplyr::across(
      tidyselect::everything(),
      \(.x) mean(.x, na.rm = TRUE)
    ))

  return(out)
}
