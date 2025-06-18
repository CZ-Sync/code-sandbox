#' Subset HUC12 polygons and intersect them
#'
#' @param x terra SpatVector; Mainstems HUC12 dataset
#' @param y terra SpatVector; water use HUC12 dataset
#' @param x_ind num; indices of Mainstems rows to subset `x` by
#' @param y_ind num; indices of water use rows to subset `y` by
#'
#' @return data.table with all columns from `x` and `y`, in addition to 
#'  `intersect_area`, which contains the area of the intersection of the two
#'   corresponding huc 12 polygons in m^2.
#'
calc_intersect_area <- function(x, y, x_ind, y_ind) {
  intersect_polys <- terra::intersect(
    y[unique(y_ind)],
    x[unique(x_ind)]
  )

  out <- data.table::as.data.table(intersect_polys)
  out[["intersect_area"]] <- intersect_polys |>
    sf::st_as_sf() |>
    sf::st_transform("ESRI:102003") |>
    sf::st_area() |>
    as.numeric()

  return(out)
}

#' Make grouping vector
#'
#' Each group will be approximately the same size, as specified by `approx_size`
#'   and each unique value within `x` will only occur within one group
#'
#' @param x a vector (num, chr) to split into groups; must be sorted
#' @param approx_size int; the approximate number of elements within a group
#'
#' @return chr; a vector of values to identify groups associated with `x`. Pass
#'   to the `f` argument of `base::split()`
#'
unique_split_factor <- function(x, approx_size) {
  # Ensure x is sorted
  if (!any(identical(x, sort(x)), identical(x, sort(x, decreasing = TRUE)))) {
    cli::cli_abort("{.arg x} must be sorted (ascending or descending order).")
  }

  # Get variables needed for calculations
  length_out <- length(x)
  n_groups <- ceiling(length_out / approx_size)

  # Create a factor based on an even split of `x` (equal-ish group sizes)
  f_even_split <- rep(
    seq(1, n_groups),
    each = ceiling(length_out / n_groups),
    length.out = length_out
  ) |>
    as.factor()

  # Get unique values from each evenly split group - reverse the order to make
  #   accumulate() work as desired
  l_even_split <- split(x, f = f_even_split) |>
    purrr::map(unique) |>
    rev()
  names(l_even_split) <- as.character(seq_along(l_even_split))

  # Get factor for evenly split groups where each value of `x` appears in only 1
  #   group.
  l_even_split |>
    purrr::accumulate(setdiff, .dir = "backward") |>
    rev() |>
    purrr::map(~ which(x %in% .x)) |>
    purrr::imap(~ rep(.y, length(.x))) |>
    unlist(use.names = FALSE)
}

#' Get HUC12 total lake storage volume from Hydrolakes
#' 
#' Methods developed by Amy Galanter in Python and converted to R for easy
#' integration into the pipeline.
#'
#' @param hydrolakes_gdb chr; path to Hydrolakes data
#' @param mainstems_gpkg chr; path to mainstems data that has been unzipped and
#'   the geometries made consistent (MULTISURFACE -> MULTIPOLYGON).
#' @param chunk_size int;
#'
#' @return data frame (tidytable) of HUC12s and lake volumes
#'
hydrolakes_to_huc12 <- function(hydrolakes_gdb, mainstems_gpkg,
                                chunk_size = 1000) {
  # Quickly check arguments ----
  stopifnot(
    is.character(hydrolakes_gdb),
    is.character(mainstems_gpkg),
    rlang::is_integerish(chunk_size),
    file.exists(hydrolakes_gdb),
    file.exists(mainstems_gpkg)
  )

  # Read in the mainstems HUC12 dataset and merge duplicate HUC12s ----
  cli::cli_progress_message(" Reading Mainstems data")
  mainstems <- terra::vect(mainstems_gpkg)

  ms_huc12 <- mainstems[["HUC12"]][["HUC12"]]
  dups <- ms_huc12[duplicated(ms_huc12)]
  dup_idx <- ms_huc12 %in% dups

  mainstems_dups <- mainstems |>
    tidyterra::filter(dup_idx) |>
    terra::aggregate(by = "HUC12")

  mainstems <- mainstems |>
    tidyterra::filter(!dup_idx) |>
    tidyterra::bind_spat_rows(mainstems_dups)

  # Read in the HydroLakes dataset ----
  cli::cli_progress_message(" Reading Hydrolakes data")
  hydrolakes <- terra::vect(
    hydrolakes_gdb,
    query = paste(
      "SELECT Hylak_id, Lake_area, Vol_total FROM \"HydroLAKES_polys_v10\"",
      "WHERE  Country = 'United States of America'",
      "ORDER By Pour_lat, Pour_long"
    )
  ) |>
    terra::project(terra::crs(mainstems)) |>
    tidyterra::mutate(
      Vol_total_km3 = Vol_total * 0.001,
      sa_vol = Lake_area / (Vol_total * 0.001),
      .keep = "unused"
    )

  # Get relationship table ----
  cli::cli_progress_message(" Identifying intersecting polygons")
  intersect_df <- terra::relate(
    hydrolakes,
    mainstems,
    relation = "intersects",
    pairs = TRUE
  ) |>
    as.data.frame() |>
    setNames(c("hydrolakes", "mainstems")) |>
    dplyr::arrange(mainstems)

  # Calculate intersections in batches -----
  # Split table
  intersect_df_list <- split(
    intersect_df,
    f = unique_split_factor(intersect_df[["mainstems"]], approx_size = 1000)
  )

  # Prep for batch
  intersection_list <- vector("list", length(intersect_df_list))
  cli::cli_progress_bar(
    " Intersecting polygons",
    total = length(intersect_df_list)
  )

  # Calculate intersections
  for (i in seq_along(intersect_df_list)) {
    intersection_list[[i]] <- calc_intersect_area(
      x = hydrolakes,
      y = mainstems,
      x_ind = intersect_df_list[[i]][["hydrolakes"]],
      y_ind = intersect_df_list[[i]][["mainstems"]]
    )
    cli::cli_progress_update()
  }

  intersection_df <- tidytable::bind_rows(intersection_list) |>
    tidytable::group_by(Hylak_id) |>
    tidytable::mutate(n = tidytable::n()) |>
    tidytable::ungroup()

  # Calculate volume for non-split lakes ----
  non_split_lakes <- tidytable::filter(intersection_df, n == 1)

  non_split_lakes_grouped <- non_split_lakes |>
    tidytable::group_by(HUC12) |>
    tidytable::summarise(
      Hylak_id = tidytable::n(),
      Vol_total_km3 = sum(Vol_total_km3, na.rm = TRUE)
    ) |>
    tidytable::mutate(multi_flag = 0)

  # Calculate volume for split lakes ----
  split_lakes <- tidytable::filter(intersection_df, n > 1) |>
    tidytable::mutate(
      area_m2 = intersect_area,
      area_km2 = area_m2 * 1e-6,
      est_vol_lake_km3 = area_km2 / sa_vol
    )

  split_lakes_grouped <- split_lakes |>
    tidytable::group_by(HUC12) |>
    tidytable::summarise(
      Hylak_id = tidytable::n(),
      Vol_total_km3 = sum(est_vol_lake_km3, na.rm = TRUE)
    ) |>
    tidytable::mutate(multi_flag = 1)

  # Get volume totals by HUC ----
  hydrolakes_huc_aggregation <- tidytable::bind_rows(
    non_split_lakes_grouped,
    split_lakes_grouped
  ) |>
    tidytable::group_by(HUC12) |>
    tidytable::summarise(
      Hylak_id = sum(Hylak_id, na.rm = TRUE),
      Vol_total_km3 = sum(Vol_total_km3, na.rm = TRUE),
      num_partial_water_body = sum(multi_flag, na.rm = TRUE)
    ) |>
    tidytable::mutate(
      num_whole_lakes = Hylak_id - num_partial_water_body,
      vol_m3 = Vol_total_km3 * 1e9
    ) |>
    tidytable::select(HUC12, num_partial_water_body, num_whole_lakes, vol_m3)

  out <- mainstems |>
    as.data.frame() |>
    tidytable::select(HUC12) |>
    tidytable::left_join(hydrolakes_huc_aggregation) |>
    tidytable::replace_na(list(
      num_partial_water_body = 0,
      num_whole_lakes = 0,
      vol_m3 = 0
    ))

  return(out)
}
