#' Add OCONUS information to HUC12-to-hydrologic region crosswalk
#'
#' @param in_csv chr; path to HUC12 to hydrologic units crosswalk.
#' @param huc_sf sf object; hydrologic regions.
#' @param ... additional arguments sent to `readr::read_csv()`
#'
#' @return sf; HUC12-to-hydrologic region crosswalk with additional OCONUS rows
#'
prep_vm_crosswalk <- function(in_csv, huc_sf, ...) {
  conus_xwalk <- readr::read_csv(in_csv, ...)

  expected_colnames <- c(
    "HUC12", "HUC8", "Region", "Region_nam", "AggRegion",
    "AggRegion_nam", "Area_sqkm"
  )

  assertthat::assert_that(
    identical(colnames(conus_xwalk), expected_colnames),
    msg = "The column names of `in_csv` are not the expected column names."
  )

  oconus_xwalk <- sf::st_drop_geometry(huc_sf) |>
    dplyr::select(HUC12 = HUC, Area_sqkm = AREASQKM) |>
    # Retain only OCONUS
    dplyr::filter(stringr::str_starts(HUC12, paste(19:22, collapse = "|"))) |>
    dplyr::mutate(
      # Get HUC 8s
      HUC8 = substr(HUC12, 1, 8),
      # Make Alaska VM Region 19 and Islands VM Region 20
      Region = dplyr::case_when(
        startsWith(HUC12, "19") ~ "19",
        substr(HUC12, 1, 2) %in% as.character(20:22) ~ "20"
      ),
      Region_nam = dplyr::case_when(
        startsWith(HUC12, "19") ~ "Alaska",
        substr(HUC12, 1, 2) %in% as.character(20:22) ~ "Islands (HI, PR, PacIsl)"
      ),
      # Make OCONUS AggRegion 5
      AggRegion = "5",
      AggRegion_nam = "OCONUS",
      # Keep Area_sqkm last columns
      .after = 1
    )

  # Sanity chack that column names and are identical
  assertthat::assert_that(
    identical(colnames(conus_xwalk), colnames(oconus_xwalk)),
    identical(purrr::map(conus_xwalk, class), purrr::map(oconus_xwalk, class)),
    msg = "Column names and classes must be identical for conus_xwalk and oconus_xwalk"
  )

  dplyr::bind_rows(conus_xwalk, oconus_xwalk)
}

#' Prepare hydrologic units spatial file
#'
#' @param hr_shp sf object; hydrologic unit polygons
#'
#' @return
#'
prep_van_metre_sf <- function(hr_shp) {
  sf::st_read(hr_shp) |>
    # Simplify shape (topologically-aware)
    rmapshaper::ms_simplify(0.01) |>
    # Rename and group polygons
    dplyr::rename(Region = HR_Number, Region_nam = Name) |>
    dplyr::group_by(Region, Region_nam) |>
    dplyr::summarise(id = unique(Region), .groups = "drop") |>
    dplyr::mutate(Region = as.character(Region)) |>
    # Fix misspellings
    dplyr::mutate(Region_nam = dplyr::case_match(
      Region_nam,
      "Tennesse-Missouri" ~ "Tennessee-Missouri",
      "Gulf Cost" ~ "Gulf Coast",
      "Souris-Red_Rainy" ~ "Souris-Red-Rainy",
      "Pacific NW" ~ "Pacific Northwest",
      .default = Region_nam
    ))
}
