#' Unzip and fix geometry type issue in Mainstems data
#'
#' @param in_zip character; path to zipped GDB of Mainstems HUC data
#' @param layer character; name of GDB layer to extract. Examples include
#'   "WBDHU12" for HUC12s and "WBDHU8" for HUC8s
#' @param out_file 
#'
#' @return
#' @export
#'
#' @examples
fix_mainstems_geom <- function(in_zip, layer,  out_file) {
  # Unzip geodatabase
  gdb_file <- unzip_files(
    zip_files = in_zip,
    path_out = tempdir(check = TRUE),
    file_out_pattern = ".."
  )
  
  # Change all geometry type to MULTIPOLYGON
  # https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
  # https://github.com/r-spatial/sf/issues/748
  # https://gdal.org/programs/ogr2ogr.html
  sf::gdal_utils(
    util = "vectortranslate",
    source = gdb_file,
    destination = out_file,
    options = c(layer, "-nlt", "MULTIPOLYGON", "-overwrite")
  )
  
  return(out_file)
}



################################################################################
#                           Prepare Mainstems HUC data                         #
################################################################################
#' Prepare Mainstems HUC data
#'
#' Does initial munge for HUC data: subset to CONUS, remove unnecessary columns,
#'   merge polygons with duplicate HUC IDs.
#'
#' @param huc_path character; path to GPKG of Mainstems HUC data
#' @param layer character; name of layer in `huc_path` to read in. Passed to
#'   `sf::st_read()`.`
#' @param crs_out 'crs' object or input string for sf::st_crs; crs of output.
#'   If `NULL` (default), the output sf object retains the same CRS as the
#'     input.
#' @param oconus lgl; should OCONUS HUCs be included?
#' @param huc_wt_csv chr; (opt) csv containing proportions of HUC12-HRU overlap;
#'   columns should include `HUC12` and `wght`
#' @param huc_wt_trshld num [0,1]; the threshold of HUC12-HRU overlap fraction;
#'   HUCs with a value less than the threshold will be excluded from analysis
#' @param exclude_non_plot_hucs lgl; If `TRUE`, HUCs identified as those that
#'   shouldn't be plotted are excluded from output. If `FALSE` (default), no
#'   additional HUCs are excluded from the output.
#'
#' @return sf; HUC polygons
#'
prepare_hucs <- function(huc_path, layer, crs_out = NULL, oconus = FALSE,
                         huc_wt_csv = NULL, huc_wt_trshld = 0.8,
                         exclude_non_plot_hucs = FALSE) {

  # Initial munge
  huc_sf <- sf::st_read(huc_path, layer = layer) |>
    dplyr::select(
      -TNMID, -METASOURCEID, -SOURCEDATADESC, -SOURCEORIGINATOR,
      -SOURCEFEATUREID, -LOADDATE, -GLOBALID
    ) |>
    dplyr::rename_with(.cols = starts_with("huc"), function(.) {
      "HUC"
    })

  # Remove OCONUS HUCs
  if (!oconus) {
    huc_sf <- dplyr::filter(
      huc_sf,
      !startsWith(HUC, "19"), # drop alaska
      !startsWith(HUC, "20"), # drop hawaii
      !startsWith(HUC, "21"), # drop puerto rico
      !startsWith(HUC, "22") # drop pacific islands
    )
  }

  # Remove great lakes and shorelines
  if (exclude_non_plot_hucs) {
    huc_exclusions <- c(
      # Great lakes
      "041800000200", "041900000200", "042400000200", "042600000200",
      "041502000200", "04180000", "04190000", "04240000", "04260000",
      "04150200",

      # Great Salt Lake
      "160203100200", "16020310",

      # Great Lakes shorelines
      "041505000000", "042701010000", "041502000100", "041506000000",
      "042600000101", "041800000101", "041800000102", "090300091423",
      "041503090407", "042400000102", "042400000101", "042600000102"
    )

    huc_sf <- dplyr::filter(
      huc_sf,
      !STATES %in% c("MX", "CN"),
      !HUC %in% huc_exclusions
    )
  }

  ###############################
  # Remove HUCs below threshold #
  ###############################
  if (!is.null(huc_wt_csv)) {
    hucs_blw_thrsh <- purrr::map(
      huc_wt_csv,
      \(x) {
        readr::read_csv(
          x,
          col_select = tidyselect::all_of(c("huc_id", "weight")),
          col_types = "cn"
        )
      }
    ) |>
      purrr::list_rbind()

    message(sprintf(
      "Removing %s HUCs, which fall below the frational coverage threshold of %s",
      length(hucs_blw_thrsh),
      huc_wt_trshld
    ))

    huc_sf <- dplyr::filter(huc_sf, !HUC %in% hucs_blw_thrsh)
  }

  ######################################
  # Merge polygons with duplicate HUCS #
  ######################################
  # Identify duplicate HUCs
  dup_hucs <- huc_sf |>
    dplyr::filter(duplicated(HUC)) |>
    dplyr::pull(HUC)

  if (length(dup_hucs) == 0) {
    huc_out <- huc_sf
  } else {
    dup_data <- huc_sf |>
      dplyr::filter(HUC %in% dup_hucs) |>
      dplyr::group_by(HUC)

    # Error if data from duplicate HUCS differs
    duplicate_huc_count <- dup_data |>
      sf::st_drop_geometry() |>
      dplyr::summarise(dplyr::across(
        dplyr::where(~ !is.numeric(.x)),
        ~ dplyr::n_distinct(.x)
      )) |>
      dplyr::select(dplyr::where(is.numeric)) |>
      as.matrix() |>
      min()

    if (duplicate_huc_count > 1) {
      stop("Rows with duplicate HUCs contain data that differs")
    }

    # Merge duplicate HUC geometries
    merged_geom <- dup_data |>
      dplyr::summarize() |>
      sf::st_geometry()

    # Aggregate WBD data and set the geometry
    # Numeric data is summed; the first observation is used for non-numeric data
    merged_data <- dup_data |>
      sf::st_drop_geometry() |>
      dplyr::summarise(
        dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        dplyr::across(dplyr::where(~ !is.numeric(.x)), ~ dplyr::first(.x))
      ) |>
      dplyr::select(tidyselect::all_of(
        colnames(sf::st_drop_geometry(huc_sf))
      )) |>
      sf::st_set_geometry(merged_geom) |>
      dplyr::rename(SHAPE = geometry)

    # Add merged duplicates back into full dataset
    huc_out <- huc_sf |>
      dplyr::filter(!HUC %in% dup_hucs) |>
      dplyr::bind_rows(merged_data) |>
      dplyr::arrange(HUC)
  }

  # Ensure consistent geometry
  huc_out <- sf::st_cast(huc_out, "MULTIPOLYGON")

  # Transform data
  if (is.null(crs_out)) {
    crs_out <- sf::st_crs(huc_out)
  }

  if (sf::st_crs(huc_out) != sf::st_crs(crs_out)) {
    message("Reprojecting `huc_path` to `crs_out`")

    huc_out <- terra::vect(huc_out) |>
      terra::project(y = crs_out) |>
      sf::st_as_sf()
  }

  return(huc_out)
}
