#' Download files quickly (multiple simultaneously) and fault-tolerantly
#'
#' @param urls chr; vector of URLs to download from. Supports HTTP/S and FTP/S.
#'   Passed to `curl::multi_download()`.
#' @param out_paths chr; Output paths for downloaded files. Must be same length
#'   as `urls`. Must not be defined if `dest_dir` is defined.
#' @param dest_dir chr; directory for all files to be downloaded into. Filenames
#'   are generated from `basename()`. Must be length 1. Must not be defined if
#'   `out_paths` is defined.
#' @param ignore_existing lgl, if TRUE, for all files that already exist
#'   download will not be attempted. If FALSE, a download will be attempted for
#'   all files.
#' @param create_dir lgl; if directory where files are saved does not exist,
#'   create it.
#' @param resume lgl; if TRUE, if file exists, attempt to resume download if
#'   download is incomplete and don't attempt download if download is complete.
#'   If FALSE, download will be restarted for any files with incomplete
#'   downloads. Passed to `curl::multi_download()`.
#' @param ... Additional arguments passed to `curl::multi_download()`. A list of
#'   available options can be found with `curl::curl_options()`.
#'
#' @return chr; list of file paths of downloaded files.
#'
download_files <- function(urls, out_paths, dest_dir, ignore_existing = FALSE,
                           create_dir = TRUE, resume = TRUE, ...) {
  # Build out_paths if dest_dir was passed
  if (rlang::check_exclusive(dest_dir, out_paths) == "dest_dir") {
    out_paths <- file.path(dest_dir, basename(urls))
  }

  # Remove existing files from download list if ignore_existing == TRUE
  if (ignore_existing) {
    files_exist <- file.exists(out_paths)
    existing_files <- out_paths[files_exist]
    download_files <- out_paths[!files_exist]
    download_urls <- urls[!files_exist]
  } else {
    existing_files <- integer(0)
    download_urls <- urls
    download_files <- out_paths
  }

  if (length(existing_files) > 0) {
    cli::cli_inform(c(
      "Skipping {length(existing_files)} existing file{?s}.",
      "Downloading {length(download_files)} file{?s}.",
      "To download all files, set {.code ignore_existing = FALSE}."
    ))
  }

  # Check for and handle non-existant destination directories
  dest_dirs <- unique(dirname(out_paths))

  if (!all(dir.exists(dest_dirs)) && create_dir) {
    dir.create(dest_dirs, recursive = TRUE)
  }

  if (!all(dir.exists(dest_dirs)) && !create_dir) {
    dirs_nonexist <- dest_dirs[!dir.exists(dest_dirs)]
    dirs_message <- setNames(
      sprintf("{.path %s}", dirs_nonexist),
      rep("*", times = length(dirs_nonexist))
    )
    cli::cli_abort(c(
      "Destination {cli::qty(dirs_nonexist)} director{?y/ies} {?does/do} must
      exist.",
      "x" = "The following {cli::qty(dirs_nonexist)} {?does/do} not exist:",
      dirs_message,
      "i" = "Set {.code create_dir = FALSE} and rerun to address the problem."
    ))
  }

  # Download files
  out <- curl::multi_download(
    urls = download_urls,
    destfiles = download_files,
    resume = resume,
    ...
  )

  if (!all(out[["success"]])) {
    unsuccess <- !out[["success"]]
    out[unsuccess, c("status_code", "url", "destfile", "error")]

    msg_bullets <- sprintf(
      "{.file %s}: (Error %s) %s.",
      basename(out[["destfile"]]),
      out[["status_code"]],
      out[["error"]]
    ) |>
      setNames(rep.int("x", times = nrow(out)))

    cli::cli_abort(c(
      "{nrow(out)} file{?s} {?was/were} not successfully downloaded.",
      msg_bullets
    ))
  }

  return(out_paths)
}

#' Remove dates from vector that match dates in the SNODAS missing date file
#'
#' @param in_dates character; vector of dates in YYYYMMDD format
#' @param missing_files_txt character; file path for SNODAS missing file report
#'
#' @return character; a subset of in_dates with dates with missing data
#'   removed
remove_missing_snodas <- function(in_dates, missing_files_txt) {
  # Read missing dates text file
  missing_dates_txt <- readLines(missing_files_txt)

  # Subset dates for masked files
  start_line <- which(startsWith(missing_dates_txt, "MISSING MASKED FILES"))
  end_line <- which(startsWith(missing_dates_txt, "MISSING UNMASKED FILES"))
  missing_dates <- grep(
    "\\d{4}-\\d{2}-\\d{2}",
    missing_dates_txt[seq(start_line, end_line)],
    value = TRUE
  ) |>
    as.Date(format = "%Y-%m-%d") |>
    format(format = "%Y%m%d")

  # Return dates not listed in missing dates text file for masked data
  setdiff(in_dates, missing_dates)
}
