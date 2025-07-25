#' Initialize ScienceBase session and download files
#'
#' @param sb_id chr; ScienceBase ID
#' @param names chr; names of files to download from ScienceBase
#' @param destinations  chr; write path location for downloaded files
#' @param renviron_file chr; path to .Renviron file where credentials are cached
#' @param ... additional arguments passed to `sbtools::item_file_download()`
#'
#' @return chr; path to downloaded files
#'
sb_initialize_and_download <- function(sb_id, names, destinations,
                                       renviron_file = ".Renviron", ...) {
  # Initialize ScienceBase session
  sb_login_cached(renviron_file = renviron_file)

  # Download SB files
  sbtools::item_file_download(
    sb_id = sb_id,
    names = names,
    destinations = destinations,
    ...
  )

  return(destinations)
}

#' Login to ScienceBase using cached credentials
#'
#' @param renviron_file chr; path to .Renviron file
#'
#' @return `TRUE` if logged in. Error if not.
#'
sb_login_cached <- function(renviron_file) {
  # If logged in, return TRUE and skip the rest
  if (sbtools::is_logged_in()) {
    return(TRUE)
  }

  # Try a token refresh
  tryCatch(
    sbtools:::token_refresh(),
    warning = function(x) {},
    error = function(x) FALSE
  )

  if (sbtools::is_logged_in()) {
    return(TRUE)
  }

  # If .Renviron file does not exist, re-initialize
  if (!file.exists(renviron_file)) {
    cli::cli_abort(c(
      "Could not find the specified file: {.file {renviron_file}}.",
      "i" = "Follow the instructions in {.file README.md} to initalize and cache
      ScienceBase login credentials."
    ))
  }

  # Read .Renviron file
  existing <- readLines(renviron_file)
  sb_token_idx <- which(startsWith(existing, "sb_token="))
  sb_username_idx <- which(startsWith(existing, "sb_username="))

  # If SB credentials not found, throw error
  if (any(length(sb_token_idx) == 0, length(sb_username_idx) == 0)) {
    cli::cli_abort(c(
      "Could not find the username or token in the specified file:
      {.file {renviron_file}}.",
      "i" = "Follow the instructions in {.file README.md} to re-initalize and
      cache ScienceBase login credentials."
    ))
  }

  # Get ScienceBase credentials
  sb_token <- stringr::str_remove(existing[sb_token_idx], "^sb_token=")
  sb_username <- stringr::str_remove(existing[sb_username_idx], "^sb_username=")

  # Initialize ScienceBase session with cached credentials
  sbtools::initialize_sciencebase_session(
    username = sb_username,
    token_text = sb_token
  )

  if (sbtools::is_logged_in()) {
    return(TRUE)
  } else {
    cli::cli_abort(c(
      "Could not login to ScienceBase using cached credentials.",
      "i" = "Follow the instructions in {.file README.md} to re-initalize and
      cache ScienceBase login credentials."
    ))
  }
}
