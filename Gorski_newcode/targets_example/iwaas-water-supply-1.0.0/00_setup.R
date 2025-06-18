# Load scripts
source("01_configuration/src/sb_cache.R")

# ScienceBase
initialize_and_cache_sb(
  sb_username = rstudioapi::showPrompt("Username", "SB Username"),
  renviron_file = ".Renviron",
  override_login = FALSE
)
