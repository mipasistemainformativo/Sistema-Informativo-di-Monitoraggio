# ============================================================= #
# Script: 06_render_dashboard_SIM_CA_new.R
#
# Obiettivo:
#   1. scaricare da Drive gli ultimi output del Conto Annuale;
#   2. avviare localmente la dashboard R Markdown con runtime Shiny;
#   3. mantenere separato il render CA dal render integrato.
#
# Nota server/deploy:
#   La dashboard legge solo file locali passati in params. La parte Drive
#   rimane nel render/launcher. In futuro, su server, gli stessi parametri
#   potranno puntare a una cartella dati montata o aggiornata da pipeline.
# ============================================================= #

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")
source("03_Scripts/00_spatial_helpers.R")
source("03_Scripts/Conto_annuale/00_ca_config.R") # mettere in rmarkown 

# mettere in Rmarkdown
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(googledrive)
  library(rmarkdown)
  library(purrr)
})

# 1) AUTENTICAZIONE DRIVE ----------------------------------------------------

if (exists("SIM_DRIVE_EMAIL")) {
  options(gargle_oauth_email = SIM_DRIVE_EMAIL)
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive",
    cache = TRUE
  )
} else {
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
}

# 2) PARAMETRI ---------------------------------------------------------------


FILE_RMD <- file.path(
  "03_Scripts",
  "Conto_annuale",
  "05_dashboard_SIM_ContoAnnuale_v4.Rmd"
)

if (!file.exists(FILE_RMD)) {
  stop("File Rmd non trovato: ", FILE_RMD)
}

FILE_RMD <- normalizePath(FILE_RMD, winslash = "/", mustWork = TRUE)


# Elimina eventuale cache del markdown
cache_dir <- file.path(
  dirname(FILE_RMD),
  paste0(
    tools::file_path_sans_ext(basename(FILE_RMD)),
    "_cache"
  )
)

if (dir.exists(cache_dir)) {
  message("Rimuovo cache di: ", basename(FILE_RMD))
  unlink(cache_dir, recursive = TRUE, force = TRUE)
}


RUN_ID_DASHBOARD <- format(Sys.time(), "%Y%m%d_%H%M%S")

DIR_DASH_LOCAL <- normalizePath(
  file.path(DIR_TEMP, "Conto_annuale", "Dashboard", RUN_ID_DASHBOARD),
  winslash = "/",
  mustWork = FALSE
)

DIR_INPUT_LOCAL <- file.path(DIR_DASH_LOCAL, "input")
DIR_LOGS_LOCAL <- file.path(DIR_DASH_LOCAL, "logs")

dir.create(DIR_INPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)

DRIVE_LOGS <- file.path(DRIVE_CA_LOGS, "Dashboard", RUN_ID_DASHBOARD)

console_log <- start_console_log(
  log_dir = DIR_LOGS_LOCAL,
  run_id = RUN_ID_DASHBOARD,
  script_name = "06_render_dashboard_SIM_CA_new"
)

# 3) HELPERS DRIVE -----------------------------------------------------------

find_latest_run_folder <- function(drive_folder_rel) {
  folder <- sim_drive_ls_path(drive_folder_rel, create = FALSE)
  runs <- googledrive::drive_ls(folder) %>%
    dplyr::filter(
      stringr::str_detect(.data$name, "^\\d{8}_\\d{6}$"),
      .data$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder" | TRUE
    ) %>%
    dplyr::arrange(dplyr::desc(.data$name)) %>%
    dplyr::slice(1)
  if (nrow(runs) == 0L) {
    stop("Nessuna cartella RUN_ID trovata in: ", drive_folder_rel)
  }
  runs
}

find_file_in_run <- function(run_folder, filename) {
  files <- googledrive::drive_ls(run_folder) %>%
    dplyr::filter(.data$name == filename) %>%
    dplyr::slice(1)
  if (nrow(files) == 0L) {
    stop("File non trovato nella cartella RUN_ID ", run_folder$name[[1]], ": ", filename)
  }
  files
}

download_file_from_run <- function(run_folder, filename, local_name = filename) {
  file <- find_file_in_run(run_folder, filename)
  local_path <- file.path(DIR_INPUT_LOCAL, local_name)
  googledrive::drive_download(file = file, path = local_path, overwrite = TRUE)
  if (!file.exists(local_path)) {
    stop("Download non riuscito: ", local_path)
  }
  normalizePath(local_path, winslash = "/", mustWork = TRUE)
}

# 4) RUN DASHBOARD -----------------------------------------------------------

# DRIVE_CA_INDICATORS <- file.path(DRIVE_DIR_INDICATORS, "Conto_annuale")

status_run <- "failed"

tryCatch({
  
  # Output file 03: indicatori CA e FACT wide.
  latest_indicators_run <- find_latest_run_folder(DRIVE_CA_INDICATORS)
  run_id_indicatori_ca <- latest_indicators_run$name[[1]]
  
  # Output file 04: catalogo SIM e fact aggregati per dashboard.
  DRIVE_CA_DASHBOARD_CATALOG <- file.path(DRIVE_CA_INDICATORS, "Dashboard")
  latest_catalog_run <- find_latest_run_folder(DRIVE_CA_DASHBOARD_CATALOG)
  run_id_catalogo_ca <- latest_catalog_run$name[[1]]
  
  message("Ultimo RUN indicatori CA: ", run_id_indicatori_ca)
  message("Ultimo RUN catalogo/dashboard CA: ", run_id_catalogo_ca)
  
  local_files <- list(
    file_fact_ca_dashboard = download_file_from_run(latest_indicators_run, "FACT_CA_DASHBOARD.rds"),
    file_dash_sections_ca = download_file_from_run(latest_catalog_run, "DASH_SECTIONS_CA.rds"),
    file_dash_filters_ca = download_file_from_run(latest_catalog_run, "DASH_FILTERS_CA.rds"),
    file_dash_indicators_ca = download_file_from_run(latest_catalog_run, "DASH_INDICATORS_CA.rds"),
    file_fact_ca_coverage = download_file_from_run(latest_catalog_run, "FACT_CA_COVERAGE.rds"),
    file_fact_ca_regione = download_file_from_run(latest_catalog_run, "FACT_CA_REGIONE.rds"),
    file_fact_ca_zona = download_file_from_run(latest_catalog_run, "FACT_CA_ZONA.rds"),
    file_fact_ca_fg = download_file_from_run(latest_catalog_run, "FACT_CA_FG.rds"),
    file_fact_ca_tipologia = download_file_from_run(latest_catalog_run, "FACT_CA_TIPOLOGIA.rds"),
    file_fact_ca_trend = download_file_from_run(latest_catalog_run, "FACT_CA_TREND.rds")
  )
  
  message("Dashboard Conto Annuale:")
  message(" - Rmd: ", FILE_RMD)
  message(" - RUN_ID_DASHBOARD: ", RUN_ID_DASHBOARD)
  purrr::iwalk(local_files, ~ message(" - ", .y, ": ", .x))
  
  status_run <- "running"
  
  rmarkdown::run(
    file = FILE_RMD,
    shiny_args = list(launch.browser = TRUE),
    render_args = list(
      params = c(
        local_files,
        list(
          run_id_indicatori_ca = run_id_indicatori_ca,
          run_id_catalogo_ca = run_id_catalogo_ca
        )
      ),
      knit_root_dir = getwd(),
      envir = new.env(parent = globalenv())
    )
  )
  
  status_run <- "completed"
  
  }, error = function(e) {
    message("ERRORE dashboard Conto Annuale: ", conditionMessage(e))
    status_run <<- "failed"
    stop(e)
# }, error = function(e) {
#   message("ERRORE dashboard Conto Annuale: ", conditionMessage(e))
#   status_run <<- "failed"
#   return(invisible(NULL))
  
}, finally = {
  console_log_path <- stop_console_log(console_log, status = status_run)
  message("Log generato: ", basename(console_log_path), " | Percorso locale: ", console_log_path)
  
  drive_upload_or_update(
    local_path = console_log_path,
    drive_folder_rel = DRIVE_LOGS
  )
  
  message("Log caricato su Drive: ", DRIVE_LOGS, "/", basename(console_log_path))
})

message(
  "--- Dashboard Conto Annuale terminata. RUN_ID: ",
  RUN_ID_DASHBOARD,
  " | status: ",
  status_run,
  " ---"
)