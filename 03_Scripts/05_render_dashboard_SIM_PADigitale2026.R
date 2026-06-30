# ============================================================ #
# Script: 05_run_dashboard_SIM_PADigitale2026.R
#
# Obiettivo:
#   1. scaricare da Drive gli input prodotti dallo script indicatori;
#   2. avviare localmente la dashboard R Markdown con runtime Shiny.
# ============================================================ #

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")

suppressPackageStartupMessages({
  library(googledrive)
  library(rmarkdown)
})

googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive"
)

# --------------------------------------------------------------------------- #
# PARAMETRI
# --------------------------------------------------------------------------- #

RUN_ID_INDICATORS <- "20260624_163200"

FILE_RMD <- file.path(
  "03_Scripts",
  "PAdigitale2026",
  "05_dashboard_SIM_PADigitale2026.Rmd"
)

ANNO_NUTS <- 2024
RISOLUZIONE_NUTS <- "10"

if (!file.exists(FILE_RMD)) {
  stop("File Rmd non trovato: ", FILE_RMD)
}

FILE_RMD <- normalizePath(
  FILE_RMD,
  winslash = "/",
  mustWork = TRUE
)

get_config_value <- function(name, default = NULL) {
  if (exists(name, inherits = TRUE)) {
    get(name, inherits = TRUE)
  } else {
    default
  }
}

DRIVE_DIR_INDICATORS_BASE <- get_config_value(
  "DRIVE_DIR_INDICATORS",
  get_config_value(
    "DRIVE_DIR_INDICATORI",
    file.path("01_Dataset", "Indicators")
  )
)

DRIVE_DIR_METADATA_BASE <- get_config_value(
  "DRIVE_DIR_METADATA",
  "02_Metadata"
)

DRIVE_DIR_INDICATORS_MET <- get_config_value(
  "DRIVE_DIR_INDICATORS_MET",
  file.path(
    DRIVE_DIR_METADATA_BASE,
    "Indicators_met"
  )
)

RUN_ID_DASHBOARD <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

DIR_DASH_LOCAL <- normalizePath(
  file.path(
    DIR_TEMP,
    "PADigitale2026",
    "Dashboard",
    RUN_ID_DASHBOARD
  ),
  winslash = "/",
  mustWork = FALSE
)

DIR_INPUT_LOCAL <- file.path(
  DIR_DASH_LOCAL,
  "input"
)

DIR_LOGS_LOCAL <- file.path(
  DIR_TEMP,
  "PADigitale2026",
  "Logs",
  RUN_ID_DASHBOARD
)

dir.create(
  DIR_INPUT_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  DIR_LOGS_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

DRIVE_INDICATORS <- file.path(
  DRIVE_DIR_INDICATORS_BASE,
  "PADigitale2026",
  RUN_ID_INDICATORS
)

DRIVE_METADATA <- file.path(
  DRIVE_DIR_INDICATORS_MET,
  "PADigitale2026",
  RUN_ID_INDICATORS
)

DRIVE_LOGS <- file.path(
  DRIVE_DIR_LOGS,
  "PADigitale2026",
  RUN_ID_DASHBOARD
)

console_log <- start_console_log(
  log_dir = DIR_LOGS_LOCAL,
  run_id = RUN_ID_DASHBOARD,
  script_name = "05_run_dashboard_SIM_PADigitale2026"
)

download_and_check <- function(drive_folder, filename) {
  local_path <- normalizePath(
    file.path(DIR_INPUT_LOCAL, filename),
    winslash = "/",
    mustWork = FALSE
  )
  
  dir.create(
    dirname(local_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  drive_download_from_path(
    drive_file_rel = file.path(
      drive_folder,
      filename
    ),
    local_path = local_path
  )
  
  if (!file.exists(local_path)) {
    stop("File non scaricato: ", local_path)
  }
  
  normalizePath(
    local_path,
    winslash = "/",
    mustWork = TRUE
  )
}

status_run <- "failed"

tryCatch({
  
  local_fact <- download_and_check(
    DRIVE_INDICATORS,
    "FACT_PADIGITALE2026_DASHBOARD.json"
  )
  
  local_dim_enti <- download_and_check(
    DRIVE_INDICATORS,
    "DIM_ENTI_PADIGITALE2026.json"
  )
  
  local_dim_avvisi <- download_and_check(
    DRIVE_INDICATORS,
    "DIM_AVVISI_PADIGITALE2026.json"
  )
  
  local_met_indicatori <- download_and_check(
    DRIVE_METADATA,
    "MET_INDICATORS_PADIGITALE2026.json"
  )
  
  local_met_filtri <- download_and_check(
    DRIVE_METADATA,
    "MET_FILTERS_PADIGITALE2026.json"
  )
  
  message("Rmd: ", FILE_RMD)
  message("Fact: ", local_fact)
  message("Dimensione enti: ", local_dim_enti)
  message("Dimensione avvisi: ", local_dim_avvisi)
  message("Metadati indicatori: ", local_met_indicatori)
  message("Metadati filtri: ", local_met_filtri)
  
  status_run <- "running"
  
  # I parametri del documento devono essere passati in render_args.
  # rmarkdown::run() non ha un argomento params diretto.
  rmarkdown::run(
    file = FILE_RMD,
    shiny_args = list(
      launch.browser = TRUE
    ),
    render_args = list(
      params = list(
        file_fact_dashboard = local_fact,
        file_dim_enti = local_dim_enti,
        file_dim_avvisi = local_dim_avvisi,
        file_metadata_indicatori = local_met_indicatori,
        file_metadata_filtri = local_met_filtri,
        run_id_indicatori = RUN_ID_INDICATORS,
        anno_nuts = ANNO_NUTS,
        risoluzione_nuts = RISOLUZIONE_NUTS
      ),
      knit_root_dir = getwd(),
      envir = new.env(parent = globalenv())
    )
  )
  
  status_run <- "completed"
  
}, error = function(e) {
  
  message(
    "ERRORE dashboard: ",
    conditionMessage(e)
  )
  
  status_run <<- "failed"
  stop(e)
  
}, finally = {
  
  console_log_path <- stop_console_log(
    console_log,
    status = status_run
  )
  
  drive_upload_or_update(
    local_path = console_log_path,
    drive_folder_rel = DRIVE_LOGS
  )
})

message(
  "--- Dashboard terminata. RUN_ID: ",
  RUN_ID_DASHBOARD,
  " | status: ",
  status_run,
  " ---"
)
