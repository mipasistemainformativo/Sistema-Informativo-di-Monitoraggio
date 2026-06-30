# ............................................................. #
# Script: 06_render_dashboard_SIM_integrata.R
#
# Avvia una shell SIM unica e, in processi separati, le dashboard
# Conto annuale e PA Digitale 2026.
# ............................................................. #

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")

suppressPackageStartupMessages({
  library(callr)
  library(googledrive)
  library(rmarkdown)
})

# googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive",
  cache = ".secrets",
  email = TRUE
)

# 1) PARAMETRI DA CONFIGURARE -------------------------------------------------- 

RUN_ID_PADIGITALE <- "20260624_163200"

FILE_HOME <- file.path("03_Scripts", "SIM", "06_dashboard_SIM_integrata.Rmd")
FILE_PADIGITALE <- file.path("03_Scripts", "PAdigitale2026", "05_dashboard_SIM_PADigitale2026.Rmd")
FILE_INDICATORS_PAGOPA <- file.path("03_Scripts", "PagoPA", "05_dashboard_SIM_PagoPA.Rmd")
FILE_ANAC <- file.path("03_Scripts", "ANAC", "05_dashboard_SIM_ANAC.Rmd")

# Conto Annuale
# Per cambiare dashboard CA, modifica solo questo nome file.
NOME_DASHBOARD_CONTO_ANNUALE <- "05_dashboard_SIM_ContoAnnuale_v4.Rmd"

FILE_CONTO_ANNUALE <- file.path(
  "03_Scripts","Conto_annuale",
  NOME_DASHBOARD_CONTO_ANNUALE
  )

PORT_HOME <- 8010L
PORT_CONTO_ANNUALE <- 8011L
PORT_PADIGITALE <- 8012L
PORT_INDICATORS_PAGOPA <- 8013L
PORT_ANAC <- 8014L

# Lista di perimetro comune. Il percorso stabile è definito in 00_config.R.
DRIVE_MASTER_PA_FILE <- DRIVE_FILE_LISTA_RACCORDO_SIM

# Path input to datasets used by fchildren
# Conto Annuale - nuova dashboard
DRIVE_DIR_DATASET <- "01_Dataset"
DRIVE_DIR_INDICATORS <- file.path(DRIVE_DIR_DATASET, "Indicators")
DRIVE_CA_INDICATORS <- file.path(DRIVE_DIR_INDICATORS, "Conto_annuale")
DRIVE_CA_DASHBOARD_CATALOG <- file.path(DRIVE_CA_INDICATORS, "Dashboard")

# PA Digitale.
DRIVE_PAD_INDICATORS <- file.path(DRIVE_DIR_INDICATORS_PAD26, RUN_ID_PADIGITALE)
DRIVE_PAD_METADATA <- file.path(DRIVE_DIR_INDICATORS_MET_PAD26, RUN_ID_PADIGITALE)

#ANAC
DRIVE_ANAC_INDICATORS     <- file.path(DRIVE_DIR_INDICATORS_ANAC, "INDICATORS_ANAC.json")
DRIVE_ANAC_INDICATORS_CPV <- file.path(DRIVE_DIR_INDICATORS_ANAC, "INDICATORS_CPV_ANAC.json")


# 2) HELPERS ------------------------------------------------------------------- 

required_files <- c(FILE_HOME, FILE_PADIGITALE, FILE_CONTO_ANNUALE, FILE_INDICATORS_PAGOPA, FILE_ANAC)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) stop("File Rmd mancanti: ", paste(missing_files, collapse = ", "))

RUN_ID_DASHBOARD <- format(Sys.time(), "%Y%m%d_%H%M%S")
DIR_RUN <- file.path(DIR_TEMP, "SIM", "Dashboard", RUN_ID_DASHBOARD)
DIR_INPUT <- file.path(DIR_RUN, "input")
DIR_LOGS <- file.path(DIR_RUN, "logs")
dir.create(DIR_INPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)

console_log <- start_console_log(DIR_LOGS, RUN_ID_DASHBOARD, "06_render_dashboard_SIM_integrata")

message("Pandoc padre disponibile: ", rmarkdown::pandoc_available())
message("Pandoc padre versione: ", as.character(rmarkdown::pandoc_version()))
message("RSTUDIO_PANDOC padre: ", Sys.getenv("RSTUDIO_PANDOC"))

find_latest_drive_file <- function(drive_folder_rel, pattern) {
  folder <- drive_get(path = drive_folder_rel)
  files <- drive_ls(folder) |>
    dplyr::filter(stringr::str_detect(.data$name, stringr::regex(pattern, ignore_case = TRUE))) |>
    dplyr::arrange(dplyr::desc(.data$name)) |>
    dplyr::slice(1)
  if (nrow(files) == 0L) stop("Nessun file in ", drive_folder_rel, " con pattern ", pattern)
  files
}

download_latest <- function(drive_folder_rel, pattern, local_name = NULL) {
  file <- find_latest_drive_file(drive_folder_rel, pattern)
  local_name <- local_name %||% file$name[[1]]
  local_path <- file.path(DIR_INPUT, local_name)
  drive_download(file, path = local_path, overwrite = TRUE)
  normalizePath(local_path, winslash = "/", mustWork = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || !nzchar(x)) y else x

download_drive_file <- function(drive_file_rel, local_name = basename(drive_file_rel)) {
  local_path <- file.path(DIR_INPUT, local_name)
  drive_download_from_path(drive_file_rel, local_path)
  if (!file.exists(local_path)) stop("Download non riuscito: ", drive_file_rel)
  normalizePath(local_path, winslash = "/", mustWork = TRUE)
}

download_exact <- function(folder, filename) {
  local_path <- file.path(DIR_INPUT, filename)
  drive_download_from_path(file.path(folder, filename), local_path)
  if (!file.exists(local_path)) stop("Download non riuscito: ", filename)
  normalizePath(local_path, winslash = "/", mustWork = TRUE)
}

find_latest_run_folder <- function(drive_folder_rel) {
  folder <- googledrive::drive_get(path = drive_folder_rel)
  
  runs <- googledrive::drive_ls(folder) |>
    dplyr::filter(stringr::str_detect(.data$name, "^\\d{8}_\\d{6}$")) |>
    dplyr::arrange(dplyr::desc(.data$name)) |>
    dplyr::slice(1)
  
  if (nrow(runs) == 0L) {
    stop("Nessuna cartella RUN_ID trovata in: ", drive_folder_rel)
  }
  
  runs
}

download_file_from_run_integrata <- function(run_folder, filename, local_name = filename) {
  files <- googledrive::drive_ls(run_folder) |>
    dplyr::filter(.data$name == filename) |>
    dplyr::slice(1)
  
  if (nrow(files) == 0L) {
    stop("File non trovato nella cartella RUN_ID ", run_folder$name[[1]], ": ", filename)
  }
  
  local_path <- file.path(DIR_INPUT, local_name)
  
  googledrive::drive_download(
    file = files,
    path = local_path,
    overwrite = TRUE
  )
  
  if (!file.exists(local_path)) {
    stop("Download non riuscito: ", local_path)
  }
  
  normalizePath(local_path, winslash = "/", mustWork = TRUE)
}

# run_rmd_child <- function(
#     file,
#     port,
#     params = list(),
#     app_name
# ) {
#   stdout_file <- file.path(
#     DIR_LOGS,
#     paste0(app_name, "_stdout.log")
#   )
#   
#   stderr_file <- file.path(
#     DIR_LOGS,
#     paste0(app_name, "_stderr.log")
#   )
#   
#   proc <- callr::r_bg(
#     func = function(file, port, params, root) {
#       setwd(root)
#       
#       rmarkdown::run(
#         file = file,
#         shiny_args = list(
#           host = "127.0.0.1",
#           port = port,
#           launch.browser = FALSE
#         ),
#         render_args = list(
#           params = params,
#           knit_root_dir = root,
#           envir = new.env(parent = globalenv())
#         )
#       )
#     },
#     args = list(
#       file = normalizePath(
#         file,
#         winslash = "/",
#         mustWork = TRUE
#       ),
#       port = port,
#       params = params,
#       root = normalizePath(
#         getwd(),
#         winslash = "/",
#         mustWork = TRUE
#       )
#     ),
#     stdout = stdout_file,
#     stderr = stderr_file,
#     supervise = TRUE
#   )
#   
#   attr(proc, "app_name") <- app_name
#   attr(proc, "stdout_file") <- stdout_file
#   attr(proc, "stderr_file") <- stderr_file
#   
#   proc
# }

run_rmd_child <- function(
    file,
    port,
    params = list(),
    app_name
) {
  stdout_file <- file.path(
    DIR_LOGS,
    paste0(app_name, "_stdout.log")
  )
  
  stderr_file <- file.path(
    DIR_LOGS,
    paste0(app_name, "_stderr.log")
  )
  
  pandoc_path <- Sys.getenv("RSTUDIO_PANDOC")
  
  proc <- callr::r_bg(
    func = function(file, port, params, root, pandoc_path) {
      setwd(root)
      
      if (nzchar(pandoc_path)) {
        Sys.setenv(RSTUDIO_PANDOC = pandoc_path)
      }
      
      message("Dashboard figlia: ", basename(file))
      message("Working directory figlio: ", getwd())
      message("RSTUDIO_PANDOC figlio: ", Sys.getenv("RSTUDIO_PANDOC"))
      message("Pandoc disponibile figlio: ", rmarkdown::pandoc_available())
      
      if (!rmarkdown::pandoc_available()) {
        stop(
          paste(
            "Pandoc non trovato.",
            "Installare RStudio oppure Quarto."
          ),
          call. = FALSE
        )
      }
      
      message("Pandoc versione figlio: ", as.character(rmarkdown::pandoc_version()))
      
      rmarkdown::run(
        file = file,
        shiny_args = list(
          host = "127.0.0.1",
          port = port,
          launch.browser = FALSE
        ),
        render_args = list(
          params = params,
          knit_root_dir = root,
          envir = new.env(parent = globalenv())
        )
      )
    },
    args = list(
      file = normalizePath(
        file,
        winslash = "/",
        mustWork = TRUE
      ),
      port = port,
      params = params,
      root = normalizePath(
        getwd(),
        winslash = "/",
        mustWork = TRUE
      ),
      pandoc_path = pandoc_path
    ),
    stdout = stdout_file,
    stderr = stderr_file,
    supervise = TRUE
  )
  
  attr(proc, "app_name") <- app_name
  attr(proc, "stdout_file") <- stdout_file
  attr(proc, "stderr_file") <- stderr_file
  
  proc
}


status_run <- "failed"
children <- list()

tryCatch({
  master_pa <- download_drive_file(DRIVE_MASTER_PA_FILE)
  
  path_json_indicators <- download_exact(DRIVE_DIR_INDICATORS_PAGOPA, "INDICATORS_PAGOPA.json")
  path_fil_reg <- download_exact(DRIVE_DIR_CLASSIFICATION_MET, "fil_reg.rds")
  
  path_anac_indicators     <- download_drive_file(DRIVE_ANAC_INDICATORS,     "INDICATORS_ANAC.json")
  path_anac_indicators_cpv <- download_drive_file(DRIVE_ANAC_INDICATORS_CPV, "INDICATORS_CPV_ANAC.json")
  
  anac_params <- list(
    file_indicators_anac     = path_anac_indicators,
    file_indicators_cpv_anac = path_anac_indicators_cpv,
    file_lista_raccordo      = master_pa
  )
  
  my_indicators_params <- list(
    file_indicators = path_json_indicators,
    file_regioni = path_fil_reg
  )
  
  pad_params <- list(
    file_fact_dashboard = download_exact(DRIVE_PAD_INDICATORS, "FACT_PADIGITALE2026_DASHBOARD.json"),
    file_dim_enti = download_exact(DRIVE_PAD_INDICATORS, "DIM_ENTI_PADIGITALE2026.json"),
    file_dim_avvisi = download_exact(DRIVE_PAD_INDICATORS, "DIM_AVVISI_PADIGITALE2026.json"),
    file_metadata_indicatori = download_exact(DRIVE_PAD_METADATA, "MET_INDICATORS_PADIGITALE2026.json"),
    file_metadata_filtri = download_exact(DRIVE_PAD_METADATA, "MET_FILTERS_PADIGITALE2026.json"),
    run_id_indicatori = RUN_ID_PADIGITALE,
    anno_nuts = 2024,
    risoluzione_nuts = "10"
  )

  latest_indicators_run_ca <- find_latest_run_folder(DRIVE_CA_INDICATORS)
  run_id_indicatori_ca <- latest_indicators_run_ca$name[[1]]
  
  latest_catalog_run <- find_latest_run_folder(DRIVE_CA_DASHBOARD_CATALOG)
  run_id_catalogo_ca <- latest_catalog_run$name[[1]]
  
  message("Ultimo RUN indicatori CA: ", run_id_indicatori_ca)
  message("Ultimo RUN catalogo/dashboard CA: ", run_id_catalogo_ca)
  
  ca_files <- list(
    file_fact_ca_dashboard = download_file_from_run_integrata(latest_indicators_run_ca, "FACT_CA_DASHBOARD.rds"),
    file_dash_sections_ca = download_file_from_run_integrata(latest_catalog_run, "DASH_SECTIONS_CA.rds"),
    file_dash_filters_ca = download_file_from_run_integrata(latest_catalog_run, "DASH_FILTERS_CA.rds"),
    file_dash_indicators_ca = download_file_from_run_integrata(latest_catalog_run, "DASH_INDICATORS_CA.rds"),
    file_fact_ca_coverage = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_COVERAGE.rds"),
    file_fact_ca_regione = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_REGIONE.rds"),
    file_fact_ca_zona = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_ZONA.rds"),
    file_fact_ca_fg = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_FG.rds"),
    file_fact_ca_tipologia = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_TIPOLOGIA.rds"),
    file_fact_ca_trend = download_file_from_run_integrata(latest_catalog_run, "FACT_CA_TREND.rds")
  )
  
  ca_params <- c(
    ca_files,
    list(
      run_id_indicatori_ca = run_id_indicatori_ca,
      run_id_catalogo_ca = run_id_catalogo_ca
    )
  )

  children$conto_annuale <- run_rmd_child(file = FILE_CONTO_ANNUALE, port = PORT_CONTO_ANNUALE, params = ca_params, app_name = "conto_annuale")
  children$padigitale <- run_rmd_child(file = FILE_PADIGITALE, port = PORT_PADIGITALE, params = pad_params, app_name = "padigitale")
  children$indicators_pagopa <- run_rmd_child( file = FILE_INDICATORS_PAGOPA, port = PORT_INDICATORS_PAGOPA, params = my_indicators_params, app_name = "indicators_pagopa")
  children$anac <- run_rmd_child(file = FILE_ANAC, port = PORT_ANAC, params = anac_params, app_name = "anac")
  
  Sys.sleep(8)
  
  for (nm in names(children)) {
    proc <- children[[nm]]
    
    message(
      "Processo ", nm,
      " | vivo: ", proc$is_alive(),
      " | exit status: ",
      proc$get_exit_status() %||% "in esecuzione"
    )
    
    if (!proc$is_alive()) {
      stderr_file <- attr(proc, "stderr_file")
      stdout_file <- attr(proc, "stdout_file")
      
      stderr_text <- if (file.exists(stderr_file)) {
        paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
      } else {
        "<stderr non disponibile>"
      }
      
      stdout_text <- if (file.exists(stdout_file)) {
        paste(readLines(stdout_file, warn = FALSE), collapse = "\n")
      } else {
        "<stdout non disponibile>"
      }
      
      stop(
        "La dashboard figlia '", nm, "' si è chiusa.\n",
        "STDERR:\n", stderr_text, "\n\n",
        "STDOUT:\n", stdout_text,
        call. = FALSE
      )
    }
  }

  status_run <- "running"
  rmarkdown::run(
    file = normalizePath(FILE_HOME, winslash = "/", mustWork = TRUE),
    shiny_args = list(
      host = "127.0.0.1",
      port = PORT_HOME,
      launch.browser = TRUE
    ),
    render_args = list(
      params = list(
        file_master_pa = master_pa,
        
        url_conto_annuale = sprintf(
          "http://127.0.0.1:%d",
          PORT_CONTO_ANNUALE
        ),
        
        url_padigitale = sprintf(
          "http://127.0.0.1:%d",
          PORT_PADIGITALE
        ),
        url_indicators_pagopa = sprintf("http://127.0.0.1:%d", PORT_INDICATORS_PAGOPA),
        url_anac = sprintf("http://127.0.0.1:%d", PORT_ANAC),
        
        id_ente_col = "codice_fiscale",
        nome_ente_col = "ragione_sociale",
        regione_col = "regione_bdap",
        macro_gruppo_col = "desc_fg",
        
        flag_conto_annuale_col = NULL,
        flag_padigitale_col = NULL,
        flag_anac_col = NULL,
        
        anno_nuts = 2024,
        risoluzione_nuts = "10"
      ),
      
      knit_root_dir = getwd(),
      envir = new.env(parent = globalenv())
    )
  )
  status_run <- "completed"
}, error = function(e) {
  message("ERRORE dashboard integrata: ", conditionMessage(e))
  status_run <<- "failed"
  stop(e)
}, finally = {
  for (proc in children) {
    if (!is.null(proc) && proc$is_alive()) proc$kill()
  }
  stop_console_log(console_log, status = status_run)
})
