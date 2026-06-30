# #==============================================================================#
# ####                   DA COPIARE A INIZIO SCRIPT                         ----
# #==============================================================================#
#
# 
# # 0) Pulizia ambiente ---------------------------------------------------------
# 
# rm(list = ls())
# 
# 
# # 1) Configurazione e helper --------------------------------------------------
# 
# source("03_Scripts/00_config.R")
# source("03_Scripts/00_drive_helpers.R")
# source("03_Scripts/helper_console_log.R")
# 
# 
# # 2) Pacchetti ---------------------------------------------------------------
# 
# library(TUTTE I PACCHETTI CHE TI SERVONO)
# 
# 
# # 3) Autenticazione Drive --------------------------------------------------------
# 
# googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
# 
# 
# # 4) Parametri del run --------------------------------------------------------
# 
# RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
# message("RUN_ID import: ", RUN_ID)
# 
# # parametro per pulire la cartella temp alla fine del run
# delete_local_temp <- FALSE
# 
# 
# # 5) Directory locali e Drive -------------------------------------------------
# 
# DIR_PAD26_SOURCE_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Source", RUN_ID)
# DIR_PAD26_PROCESSED_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID)
# DIR_PAD26_METADATA_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID)
# DIR_PAD26_LOGS_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID)
# 
# DRIVE_PAD26_SOURCE <- file.path(DRIVE_DIR_SOURCE, "PADigitale2026", RUN_ID)
# DRIVE_PAD26_PROCESSED <- file.path(DRIVE_DIR_PROCESSED, "PADigitale2026", RUN_ID)
# DRIVE_PAD26_METADATA <- file.path(DRIVE_DIR_METADATA, "Source_met", "PADigitale2026", RUN_ID)
# DRIVE_PAD26_LOGS <- file.path(DRIVE_DIR_LOGS, "PADigitale2026", RUN_ID)
# 
# # 6) Creazione directory locali ----------------------------------------------
# 
# dir.create(DIR_PAD26_SOURCE_LOCAL, recursive = TRUE, showWarnings = FALSE)
# dir.create(DIR_PAD26_PROCESSED_LOCAL, recursive = TRUE, showWarnings = FALSE)
# dir.create(DIR_PAD26_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)
# dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)
# 
# 
# # 7) Avvio console log --------------------------------------------------------
# 
# console_log <- start_console_log(
#   log_dir = DIR_PAD26_LOGS_LOCAL,
#   run_id = RUN_ID,
#   script_name = "01_import_PAdigitale2026"
# )
# 

#==============================================================================#
# ####                   DA COPIARE A FINE SCRIPT                         ----
#==============================================================================#

# # Chiude il file e ripristina la console.
# console_log_path <- stop_console_log(
#   console_log,
#   status = "completed"
# )
# 
# # Carica o aggiorna il log nella cartella 05_Logs su Drive.
# drive_upload_or_update(
#   local_path = console_log_path,
#   drive_folder_rel = DRIVE_DIR_LOGS
# )





#==============================================================================#
####                    CONSOLE LOGGING HELPERS                            ----
#==============================================================================#

# Individua il nome dello script in esecuzione.
# In RStudio usa il file aperto/source; da terminale usa l'argomento --file.
get_current_script_name <- function(default = "script") {
  
  script_path <- NULL
  
  # Caso 1: esecuzione in RStudio
  if (
    interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    script_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) NULL
    )
  }
  
  # Caso 2: Rscript nome_script.R
  if (is.null(script_path) || !nzchar(script_path)) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    
    if (length(file_arg) > 0L) {
      script_path <- sub("^--file=", "", file_arg[1])
    }
  }
  
  if (is.null(script_path) || !nzchar(script_path)) {
    return(default)
  }
  
  tools::file_path_sans_ext(
    basename(script_path)
  )
}


# Avvia il log della console.
start_console_log <- function(
    log_dir,
    run_id = NULL,
    script_name = NULL,
    append = FALSE
) {
  
  if (
    missing(log_dir) ||
    is.null(log_dir) ||
    !nzchar(as.character(log_dir))
  ) {
    stop("`log_dir` must be a valid directory path.")
  }
  
  dir.create(
    log_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Se run_id non è stato fornito, lo crea automaticamente.
  if (
    is.null(run_id) ||
    length(run_id) == 0L ||
    is.na(run_id[1]) ||
    !nzchar(as.character(run_id[1]))
  ) {
    run_id <- format(
      Sys.time(),
      "%Y-%m-%d_%H-%M-%S"
    )
  } else {
    run_id <- as.character(run_id[1])
  }
  
  # Se script_name non è fornito, prova a rilevarlo automaticamente.
  if (
    is.null(script_name) ||
    length(script_name) == 0L ||
    is.na(script_name[1]) ||
    !nzchar(as.character(script_name[1]))
  ) {
    script_name <- get_current_script_name(
      default = "script"
    )
  } else {
    script_name <- as.character(script_name[1])
  }
  
  # Pulisce i nomi per renderli validi nel filesystem.
  clean_filename_component <- function(x) {
    x <- gsub(
      "[^[:alnum:]_.-]+",
      "_",
      as.character(x)
    )
    
    gsub(
      "^_+|_+$",
      "",
      x
    )
  }
  
  script_name <- clean_filename_component(script_name)
  run_id <- clean_filename_component(run_id)
  
  log_path <- file.path(
    log_dir,
    paste0(
      script_name,
      ".",
      run_id,
      ".log"
    )
  )
  
  log_connection <- file(
    log_path,
    open = if (isTRUE(append)) "at" else "wt",
    encoding = "UTF-8"
  )
  
  initial_output_sinks <- sink.number(
    type = "output"
  )
  
  initial_message_sinks <- sink.number(
    type = "message"
  )
  
  # Output normale: scritto nel log e ancora visibile nella console.
  sink(
    log_connection,
    type = "output",
    split = TRUE
  )
  
  # Messaggi e warning: scritti nel log.
  sink(
    log_connection,
    type = "message"
  )
  
  start_time <- Sys.time()
  
  cat(
    "\n",
    paste(rep("=", 78), collapse = ""),
    "\n",
    "SCRIPT: ", script_name, "\n",
    "RUN ID: ", run_id, "\n",
    "START: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "WORKING DIRECTORY: ", getwd(), "\n",
    "LOG FILE: ", normalizePath(log_path, mustWork = FALSE), "\n",
    paste(rep("=", 78), collapse = ""),
    "\n\n",
    sep = ""
  )
  
  invisible(
    list(
      connection = log_connection,
      path = log_path,
      script_name = script_name,
      run_id = run_id,
      start_time = start_time,
      initial_output_sinks = initial_output_sinks,
      initial_message_sinks = initial_message_sinks
    )
  )
}



# Chiude il log e ripristina la console.
stop_console_log <- function(log_obj, status = "completed") {
  
  if (is.null(log_obj)) {
    return(invisible(NULL))
  }
  
  end_time <- Sys.time()
  
  cat(
    "\n",
    paste(rep("-", 78), collapse = ""),
    "\n",
    "STATUS: ", status, "\n",
    "END:    ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "ELAPSED: ",
    round(
      as.numeric(
        difftime(
          end_time,
          log_obj$start_time,
          units = "secs"
        )
      ),
      2
    ),
    " seconds\n",
    paste(rep("-", 78), collapse = ""),
    "\n",
    sep = ""
  )
  
  # Ripristina tutti i sink aperti da start_console_log().
  while (
    sink.number(type = "message") >
    log_obj$initial_message_sinks
  ) {
    sink(type = "message")
  }
  
  while (
    sink.number(type = "output") >
    log_obj$initial_output_sinks
  ) {
    sink(type = "output")
  }
  
  if (isOpen(log_obj$connection)) {
    close(log_obj$connection)
  }
  
  message(
    "Console log saved to: ",
    normalizePath(log_obj$path, mustWork = FALSE)
  )
  
  invisible(log_obj$path)
}
