source("03_Scripts/00_sim_helpers.R")

# 1) PATH DIRECTORIES CONTO ANNUALE ------------------------------------------------------------

DRIVE_CA_SOURCE <- file.path(DRIVE_DIR_SOURCE, "Conto_annuale")

DRIVE_CA_PROCESSED <- file.path(DRIVE_DIR_PROCESSED, "Conto_annuale")

DRIVE_CA_INDICATORS <- file.path(DRIVE_DIR_INDICATORS, "Conto_annuale")

DRIVE_CA_OUTPUT <- file.path(DRIVE_DIR_OUTPUT, "Conto_annuale")

DRIVE_CA_LOGS <- file.path(DRIVE_DIR_LOGS, "Conto_annuale")

DRIVE_CA_DOCS <- file.path(DRIVE_DIR_DOCS, "Conto_annuale")

# DRIVE_CA_SOURCE_MET <- file.path(DIR_SOURCE_MET, "Conto_annuale")

DRIVE_CA_VARIABLES_MET <- file.path(DRIVE_DIR_METADATA, "Source_met", "Variables_met", "Conto_annuale")

DRIVE_CA_INDICATORS_MET <- file.path(DRIVE_DIR_METADATA, "Indicators_met", "Conto_annuale")

# 2) CREAZIONE CARTELLE DRIVE CONTO ANNUALE -------------------------------

cartelle_ca <- c(
  DRIVE_CA_SOURCE,
  DRIVE_CA_PROCESSED,
  DRIVE_CA_INDICATORS,
  DRIVE_CA_OUTPUT,
  DRIVE_CA_LOGS,
  DRIVE_CA_DOCS,
  DRIVE_CA_VARIABLES_MET,
  DRIVE_CA_INDICATORS_MET
)

purrr::walk(
  cartelle_ca,
  sim_drive_mkdir_path
)

message("Struttura Drive Conto Annuale implementata/verificata.")


# 3) FUNZIONI DRIVE ---------------------------------------------------------

ca_drive_files <- function(anno, sottocartella) {
  dir <- sim_drive_ls_path(
    file.path(DRIVE_DIR_SOURCE, "Conto_annuale", paste0("CA_", anno), sottocartella),
    create = FALSE
  )
  googledrive::drive_ls(dir)
}

ca_download_read <- function(file, local_name) {
  local <- sim_drive_download_to_temp(
    file,
    local_name = local_name,
    overwrite = TRUE
  )
  df <- sim_read_any_table(local) %>%
    janitor::clean_names()
  unlink(local)
  df
}

ca_find_file <- function(anno, sottocartella, pattern) {
  files <- ca_drive_files(anno, sottocartella)
  
  files %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(.data$name)
}

ca_read_dataset <- function(anno, pattern, dataset_nome) {
  files <- ca_find_file(anno, "Dati", pattern)
  
  if (nrow(files) == 0) {
    warning("Dataset ", dataset_nome, " non trovato per anno ", anno)
    return(tibble::tibble())
  }
  
  ext <- tools::file_ext(files$name[1])
  
  ca_download_read(
    files[1, ],
    local_name = paste0(dataset_nome, "_", anno, ".", ext)
  ) %>%
    dplyr::mutate(
      anno = anno,
      dataset_origine = dataset_nome,
      file_origine = files$name[1],
      .before = 1
    )
}

# Upload versionato: non sostituisce e non cestina file esistenti.
# Serve a evitare errori 403 quando l'account non può eliminare file già presenti.
save_rds_upload_versioned <- function(obj, drive_path, filename) {
  # dir_drive <- sim_drive_mkdir_path(drive_path)
  dir_drive <- sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  saveRDS(obj, local_file)
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename
  )
  unlink(local_file)
}

write_csv_upload_versioned <- function(obj, drive_path, filename) {
  #dir_drive <- sim_drive_mkdir_path(drive_path)
  dir_drive <- sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  readr::write_csv(obj, local_file)
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename
  )
  unlink(local_file)
}

write_json_upload_versioned <- function(obj, drive_path, filename) {
  dir_drive <- sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  
  jsonlite::write_json(
    obj,
    path = local_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename
  )
  
  unlink(local_file)
}

# download RDS

drive_latest_file <- function(drive_path, pattern) {
  
  dir_drive <- sim_drive_ls_path(
    drive_path,
    create = FALSE
  )
  
  file <- googledrive::drive_ls(dir_drive) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$name)) %>%
    dplyr::slice(1)
  
  if (nrow(file) == 0) {
    stop(
      "Nessun file trovato in ",
      drive_path,
      " con pattern: ",
      pattern
    )
  }
  
  file
}

read_latest_rds <- function(drive_path, pattern) {
  
  file <- drive_latest_file(
    drive_path = drive_path,
    pattern = pattern
  )
  
  local_file <- sim_drive_download_to_temp(
    file,
    local_name = file$name[1],
    overwrite = TRUE
  )
  
  obj <- readRDS(local_file)
  
  unlink(local_file)
  
  message("File letto da Drive: ", drive_path, "/", file$name[1])
  
  obj
}

