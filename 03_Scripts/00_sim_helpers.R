# ============================================================
# 00_sim_helpers.R
# Helper comuni SIM - versione Drive-centrica
# ============================================================
# Assunzione architetturale:
# - GitHub locale: codice + 07_Temp
# - Google Drive: dati, metadata, output, logs, docs
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(writexl)
  library(googledrive)
  library(purrr)
  library(janitor)
  library(tidyr)
})

# 1) Testo e chiavi ---------------------------------------------------------

sim_normalizza_testo <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_squish()
}

sim_normalizza_cf <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[^A-Z0-9]", "") %>%
    na_if("")
}

sim_safe_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

sim_safe_div <- function(num, den, moltiplica = 1) {
  dplyr::if_else(!is.na(den) & den != 0, moltiplica * num / den, NA_real_)
}

# 2) Drive folders ---------------------------------------------------------

# Funzioni per trovare cartelle Drive

## Recupera la root Drive del progetto usando
sim_drive_root <- function() {
  googledrive::drive_get(googledrive::as_id(DRIVE_ROOT_ID))
}

# Serve per trovare una cartella Drive a partire da un path relativo,
# se non la trova la crea
# tipo: sim_drive_ls_path("01_Dataset/Processed/Conto_annuale")
# sim_drive_ls_path("01_Dataset/Processed/Conto_annuale")
sim_drive_ls_path <- function(path, create = FALSE) {
  # path relativo alla root Drive, ad esempio "01_Dataset/Source/Conto_annuale"
  root <- sim_drive_root()
  parts <- stringr::str_split(path, "/", simplify = FALSE)[[1]]
  current <- root
  for (p in parts) {
    if (is.na(p) || p == "") next
    items <- googledrive::drive_ls(current)
    hit <- items %>% dplyr::filter(.data$name == p)
    if (nrow(hit) == 0) {
      if (isTRUE(create)) {
        current <- googledrive::drive_mkdir(name = p, path = current)
      } else {
        stop("Cartella Drive non trovata: ", path, " (manca: ", p, ")")
      }
    } else {
      current <- hit[1, ]
    }
  }
  current
}

# È solo una scorciatoia per:
# sim_drive_ls_path(path, create = TRUE)
sim_drive_mkdir_path <- function(path) {
  sim_drive_ls_path(path, create = TRUE)
}

# sim_drive_upload_replace <- function(local_file,
sim_drive_upload <- function(local_file,
                            drive_dir,
                            name = basename(local_file)) {
  
  googledrive::drive_upload(
    media = local_file,
    path = drive_dir,
    name = name
  )
  
}

# Funzioni per scaricare da Drive
# Prende un file Drive già trovato con drive_ls() e lo scarica in: DIR_TEMP
sim_drive_download_to_temp <- function(file_tbl, local_name = NULL, overwrite = TRUE) {
  if (nrow(file_tbl) == 0) stop("File Drive non trovato.")
  if (is.null(local_name)) local_name <- file_tbl$name[1]
  local_path <- file.path(DIR_TEMP, local_name)
  googledrive::drive_download(file_tbl[1, ], path = local_path, overwrite = overwrite)
  local_path
}

# 3) Lettura robusta --------------------------------------------------------

# Legge un file già locale. (.xlsx,.xls, .csv ,.txt,.rds)
sim_read_any_table <- function(path) {
  ext <- tools::file_ext(path) %>% tolower()
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path) %>% janitor::clean_names()
  } else if (ext %in% c("csv", "txt")) {
    # prova separatore ;, poi ,
    out <- tryCatch(
      readr::read_delim(path, delim = ";", show_col_types = FALSE, locale = readr::locale(decimal_mark = ",")),
      error = function(e) NULL
    )
    if (is.null(out) || ncol(out) <= 1) {
      out <- readr::read_csv(path, show_col_types = FALSE)
    }
    out %>% janitor::clean_names()
  } else if (ext == "rds") {
    readRDS(path)
  } else {
    stop("Formato non gestito: ", ext, " file: ", path)
  }
}

# Funzioni per caricare su Drive

# salva df localmente in 07_Temp/filename;
# carica il CSV su Drive nella cartella drive_path;
# cancella il file locale temporaneo

sim_write_csv_upload <- function(df, drive_path, filename) {
  #dir_drive <- sim_drive_mkdir_path(drive_path)
  dir_drive <-sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  readr::write_csv(df, local_file)
  sim_drive_upload_replace(local_file, dir_drive, filename)
  unlink(local_file)
}

sim_write_xlsx_upload <- function(list_or_df, drive_path, filename) {
  #dir_drive <- sim_drive_mkdir_path(drive_path)
  dir_drive <-sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  writexl::write_xlsx(list_or_df, local_file)
  sim_drive_upload_replace(local_file, dir_drive, filename)
  unlink(local_file)
}

sim_save_rds_upload <- function(obj, drive_path, filename) {
  #dir_drive <- sim_drive_mkdir_path(drive_path)
  dir_drive <-sim_drive_ls_path(drive_path, create = TRUE)
  local_file <- file.path(DIR_TEMP, filename)
  saveRDS(obj, local_file)
  sim_drive_upload_replace(local_file, dir_drive, filename)
  unlink(local_file)
}


# Cerca un file con nome esatto dentro una cartella Drive, lo scarica in 07_Temp, lo legge con:
# readRDS() e poi cancella il temporaneo
sim_read_rds_from_drive <- function(drive_path, filename) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  file_tbl <- googledrive::drive_ls(dir_drive) %>% dplyr::filter(.data$name == filename)
  if (nrow(file_tbl) == 0) stop("File non trovato su Drive: ", file.path(drive_path, filename))
  local <- sim_drive_download_to_temp(file_tbl, local_name = filename, overwrite = TRUE)
  obj <- readRDS(local)
  unlink(local)
  obj
}

sim_read_csv_from_drive <- function(drive_path, filename) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  file_tbl <- googledrive::drive_ls(dir_drive) %>% dplyr::filter(.data$name == filename)
  if (nrow(file_tbl) == 0) stop("File non trovato su Drive: ", file.path(drive_path, filename))
  local <- sim_drive_download_to_temp(file_tbl, local_name = filename, overwrite = TRUE)
  df <- readr::read_csv(local, show_col_types = FALSE)
  unlink(local)
  df
}

# 4) Log standard -----------------------------------------------------------

sim_log_upload <- function(df, fonte, tipo_log, anno = NULL) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  anno_txt <- ifelse(is.null(anno), "multianno", as.character(anno))
  filename <- paste0("log_", tipo_log, "_", fonte, "_", anno_txt, "_", ts, ".csv")
  sim_write_csv_upload(
    df,
    drive_path = file.path(DRIVE_DIR_LOGS, fonte),
    filename = filename
  )
  invisible(filename)
}

# 5) Lista MPA --------------------------------------------------------------

sim_leggi_lista_mpa <- function(pattern = "MPA") {
  # Cerca la lista MPA dentro DRIVE_DIR_LISTS, senza usare link hard-coded.
  lists_dir <- sim_drive_ls_path(DRIVE_DIR_LISTS, create = FALSE)
  files <- googledrive::drive_ls(lists_dir)
  file_mpa <- files %>%
    dplyr::filter(stringr::str_detect(.data$name, regex(pattern, ignore_case = TRUE))) %>%
    dplyr::arrange(.data$name)
  if (nrow(file_mpa) == 0) stop("Nessun file MPA trovato in ", DRIVE_DIR_LISTS)
  local <- sim_drive_download_to_temp(file_mpa[1, ], local_name = "lista_MPA.xlsx", overwrite = TRUE)
  lista <- readxl::read_excel(local) %>% janitor::clean_names()
  unlink(local)
  lista %>%
    mutate(
      codice_fiscale = sim_normalizza_cf(dplyr::coalesce(
        !!!rlang::syms(intersect(c("codice_fiscale", "cf", "cod_fiscale"), names(.)))
      ))
    )
}

