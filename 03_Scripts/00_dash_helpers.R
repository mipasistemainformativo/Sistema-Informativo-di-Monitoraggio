# ============================================================================

# Lettura file versionati da Google Drive

# ============================================================================

# Lettura ultimo file versionato ------------------------------------------

drive_latest_file <- function(drive_path, pattern) {
  
  dir_drive <- drive_ls_path(
    drive_path,
    create = FALSE
  )
  
  files <- googledrive::drive_ls(dir_drive) %>%
    dplyr::filter(
      stringr::str_detect(
        name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(name)
    )
  
  if (nrow(files) == 0) {
    
    ```
    stop(
      "Nessun file trovato in ",
      drive_path,
      " con pattern: ",
      pattern
    )
    ```
    
  }
  
  files[1, ]
  
}

# Lettura ultimo RDS versionato -------------------------------------------

read_latest_rds <- function(drive_path, pattern) {
  
  file <- drive_latest_file(
    drive_path = drive_path,
    pattern = pattern
  )
  
  local_file <- drive_download_to_temp(
    file,
    local_name = file$name[1],
    overwrite = TRUE
  )
  
  obj <- readRDS(local_file)
  
  unlink(local_file)
  
  message(
    "File letto da Drive: ",
    file$name[1]
  )
  
  obj
  
}

# Lettura ultimo CSV versionato -------------------------------------------

read_latest_csv <- function(drive_path, pattern) {
  
  file <- drive_latest_file(
    drive_path = drive_path,
    pattern = pattern
  )
  
  local_file <- drive_download_to_temp(
    file,
    local_name = file$name[1],
    overwrite = TRUE
  )
  
  obj <- readr::read_csv(
    local_file,
    show_col_types = FALSE
  )
  
  unlink(local_file)
  
  message(
    "File letto da Drive: ",
    file$name[1]
  )
  
  obj
  
}

# Lettura ultimo JSON versionato ------------------------------------------

read_latest_json <- function(drive_path, pattern) {
  
  file <- drive_latest_file(
    drive_path = drive_path,
    pattern = pattern
  )
  
  local_file <- drive_download_to_temp(
    file,
    local_name = file$name[1],
    overwrite = TRUE
  )
  
  obj <- jsonlite::fromJSON(
    local_file,
    simplifyDataFrame = TRUE
  )
  
  unlink(local_file)
  
  message(
    "File letto da Drive: ",
    file$name[1]
  )
  
  obj
  
}

# Elenco file presenti su Drive -------------------------------------------

drive_list_files <- function(
    drive_path,
    pattern = NULL
) {
  
  dir_drive <- drive_ls_path(
    drive_path,
    create = FALSE
  )
  
  files <- googledrive::drive_ls(dir_drive)
  
  if (!is.null(pattern)) {
    
    ```
    files <- files %>%
      dplyr::filter(
        stringr::str_detect(
          name,
          stringr::regex(pattern, ignore_case = TRUE)
        )
      )
    ```
    
  }
  
  files %>%
    dplyr::select(name)
  
}

# ============================================================================

# ESEMPI D'USO

# ============================================================================

# master_ca <- read_latest_rds(

# drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),

# pattern = "^master_CA_multianno_.*\.rds$"

# )

# indicatori_ca <- read_latest_rds(

# drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),

# pattern = "^indicatori_CA_PA_multianno_.*\.rds$"

# )

# indicatori_long <- read_latest_rds(

# drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),

# pattern = "^indicatori_SIM_CA_long_multianno_.*\.rds$"

# )

# overview <- read_latest_csv(

# drive_path = file.path(DRIVE_DIR_OUTPUT, "Conto_annuale"),

# pattern = "^sim_CA_overview_multianno_.*\.csv$"

# )

# drive_list_files(

# drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),

# pattern = "^master_CA"

# )
