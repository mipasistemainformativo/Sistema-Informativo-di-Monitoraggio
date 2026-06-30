# ============================================================ #
# 00_drive_helpers.R
# Funzioni comuni per leggere/scrivere file su Google Drive
# ============================================================ #

library(dplyr)
library(stringr)
library(googledrive)

escape_regex <- function(x) {
  stringr::str_replace_all(
    x,
    "([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])",
    "\\\\\\1"
  )
}

drive_get_path <- function(path_rel, root_id = DRIVE_ROOT_ID) {
  
  parti <- stringr::str_split(path_rel, "/", simplify = FALSE)[[1]]
  parti <- parti[parti != ""]
  
  current <- googledrive::drive_get(googledrive::as_id(root_id))
  
  for (p in parti) {
    
    children <- googledrive::drive_ls(current)
    
    current <- children %>%
      dplyr::filter(name == p)
    
    if (nrow(current) == 0) {
      stop("Elemento non trovato su Drive: ", path_rel, " | manca: ", p)
    }
    
    if (nrow(current) > 1) {
      stop("Path ambiguo su Drive: ", path_rel, " | duplicato: ", p)
    }
  }
  
  current
}

drive_ensure_folder <- function(path_rel, root_id = DRIVE_ROOT_ID) {
  
  parti <- stringr::str_split(path_rel, "/", simplify = FALSE)[[1]]
  parti <- parti[parti != ""]
  
  current <- googledrive::drive_get(googledrive::as_id(root_id))
  
  for (p in parti) {
    
    children <- googledrive::drive_ls(current)
    found <- children %>% dplyr::filter(name == p)
    
    if (nrow(found) == 0) {
      current <- googledrive::drive_mkdir(name = p, path = current)
      message("Creata cartella Drive: ", p)
    } else if (nrow(found) == 1) {
      current <- found
    } else {
      stop("Cartella ambigua su Drive: ", p)
    }
  }
  
  current
}

# drive_upload_or_update <- function(local_path, drive_folder_rel, drive_name = basename(local_path)) {
#   
#   if (!file.exists(local_path)) {
#     stop("File locale non trovato: ", local_path)
#   }
#   
#   folder <- drive_ensure_folder(drive_folder_rel)
#   
#   existing <- googledrive::drive_find(
#     pattern = paste0("^", escape_regex(drive_name), "$"),
#     q = paste0("'", folder$id, "' in parents"),
#     n_max = 10
#   )
#   
#   if (nrow(existing) == 0) {
#     
#     googledrive::drive_upload(
#       media = local_path,
#       path = folder,
#       name = drive_name,
#       overwrite = FALSE
#     )
#     
#     message("Caricato su Drive: ", drive_folder_rel, "/", drive_name)
#     
#   } else if (nrow(existing) == 1) {
#     
#     googledrive::drive_update(
#       file = existing,
#       media = local_path
#     )
#     
#     message("Aggiornato su Drive: ", drive_folder_rel, "/", drive_name)
#     
#   } else {
#     stop("File duplicato su Drive: ", drive_folder_rel, "/", drive_name)
#   }
# }

drive_upload_or_update <- function(local_path, drive_folder_rel, drive_name = basename(local_path)) {
  
  if (!file.exists(local_path)) {
    stop("File locale non trovato: ", local_path)
  }
  
  if (
    missing(drive_folder_rel) ||
    is.null(drive_folder_rel) ||
    length(drive_folder_rel) == 0 ||
    is.na(drive_folder_rel) ||
    drive_folder_rel == "" ||
    !stringr::str_detect(drive_folder_rel, "^(01_Dataset|02_Metadata|03_Scripts|04_Output|05_Logs|06_Docs|07_Temp)/")
  ) {
    stop("ERRORE: drive_folder_rel non valido: ", drive_folder_rel)
  }
  
  folder <- drive_ensure_folder(drive_folder_rel)
  
  existing <- googledrive::drive_find(
    pattern = paste0("^", escape_regex(drive_name), "$"),
    q = paste0("'", folder$id, "' in parents"),
    n_max = 10
  )
  
  if (nrow(existing) == 0) {
    googledrive::drive_upload(
      media = local_path,
      path = folder,
      name = drive_name,
      overwrite = FALSE
    )
    message("Caricato su Drive: ", drive_folder_rel, "/", drive_name)
    
  } else if (nrow(existing) == 1) {
    googledrive::drive_update(
      file = existing,
      media = local_path
    )
    message("Aggiornato su Drive: ", drive_folder_rel, "/", drive_name)
    
  } else {
    stop("File duplicato su Drive: ", drive_folder_rel, "/", drive_name)
  }
}


drive_download_from_path <- function(drive_file_rel, local_path, overwrite = TRUE) {
  
  drive_file <- drive_get_path(drive_file_rel)
  
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  
  googledrive::drive_download(
    file = drive_file,
    path = local_path,
    overwrite = overwrite
  )
  
  message("Scaricato da Drive: ", drive_file_rel, " -> ", local_path)
  
  local_path
}