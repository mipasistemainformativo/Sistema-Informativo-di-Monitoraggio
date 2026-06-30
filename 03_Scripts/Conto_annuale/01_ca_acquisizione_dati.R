# ============================================================
# 01_ca_acquisizione_dati.R
# Fonte: Conto Annuale
# Fase: acquisizione e archiviazione dati grezzi su Drive
# ============================================================
# Questa versione è Drive-centrica.
# I file dati sono salvati su Google Drive in:
#   01_Dataset/Source/Conto_annuale/<anno>/
# ============================================================

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")

if (exists("SIM_DRIVE_EMAIL")) {
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive"
  )
} else {
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
}

anni_ca <- c(2021, 2022, 2023)



# Cartella Drive di destinazione
sim_drive_mkdir_path(file.path(DRIVE_DIR_SOURCE, "Conto_annuale"))

# NOTA:
# Se nel tuo script precedente hai già la logica di download RGS/Conto Annuale,
# mantieni quella parte. Qui lo standard è che ogni anno finisca su Drive in:
# 01_Dataset/Source/Conto_annuale/<anno>/Anagrafiche
# 01_Dataset/Source/Conto_annuale/<anno>/Dati
#
# Questo script non riscarica nulla se la cartella anno contiene già file.

log_acquisizione <- purrr::map_dfr(anni_ca, function(anno) {
  anno_path <- file.path(DRIVE_DIR_SOURCE, "Conto_annuale", paste0("CA_", anno))
  anno_dir <- sim_drive_mkdir_path(anno_path)
  anag_dir <- sim_drive_mkdir_path(file.path(anno_path, "Anagrafiche"))
  dati_dir <- sim_drive_mkdir_path(file.path(anno_path, "Dati"))
  
  n_file_anag <- nrow(googledrive::drive_ls(anag_dir))
  n_file_dati <- nrow(googledrive::drive_ls(dati_dir))
  
  status <- dplyr::case_when(
    n_file_anag > 0 & n_file_dati > 0 ~ "gia_presente_su_drive",
    TRUE ~ "cartelle_create_ma_file_da_caricare_o_download_da_completare"
  )
  
  tibble::tibble(
    anno = anno,
    status = status,
    n_file_anag = n_file_anag,
    n_file_dati = n_file_dati,
    drive_path = anno_path,
    timestamp = as.character(Sys.time())
  )
})

sim_log_upload(log_acquisizione, fonte = "Conto_annuale", tipo_log = "acquisizione")

print(log_acquisizione)

message("--------------------------------------------------")
message("LOG ACQUISIZIONE CONTO ANNUALE")
message("--------------------------------------------------")

for(i in seq_len(nrow(log_acquisizione))) {
  
  message(
    "Anno: ", log_acquisizione$anno[i],
    " | Stato: ", log_acquisizione$status[i],
    " | File anagrafica: ", log_acquisizione$n_file_anag[i],
    " | File dati: ", log_acquisizione$n_file_dati[i]
  )
  
}

message("--------------------------------------------------")
message(
  "Totale anni verificati: ",
  nrow(log_acquisizione)
)

message(
  "Anni completi: ",
  sum(log_acquisizione$status == "gia_presente_su_drive", na.rm = TRUE)
)

message(
  "Anni da completare: ",
  sum(log_acquisizione$status != "gia_presente_su_drive", na.rm = TRUE)
)

message("--------------------------------------------------")
message(
  "Acquisizione verificata su Drive. ",
  "Se tutti gli anni risultano 'gia_presente_su_drive', ",
  "procedere con lo script 02."
)
message("--------------------------------------------------")
