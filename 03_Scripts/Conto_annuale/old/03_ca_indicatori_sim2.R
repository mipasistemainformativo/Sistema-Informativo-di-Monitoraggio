# ============================================================
# 03_ca_indicatori_sim.R
# Fonte: Conto Annuale
# Fase: calcolo indicatori SIM da master MPA arricchito CA
# ============================================================
# Il master contiene tutto il perimetro MPA. Le PA non coperte
# dal Conto Annuale rimangono nel dataset con valori NA.
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
  googledrive::drive_auth(
    scopes = "https://www.googleapis.com/auth/drive"
  )
}

processed_ca_dir <- sim_drive_ls_path(
  file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  create = FALSE
)

file_master_ca <- googledrive::drive_ls(processed_ca_dir) %>%
  dplyr::filter(
    stringr::str_detect(
      name,
      stringr::regex("^master_CA_MPA_multianno_.*\\.rds$", ignore_case = TRUE)
    )
  ) %>%
  dplyr::arrange(dplyr::desc(name)) %>%
  dplyr::slice(1)

if (nrow(file_master_ca) == 0) {
  stop("Nessun file master_CA_MPA_multianno_*.rds trovato in Processed/Conto_annuale.")
}

local_master_ca <- sim_drive_download_to_temp(
  file_master_ca,
  local_name = file_master_ca$name[1],
  overwrite = TRUE
)

master_ca <- readRDS(local_master_ca)
unlink(local_master_ca)

message("Master CA letto da Drive: ", file_master_ca$name[1])

# assicura presenza colonne attese
cols_attese <- c(
  "PERSONALE_UOMINI", "PERSONALE_DONNE", "PERSONALE_TOT",
  "ASSUN_UOMINI", "ASSUN_DONNE", "ASSUN_TOT",
  "CESS_UOMINI", "CESS_DONNE", "CESS_TOT",
  "GIORNI_FORM_TOT", "ETA_MEDIA_PA", "UNDER35", "OVER55", "OVER65",
  "QUOTA_UNDER35_PERC", "QUOTA_OVER55_PERC", "QUOTA_OVER65_PERC",
  "fonte_conto_annuale"
)

for (cc in cols_attese) {
  if (!cc %in% names(master_ca)) master_ca[[cc]] <- NA_real_
}

indicatori_ca <- master_ca %>%
  dplyr::mutate(
    PERSONALE_TOT = dplyr::coalesce(PERSONALE_TOT, PERSONALE_UOMINI + PERSONALE_DONNE),
    ASSUN_TOT = dplyr::coalesce(ASSUN_TOT, ASSUN_UOMINI + ASSUN_DONNE),
    CESS_TOT = dplyr::coalesce(CESS_TOT, CESS_UOMINI + CESS_DONNE),
    SALDO_TOT = ASSUN_TOT - CESS_TOT,
    TURNOVER_PCT = sim_safe_div(ASSUN_TOT + CESS_TOT, PERSONALE_TOT, 100),
    CRESCITA_PCT = sim_safe_div(ASSUN_TOT - CESS_TOT, PERSONALE_TOT, 100),
    PERC_DONNE = sim_safe_div(PERSONALE_DONNE, PERSONALE_TOT, 100),
    ETA_MEDIA_TOT = ETA_MEDIA_PA,
    UNDER35_PCT = QUOTA_UNDER35_PERC,
    OVER55_PCT = QUOTA_OVER55_PERC,
    OVER65_PCT = QUOTA_OVER65_PERC,
    FORM_MEDIA_TOT = sim_safe_div(GIORNI_FORM_TOT, PERSONALE_TOT, 1)
  )

indicatori_long <- indicatori_ca %>%
  dplyr::select(
    dplyr::any_of(c(
      "anno", "codice_fiscale", "ragione_sociale", "denominazione",
      "codice_unita_s13", "codice_unita_mpa", "codice_regione", "dizione_regione",
      "descr_tipologia_istat_s13", "presente_mpa", "fonte_conto_annuale",
      "PERSONALE_TOT", "ASSUN_TOT", "CESS_TOT", "SALDO_TOT",
      "TURNOVER_PCT", "CRESCITA_PCT", "PERC_DONNE", "ETA_MEDIA_TOT",
      "UNDER35_PCT", "OVER55_PCT", "OVER65_PCT",
      "GIORNI_FORM_TOT", "FORM_MEDIA_TOT"
    ))
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::any_of(c(
      "PERSONALE_TOT", "ASSUN_TOT", "CESS_TOT", "SALDO_TOT",
      "TURNOVER_PCT", "CRESCITA_PCT", "PERC_DONNE", "ETA_MEDIA_TOT",
      "UNDER35_PCT", "OVER55_PCT", "OVER65_PCT",
      "GIORNI_FORM_TOT", "FORM_MEDIA_TOT"
    )),
    names_to = "indicatore_id",
    values_to = "valore"
  ) %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno"
  )

sim_save_rds_upload(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = "indicatori_CA_PA_multianno.rds"
)

sim_write_csv_upload(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = "indicatori_CA_PA_multianno.csv"
)

sim_save_rds_upload(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = "indicatori_SIM_CA_long_multianno.rds"
)

sim_write_csv_upload(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = "indicatori_SIM_CA_long_multianno.csv"
)

overview <- indicatori_long %>%
  dplyr::group_by(anno, indicatore_id) %>%
  dplyr::summarise(
    n_pa_perimetro_mpa = dplyr::n_distinct(codice_fiscale),
    n_pa_con_valore = dplyr::n_distinct(codice_fiscale[!is.na(valore)]),
    quota_copertura_indicatore = n_pa_con_valore / n_pa_perimetro_mpa,
    valore_totale = sum(valore, na.rm = TRUE),
    valore_medio = mean(valore, na.rm = TRUE),
    .groups = "drop"
  )

# 8) OUTPUT SU DRIVE --------------------------------------------------------

# sim_write_csv_upload(
#   overview,
#   drive_path = file.path(DRIVE_DIR_OUTPUT, "Conto_annuale"),
#   filename = "sim_CA_overview_multianno.csv"
# )
# 
# sim_log_upload(
#   indicatori_long %>% dplyr::count(anno, indicatore_id, name = "n_righe"),
#   fonte = "Conto_annuale",
#   tipo_log = "indicatori"
# )
# 
# message("Indicatori CA caricati su Drive.")

timestamp_output <- format(Sys.time(), "%Y%m%d_%H%M%S")

filename_indicatori_rds <- paste0("indicatori_CA_PA_multianno_", timestamp_output, ".rds")
filename_indicatori_csv <- paste0("indicatori_CA_PA_multianno_", timestamp_output, ".csv")

filename_long_rds <- paste0("indicatori_SIM_CA_long_multianno_", timestamp_output, ".rds")
filename_long_csv <- paste0("indicatori_SIM_CA_long_multianno_", timestamp_output, ".csv")

filename_overview_csv <- paste0("sim_CA_overview_multianno_", timestamp_output, ".csv")

sim_save_rds_upload(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_indicatori_rds
)

sim_write_csv_upload(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_indicatori_csv
)

sim_save_rds_upload(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_long_rds
)

sim_write_csv_upload(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_long_csv
)

sim_write_csv_upload(
  overview,
  drive_path = file.path(DRIVE_DIR_OUTPUT, "Conto_annuale"),
  filename = filename_overview_csv
)

message("File Indicatori CA caricati su Drive:")
message(" - ", filename_indicatori_rds)
message(" - ", filename_indicatori_csv)
message(" - ", filename_long_rds)
message(" - ", filename_long_csv)
message(" - ", filename_overview_csv)

