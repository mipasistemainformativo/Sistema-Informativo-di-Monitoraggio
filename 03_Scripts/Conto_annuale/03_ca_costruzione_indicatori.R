# ==============================================================================
# 03_ca_costruzione_indicatori.R
# Fonte: Conto Annuale
# Fase: costruzione indicatori CA completi + FACT dashboard
# ==============================================================================

rm(list = ls())

# 1) SOURCE -------------------------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")
source("03_Scripts/Conto_annuale/00_ca_config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(googledrive)
  library(tibble)
  library(purrr)
})

# 2) AUTENTICAZIONE DRIVE -----------------------------------------------------

message("[1/10] Avvio autenticazione Google Drive...")

if (exists("SIM_DRIVE_EMAIL")) {
  options(gargle_oauth_email = SIM_DRIVE_EMAIL)
  
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive",
    cache = TRUE
  )
} else {
  googledrive::drive_auth(
    scopes = "https://www.googleapis.com/auth/drive",
    cache = TRUE
  )
}

message("[1/10] Autenticazione Google Drive completata.")

# 3) PARAMETRI DEL RUN --------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID indicatori CA: ", RUN_ID)

script_name <- "03_ca_costruzione_indicatori.R"

console_log <- start_console_log(
  log_dir = DRIVE_CA_LOGS,
  run_id = RUN_ID,
  script_name = script_name
)

status_run <- "failed"

# 4) FUNZIONI -----------------------------------------------------------------

# Trova l'ultima cartella RUN_ID dentro una cartella Drive stabile.
# Esempio: 01_Dataset/Processed/Conto_annuale/20260626_101530
ca_latest_run_folder <- function(drive_path) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  
  runs <- googledrive::drive_ls(dir_drive) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        "^\\d{8}_\\d{6}$"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$name))
  
  if (nrow(runs) == 0) {
    stop("Nessuna cartella RUN_ID trovata in: ", drive_path)
  }
  
  runs[1, ]
}

# Legge un file RDS con nome stabile dall'ultima cartella RUN_ID disponibile.
read_latest_run_rds <- function(drive_path, filename) {
  run_folder <- ca_latest_run_folder(drive_path)
  
  file <- googledrive::drive_ls(run_folder) %>%
    dplyr::filter(.data$name == filename) %>%
    dplyr::slice(1)
  
  if (nrow(file) == 0) {
    stop(
      "File ", filename,
      " non trovato nell'ultima cartella RUN_ID: ",
      run_folder$name[1]
    )
  }
  
  local_file <- sim_drive_download_to_temp(
    file,
    local_name = filename,
    overwrite = TRUE
  )
  
  obj <- readRDS(local_file)
  unlink(local_file)
  
  message(
    "File letto da Drive: ",
    drive_path, "/", run_folder$name[1], "/", filename
  )
  
  obj
}

# Compatibilità con vecchio sistema di versionamento nel nome file.
# Da usare solo come fallback se non esistono cartelle RUN_ID.
drive_latest_file <- function(drive_path, pattern) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  
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

read_latest_rds_legacy <- function(drive_path, pattern) {
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
  
  message("File legacy letto da Drive: ", drive_path, "/", file$name[1])
  
  obj
}

add_missing_numeric <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) {
      df[[cc]] <- NA_real_
    }
  }
  df
}

ca_zona_geografica <- function(regione) {
  regione_norm <- regione %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_squish()
  
  dplyr::case_when(
    regione_norm %in% c(
      "PIEMONTE",
      "VALLE D'AOSTA",
      "VALLE D'AOSTA/VALLÉE D'AOSTE",
      "LOMBARDIA",
      "LIGURIA",
      "TRENTINO-ALTO ADIGE",
      "TRENTINO-ALTO ADIGE/SÜDTIROL",
      "VENETO",
      "FRIULI-VENEZIA GIULIA",
      "EMILIA-ROMAGNA"
    ) ~ "Nord",
    
    regione_norm %in% c(
      "TOSCANA",
      "UMBRIA",
      "MARCHE",
      "LAZIO"
    ) ~ "Centro",
    
    regione_norm %in% c(
      "ABRUZZO",
      "MOLISE",
      "CAMPANIA",
      "PUGLIA",
      "BASILICATA",
      "CALABRIA",
      "SICILIA",
      "SARDEGNA"
    ) ~ "Sud e Isole",
    
    TRUE ~ NA_character_
  )
}

ca_print_present_missing <- function(label, expected, available) {
  present <- intersect(expected, available)
  missing <- setdiff(expected, available)
  
  message("")
  message(strrep("-", 80))
  message(label)
  message(strrep("-", 80))
  message("Presenti: ", length(present), " / ", length(expected))
  
  if (length(missing) > 0) {
    message("Mancanti: ", paste(missing, collapse = ", "))
  } else {
    message("Nessuna variabile mancante.")
  }
  
  message(strrep("-", 80))
  message("")
  
  invisible(list(present = present, missing = missing))
}

ca_print_numeric_summary <- function(df, vars, label) {
  vars <- intersect(vars, names(df))
  
  message("")
  message(strrep("-", 80))
  message(label)
  message(strrep("-", 80))
  
  if (length(vars) == 0) {
    message("Nessuna variabile disponibile per il riepilogo.")
    message(strrep("-", 80))
    message("")
    return(invisible(NULL))
  }
  
  for (vv in vars) {
    message("Variabile: ", vv)
    print(summary(df[[vv]]))
    message("")
  }
  
  message(strrep("-", 80))
  message("")
  
  invisible(NULL)
}

# 5) LETTURA ULTIMO MASTER CA -------------------------------------------------

message("Ricerca ultima cartella RUN_ID del master CA...")

processed_dir <- sim_drive_ls_path(DRIVE_CA_PROCESSED, create = FALSE)

latest_master_run <- googledrive::drive_ls(processed_dir) %>%
  dplyr::filter(stringr::str_detect(.data$name, "^\\d{8}_\\d{6}$")) %>%
  dplyr::arrange(dplyr::desc(.data$name)) %>%
  dplyr::slice(1)

if (nrow(latest_master_run) == 0) {
  stop("Nessuna cartella RUN_ID trovata in: ", DRIVE_CA_PROCESSED)
}

message("Ultima cartella master CA trovata: ", latest_master_run$name[1])

file_master_ca <- googledrive::drive_ls(latest_master_run) %>%
  dplyr::filter(.data$name == "master_CA_multianno.rds")

if (nrow(file_master_ca) == 0) {
  stop(
    "File master_CA_multianno.rds non trovato nella cartella RUN_ID: ",
    latest_master_run$name[1]
  )
}

local_master_ca <- sim_drive_download_to_temp(
  file_master_ca[1, ],
  local_name = paste0("master_CA_multianno_", latest_master_run$name[1], ".rds"),
  overwrite = TRUE
)

master_ca <- readRDS(local_master_ca)

unlink(local_master_ca)

master_ca <- tibble::as_tibble(master_ca)

message(
  "Master CA caricato: ",
  DRIVE_CA_PROCESSED, "/", latest_master_run$name[1], "/master_CA_multianno.rds"
)

message(
  "Master CA: ",
  format(nrow(master_ca), big.mark = ".", decimal.mark = ","),
  " righe, ",
  ncol(master_ca),
  " colonne."
)

# 6) VARIABILI NUMERICHE DEL MASTER -------------------------------------------

message("[3/10] Identificazione automatica delle variabili numeriche del master...")

cols_numeriche_attese <- master_ca %>%
  dplyr::select(where(is.numeric)) %>%
  names()

cols_da_escludere_indicatori <- c(
  "codice_fiscale",
  "anno",
  "presente_mpa",
  "presente_MPA",
  "presente_s13",
  "presente_bdap",
  "fonte_conto_annuale",
  "IN_FONTE_CA",
  "n_istituzioni_ca",
  "bdap_record_storicizzato",
  "bdap_storicizzazione_ambigua"
)

cols_numeriche_attese <- setdiff(
  cols_numeriche_attese,
  cols_da_escludere_indicatori
)

message(
  "Variabili numeriche candidate come indicatori: ",
  length(cols_numeriche_attese)
)

message(
  paste(cols_numeriche_attese, collapse = ", ")
)

# 7) COSTRUZIONE INDICATORI PA-ANNO ------------------------------------------

message("[4/10] Costruzione indicatori CA wide a livello PA-anno...")

indicatori_ca_wide <- master_ca %>%
  dplyr::mutate(
    # -------------------------------------------------------------------------
    # Copertura fonte
    # -------------------------------------------------------------------------
    fonte_conto_annuale = dplyr::coalesce(
      as.numeric(fonte_conto_annuale),
      0
    ),
    
    IN_FONTE_CA = fonte_conto_annuale,
    
    # -------------------------------------------------------------------------
    # Stock personale
    # -------------------------------------------------------------------------
    PERSONALE_TOT = dplyr::coalesce(
      PERSONALE_TOT,
      PERSONALE_UOMINI + PERSONALE_DONNE
    ),
    
    TEMPO_PIENO_TOT = dplyr::coalesce(
      TEMPO_PIENO_TOT,
      TEMPO_PIENO_UOMINI + TEMPO_PIENO_DONNE
    ),
    
    TOT_PART_TIME = dplyr::coalesce(
      TOT_PART_TIME,
      PART_TIME_UOMINI + PART_TIME_DONNE
    ),
    
    QUOTA_DONNE_PERC = sim_safe_div(
      PERSONALE_DONNE,
      PERSONALE_TOT,
      100
    ),
    
    QUOTA_UOMINI_PERC = sim_safe_div(
      PERSONALE_UOMINI,
      PERSONALE_TOT,
      100
    ),
    
    PERC_PART_TIME = dplyr::coalesce(
      PERC_PART_TIME,
      sim_safe_div(TOT_PART_TIME, PERSONALE_TOT, 100)
    ),
    
    PERC_TEMPO_PIENO = sim_safe_div(
      TEMPO_PIENO_TOT,
      PERSONALE_TOT,
      100
    ),
    
    # -------------------------------------------------------------------------
    # Flussi occupazionali
    # -------------------------------------------------------------------------
    ASSUN_TOT = dplyr::coalesce(
      ASSUN_TOT,
      ASSUN_UOMINI + ASSUN_DONNE
    ),
    
    CESS_TOT = dplyr::coalesce(
      CESS_TOT,
      CESS_UOMINI + CESS_DONNE
    ),
    
    SALDO_ASSUN_CESS = ASSUN_TOT - CESS_TOT,
    
    TURNOVER_PERC = sim_safe_div(
      ASSUN_TOT + CESS_TOT,
      PERSONALE_TOT,
      100
    ),
    
    TASSO_CRESCITA_PERC = sim_safe_div(
      ASSUN_TOT - CESS_TOT,
      PERSONALE_TOT,
      100
    ),
    
    # -------------------------------------------------------------------------
    # Struttura per età e ricambio generazionale
    # -------------------------------------------------------------------------
    QUOTA_UNDER35_PERC = dplyr::coalesce(
      QUOTA_UNDER35_PERC,
      sim_safe_div(UNDER35, PERSONALE_TOT_ETA, 100)
    ),
    
    QUOTA_OVER55_PERC = dplyr::coalesce(
      QUOTA_OVER55_PERC,
      sim_safe_div(OVER55, PERSONALE_TOT_ETA, 100)
    ),
    
    QUOTA_OVER65_PERC = dplyr::coalesce(
      QUOTA_OVER65_PERC,
      sim_safe_div(OVER65, PERSONALE_TOT_ETA, 100)
    ),
    
    INDICE_RICAMBIO_GENERAZIONALE = dplyr::coalesce(
      INDICE_RICAMBIO_GENERAZIONALE,
      sim_safe_div(UNDER35, OVER55, 1)
    ),
    
    # -------------------------------------------------------------------------
    # Formazione
    # -------------------------------------------------------------------------
    PERS_FORM_TOT = dplyr::coalesce(
      PERS_FORM_TOT,
      PERS_FORM_UOMINI + PERS_FORM_DONNE
    ),
    
    PERC_PERSONALE_FORMATO = sim_safe_div(
      PERS_FORM_TOT,
      PERSONALE_TOT,
      100
    ),
    
    GIORNI_FORM_PER_DIPENDENTE = dplyr::coalesce(
      GIORNI_FORM_PER_DIPENDENTE,
      sim_safe_div(GIORNI_FORM_TOT, PERSONALE_TOT, 1)
    ),
    
    SPESA_FORMAZIONE_PER_DIPENDENTE = dplyr::coalesce(
      SPESA_FORMAZIONE_PER_DIPENDENTE,
      sim_safe_div(SPESA_FORMAZIONE_L020, PERSONALE_TOT, 1)
    ),
    
    INCIDENZA_SPESA_FORMAZIONE_PERC = dplyr::coalesce(
      INCIDENZA_SPESA_FORMAZIONE_PERC,
      sim_safe_div(SPESA_FORMAZIONE_L020, TOTALE_SPESA, 100)
    ),
    
    # -------------------------------------------------------------------------
    # Progressioni di carriera
    # -------------------------------------------------------------------------
    TASSO_PASSAGGI_QUALIFICA_PERC = sim_safe_div(
      PASSAGGI_QUALIFICA_TOT,
      PERSONALE_TOT,
      100
    ),
    
    # -------------------------------------------------------------------------
    # Variabili territoriali e descrittive per dashboard
    # -------------------------------------------------------------------------
    zona_geografica = ca_zona_geografica(regione_bdap),
    
    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno"
  )

message("Controllo indicatori_ca_wide:")
message(" - righe: ", nrow(indicatori_ca_wide))
message(" - colonne: ", ncol(indicatori_ca_wide))

ca_print_numeric_summary(
  indicatori_ca_wide,
  vars = c(
    "ETA_MEDIA_PA",
    "PERC_PERSONALE_FORMATO",
    "TASSO_PASSAGGI_QUALIFICA_PERC",
    "QUOTA_LAUREA_PERC",
    "QUOTA_LAVORO_AGILE_PERC",
    "GG_ASSENZA_PER_DIP",
    "QUOTA_PRECARI_PERC",
    "ANZIANITA_MEDIA_PA"
  ),
  label = "CONTROLLO INDICATORI CHIAVE DOPO MUTATE"
)

message("Distribuzione zona_geografica:")
print(table(indicatori_ca_wide$zona_geografica, useNA = "ifany"))

# 8) COLONNE FILTRO E INDICATORI ---------------------------------------------

message("[5/10] Preparazione colonne filtro e indicatori...")

filter_cols_attese <- c(
  "anno",
  "codice_fiscale",
  "codice_reg",
  "ragione_sociale",
  "fg",
  "desc_fg",
  "codice_unita_mpa",
  "codice_unita_s13",
  "id_ente_bdap",
  "codice_ente_ipa",
  "codice_ente_siope",
  "codice_istat_comune",
  "codice_comune",
  "comune",
  "codice_provincia",
  "sigla_provincia",
  "provincia",
  "codice_regione_bdap",
  "regione_bdap",
  "zona_geografica",
  "ateco_bdap",
  "descr_ateco_bdap",
  "codice_forma_giuridica_bdap",
  "descr_forma_giuridica_bdap",
  "codice_tipologia_siope_bdap",
  "descr_tipologia_siope_bdap",
  "codice_categoria_ipa_bdap",
  "descr_categoria_ipa_bdap",
  "codice_tipologia_ipa_bdap",
  "descr_tipologia_ipa_bdap",
  "codice_tipologia_mtur_bdap",
  "descr_tipologia_mtur_bdap",
  "codice_tipologia_dt_bdap",
  "descr_tipologia_dt_bdap",
  "codice_tipologia_istat_s13_bdap",
  "descr_tipologia_istat_s13_bdap",
  "codice_tipologia_dlgs_118_2011_bdap",
  "descr_tipologia_dlgs_118_2011_bdap",
  "desc_tipo_istituzione_ca",
  "desc_istituzione_ca",
  "presente_mpa",
  "presente_MPA",
  "presente_s13",
  "presente_bdap",
  "fonte_conto_annuale",
  "IN_FONTE_CA"
)

filter_cols <- intersect(filter_cols_attese, names(indicatori_ca_wide))

ca_print_present_missing(
  label = "CONTROLLO COLONNE FILTRO",
  expected = filter_cols_attese,
  available = names(indicatori_ca_wide)
)

# Indicatori completi: tutte le variabili numeriche disponibili, escluse le
# colonne filtro/identificative. Così se il master aggiunge nuove variabili CA,
# entrano automaticamente in INDICATORS_CA_LONG.
indicator_cols_complete <- indicatori_ca_wide %>%
  dplyr::select(where(is.numeric)) %>%
  names()

indicator_cols_complete <- setdiff(
  indicator_cols_complete,
  c(
    filter_cols,
    "anno",
    "presente_mpa",
    "presente_MPA",
    "presente_s13",
    "presente_bdap",
    "fonte_conto_annuale",
    "IN_FONTE_CA",
    "n_istituzioni_ca",
    "bdap_record_storicizzato",
    "bdap_storicizzazione_ambigua"
  )
)

message("Numero indicatori CA completi individuati: ", length(indicator_cols_complete))
message("Indicatori CA completi individuati:")
message(paste(indicator_cols_complete, collapse = ", "))

indicator_cols_dashboard_attesi <- c(
  "IN_FONTE_CA",
  
  "PERSONALE_TOT",
  "PERSONALE_UOMINI",
  "PERSONALE_DONNE",
  "QUOTA_UOMINI_PERC",
  "QUOTA_DONNE_PERC",
  
  "TEMPO_PIENO_TOT",
  "TOT_PART_TIME",
  "PERC_TEMPO_PIENO",
  "PERC_PART_TIME",
  
  "ASSUN_TOT",
  "CESS_TOT",
  "SALDO_ASSUN_CESS",
  "TURNOVER_PERC",
  "TASSO_CRESCITA_PERC",
  
  "ETA_MEDIA_PA",
  "UNDER35",
  "OVER55",
  "OVER65",
  "QUOTA_UNDER35_PERC",
  "QUOTA_OVER55_PERC",
  "QUOTA_OVER65_PERC",
  "INDICE_RICAMBIO_GENERAZIONALE",
  
  "PERS_FORM_TOT",
  "PERC_PERSONALE_FORMATO",
  "GIORNI_FORM_TOT",
  "GIORNI_FORM_PER_DIPENDENTE",
  "SPESA_FORMAZIONE_L020",
  "SPESA_FORMAZIONE_PER_DIPENDENTE",
  "INCIDENZA_SPESA_FORMAZIONE_PERC",
  
  "QUOTA_LAUREA_PERC",
  "QUOTA_DIPLOMA_PERC",
  "QUOTA_LAVORO_AGILE_PERC",
  "QUOTA_TELE_LAVORO_PERC",
  "QUOTA_MOD_FLESSIBILE_PERC",
  "GG_ASSENZA_PER_DIP",
  "GG_ASSENZA_MALATTIA_PER_DIP",
  "PASSAGGI_QUALIFICA_TOT",
  "TASSO_PASSAGGI_QUALIFICA_PERC",
  "ANZIANITA_MEDIA_PA",
  "QUOTA_ANZIANITA_BREVE_PERC",
  "QUOTA_ANZIANITA_LUNGA_PERC",
  "QUOTA_PRECARI_PERC"
)

indicator_cols_dashboard <- intersect(
  indicator_cols_dashboard_attesi,
  names(indicatori_ca_wide)
)

ca_print_present_missing(
  label = "CONTROLLO INDICATORI DASHBOARD ATTESI",
  expected = indicator_cols_dashboard_attesi,
  available = names(indicatori_ca_wide)
)

# 9) OUTPUT 1: INDICATORS_CA_LONG ----------------------------------------

message("[6/10] Costruzione INDICATORS_CA_LONG in formato long...")

INDICATORS_CA_LONG <- indicatori_ca_wide %>%
  dplyr::select(
    dplyr::any_of(c(
      filter_cols,
      indicator_cols_complete
    ))
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::any_of(indicator_cols_complete),
    names_to = "indicatore_id",
    values_to = "valore"
  ) %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno",
    run_id = RUN_ID
  )

# 10) OUTPUT 2: FACT_CA_DASHBOARD --------------------------------------------

message("[7/10] Costruzione FACT_CA_DASHBOARD in formato wide...")

FACT_CA_DASHBOARD <- indicatori_ca_wide %>%
  dplyr::select(
    dplyr::any_of(c(
      filter_cols,
      indicator_cols_dashboard
    ))
  ) %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno",
    run_id = RUN_ID
  )

# 11) CONTROLLI ---------------------------------------------------------------

message("[8/10] Controlli sugli output costruiti...")
message("Controllo output indicatori CA:")
message(" - INDICATORS_CA_LONG: ", nrow(INDICATORS_CA_LONG), " righe")
message(" - INDICATORS_CA_LONG: ", ncol(INDICATORS_CA_LONG), " colonne")
message(" - FACT_CA_DASHBOARD: ", nrow(FACT_CA_DASHBOARD), " righe")
message(" - FACT_CA_DASHBOARD: ", ncol(FACT_CA_DASHBOARD), " colonne")

message("Numero indicatori distinti in INDICATORS_CA_LONG: ")
print(dplyr::n_distinct(INDICATORS_CA_LONG$indicatore_id))

message("Prime variabili indicatore presenti in INDICATORS_CA_LONG:")
print(
  INDICATORS_CA_LONG %>%
    dplyr::distinct(indicatore_id) %>%
    dplyr::arrange(indicatore_id) %>%
    utils::head(30)
)

message("Controllo colonne chiave FACT_CA_DASHBOARD:")
ca_print_present_missing(
  label = "CONTROLLO COLONNE FACT DASHBOARD",
  expected = c(filter_cols, indicator_cols_dashboard),
  available = names(FACT_CA_DASHBOARD)
)

# 12) EXPORT SU CARTELLA RUN --------------------------------------------------

message("[9/10] Export locale e upload su Drive...")

tryCatch({
  
  # Cartella Drive del run:
  # 01_Dataset/Indicators/Conto_annuale/<RUN_ID>
  DRIVE_CA_INDICATORS_RUN <- file.path(
    DRIVE_CA_INDICATORS,
    RUN_ID
  )
  
  # Cartella Drive log del run:
  # 05_Logs/Conto_annuale/<RUN_ID>
  DRIVE_CA_LOGS_RUN <- file.path(
    DRIVE_CA_LOGS,
    RUN_ID
  )
  
  # Cartella locale temporanea del run:
  # 07_Temp/Conto_annuale/Indicators/<RUN_ID>
  DIR_CA_INDICATORS_LOCAL <- file.path(
    DIR_TEMP,
    "Conto_annuale",
    "Indicators",
    RUN_ID
  )
  
  dir.create(
    DIR_CA_INDICATORS_LOCAL,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Nomi file stabili dentro la cartella RUN_ID.
  # Non si salva JSON: INDICATORS_CA_LONG è long e può essere molto grande.
  filename_indicators_long_rds <- "INDICATORS_CA_LONG.rds"
  #filename_indicators_long_csv <- "INDICATORS_CA_LONG.csv"
  filename_fact_rds <- "FACT_CA_DASHBOARD.rds"
  filename_fact_csv <- "FACT_CA_DASHBOARD.csv"
  
  local_indicators_long_rds <- file.path(DIR_CA_INDICATORS_LOCAL, filename_indicators_long_rds)
  #local_indicators_long_csv <- file.path(DIR_CA_INDICATORS_LOCAL, filename_indicators_long_csv)
  local_fact_rds <- file.path(DIR_CA_INDICATORS_LOCAL, filename_fact_rds)
  local_fact_csv <- file.path(DIR_CA_INDICATORS_LOCAL, filename_fact_csv)
  
  message("Cartella locale indicatori CA: ", DIR_CA_INDICATORS_LOCAL)
  message("Cartella Drive indicatori CA: ", DRIVE_CA_INDICATORS_RUN)
  
  saveRDS(INDICATORS_CA_LONG, local_indicators_long_rds)
  #readr::write_csv(INDICATORS_CA_LONG, local_indicators_long_csv)
  saveRDS(FACT_CA_DASHBOARD, local_fact_rds)
  readr::write_csv(FACT_CA_DASHBOARD, local_fact_csv)
  
  message("File locali creati:")
  message(" - ", local_indicators_long_rds)
  #message(" - ", local_indicators_long_csv)
  message(" - ", local_fact_rds)
  message(" - ", local_fact_csv)
  
  drive_upload_or_update(
    local_path = local_indicators_long_rds,
    drive_folder_rel = DRIVE_CA_INDICATORS_RUN
  )
  
  # drive_upload_or_update(
  #   local_path = local_indicators_long_csv,
  #   drive_folder_rel = DRIVE_CA_INDICATORS_RUN
  # )
  
  drive_upload_or_update(
    local_path = local_fact_rds,
    drive_folder_rel = DRIVE_CA_INDICATORS_RUN
  )
  
  drive_upload_or_update(
    local_path = local_fact_csv,
    drive_folder_rel = DRIVE_CA_INDICATORS_RUN
  )
  
  message("File indicatori CA caricati su Drive:")
  message(" - ", DRIVE_CA_INDICATORS_RUN, "/", filename_indicators_long_rds)
  #message(" - ", DRIVE_CA_INDICATORS_RUN, "/", filename_indicators_long_csv)
  message(" - ", DRIVE_CA_INDICATORS_RUN, "/", filename_fact_rds)
  message(" - ", DRIVE_CA_INDICATORS_RUN, "/", filename_fact_csv)
  
  # Pulizia solo dei file creati da questo script/run.
  files_to_remove <- c(
    local_indicators_long_rds,
    #local_indicators_long_csv,
    local_fact_rds,
    local_fact_csv
  )
  
  files_to_remove <- files_to_remove[file.exists(files_to_remove)]
  
  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
    message("Pulizia file temporanei del run completata:")
    message(" - ", paste(files_to_remove, collapse = "\n - "))
  } else {
    message("Nessun file temporaneo del run da eliminare.")
  }
  
  status_run <- "completed"
  
}, error = function(e) {
  
  status_run <<- "failed"
  
  message(
    "ERRORE costruzione indicatori CA: ",
    conditionMessage(e)
  )
  
  stop(e)
  
}, finally = {
  
  message("[10/10] Chiusura console log e upload log su Drive...")
  
  console_log_path <- stop_console_log(
    console_log,
    status = status_run
  )
  
  message(
    "Log generato: ",
    basename(console_log_path),
    " | Percorso locale: ",
    console_log_path
  )
  
  DRIVE_CA_LOGS_RUN <- file.path(
    DRIVE_CA_LOGS,
    RUN_ID
  )
  
  drive_upload_or_update(
    local_path = console_log_path,
    drive_folder_rel = DRIVE_CA_LOGS_RUN
  )
  
  message(
    "Log caricato su Drive: ",
    DRIVE_CA_LOGS_RUN,
    "/",
    basename(console_log_path)
  )
})

message(
  "--- Costruzione indicatori CA terminata. RUN_ID: ",
  RUN_ID,
  " | status: ",
  status_run,
  " ---"
)
