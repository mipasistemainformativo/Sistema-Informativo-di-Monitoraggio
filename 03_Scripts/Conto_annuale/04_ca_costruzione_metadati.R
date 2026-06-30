# ==============================================================================
# 04_ca_costruzione_metadati.R
# Fonte: Conto Annuale
# Fase: costruzione metadati variabili, indicatori e filtri per SIM/dashboard
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

message("[1/12] Avvio autenticazione Google Drive...")

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

message("[1/12] Autenticazione Google Drive completata.")

# 3) PARAMETRI RUN ------------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID metadati CA: ", RUN_ID)

script_name <- "04_ca_costruzione_metadati.R"

console_log <- start_console_log(
  log_dir = DRIVE_CA_LOGS,
  run_id = RUN_ID,
  script_name = script_name
)

status_run <- "failed"

# 4) FUNZIONI -----------------------------------------------------------------

ca_latest_run_folder <- function(drive_path) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  
  runs <- googledrive::drive_ls(dir_drive) %>%
    dplyr::filter(stringr::str_detect(.data$name, "^\\d{8}_\\d{6}$")) %>%
    dplyr::arrange(dplyr::desc(.data$name))
  
  if (nrow(runs) == 0) {
    stop("Nessuna cartella RUN_ID trovata in: ", drive_path)
  }
  
  runs[1, ]
}

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
  
  attr(obj, "source_run_id") <- run_folder$name[1]
  obj
}

ca_safe_stat <- function(x) {
  if (is.null(x)) {
    return(tibble::tibble(
      n_missing = NA_integer_,
      pct_missing = NA_real_,
      n_valori_distinti = NA_integer_,
      esempi_valori = NA_character_,
      tipo_dato_dopo_import = NA_character_
    ))
  }
  
  esempi <- x %>%
    as.character() %>%
    stringr::str_squish() %>%
    unique()
  esempi <- esempi[!is.na(esempi) & esempi != ""]
  esempi <- utils::head(esempi, 5)
  
  tibble::tibble(
    n_missing = sum(is.na(x)),
    pct_missing = round(100 * mean(is.na(x)), 2),
    n_valori_distinti = dplyr::n_distinct(x, na.rm = TRUE),
    esempi_valori = paste(esempi, collapse = " | "),
    tipo_dato_dopo_import = paste(class(x), collapse = ";")
  )
}

ca_add_stats_from_df <- function(metadata_tbl, df, var_col = "nome_variabile_standardizzato") {
  metadata_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .stats = list({
        vv <- .data[[var_col]]
        if (!is.na(vv) && vv %in% names(df)) ca_safe_stat(df[[vv]]) else ca_safe_stat(NULL)
      })
    ) %>%
    tidyr::unnest_wider(.stats) %>%
    dplyr::ungroup()
}

ca_add_stats_from_long <- function(metadata_tbl, long_df, indicator_col = "Nome_variabile") {
  stats_ind <- long_df %>%
    dplyr::group_by(indicatore_id) %>%
    dplyr::summarise(
      n_missing = sum(is.na(valore)),
      pct_missing = round(100 * mean(is.na(valore)), 2),
      n_valori_distinti = dplyr::n_distinct(valore, na.rm = TRUE),
      esempi_valori = paste(
        utils::head(unique(as.character(valore[!is.na(valore)])), 5),
        collapse = " | "
      ),
      .groups = "drop"
    )
  
  metadata_tbl %>%
    dplyr::left_join(stats_ind, by = c("Nome_variabile" = "indicatore_id"))
}

write_csv_xlsx <- function(df, path_no_ext) {
  csv_path <- paste0(path_no_ext, ".csv")
  xlsx_path <- paste0(path_no_ext, ".xlsx")
  rds_path <- paste0(path_no_ext, ".rds")
  
  readr::write_csv(df, csv_path)
  saveRDS(df, rds_path)
  
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, xlsx_path)
  } else if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
  } else {
    warning("Pacchetti writexl/openxlsx non disponibili: file .xlsx non scritto per ", basename(path_no_ext))
    xlsx_path <- NA_character_
  }
  
  c(csv = csv_path, xlsx = xlsx_path, rds = rds_path)
}

print_check <- function(title, value) {
  message(strrep("-", 80))
  message(title)
  message(strrep("-", 80))
  print(value)
  message("")
}

# 5) LETTURA OUTPUT DA FILE 02 E 03 ------------------------------------------

message("[2/12] Lettura ultimo master CA e ultimi output indicatori...")

# Nome stabile prodotto dal file 02: master_CA_multianno.rds.
# Tengo comunque un fallback maiuscolo per compatibilità con eventuali versioni precedenti.
master_ca <- tryCatch(
  read_latest_run_rds(
    drive_path = DRIVE_CA_PROCESSED,
    filename = "master_CA_multianno.rds"
  ),
  error = function(e) {
    message("Fallback lettura master CA: ", conditionMessage(e))
    read_latest_run_rds(
      drive_path = DRIVE_CA_PROCESSED,
      filename = "MASTER_CA_MULTI_ANNO.rds"
    )
  }
)

INDICATORS_CA_LONG <- read_latest_run_rds(
  drive_path = DRIVE_CA_INDICATORS,
  filename = "INDICATORS_CA_LONG.rds"
)

FACT_CA_DASHBOARD <- read_latest_run_rds(
  drive_path = DRIVE_CA_INDICATORS,
  filename = "FACT_CA_DASHBOARD.rds"
)

source_run_master <- attr(master_ca, "source_run_id")
source_run_indicatori <- attr(INDICATORS_CA_LONG, "source_run_id")

message("Run master usato: ", source_run_master)
message("Run indicatori usato: ", source_run_indicatori)
message("Righe master CA: ", nrow(master_ca), " | colonne: ", ncol(master_ca))
message("Righe INDICATORS_CA_LONG: ", nrow(INDICATORS_CA_LONG), " | colonne: ", ncol(INDICATORS_CA_LONG))
message("Righe FACT_CA_DASHBOARD: ", nrow(FACT_CA_DASHBOARD), " | colonne: ", ncol(FACT_CA_DASHBOARD))

anni_metadata <- sort(unique(FACT_CA_DASHBOARD$anno))
anno_metadata <- paste(anni_metadata, collapse = ";")

# 6) DIZIONARIO VARIABILI ORIGINALI/STANDARDIZZATE CA ------------------------

message("[3/12] Costruzione dizionario variabili CA originali/importate...")

# Nota metodologica:
# MET_VARIABLES_CA documenta le variabili di fonte o di import/standardizzazione
# costruite nel file 02_ca_costruzione_master.R. Gli indicatori calcolati nel
# file 03 sono documentati in MET_INDICATORS_CA, non qui.

MET_VARIABLES_DICT_CA <- tibble::tribble(
  ~fonte, ~dataset_id, ~nome_variabile_originale, ~nome_variabile_standardizzato, ~tipo_dato_originale, ~unita_di_misura, ~descrizione, ~note,
  "Conto Annuale", "ANAGRAFICA_ISTITUZIONI", "CODI_FISCALE", "codice_fiscale", "character", "codice", "Codice fiscale dell'amministrazione, normalizzato a 11 caratteri.", "Padding a sinistra e rimozione caratteri non alfanumerici nel file 02.",
  "Conto Annuale", "ANAGRAFICA_ISTITUZIONI", "CODI_TIPO_ISTITUZIONE + CODI_ISTITUZIONE", "istituzione", "character", "codice", "Chiave istituzione del Conto Annuale.", "Concatenazione dei codici CA usata per raccordo con anagrafica.",
  "Conto Annuale", "ANAGRAFICA_ISTITUZIONI", "DESC_TIPO_ISTITUZIONE", "desc_tipo_istituzione_ca", "character", NA_character_, "Descrizione della tipologia di istituzione nel Conto Annuale.", NA_character_,
  "Conto Annuale", "ANAGRAFICA_ISTITUZIONI", "DESC_ISTITUZIONE", "desc_istituzione_ca", "character", NA_character_, "Denominazione dell'istituzione nel Conto Annuale.", NA_character_,
  
  "Conto Annuale", "OCCUPAZIONE", "personale_tempo_pieno_uomini", "TEMPO_PIENO_UOMINI", "numeric", "unità", "Personale maschile a tempo pieno.", "Variabile di fonte aggregata per anno e istituzione.",
  "Conto Annuale", "OCCUPAZIONE", "personale_tempo_pieno_donne", "TEMPO_PIENO_DONNE", "numeric", "unità", "Personale femminile a tempo pieno.", "Variabile di fonte aggregata per anno e istituzione.",
  "Conto Annuale", "OCCUPAZIONE", "part_time_inf50_percent_uomini", "PART_TIME_INF50_UOMINI", "numeric", "unità", "Personale maschile part-time inferiore al 50%.", "Variabile di fonte aggregata per anno e istituzione.",
  "Conto Annuale", "OCCUPAZIONE", "part_time_inf50_percent_donne", "PART_TIME_INF50_DONNE", "numeric", "unità", "Personale femminile part-time inferiore al 50%.", "Variabile di fonte aggregata per anno e istituzione.",
  "Conto Annuale", "OCCUPAZIONE", "part_time_sup50_percent_uomini", "PART_TIME_SUP50_UOMINI", "numeric", "unità", "Personale maschile part-time superiore al 50%.", "Variabile di fonte aggregata per anno e istituzione.",
  "Conto Annuale", "OCCUPAZIONE", "part_time_sup50_percent_donne", "PART_TIME_SUP50_DONNE", "numeric", "unità", "Personale femminile part-time superiore al 50%.", "Variabile di fonte aggregata per anno e istituzione.",
  
  "Conto Annuale", "ASSUNTI", "uomini / assun_uomini", "ASSUN_UOMINI", "numeric", "unità", "Assunzioni di personale maschile nell'anno.", "Nome colonna individuato automaticamente tra candidati nel file 02.",
  "Conto Annuale", "ASSUNTI", "donne / assun_donne", "ASSUN_DONNE", "numeric", "unità", "Assunzioni di personale femminile nell'anno.", "Nome colonna individuato automaticamente tra candidati nel file 02.",
  "Conto Annuale", "ASSUNTI", "totale / assun_tot", "ASSUN_TOT", "numeric", "unità", "Assunzioni totali nell'anno.", "Se non presente come totale, viene ricostruita da uomini + donne.",
  "Conto Annuale", "CESSATI", "uomini / cess_uomini", "CESS_UOMINI", "numeric", "unità", "Cessazioni di personale maschile nell'anno.", "Nome colonna individuato automaticamente tra candidati nel file 02.",
  "Conto Annuale", "CESSATI", "donne / cess_donne", "CESS_DONNE", "numeric", "unità", "Cessazioni di personale femminile nell'anno.", "Nome colonna individuato automaticamente tra candidati nel file 02.",
  "Conto Annuale", "CESSATI", "totale / cess_tot", "CESS_TOT", "numeric", "unità", "Cessazioni totali nell'anno.", "Se non presente come totale, viene ricostruita da uomini + donne.",
  
  "Conto Annuale", "ETA_MEDIA", "fascia_eta", "fascia_eta", "character", "classe", "Fascia di età del personale.", "Usata per costruire under 35, over 55 e over 65.",
  "Conto Annuale", "ETA_MEDIA", "uomini", "uomini_eta", "numeric", "unità", "Personale maschile per fascia di età.", "Aggregato nel file 02 in variabili UNDER/OVER per sesso.",
  "Conto Annuale", "ETA_MEDIA", "donne", "donne_eta", "numeric", "unità", "Personale femminile per fascia di età.", "Aggregato nel file 02 in variabili UNDER/OVER per sesso.",
  "Conto Annuale", "ETA_MEDIA", "media_uomini", "media_uomini_eta", "numeric", "anni", "Età media maschile nella fascia.", "Usata per calcolare l'età media ponderata PA.",
  "Conto Annuale", "ETA_MEDIA", "media_donne", "media_donne_eta", "numeric", "anni", "Età media femminile nella fascia.", "Usata per calcolare l'età media ponderata PA.",
  
  "Conto Annuale", "FORMAZIONE", "form_uomini", "PERS_FORM_UOMINI", "numeric", "unità", "Personale maschile formato.", "Nel CA FORM_UOMINI indica persone formate, non giorni.",
  "Conto Annuale", "FORMAZIONE", "form_donne", "PERS_FORM_DONNE", "numeric", "unità", "Personale femminile formato.", "Nel CA FORM_DONNE indica persone formate, non giorni.",
  "Conto Annuale", "FORMAZIONE", "form_media_uomini", "FORM_MEDIA_UOMINI_CA", "numeric", "giorni medi", "Giorni medi di formazione per uomo formato.", "Usata per stimare giorni totali: persone formate x giorni medi.",
  "Conto Annuale", "FORMAZIONE", "form_media_donne", "FORM_MEDIA_DONNE_CA", "numeric", "giorni medi", "Giorni medi di formazione per donna formata.", "Usata per stimare giorni totali: persone formate x giorni medi.",
  
  "Conto Annuale", "COSTO_LAVORO", "voce_spesa", "VOCE_SPESA_STR", "character", "codice", "Codici delle voci di spesa presenti per l'ente.", "La voce L020 identifica la spesa per formazione.",
  "Conto Annuale", "COSTO_LAVORO", "totale_spesa", "TOTALE_SPESA", "numeric", "euro", "Spesa complessiva rilevata nel dataset costo lavoro.", "Aggregata per anno e istituzione.",
  "Conto Annuale", "COSTO_LAVORO", "totale_spesa con voce L020", "SPESA_FORMAZIONE_L020", "numeric", "euro", "Spesa per formazione, identificata dalla voce L020.", "Somma della spesa con voce_spesa = L020.",
  
  "Conto Annuale", "TITOLI_STUDIO_DATI", "titolo_studio", "titolo_studio", "character", "classe", "Titolo di studio dichiarato.", "Riclassificato in laurea, diploma, media inferiore/nessun titolo.",
  "Conto Annuale", "TITOLI_STUDIO_DATI", "uomini", "uomini_titoli", "numeric", "unità", "Personale maschile per titolo di studio.", "Aggregato per costruire i totali per titolo.",
  "Conto Annuale", "TITOLI_STUDIO_DATI", "donne", "donne_titoli", "numeric", "unità", "Personale femminile per titolo di studio.", "Aggregato per costruire i totali per titolo.",
  
  "Conto Annuale", "MODALITA_LAVORO_FLESSIBILE", "lavoro_agile_uomini", "PERS_LAVORO_AGILE_UOMINI", "numeric", "unità", "Personale maschile in lavoro agile.", "Nome colonna individuato tra candidati nel file 02.",
  "Conto Annuale", "MODALITA_LAVORO_FLESSIBILE", "lavoro_agile_donne", "PERS_LAVORO_AGILE_DONNE", "numeric", "unità", "Personale femminile in lavoro agile.", "Nome colonna individuato tra candidati nel file 02.",
  "Conto Annuale", "MODALITA_LAVORO_FLESSIBILE", "tele_lavoro_uomini", "PERS_TELE_LAVORO_UOMINI", "numeric", "unità", "Personale maschile in telelavoro.", "Nome colonna individuato tra candidati nel file 02.",
  "Conto Annuale", "MODALITA_LAVORO_FLESSIBILE", "tele_lavoro_donne", "PERS_TELE_LAVORO_DONNE", "numeric", "unità", "Personale femminile in telelavoro.", "Nome colonna individuato tra candidati nel file 02.",
  
  "Conto Annuale", "ASSENZE", "assenze_uomini", "ASSENZE_UOMINI", "numeric", "giorni", "Giorni di assenza del personale maschile.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "ASSENZE", "assenze_donne", "ASSENZE_DONNE", "numeric", "giorni", "Giorni di assenza del personale femminile.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "ASSENZE", "causale_assenza", "causale_assenza", "character", "codice", "Causale dell'assenza.", "Le causali 01-03 sono trattate come assenze per malattia/salute.",
  
  "Conto Annuale", "PASSAGGI_QUALIFICA", "numero_passaggi", "PASSAGGI_QUALIFICA_TOT", "numeric", "unità", "Numero di passaggi di qualifica.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "PASSAGGI_QUALIFICA", "tipo_passaggio", "tipo_passaggio", "character", "classe", "Tipologia del passaggio di qualifica.", "Usata per distinguere concorso/selezione e progressione interna.",
  
  "Conto Annuale", "ANZIANITA", "fascia_anzianita", "fascia_anzianita", "character", "classe", "Fascia di anzianità di servizio.", "Usata per anzianità media e quote breve/lunga.",
  "Conto Annuale", "ANZIANITA", "uomini", "uomini_anzianita", "numeric", "unità", "Personale maschile per fascia di anzianità.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "ANZIANITA", "donne", "donne_anzianita", "numeric", "unità", "Personale femminile per fascia di anzianità.", "Aggregato per anno e istituzione.",
  
  "Conto Annuale", "LAVORO_FLESSIBILE", "personale_tempo_determinato_uomini", "PERS_TD_UOMINI", "numeric", "unità", "Personale maschile a tempo determinato.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "LAVORO_FLESSIBILE", "personale_tempo_determinato_donne", "PERS_TD_DONNE", "numeric", "unità", "Personale femminile a tempo determinato.", "Aggregato per anno e istituzione.",
  "Conto Annuale", "LAVORO_FLESSIBILE", "formazione_lavoro_uomini/donne", "PERS_FL_TOT", "numeric", "unità", "Personale con contratto di formazione lavoro.", "Aggregato uomini + donne.",
  "Conto Annuale", "LAVORO_FLESSIBILE", "interinale_uomini/donne", "PERS_INTERINALE_TOT", "numeric", "unità", "Personale interinale.", "Aggregato uomini + donne.",
  "Conto Annuale", "LAVORO_FLESSIBILE", "lavoro_socialmente_utile_uomini/donne", "PERS_LSU_TOT", "numeric", "unità", "Personale in lavori socialmente utili.", "Aggregato uomini + donne."
)

MET_VARIABLES_CA <- ca_add_stats_from_df(
  metadata_tbl = MET_VARIABLES_DICT_CA,
  df = master_ca,
  var_col = "nome_variabile_standardizzato"
) %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    run_id = RUN_ID,
    source_run_master = source_run_master,
    tipo_dato_originale = dplyr::coalesce(tipo_dato_originale, tipo_dato_dopo_import),
    note = dplyr::coalesce(note, NA_character_)
  ) %>%
  dplyr::select(
    fonte,
    dataset_id,
    nome_variabile_originale,
    nome_variabile_standardizzato,
    tipo_dato_originale,
    tipo_dato_dopo_import,
    unita_di_misura,
    n_missing,
    pct_missing,
    n_valori_distinti,
    esempi_valori,
    descrizione,
    note,
    run_id,
    source_run_master
  )

message("Variabili CA documentate: ", nrow(MET_VARIABLES_CA))

# 7) DIZIONARIO INDICATORI ----------------------------------------------------

message("[4/12] Costruzione dizionario indicatori CA...")

indicatori_presenti <- sort(unique(INDICATORS_CA_LONG$indicatore_id))

metadata_indicatori_base <- tibble::tribble(
  ~indicatore, ~nome_indicatore_standard, ~tabella_input, ~formula, ~x1_standard, ~x2_standard, ~x3_standard, ~additivo, ~unita_misura, ~descrizione, ~denominatore, ~fenomeno_osservabile, ~note_standard,
  
  "PERSONALE_TOT", "Personale totale", "OCCUPAZIONE", "PERSONALE_UOMINI + PERSONALE_DONNE", "PERSONALE_UOMINI", "PERSONALE_DONNE", NA_character_, TRUE, "unità", "Totale del personale dell'amministrazione.", NA_character_, "Stock e composizione del personale", "Ricostruito da tempo pieno e part-time se il totale non è già disponibile.",
  "PERSONALE_UOMINI", "Personale uomini", "OCCUPAZIONE", "TEMPO_PIENO_UOMINI + PART_TIME_UOMINI", "TEMPO_PIENO_UOMINI", "PART_TIME_UOMINI", NA_character_, TRUE, "unità", "Totale del personale maschile.", NA_character_, "Stock e composizione del personale", NA_character_,
  "PERSONALE_DONNE", "Personale donne", "OCCUPAZIONE", "TEMPO_PIENO_DONNE + PART_TIME_DONNE", "TEMPO_PIENO_DONNE", "PART_TIME_DONNE", NA_character_, TRUE, "unità", "Totale del personale femminile.", NA_character_, "Stock e composizione del personale", NA_character_,
  "TEMPO_PIENO_UOMINI", "Tempo pieno uomini", "OCCUPAZIONE", "Valore diretto aggregato da fonte", "personale_tempo_pieno_uomini", NA_character_, NA_character_, TRUE, "unità", "Personale maschile a tempo pieno.", NA_character_, "Stock e composizione del personale", NA_character_,
  "TEMPO_PIENO_DONNE", "Tempo pieno donne", "OCCUPAZIONE", "Valore diretto aggregato da fonte", "personale_tempo_pieno_donne", NA_character_, NA_character_, TRUE, "unità", "Personale femminile a tempo pieno.", NA_character_, "Stock e composizione del personale", NA_character_,
  "TEMPO_PIENO_TOT", "Tempo pieno totale", "OCCUPAZIONE", "TEMPO_PIENO_UOMINI + TEMPO_PIENO_DONNE", "TEMPO_PIENO_UOMINI", "TEMPO_PIENO_DONNE", NA_character_, TRUE, "unità", "Totale del personale a tempo pieno.", NA_character_, "Stock e composizione del personale", NA_character_,
  "PART_TIME_UOMINI", "Part-time uomini", "OCCUPAZIONE", "PART_TIME_INF50_UOMINI + PART_TIME_SUP50_UOMINI", "PART_TIME_INF50_UOMINI", "PART_TIME_SUP50_UOMINI", NA_character_, TRUE, "unità", "Personale maschile part-time.", NA_character_, "Stock e composizione del personale", NA_character_,
  "PART_TIME_DONNE", "Part-time donne", "OCCUPAZIONE", "PART_TIME_INF50_DONNE + PART_TIME_SUP50_DONNE", "PART_TIME_INF50_DONNE", "PART_TIME_SUP50_DONNE", NA_character_, TRUE, "unità", "Personale femminile part-time.", NA_character_, "Stock e composizione del personale", NA_character_,
  "TOT_PART_TIME", "Part-time totale", "OCCUPAZIONE", "PART_TIME_UOMINI + PART_TIME_DONNE", "PART_TIME_UOMINI", "PART_TIME_DONNE", NA_character_, TRUE, "unità", "Totale del personale part-time.", NA_character_, "Stock e composizione del personale", NA_character_,
  "PERC_TEMPO_PIENO", "Quota tempo pieno", "OCCUPAZIONE", "100 * TEMPO_PIENO_TOT / PERSONALE_TOT", "TEMPO_PIENO_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale a tempo pieno sul personale totale.", "PERSONALE_TOT", "Stock e composizione del personale", NA_character_,
  "PERC_PART_TIME", "Quota part-time", "OCCUPAZIONE", "100 * TOT_PART_TIME / PERSONALE_TOT", "TOT_PART_TIME", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale part-time sul personale totale.", "PERSONALE_TOT", "Stock e composizione del personale", NA_character_,
  "QUOTA_UOMINI_PERC", "Quota uomini", "OCCUPAZIONE", "100 * PERSONALE_UOMINI / PERSONALE_TOT", "PERSONALE_UOMINI", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale maschile sul personale totale.", "PERSONALE_TOT", "Stock e composizione del personale", NA_character_,
  "QUOTA_DONNE_PERC", "Quota donne", "OCCUPAZIONE", "100 * PERSONALE_DONNE / PERSONALE_TOT", "PERSONALE_DONNE", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale femminile sul personale totale.", "PERSONALE_TOT", "Stock e composizione del personale", NA_character_,
  
  "ASSUN_UOMINI", "Assunzioni uomini", "ASSUNTI", "Valore diretto aggregato da fonte", "assun_uomini/uomini", NA_character_, NA_character_, TRUE, "unità", "Assunzioni maschili nell'anno.", NA_character_, "Flussi occupazionali", NA_character_,
  "ASSUN_DONNE", "Assunzioni donne", "ASSUNTI", "Valore diretto aggregato da fonte", "assun_donne/donne", NA_character_, NA_character_, TRUE, "unità", "Assunzioni femminili nell'anno.", NA_character_, "Flussi occupazionali", NA_character_,
  "ASSUN_TOT", "Assunzioni totali", "ASSUNTI", "ASSUN_UOMINI + ASSUN_DONNE", "ASSUN_UOMINI", "ASSUN_DONNE", NA_character_, TRUE, "unità", "Assunzioni totali nell'anno.", NA_character_, "Flussi occupazionali", "Se disponibile una colonna totale in fonte, viene usata; altrimenti uomini + donne.",
  "CESS_UOMINI", "Cessazioni uomini", "CESSATI", "Valore diretto aggregato da fonte", "cess_uomini/uomini", NA_character_, NA_character_, TRUE, "unità", "Cessazioni maschili nell'anno.", NA_character_, "Flussi occupazionali", NA_character_,
  "CESS_DONNE", "Cessazioni donne", "CESSATI", "Valore diretto aggregato da fonte", "cess_donne/donne", NA_character_, NA_character_, TRUE, "unità", "Cessazioni femminili nell'anno.", NA_character_, "Flussi occupazionali", NA_character_,
  "CESS_TOT", "Cessazioni totali", "CESSATI", "CESS_UOMINI + CESS_DONNE", "CESS_UOMINI", "CESS_DONNE", NA_character_, TRUE, "unità", "Cessazioni totali nell'anno.", NA_character_, "Flussi occupazionali", "Se disponibile una colonna totale in fonte, viene usata; altrimenti uomini + donne.",
  "SALDO_ASSUN_CESS", "Saldo assunzioni-cessazioni", "ASSUNTI; CESSATI", "ASSUN_TOT - CESS_TOT", "ASSUN_TOT", "CESS_TOT", NA_character_, TRUE, "unità", "Differenza tra assunzioni e cessazioni nell'anno.", NA_character_, "Flussi occupazionali", NA_character_,
  "TURNOVER_PERC", "Turnover", "ASSUNTI; CESSATI; OCCUPAZIONE", "100 * (ASSUN_TOT + CESS_TOT) / PERSONALE_TOT", "ASSUN_TOT", "CESS_TOT", "PERSONALE_TOT", FALSE, "%", "Rapporto percentuale tra flussi in entrata/uscita e personale totale.", "PERSONALE_TOT", "Flussi occupazionali", "Indicatore non additivo: aggregare ricalcolando numeratore e denominatore.",
  "TASSO_CRESCITA_PERC", "Tasso di crescita del personale", "ASSUNTI; CESSATI; OCCUPAZIONE", "100 * (ASSUN_TOT - CESS_TOT) / PERSONALE_TOT", "ASSUN_TOT", "CESS_TOT", "PERSONALE_TOT", FALSE, "%", "Saldo assunzioni-cessazioni rapportato al personale totale.", "PERSONALE_TOT", "Flussi occupazionali", "Indicatore non additivo: aggregare ricalcolando numeratore e denominatore.",
  
  "PERSONALE_TOT_ETA", "Personale totale nelle classi di età", "ETA_MEDIA", "uomini + donne per fascia di età", "uomini", "donne", NA_character_, TRUE, "unità", "Totale personale coperto dal dataset età media.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "ETA_MEDIA_PA", "Età media", "ETA_MEDIA", "media ponderata: sum(media_uomini*uomini + media_donne*donne) / PERSONALE_TOT_ETA", "media_uomini", "media_donne", "PERSONALE_TOT_ETA", FALSE, "anni", "Età media ponderata del personale dell'amministrazione.", "PERSONALE_TOT_ETA", "Età e ricambio generazionale", "Le medie CA sono già in anni; non vengono riscalate.",
  "UNDER35_UOMINI", "Under 35 uomini", "ETA_MEDIA", "sum(uomini nelle fasce E0, E20, E25, E30)", "uomini", "fascia_eta", NA_character_, TRUE, "unità", "Personale maschile con meno di 35 anni.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "UNDER35_DONNE", "Under 35 donne", "ETA_MEDIA", "sum(donne nelle fasce E0, E20, E25, E30)", "donne", "fascia_eta", NA_character_, TRUE, "unità", "Personale femminile con meno di 35 anni.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "UNDER35", "Under 35 totale", "ETA_MEDIA", "UNDER35_UOMINI + UNDER35_DONNE", "UNDER35_UOMINI", "UNDER35_DONNE", NA_character_, TRUE, "unità", "Personale con meno di 35 anni.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER55_UOMINI", "Over 55 uomini", "ETA_MEDIA", "sum(uomini nelle fasce E55, E60, E65, E68)", "uomini", "fascia_eta", NA_character_, TRUE, "unità", "Personale maschile di 55 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER55_DONNE", "Over 55 donne", "ETA_MEDIA", "sum(donne nelle fasce E55, E60, E65, E68)", "donne", "fascia_eta", NA_character_, TRUE, "unità", "Personale femminile di 55 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER55", "Over 55 totale", "ETA_MEDIA", "OVER55_UOMINI + OVER55_DONNE", "OVER55_UOMINI", "OVER55_DONNE", NA_character_, TRUE, "unità", "Personale di 55 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER65_UOMINI", "Over 65 uomini", "ETA_MEDIA", "sum(uomini nelle fasce E65, E68)", "uomini", "fascia_eta", NA_character_, TRUE, "unità", "Personale maschile di 65 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER65_DONNE", "Over 65 donne", "ETA_MEDIA", "sum(donne nelle fasce E65, E68)", "donne", "fascia_eta", NA_character_, TRUE, "unità", "Personale femminile di 65 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "OVER65", "Over 65 totale", "ETA_MEDIA", "OVER65_UOMINI + OVER65_DONNE", "OVER65_UOMINI", "OVER65_DONNE", NA_character_, TRUE, "unità", "Personale di 65 anni e oltre.", NA_character_, "Età e ricambio generazionale", NA_character_,
  "QUOTA_UNDER35_PERC", "Quota under 35", "ETA_MEDIA", "100 * UNDER35 / PERSONALE_TOT_ETA", "UNDER35", "PERSONALE_TOT_ETA", NA_character_, FALSE, "%", "Quota percentuale di personale under 35.", "PERSONALE_TOT_ETA", "Età e ricambio generazionale", "Nel file 03 può essere ricalcolata se mancante.",
  "QUOTA_UNDER35_UOMINI_PERC", "Quota uomini tra under 35", "ETA_MEDIA", "100 * UNDER35_UOMINI / UNDER35", "UNDER35_UOMINI", "UNDER35", NA_character_, FALSE, "%", "Quota maschile tra il personale under 35.", "UNDER35", "Età e ricambio generazionale", NA_character_,
  "QUOTA_UNDER35_DONNE_PERC", "Quota donne tra under 35", "ETA_MEDIA", "100 * UNDER35_DONNE / UNDER35", "UNDER35_DONNE", "UNDER35", NA_character_, FALSE, "%", "Quota femminile tra il personale under 35.", "UNDER35", "Età e ricambio generazionale", NA_character_,
  "QUOTA_OVER55_PERC", "Quota over 55", "ETA_MEDIA", "100 * OVER55 / PERSONALE_TOT_ETA", "OVER55", "PERSONALE_TOT_ETA", NA_character_, FALSE, "%", "Quota percentuale di personale over 55.", "PERSONALE_TOT_ETA", "Età e ricambio generazionale", NA_character_,
  "QUOTA_OVER65_PERC", "Quota over 65", "ETA_MEDIA", "100 * OVER65 / PERSONALE_TOT_ETA", "OVER65", "PERSONALE_TOT_ETA", NA_character_, FALSE, "%", "Quota percentuale di personale over 65.", "PERSONALE_TOT_ETA", "Età e ricambio generazionale", NA_character_,
  "INDICE_RICAMBIO_GENERAZIONALE", "Indice di ricambio generazionale", "ETA_MEDIA", "UNDER35 / OVER55", "UNDER35", "OVER55", NA_character_, FALSE, "rapporto", "Rapporto tra personale under 35 e personale over 55.", "OVER55", "Età e ricambio generazionale", "Indicatore non additivo.",
  
  "PERS_FORM_UOMINI", "Personale formato uomini", "FORMAZIONE", "Valore diretto aggregato da fonte", "form_uomini", NA_character_, NA_character_, TRUE, "unità", "Personale maschile formato.", NA_character_, "Formazione", "Nel CA FORM_UOMINI indica persone formate.",
  "PERS_FORM_DONNE", "Personale formato donne", "FORMAZIONE", "Valore diretto aggregato da fonte", "form_donne", NA_character_, NA_character_, TRUE, "unità", "Personale femminile formato.", NA_character_, "Formazione", "Nel CA FORM_DONNE indica persone formate.",
  "PERS_FORM_TOT", "Personale formato totale", "FORMAZIONE", "PERS_FORM_UOMINI + PERS_FORM_DONNE", "PERS_FORM_UOMINI", "PERS_FORM_DONNE", NA_character_, TRUE, "unità", "Totale del personale formato.", NA_character_, "Formazione", NA_character_,
  "FORM_MEDIA_UOMINI_CA", "Giorni medi formazione uomini", "FORMAZIONE", "GIORNI_FORM_UOMINI / PERS_FORM_UOMINI", "GIORNI_FORM_UOMINI", "PERS_FORM_UOMINI", NA_character_, FALSE, "giorni medi", "Giorni medi di formazione per uomo formato.", "PERS_FORM_UOMINI", "Formazione", "Calcolato come rapporto aggregato, non come media semplice delle righe.",
  "FORM_MEDIA_DONNE_CA", "Giorni medi formazione donne", "FORMAZIONE", "GIORNI_FORM_DONNE / PERS_FORM_DONNE", "GIORNI_FORM_DONNE", "PERS_FORM_DONNE", NA_character_, FALSE, "giorni medi", "Giorni medi di formazione per donna formata.", "PERS_FORM_DONNE", "Formazione", "Calcolato come rapporto aggregato, non come media semplice delle righe.",
  "GIORNI_FORM_UOMINI", "Giorni formazione uomini", "FORMAZIONE", "sum(form_uomini * form_media_uomini)", "form_uomini", "form_media_uomini", NA_character_, TRUE, "giorni", "Giorni totali di formazione fruiti dagli uomini.", NA_character_, "Formazione", NA_character_,
  "GIORNI_FORM_DONNE", "Giorni formazione donne", "FORMAZIONE", "sum(form_donne * form_media_donne)", "form_donne", "form_media_donne", NA_character_, TRUE, "giorni", "Giorni totali di formazione fruiti dalle donne.", NA_character_, "Formazione", NA_character_,
  "GIORNI_FORM_TOT", "Giorni formazione totali", "FORMAZIONE", "GIORNI_FORM_UOMINI + GIORNI_FORM_DONNE", "GIORNI_FORM_UOMINI", "GIORNI_FORM_DONNE", NA_character_, TRUE, "giorni", "Giorni totali di formazione.", NA_character_, "Formazione", NA_character_,
  "PERC_PERSONALE_FORMATO", "Quota personale formato", "FORMAZIONE; OCCUPAZIONE", "100 * PERS_FORM_TOT / PERSONALE_TOT", "PERS_FORM_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale formato sul personale totale.", "PERSONALE_TOT", "Formazione", "Può superare 100 se il conteggio fonte include partecipazioni/persone non perfettamente comparabili con lo stock.",
  "QUOTA_PERSONALE_FORMATO_PERC", "Quota personale formato", "FORMAZIONE; OCCUPAZIONE", "100 * PERS_FORM_TOT / PERSONALE_TOT", "PERS_FORM_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale formato sul personale totale.", "PERSONALE_TOT", "Formazione", "Alias/indicatore affine a PERC_PERSONALE_FORMATO.",
  "QUOTA_PERS_FORM_DONNE_PERC", "Quota donne tra personale formato", "FORMAZIONE", "100 * PERS_FORM_DONNE / PERS_FORM_TOT", "PERS_FORM_DONNE", "PERS_FORM_TOT", NA_character_, FALSE, "%", "Quota femminile tra il personale formato.", "PERS_FORM_TOT", "Formazione", NA_character_,
  "GIORNI_FORM_PER_DIPENDENTE", "Giorni formazione per dipendente", "FORMAZIONE; OCCUPAZIONE", "GIORNI_FORM_TOT / PERSONALE_TOT", "GIORNI_FORM_TOT", "PERSONALE_TOT", NA_character_, FALSE, "giorni per dipendente", "Giorni medi di formazione per dipendente.", "PERSONALE_TOT", "Formazione", "Indicatore non additivo.",
  "SPESA_FORMAZIONE_L020", "Spesa formazione", "COSTO_LAVORO", "sum(totale_spesa se voce_spesa == 'L020')", "totale_spesa", "voce_spesa", NA_character_, TRUE, "euro", "Spesa per formazione identificata dalla voce L020.", NA_character_, "Formazione", NA_character_,
  "TOTALE_SPESA", "Spesa totale", "COSTO_LAVORO", "sum(totale_spesa)", "totale_spesa", NA_character_, NA_character_, TRUE, "euro", "Spesa complessiva rilevata nel dataset costo lavoro.", NA_character_, "Costo del lavoro", NA_character_,
  "SPESA_FORMAZIONE_PER_DIPENDENTE", "Spesa formazione per dipendente", "COSTO_LAVORO; OCCUPAZIONE", "SPESA_FORMAZIONE_L020 / PERSONALE_TOT", "SPESA_FORMAZIONE_L020", "PERSONALE_TOT", NA_character_, FALSE, "euro per dipendente", "Spesa media per formazione per dipendente.", "PERSONALE_TOT", "Formazione", "Indicatore non additivo.",
  "INCIDENZA_SPESA_FORMAZIONE_PERC", "Incidenza spesa formazione", "COSTO_LAVORO", "100 * SPESA_FORMAZIONE_L020 / TOTALE_SPESA", "SPESA_FORMAZIONE_L020", "TOTALE_SPESA", NA_character_, FALSE, "%", "Incidenza della spesa per formazione sulla spesa complessiva.", "TOTALE_SPESA", "Formazione", "Indicatore non additivo.",
  
  "PERS_TOT_TITOLI", "Personale totale con titolo di studio", "TITOLI_STUDIO_DATI", "sum(uomini + donne per titolo di studio)", "uomini", "donne", NA_character_, TRUE, "unità", "Totale del personale coperto dal dataset titoli di studio.", NA_character_, "Titoli di studio", NA_character_,
  "LAUREA_TOT", "Personale con laurea", "TITOLI_STUDIO_DATI", "sum(personale con titolo riclassificato LAUREA)", "titolo_studio", "uomini + donne", NA_character_, TRUE, "unità", "Personale con laurea o titolo superiore.", NA_character_, "Titoli di studio", NA_character_,
  "DIPLOMA_TOT", "Personale con diploma", "TITOLI_STUDIO_DATI", "sum(personale con titolo riclassificato DIPLOMA)", "titolo_studio", "uomini + donne", NA_character_, TRUE, "unità", "Personale con diploma.", NA_character_, "Titoli di studio", NA_character_,
  "MEDIA_INF_TOT", "Personale con licenza media", "TITOLI_STUDIO_DATI", "sum(personale con titolo riclassificato MEDIA_INF)", "titolo_studio", "uomini + donne", NA_character_, TRUE, "unità", "Personale con licenza media inferiore.", NA_character_, "Titoli di studio", NA_character_,
  "NESSUNO_EL_TOT", "Personale senza titolo/elementare", "TITOLI_STUDIO_DATI", "sum(personale con titolo riclassificato NESSUNO_EL)", "titolo_studio", "uomini + donne", NA_character_, TRUE, "unità", "Personale senza titolo o con licenza elementare.", NA_character_, "Titoli di studio", NA_character_,
  "QUOTA_LAUREA_PERC", "Quota laurea", "TITOLI_STUDIO_DATI", "100 * LAUREA_TOT / PERS_TOT_TITOLI", "LAUREA_TOT", "PERS_TOT_TITOLI", NA_character_, FALSE, "%", "Quota percentuale di personale con laurea.", "PERS_TOT_TITOLI", "Titoli di studio", NA_character_,
  "QUOTA_DIPLOMA_PERC", "Quota diploma", "TITOLI_STUDIO_DATI", "100 * DIPLOMA_TOT / PERS_TOT_TITOLI", "DIPLOMA_TOT", "PERS_TOT_TITOLI", NA_character_, FALSE, "%", "Quota percentuale di personale con diploma.", "PERS_TOT_TITOLI", "Titoli di studio", NA_character_,
  
  "PERS_LAVORO_AGILE_TOT", "Personale in lavoro agile", "MODALITA_LAVORO_FLESSIBILE", "PERS_LAVORO_AGILE_UOMINI + PERS_LAVORO_AGILE_DONNE", "PERS_LAVORO_AGILE_UOMINI", "PERS_LAVORO_AGILE_DONNE", NA_character_, TRUE, "unità", "Personale in lavoro agile.", NA_character_, "Modalità flessibili di lavoro", NA_character_,
  "PERS_TELE_LAVORO_TOT", "Personale in telelavoro", "MODALITA_LAVORO_FLESSIBILE", "PERS_TELE_LAVORO_UOMINI + PERS_TELE_LAVORO_DONNE", "PERS_TELE_LAVORO_UOMINI", "PERS_TELE_LAVORO_DONNE", NA_character_, TRUE, "unità", "Personale in telelavoro.", NA_character_, "Modalità flessibili di lavoro", NA_character_,
  "PERS_COWORKING_TOT", "Personale in coworking", "MODALITA_LAVORO_FLESSIBILE", "PERS_COWORKING_UOMINI + PERS_COWORKING_DONNE", "PERS_COWORKING_UOMINI", "PERS_COWORKING_DONNE", NA_character_, TRUE, "unità", "Personale in coworking.", NA_character_, "Modalità flessibili di lavoro", NA_character_,
  "PERS_MOD_FLESSIBILE_TOT", "Personale in modalità flessibile", "MODALITA_LAVORO_FLESSIBILE", "PERS_LAVORO_AGILE_TOT + PERS_TELE_LAVORO_TOT + PERS_COWORKING_TOT", "PERS_LAVORO_AGILE_TOT", "PERS_TELE_LAVORO_TOT", "PERS_COWORKING_TOT", TRUE, "unità", "Personale in almeno una modalità flessibile censita dalla fonte.", NA_character_, "Modalità flessibili di lavoro", NA_character_,
  "QUOTA_LAVORO_AGILE_PERC", "Quota lavoro agile", "MODALITA_LAVORO_FLESSIBILE; OCCUPAZIONE", "100 * PERS_LAVORO_AGILE_TOT / PERSONALE_TOT", "PERS_LAVORO_AGILE_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale in lavoro agile.", "PERSONALE_TOT", "Modalità flessibili di lavoro", NA_character_,
  "QUOTA_TELE_LAVORO_PERC", "Quota telelavoro", "MODALITA_LAVORO_FLESSIBILE; OCCUPAZIONE", "100 * PERS_TELE_LAVORO_TOT / PERSONALE_TOT", "PERS_TELE_LAVORO_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale in telelavoro.", "PERSONALE_TOT", "Modalità flessibili di lavoro", NA_character_,
  "QUOTA_MOD_FLESSIBILE_PERC", "Quota modalità flessibili", "MODALITA_LAVORO_FLESSIBILE; OCCUPAZIONE", "100 * PERS_MOD_FLESSIBILE_TOT / PERSONALE_TOT", "PERS_MOD_FLESSIBILE_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale in modalità flessibile.", "PERSONALE_TOT", "Modalità flessibili di lavoro", NA_character_,
  
  "ASSENZE_TOT", "Assenze totali", "ASSENZE", "ASSENZE_UOMINI + ASSENZE_DONNE", "ASSENZE_UOMINI", "ASSENZE_DONNE", NA_character_, TRUE, "giorni", "Giorni totali di assenza.", NA_character_, "Assenze", NA_character_,
  "ASSENZE_MALATTIA_TOT", "Assenze per malattia", "ASSENZE", "sum(assenze per causali 01-03)", "assenze_uomini", "assenze_donne", "causale_assenza", TRUE, "giorni", "Giorni di assenza per malattia/salute.", NA_character_, "Assenze", "Causali considerate: 01, 02, 03 e descrizioni equivalenti.",
  "GG_ASSENZA_PER_DIP", "Giorni assenza per dipendente", "ASSENZE; OCCUPAZIONE", "ASSENZE_TOT / PERSONALE_TOT", "ASSENZE_TOT", "PERSONALE_TOT", NA_character_, FALSE, "giorni per dipendente", "Giorni di assenza medi per dipendente.", "PERSONALE_TOT", "Assenze", "Indicatore non additivo.",
  "GG_ASSENZA_MALATTIA_PER_DIP", "Giorni assenza malattia per dipendente", "ASSENZE; OCCUPAZIONE", "ASSENZE_MALATTIA_TOT / PERSONALE_TOT", "ASSENZE_MALATTIA_TOT", "PERSONALE_TOT", NA_character_, FALSE, "giorni per dipendente", "Giorni di assenza per malattia medi per dipendente.", "PERSONALE_TOT", "Assenze", "Indicatore non additivo.",
  
  "PASSAGGI_QUALIFICA_TOT", "Passaggi di qualifica", "PASSAGGI_QUALIFICA", "sum(numero_passaggi)", "numero_passaggi", NA_character_, NA_character_, TRUE, "unità", "Totale dei passaggi di qualifica.", NA_character_, "Progressioni di carriera", NA_character_,
  "PASSAGGI_CONCORSO_TOT", "Passaggi tramite concorso/selezione", "PASSAGGI_QUALIFICA", "sum(numero_passaggi se tipo contiene CONCORSO/SELEZIONE/ESAME)", "numero_passaggi", "tipo_passaggio", NA_character_, TRUE, "unità", "Passaggi riconducibili a concorso, selezione o esame.", NA_character_, "Progressioni di carriera", NA_character_,
  "PASSAGGI_PROGRESSIONE_TOT", "Passaggi tramite progressione", "PASSAGGI_QUALIFICA", "sum(numero_passaggi se tipo contiene PROGRESSIONE/AVANZAMENTO/INTERNO)", "numero_passaggi", "tipo_passaggio", NA_character_, TRUE, "unità", "Passaggi riconducibili a progressioni o avanzamenti interni.", NA_character_, "Progressioni di carriera", NA_character_,
  "TASSO_PASSAGGI_QUALIFICA_PERC", "Tasso passaggi di qualifica", "PASSAGGI_QUALIFICA; OCCUPAZIONE", "100 * PASSAGGI_QUALIFICA_TOT / PERSONALE_TOT", "PASSAGGI_QUALIFICA_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Passaggi di qualifica rapportati al personale totale.", "PERSONALE_TOT", "Progressioni di carriera", "Indicatore non additivo.",
  "TASSO_PROGRESSIONE_PERC", "Tasso progressioni", "PASSAGGI_QUALIFICA; OCCUPAZIONE", "100 * PASSAGGI_PROGRESSIONE_TOT / PERSONALE_TOT", "PASSAGGI_PROGRESSIONE_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Progressioni interne rapportate al personale totale.", "PERSONALE_TOT", "Progressioni di carriera", "Indicatore non additivo.",
  
  "PERS_TOT_ANZIANITA", "Personale totale con anzianità", "ANZIANITA", "sum(uomini + donne per fascia_anzianita)", "uomini", "donne", NA_character_, TRUE, "unità", "Totale personale coperto dal dataset anzianità.", NA_character_, "Anzianità di servizio", NA_character_,
  "ANZIANITA_MEDIA_PA", "Anzianità media", "ANZIANITA", "media ponderata dei midpoint delle fasce di anzianità", "fascia_anzianita", "uomini + donne", NA_character_, FALSE, "anni", "Anzianità media stimata del personale.", "PERS_TOT_ANZIANITA", "Anzianità di servizio", "Midpoint usati nel file 02: A00=2.5, A05=7.5, ..., A35=40.",
  "PERS_ANZIANITA_BREVE", "Personale con anzianità breve", "ANZIANITA", "sum(personale nelle fasce A00, A05)", "fascia_anzianita", "uomini + donne", NA_character_, TRUE, "unità", "Personale con anzianità inferiore a circa 10 anni.", NA_character_, "Anzianità di servizio", NA_character_,
  "PERS_ANZIANITA_LUNGA", "Personale con anzianità lunga", "ANZIANITA", "sum(personale nelle fasce A25, A30, A35)", "fascia_anzianita", "uomini + donne", NA_character_, TRUE, "unità", "Personale con anzianità superiore a circa 25 anni.", NA_character_, "Anzianità di servizio", NA_character_,
  "QUOTA_ANZIANITA_BREVE_PERC", "Quota anzianità breve", "ANZIANITA", "100 * PERS_ANZIANITA_BREVE / PERS_TOT_ANZIANITA", "PERS_ANZIANITA_BREVE", "PERS_TOT_ANZIANITA", NA_character_, FALSE, "%", "Quota di personale con anzianità breve.", "PERS_TOT_ANZIANITA", "Anzianità di servizio", NA_character_,
  "QUOTA_ANZIANITA_LUNGA_PERC", "Quota anzianità lunga", "ANZIANITA", "100 * PERS_ANZIANITA_LUNGA / PERS_TOT_ANZIANITA", "PERS_ANZIANITA_LUNGA", "PERS_TOT_ANZIANITA", NA_character_, FALSE, "%", "Quota di personale con anzianità lunga.", "PERS_TOT_ANZIANITA", "Anzianità di servizio", NA_character_,
  
  "PERS_TD_TOT", "Personale a tempo determinato", "LAVORO_FLESSIBILE", "PERS_TD_UOMINI + PERS_TD_DONNE", "PERS_TD_UOMINI", "PERS_TD_DONNE", NA_character_, TRUE, "unità", "Personale a tempo determinato.", NA_character_, "Lavoro flessibile e precariato", NA_character_,
  "PERS_FL_TOT", "Personale formazione lavoro", "LAVORO_FLESSIBILE", "formazione_lavoro_uomini + formazione_lavoro_donne", "formazione_lavoro_uomini", "formazione_lavoro_donne", NA_character_, TRUE, "unità", "Personale con contratto formazione lavoro.", NA_character_, "Lavoro flessibile e precariato", NA_character_,
  "PERS_INTERINALE_TOT", "Personale interinale", "LAVORO_FLESSIBILE", "interinale_uomini + interinale_donne", "interinale_uomini", "interinale_donne", NA_character_, TRUE, "unità", "Personale interinale.", NA_character_, "Lavoro flessibile e precariato", NA_character_,
  "PERS_LSU_TOT", "Personale LSU", "LAVORO_FLESSIBILE", "lavoro_socialmente_utile_uomini + lavoro_socialmente_utile_donne", "lavoro_socialmente_utile_uomini", "lavoro_socialmente_utile_donne", NA_character_, TRUE, "unità", "Personale in lavori socialmente utili.", NA_character_, "Lavoro flessibile e precariato", NA_character_,
  "PERS_PRECARIO_TOT", "Personale precario", "LAVORO_FLESSIBILE", "PERS_TD_TOT + PERS_FL_TOT + PERS_INTERINALE_TOT + PERS_LSU_TOT", "PERS_TD_TOT", "PERS_FL_TOT", "PERS_INTERINALE_TOT; PERS_LSU_TOT", TRUE, "unità", "Totale del personale precario/flessibile secondo le componenti disponibili.", NA_character_, "Lavoro flessibile e precariato", NA_character_,
  "QUOTA_TEMPO_DET_PERC", "Quota tempo determinato", "LAVORO_FLESSIBILE; OCCUPAZIONE", "100 * PERS_TD_TOT / PERSONALE_TOT", "PERS_TD_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale a tempo determinato.", "PERSONALE_TOT", "Lavoro flessibile e precariato", NA_character_,
  "QUOTA_PRECARI_PERC", "Quota precari", "LAVORO_FLESSIBILE; OCCUPAZIONE", "100 * PERS_PRECARIO_TOT / PERSONALE_TOT", "PERS_PRECARIO_TOT", "PERSONALE_TOT", NA_character_, FALSE, "%", "Quota percentuale di personale precario/flessibile sul personale totale.", "PERSONALE_TOT", "Lavoro flessibile e precariato", NA_character_,
  
  "bdap_record_storicizzato", "Record BDAP storicizzato", "Lista raccordo SIM/BDAP", "Valore informativo dal master di raccordo", "bdap_record_storicizzato", NA_character_, NA_character_, FALSE, "flag", "Flag/variabile tecnica relativa alla storicizzazione del record BDAP.", NA_character_, "Qualità raccordo enti", "Variabile tecnica da usare per controlli, non come indicatore sostantivo.",
  "bdap_storicizzazione_ambigua", "Storicizzazione BDAP ambigua", "Lista raccordo SIM/BDAP", "Valore informativo dal master di raccordo", "bdap_storicizzazione_ambigua", NA_character_, NA_character_, FALSE, "flag", "Flag/variabile tecnica che segnala possibili ambiguità nella storicizzazione BDAP.", NA_character_, "Qualità raccordo enti", "Variabile tecnica da usare per controlli, non come indicatore sostantivo.",
  "n_istituzioni_ca", "Numero istituzioni CA associate", "ANAGRAFICA_ISTITUZIONI", "count istituzioni CA per codice fiscale/anno", "istituzione", "codice_fiscale", NA_character_, TRUE, "unità", "Numero di istituzioni CA associate alla stessa amministrazione/codice fiscale.", NA_character_, "Qualità raccordo enti", "Variabile di controllo del raccordo istituzione-codice fiscale."
)

# Se il file 03 produce indicatori non ancora previsti dal dizionario, li tengo
# comunque in output con descrizione esplicita da completare, e genero warning.
indicatori_non_in_dizionario <- setdiff(indicatori_presenti, metadata_indicatori_base$indicatore)

metadata_indicatori_fallback <- tibble::tibble(
  indicatore = indicatori_non_in_dizionario,
  nome_indicatore_standard = indicatori_non_in_dizionario,
  tabella_input = NA_character_,
  formula = "Da completare: indicatore presente in INDICATORS_CA_LONG ma non nel dizionario indicatori CA.",
  x1_standard = NA_character_,
  x2_standard = NA_character_,
  x3_standard = NA_character_,
  additivo = NA,
  unita_misura = NA_character_,
  descrizione = "Indicatore presente nell'output del file 03; descrizione da completare nel dizionario metadati.",
  denominatore = NA_character_,
  fenomeno_osservabile = NA_character_,
  note_standard = "Generato automaticamente come fallback dal file 04."
)

metadata_indicatori <- dplyr::bind_rows(
  metadata_indicatori_base,
  metadata_indicatori_fallback
) %>%
  dplyr::filter(indicatore %in% indicatori_presenti) %>%
  dplyr::distinct(indicatore, .keep_all = TRUE)

MET_INDICATORS_CA <- metadata_indicatori %>%
  dplyr::transmute(
    fonte = "Conto Annuale",
    dataset_id = tabella_input,
    Nome_variabile = indicatore,
    Nome_indicatore = nome_indicatore_standard,
    Nome_filtro = NA_character_,
    Nome_sub_filtro = NA_character_,
    Formula = formula,
    X1 = x1_standard,
    X2 = x2_standard,
    X3 = x3_standard,
    Anno_di_riferimento = anno_metadata,
    additivo,
    unita_di_misura,
    descrizione,
    denominatore,
    fenomeno_osservabile,
    indicatore_derivato = !stringr::str_detect(Formula, "^Valore diretto aggregato da fonte$|^Valore informativo dal master"),
    fonte_origine = tabella_input,
    Note = note_standard,
    run_id = RUN_ID,
    source_run_indicatori = source_run_indicatori
  ) %>%
  ca_add_stats_from_long(INDICATORS_CA_LONG)

message("Indicatori CA documentati: ", nrow(MET_INDICATORS_CA))

# 8) METADATI FILTRI ----------------------------------------------------------

message("[5/12] Costruzione metadati filtri CA...")

# Preferisco questa struttura perché è coerente con PA Digitale e distingue:
# - filtri applicabili alla FACT dashboard;
# - filtri applicabili alla dimensione enti;
# - tipo controllo/interfaccia per la dashboard.
metadata_filtri <- tibble::tribble(
  ~filtro, ~label, ~tabella, ~colonna, ~tipo_controllo, ~multiselezione, ~applica_fact, ~applica_dim_enti, ~descrizione, ~note,
  "anno", "Anno", "FACT_CA_DASHBOARD", "anno", "selectize", TRUE, TRUE, FALSE, "Anno di riferimento del dato.", "Filtro temporale principale.",
  "codice_reg", "Codice regione", "FACT_CA_DASHBOARD", "codice_reg", "selectize", TRUE, TRUE, TRUE, "Codice regione dell'amministrazione.", NA_character_,
  "regione_bdap", "Regione", "FACT_CA_DASHBOARD", "regione_bdap", "selectize", TRUE, TRUE, TRUE, "Regione associata all'amministrazione secondo la base di raccordo.", NA_character_,
  "zona_geografica", "Area geografica", "FACT_CA_DASHBOARD", "zona_geografica", "selectize", TRUE, TRUE, TRUE, "Ripartizione geografica Nord/Centro/Sud e Isole costruita da regione_bdap.", "Variabile derivata nel file 03.",
  "codice_provincia", "Codice provincia", "FACT_CA_DASHBOARD", "codice_provincia", "selectize", TRUE, TRUE, TRUE, "Codice provincia dell'amministrazione.", NA_character_,
  "sigla_provincia", "Provincia", "FACT_CA_DASHBOARD", "sigla_provincia", "selectize", TRUE, TRUE, TRUE, "Sigla della provincia.", NA_character_,
  "provincia", "Nome provincia", "FACT_CA_DASHBOARD", "provincia", "selectize", TRUE, TRUE, TRUE, "Denominazione della provincia.", NA_character_,
  "comune", "Comune", "FACT_CA_DASHBOARD", "comune", "selectize", TRUE, TRUE, TRUE, "Comune associato all'amministrazione.", "Da usare con cautela per enti non comunali o sedi amministrative.",
  "fg", "Codice forma giuridica", "FACT_CA_DASHBOARD", "fg", "selectize", TRUE, TRUE, TRUE, "Codice forma giuridica.", NA_character_,
  "desc_fg", "Forma giuridica", "FACT_CA_DASHBOARD", "desc_fg", "selectize", TRUE, TRUE, TRUE, "Descrizione della forma giuridica.", NA_character_,
  "desc_tipo_istituzione_ca", "Tipo istituzione CA", "FACT_CA_DASHBOARD", "desc_tipo_istituzione_ca", "selectize", TRUE, TRUE, TRUE, "Tipologia di istituzione secondo l'anagrafica del Conto Annuale.", "Disponibile solo per amministrazioni raccordate alla fonte CA.",
  "desc_istituzione_ca", "Istituzione CA", "FACT_CA_DASHBOARD", "desc_istituzione_ca", "selectize", TRUE, TRUE, TRUE, "Denominazione dell'istituzione secondo il Conto Annuale.", "Filtro di dettaglio.",
  "descr_categoria_ipa_bdap", "Categoria IPA/BDAP", "FACT_CA_DASHBOARD", "descr_categoria_ipa_bdap", "selectize", TRUE, TRUE, TRUE, "Categoria IPA disponibile nella base di raccordo BDAP/IPA.", NA_character_,
  "descr_tipologia_ipa_bdap", "Tipologia IPA/BDAP", "FACT_CA_DASHBOARD", "descr_tipologia_ipa_bdap", "selectize", TRUE, TRUE, TRUE, "Tipologia IPA disponibile nella base di raccordo BDAP/IPA.", NA_character_,
  "descr_tipologia_siope_bdap", "Tipologia SIOPE/BDAP", "FACT_CA_DASHBOARD", "descr_tipologia_siope_bdap", "selectize", TRUE, TRUE, TRUE, "Tipologia SIOPE disponibile nella base di raccordo.", NA_character_,
  "descr_tipologia_istat_s13_bdap", "Tipologia ISTAT S13", "FACT_CA_DASHBOARD", "descr_tipologia_istat_s13_bdap", "selectize", TRUE, TRUE, TRUE, "Tipologia ISTAT S13 disponibile nella base di raccordo BDAP.", NA_character_,
  "presente_mpa", "Presente nel perimetro MPA", "FACT_CA_DASHBOARD", "presente_mpa", "checkbox", FALSE, TRUE, TRUE, "Flag di appartenenza al perimetro MPA usato come base del master.", "Nel file 03 la FACT ha una riga per ogni PA MPA e anno.",
  "presente_s13", "Presente in S13", "FACT_CA_DASHBOARD", "presente_s13", "checkbox", FALSE, TRUE, TRUE, "Flag di presenza nella lista S13.", NA_character_,
  "presente_bdap", "Presente in BDAP", "FACT_CA_DASHBOARD", "presente_bdap", "checkbox", FALSE, TRUE, TRUE, "Flag di presenza nella base BDAP.", NA_character_,
  "IN_FONTE_CA", "Presente in Conto Annuale", "FACT_CA_DASHBOARD", "IN_FONTE_CA", "checkbox", FALSE, TRUE, TRUE, "Flag che indica se l'amministrazione risulta presente nella fonte Conto Annuale.", "Serve per distinguere PA del perimetro MPA con o senza dati CA.",
  "fonte_conto_annuale", "Fonte Conto Annuale", "FACT_CA_DASHBOARD", "fonte_conto_annuale", "checkbox", FALSE, TRUE, TRUE, "Flag originario di presenza nel Conto Annuale prima dell'alias IN_FONTE_CA.", "Alias tecnico mantenuto per tracciabilità."
)

MET_FILTERS_CA <- metadata_filtri %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    .stats = list({
      cc <- colonna
      if (!is.na(cc) && cc %in% names(FACT_CA_DASHBOARD)) ca_safe_stat(FACT_CA_DASHBOARD[[cc]]) else ca_safe_stat(NULL)
    })
  ) %>%
  tidyr::unnest_wider(.stats) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    run_id = RUN_ID,
    source_run_indicatori = source_run_indicatori
  ) %>%
  dplyr::select(
    fonte,
    filtro,
    label,
    tabella,
    colonna,
    tipo_controllo,
    multiselezione,
    applica_fact,
    applica_dim_enti,
    descrizione,
    n_missing,
    pct_missing,
    n_valori_distinti,
    esempi_valori,
    note,
    run_id,
    source_run_indicatori
  )

message("Filtri CA documentati: ", nrow(MET_FILTERS_CA))

# 9) CONTROLLI METADATI -------------------------------------------------------

message("[6/12] Controlli di completezza metadati...")

indicatori_non_documentati <- setdiff(indicatori_presenti, MET_INDICATORS_CA$Nome_variabile)
indicatori_extra_documentati <- setdiff(MET_INDICATORS_CA$Nome_variabile, indicatori_presenti)
filtri_non_presenti_fact <- setdiff(MET_FILTERS_CA$colonna, names(FACT_CA_DASHBOARD))

print_check("Indicatori presenti in INDICATORS_CA_LONG", indicatori_presenti)
print_check("Indicatori non documentati", indicatori_non_documentati)
print_check("Indicatori documentati ma non prodotti dal file 03", indicatori_extra_documentati)
print_check("Filtri documentati ma non presenti nella FACT", filtri_non_presenti_fact)

variabili_originali_non_presenti_master <- MET_VARIABLES_CA %>%
  dplyr::filter(!nome_variabile_standardizzato %in% names(master_ca)) %>%
  dplyr::select(dataset_id, nome_variabile_originale, nome_variabile_standardizzato, note)

print_check("Variabili standardizzate del dizionario non presenti nel master", variabili_originali_non_presenti_master)

indicatori_fallback <- MET_INDICATORS_CA %>%
  dplyr::filter(stringr::str_detect(Formula, "Da completare")) %>%
  dplyr::pull(Nome_variabile)

if (length(indicatori_non_documentati) > 0) {
  warning("Indicatori CA non documentati: ", paste(indicatori_non_documentati, collapse = ", "))
}

if (length(indicatori_fallback) > 0) {
  warning(
    "Indicatori presenti ma documentati con fallback da completare: ",
    paste(indicatori_fallback, collapse = ", ")
  )
} else {
  message("Controllo dizionario indicatori CA superato: nessun fallback da completare.")
}

if (length(filtri_non_presenti_fact) > 0) {
  warning("Filtri documentati ma non presenti nella FACT: ", paste(filtri_non_presenti_fact, collapse = ", "))
} else {
  message("Controllo filtri CA superato: tutti i filtri documentati sono presenti nella FACT.")
}

# 10) REPORT DI SINTESI -------------------------------------------------------

message("[7/12] Costruzione report di sintesi metadati CA...")

REPORT_METADATA_CA <- tibble::tibble(
  fonte = "Conto Annuale",
  run_id = RUN_ID,
  source_run_master = source_run_master,
  source_run_indicatori = source_run_indicatori,
  anni_coperti = anno_metadata,
  n_righe_master = nrow(master_ca),
  n_colonne_master = ncol(master_ca),
  n_righe_indicators_long = nrow(INDICATORS_CA_LONG),
  n_indicatori = dplyr::n_distinct(INDICATORS_CA_LONG$indicatore_id),
  n_righe_fact_dashboard = nrow(FACT_CA_DASHBOARD),
  n_colonne_fact_dashboard = ncol(FACT_CA_DASHBOARD),
  n_met_variables = nrow(MET_VARIABLES_CA),
  n_met_indicators = nrow(MET_INDICATORS_CA),
  n_met_filters = nrow(MET_FILTERS_CA),
  n_indicatori_fallback = length(indicatori_fallback),
  note = "Metadati costruiti a partire dal master CA del file 02 e dagli output indicatori/FACT del file 03."
)

print(REPORT_METADATA_CA)

# 11) SALVATAGGIO LOCALE E DRIVE ---------------------------------------------

message("[8/12] Salvataggio metadati CA in locale...")

DIR_CA_METADATA_LOCAL <- file.path("07_Temp", "Conto_annuale", "Metadata", RUN_ID)
dir.create(DIR_CA_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)

# Cartella Drive richiesta: 02_Metadata/Conto_annuale/<RUN_ID>
DRIVE_CA_METADATA_RUN <- file.path(DRIVE_CA_INDICATORS_MET, RUN_ID)

paths_variables <- write_csv_xlsx(
  MET_VARIABLES_CA,
  file.path(DIR_CA_METADATA_LOCAL, "MET_VARIABLES_CA")
)

paths_indicators <- write_csv_xlsx(
  MET_INDICATORS_CA,
  file.path(DIR_CA_METADATA_LOCAL, "MET_INDICATORS_CA")
)

paths_filters <- write_csv_xlsx(
  MET_FILTERS_CA,
  file.path(DIR_CA_METADATA_LOCAL, "MET_FILTERS_CA")
)

paths_report <- write_csv_xlsx(
  REPORT_METADATA_CA,
  file.path(DIR_CA_METADATA_LOCAL, "REPORT_METADATA_CA")
)

message("Cartella locale metadati CA: ", DIR_CA_METADATA_LOCAL)
message("Cartella Drive metadati CA: ", DRIVE_CA_METADATA_RUN)

message("[9/12] Upload metadati CA su Drive...")

all_paths <- c(paths_variables, paths_indicators, paths_filters, paths_report)
all_paths <- all_paths[!is.na(all_paths)]

purrr::walk(
  all_paths,
  ~ drive_upload_or_update(
    local_path = .x,
    drive_folder_rel = DRIVE_CA_METADATA_RUN
  )
)

message("File metadati caricati su Drive:")
message(" - ", DRIVE_CA_METADATA_RUN, "/MET_VARIABLES_CA.csv/.xlsx/.rds")
message(" - ", DRIVE_CA_METADATA_RUN, "/MET_INDICATORS_CA.csv/.xlsx/.rds")
message(" - ", DRIVE_CA_METADATA_RUN, "/MET_FILTERS_CA.csv/.xlsx/.rds")
message(" - ", DRIVE_CA_METADATA_RUN, "/REPORT_METADATA_CA.csv/.xlsx/.rds")

# 12) LOG FINALE --------------------------------------------------------------

message("[10/12] Controlli finali disponibili in ambiente:")
message(" - MET_VARIABLES_CA: ", nrow(MET_VARIABLES_CA), " righe")
message(" - MET_INDICATORS_CA: ", nrow(MET_INDICATORS_CA), " righe")
message(" - MET_FILTERS_CA: ", nrow(MET_FILTERS_CA), " righe")
message(" - REPORT_METADATA_CA: ", nrow(REPORT_METADATA_CA), " righe")

message("[11/12] Esempi comandi di verifica post-run:")
message("dim(MET_VARIABLES_CA)")
message("dim(MET_INDICATORS_CA)")
message("dim(MET_FILTERS_CA)")
message("setdiff(unique(INDICATORS_CA_LONG$indicatore_id), MET_INDICATORS_CA$Nome_variabile)")
message("MET_INDICATORS_CA %>% filter(str_detect(Formula, 'Da completare'))")
message("list.files(file.path('07_Temp', 'Conto_annuale', 'Metadata', RUN_ID), full.names = TRUE)")

status_run <- "success"
message("[12/12] Costruzione metadati CA completata correttamente.")

# 13) CHIUSURA LOG ------------------------------------------------------------

end_time <- Sys.time()

message(
  "--- Costruzione metadati CA terminata. RUN_ID: ", RUN_ID,
  " | Stato: ", status_run,
  " | Ora fine: ", format(end_time, "%Y-%m-%d %H:%M:%S"),
  " ---"
)

if (exists("console_log")) {
  close_console_log(console_log)
}
