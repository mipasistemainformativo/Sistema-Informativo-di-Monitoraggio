# ==============================================================================
# 04_ca_catalogo_sim.R
# Fonte: Conto Annuale
# Fase: metadati documentali + catalogo dashboard + fact aggregati SIM
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
  library(writexl)
})

# 2) AUTENTICAZIONE DRIVE -----------------------------------------------------

message("[1/11] Avvio autenticazione Google Drive...")

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

message("[1/11] Autenticazione Google Drive completata.")

# 3) PARAMETRI RUN E LOG ------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID catalogo SIM CA: ", RUN_ID)

script_name <- "04_ca_catalogo_sim.R"

console_log <- start_console_log(
  log_dir = DRIVE_CA_LOGS,
  run_id = RUN_ID,
  script_name = script_name
)

status_run <- "failed"

# 4) FUNZIONI DI SUPPORTO -----------------------------------------------------

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

read_rds_from_run <- function(run_folder, filename) {
  file <- googledrive::drive_ls(run_folder) %>%
    dplyr::filter(.data$name == filename) %>%
    dplyr::slice(1)
  
  if (nrow(file) == 0) {
    stop("File non trovato nella cartella RUN_ID ", run_folder$name[1], ": ", filename)
  }
  
  local_file <- sim_drive_download_to_temp(
    file,
    local_name = paste0(run_folder$name[1], "_", filename),
    overwrite = TRUE
  )
  
  obj <- readRDS(local_file)
  unlink(local_file)
  
  message("File letto da Drive: ", run_folder$name[1], "/", filename)
  obj
}

safe_n_distinct <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}

sample_values <- function(x, n = 5) {
  vals <- unique(x[!is.na(x)])
  vals <- utils::head(vals, n)
  paste(vals, collapse = " | ")
}

label_from_name <- function(x) {
  x %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_lower() %>%
    stringr::str_to_sentence()
}

indicator_format <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "PERC|QUOTA|TASSO|INCIDENZA|TURNOVER") ~ "percentuale",
    stringr::str_detect(x, "SPESA") ~ "euro",
    stringr::str_detect(x, "ETA|ANZIANITA") ~ "numero",
    stringr::str_detect(x, "GIORNI|GG") ~ "giorni",
    TRUE ~ "numero"
  )
}

indicator_decimals <- function(fmt) {
  dplyr::case_when(
    fmt %in% c("percentuale", "euro", "giorni") ~ 1L,
    TRUE ~ 0L
  )
}

indicator_aggregation <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "PERC|QUOTA|TASSO|INCIDENZA|TURNOVER|ETA|ANZIANITA|INDICE") ~ "mean",
    TRUE ~ "sum"
  )
}

save_table_local <- function(obj, path, formats = c("rds", "csv", "xlsx")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  out <- character(0)
  
  if ("rds" %in% formats) {
    f <- paste0(path, ".rds")
    saveRDS(obj, f)
    out <- c(out, f)
  }
  
  if ("csv" %in% formats) {
    f <- paste0(path, ".csv")
    readr::write_csv(obj, f)
    out <- c(out, f)
  }
  
  if ("xlsx" %in% formats) {
    f <- paste0(path, ".xlsx")
    writexl::write_xlsx(obj, f)
    out <- c(out, f)
  }
  
  out
}

upload_files <- function(files, drive_folder_rel) {
  purrr::walk(
    files,
    ~ drive_upload_or_update(
      local_path = .x,
      drive_folder_rel = drive_folder_rel
    )
  )
}

safe_sum_col <- function(df, col) {
  if (!col %in% names(df)) return(NA_real_)
  if (all(is.na(df[[col]]))) return(NA_real_)
  sum(df[[col]], na.rm = TRUE)
}

safe_mean_col <- function(df, col) {
  if (!col %in% names(df)) return(NA_real_)
  if (all(is.na(df[[col]]))) return(NA_real_)
  mean(df[[col]], na.rm = TRUE)
}

agg_ca <- function(df, group_vars) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(group_vars))) %>%
    dplyr::summarise(
      n_pa_monitorate = dplyr::n_distinct(.data$codice_fiscale),
      n_pa_con_dato_ca = dplyr::n_distinct(.data$codice_fiscale[.data$IN_FONTE_CA == 1]),
      copertura_ca_perc = sim_safe_div(n_pa_con_dato_ca, n_pa_monitorate, 100),
      PERSONALE_TOT = safe_sum_col(dplyr::pick(dplyr::everything()), "PERSONALE_TOT"),
      PERSONALE_UOMINI = safe_sum_col(dplyr::pick(dplyr::everything()), "PERSONALE_UOMINI"),
      PERSONALE_DONNE = safe_sum_col(dplyr::pick(dplyr::everything()), "PERSONALE_DONNE"),
      ASSUN_TOT = safe_sum_col(dplyr::pick(dplyr::everything()), "ASSUN_TOT"),
      CESS_TOT = safe_sum_col(dplyr::pick(dplyr::everything()), "CESS_TOT"),
      SALDO_ASSUN_CESS = safe_sum_col(dplyr::pick(dplyr::everything()), "SALDO_ASSUN_CESS"),
      ETA_MEDIA_PA = safe_mean_col(dplyr::pick(dplyr::everything()), "ETA_MEDIA_PA"),
      QUOTA_DONNE_PERC = sim_safe_div(PERSONALE_DONNE, PERSONALE_TOT, 100),
      QUOTA_UOMINI_PERC = sim_safe_div(PERSONALE_UOMINI, PERSONALE_TOT, 100),
      TURNOVER_PERC = sim_safe_div(ASSUN_TOT + CESS_TOT, PERSONALE_TOT, 100),
      TASSO_CRESCITA_PERC = sim_safe_div(SALDO_ASSUN_CESS, PERSONALE_TOT, 100),
      QUOTA_UNDER35_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "QUOTA_UNDER35_PERC"),
      QUOTA_OVER55_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "QUOTA_OVER55_PERC"),
      QUOTA_OVER65_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "QUOTA_OVER65_PERC"),
      INDICE_RICAMBIO_GENERAZIONALE = safe_mean_col(dplyr::pick(dplyr::everything()), "INDICE_RICAMBIO_GENERAZIONALE"),
      QUOTA_LAUREA_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "QUOTA_LAUREA_PERC"),
      PERC_PART_TIME = safe_mean_col(dplyr::pick(dplyr::everything()), "PERC_PART_TIME"),
      PERC_PERSONALE_FORMATO = safe_mean_col(dplyr::pick(dplyr::everything()), "PERC_PERSONALE_FORMATO"),
      GIORNI_FORM_TOT = safe_sum_col(dplyr::pick(dplyr::everything()), "GIORNI_FORM_TOT"),
      GIORNI_FORM_PER_DIPENDENTE = safe_mean_col(dplyr::pick(dplyr::everything()), "GIORNI_FORM_PER_DIPENDENTE"),
      SPESA_FORMAZIONE_L020 = safe_sum_col(dplyr::pick(dplyr::everything()), "SPESA_FORMAZIONE_L020"),
      SPESA_FORMAZIONE_PER_DIPENDENTE = safe_mean_col(dplyr::pick(dplyr::everything()), "SPESA_FORMAZIONE_PER_DIPENDENTE"),
      INCIDENZA_SPESA_FORMAZIONE_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "INCIDENZA_SPESA_FORMAZIONE_PERC"),
      GG_ASSENZA_PER_DIP = safe_mean_col(dplyr::pick(dplyr::everything()), "GG_ASSENZA_PER_DIP"),
      GG_ASSENZA_MALATTIA_PER_DIP = safe_mean_col(dplyr::pick(dplyr::everything()), "GG_ASSENZA_MALATTIA_PER_DIP"),
      PASSAGGI_QUALIFICA_TOT = safe_sum_col(dplyr::pick(dplyr::everything()), "PASSAGGI_QUALIFICA_TOT"),
      TASSO_PASSAGGI_QUALIFICA_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "TASSO_PASSAGGI_QUALIFICA_PERC"),
      TASSO_PROGRESSIONE_PERC = safe_mean_col(dplyr::pick(dplyr::everything()), "TASSO_PROGRESSIONE_PERC"),
      .groups = "drop"
    )
}

# 5) LETTURA OUTPUT FILE 03 e LISTA MPA ----------------------------------------

message("[2/11] Lettura ultimo output prodotto dal file 03...")

latest_indicators_run <- ca_latest_run_folder(DRIVE_CA_INDICATORS)
message("Ultima cartella indicatori CA: ", latest_indicators_run$name[1])

FACT_CA_DASHBOARD <- read_rds_from_run(latest_indicators_run, "FACT_CA_DASHBOARD.rds") %>%
  tibble::as_tibble()

INDICATORS_CA_LONG <- read_rds_from_run(latest_indicators_run, "INDICATORS_CA_LONG.rds") %>%
  tibble::as_tibble()

message("FACT_CA_DASHBOARD: ", nrow(FACT_CA_DASHBOARD), " righe, ", ncol(FACT_CA_DASHBOARD), " colonne.")
message("INDICATORS_CA_LONG: ", nrow(INDICATORS_CA_LONG), " righe, ", ncol(INDICATORS_CA_LONG), " colonne.")
message("Indicatori distinti nel long: ", dplyr::n_distinct(INDICATORS_CA_LONG$indicatore_id))

# Lettura opzionale ultimo master per metadati variabili.
message("[3/11] Lettura ultimo master CA per metadati variabili...")

MASTER_CA <- tryCatch({
  latest_master_run <- ca_latest_run_folder(DRIVE_CA_PROCESSED)
  read_rds_from_run(latest_master_run, "master_CA_multianno.rds") %>% tibble::as_tibble()
}, error = function(e) {
  warning("Master CA non disponibile: ", conditionMessage(e), ". MET_VARIABLES_CA sarà costruito dal FACT.")
  FACT_CA_DASHBOARD
})

# Recupero variabili Lista raccordo SIM da escludere dai metadati variabili CA

message("[2/11] Recupero variabili Lista_raccordo_SIM da escludere da MET_VARIABLES_CA...")

lists_dir <- sim_drive_ls_path(DRIVE_DIR_LISTS, create = FALSE)

file_lista_sim <- googledrive::drive_ls(lists_dir) %>%
  dplyr::filter(
    stringr::str_detect(
      .data$name,
      stringr::regex("^Lista_raccordo_SIM\\.xlsx$", ignore_case = TRUE)
    )
  )

if (nrow(file_lista_sim) == 0) {
  stop("File Lista_raccordo_SIM.xlsx non trovato in ", DRIVE_DIR_LISTS)
}

local_lista_sim <- sim_drive_download_to_temp(
  file_lista_sim[1, ],
  local_name = "Lista_raccordo_SIM.xlsx",
  overwrite = TRUE
)

lista_sim <- readxl::read_excel(local_lista_sim) %>%
  janitor::clean_names()

unlink(local_lista_sim)

variabili_lista_sim <- names(lista_sim)

variabili_da_escludere_met_variables <- setdiff(
  variabili_lista_sim,
  "codice_fiscale"
)

variabili_da_escludere_met_variables <- c(
  variabili_da_escludere_met_variables,
  "presente_mpa",
  "presente_MPA",
  "presente_s13",
  "presente_bdap"
)

message("Variabili escluse da MET_VARIABLES_CA perché provenienti dalla Lista_raccordo_SIM/MPA:")
message(paste(variabili_da_escludere_met_variables, collapse = ", "))

# 6) METADATI DOCUMENTALI -----------------------------------------------------


# Variabili della Lista_raccordo_SIM da NON documentare come variabili CA

variabili_master_ca <- setdiff(
  names(MASTER_CA),
  variabili_da_escludere_met_variables
)

message(
  "Variabili documentate in MET_VARIABLES_CA: ",
  length(variabili_master_ca)
)

message(
  "Variabili escluse (Lista_raccordo_SIM/MPA): ",
  length(variabili_da_escludere_met_variables)
)

message("[4/11] Costruzione metadati documentali MET_*...")

MET_VARIABLES_CA <- tibble::tibble(
  fonte = "Conto Annuale",
  dataset_id = "master_CA_multianno",
  
  nome_variabile_originale = variabili_master_ca,
  nome_variabile_standardizzato = variabili_master_ca,
  
  tipo_dato_originale = purrr::map_chr(
    MASTER_CA[variabili_master_ca],
    ~ class(.x)[1]
  ),
  
  tipo_dato_import = purrr::map_chr(
    MASTER_CA[variabili_master_ca],
    ~ class(.x)[1]
  ),
  
  unita_di_misura = dplyr::case_when(
    stringr::str_detect(nome_variabile_standardizzato, "PERC|QUOTA|TASSO|INCIDENZA|TURNOVER") ~ "%",
    stringr::str_detect(nome_variabile_standardizzato, "SPESA") ~ "euro",
    stringr::str_detect(nome_variabile_standardizzato, "ETA|ANZIANITA") ~ "anni/valore medio",
    stringr::str_detect(nome_variabile_standardizzato, "GIORNI|GG") ~ "giorni",
    TRUE ~ NA_character_
  ),
  
  n_missing = purrr::map_int(
    MASTER_CA[variabili_master_ca],
    ~ sum(is.na(.x))
  ),
  
  pct_missing = round(
    100 * n_missing / nrow(MASTER_CA),
    2
  ),
  
  n_valori_distinti = purrr::map_int(
    MASTER_CA[variabili_master_ca],
    safe_n_distinct
  ),
  
  esempi_valori = purrr::map_chr(
    MASTER_CA[variabili_master_ca],
    sample_values
  ),
  
  note = "Variabile acquisita dalla fonte Conto Annuale. Sono escluse le variabili anagrafiche e di classificazione provenienti dalla Lista_raccordo_SIM.",
  
  run_id = RUN_ID
)

indicator_base <- INDICATORS_CA_LONG %>%
  dplyr::group_by(.data$indicatore_id) %>%
  dplyr::summarise(
    n_missing = sum(is.na(.data$valore)),
    pct_missing = round(100 * n_missing / dplyr::n(), 2),
    n_valori_distinti = dplyr::n_distinct(.data$valore, na.rm = TRUE),
    esempi_valori = sample_values(.data$valore),
    .groups = "drop"
  )

indicator_dictionary <- tibble::tribble(
  ~indicatore_id, ~nome_indicatore, ~tabella_input, ~formula, ~additivo, ~unita_di_misura, ~fenomeno_osservabile, ~denominatore, ~note,
  "PERSONALE_TOT", "Personale totale", "OCCUPAZIONE", "Valore diretto o somma PERSONALE_UOMINI + PERSONALE_DONNE", TRUE, "unità", "Stock del personale", NA_character_, NA_character_,
  "PERSONALE_UOMINI", "Personale uomini", "OCCUPAZIONE", "Valore da fonte/master", TRUE, "unità", "Composizione del personale", NA_character_, NA_character_,
  "PERSONALE_DONNE", "Personale donne", "OCCUPAZIONE", "Valore da fonte/master", TRUE, "unità", "Composizione del personale", NA_character_, NA_character_,
  "QUOTA_DONNE_PERC", "Quota donne", "OCCUPAZIONE", "100 * PERSONALE_DONNE / PERSONALE_TOT", FALSE, "%", "Composizione del personale", "PERSONALE_TOT", NA_character_,
  "QUOTA_UOMINI_PERC", "Quota uomini", "OCCUPAZIONE", "100 * PERSONALE_UOMINI / PERSONALE_TOT", FALSE, "%", "Composizione del personale", "PERSONALE_TOT", NA_character_,
  "TEMPO_PIENO_TOT", "Personale a tempo pieno", "OCCUPAZIONE", "Valore diretto o somma TEMPO_PIENO_UOMINI + TEMPO_PIENO_DONNE", TRUE, "unità", "Regime orario del personale", NA_character_, NA_character_,
  "TOT_PART_TIME", "Personale part-time", "OCCUPAZIONE", "Valore diretto o somma PART_TIME_UOMINI + PART_TIME_DONNE", TRUE, "unità", "Regime orario del personale", NA_character_, NA_character_,
  "PERC_PART_TIME", "Incidenza part-time", "OCCUPAZIONE", "100 * TOT_PART_TIME / PERSONALE_TOT", FALSE, "%", "Regime orario del personale", "PERSONALE_TOT", NA_character_,
  "PERC_TEMPO_PIENO", "Incidenza tempo pieno", "OCCUPAZIONE", "100 * TEMPO_PIENO_TOT / PERSONALE_TOT", FALSE, "%", "Regime orario del personale", "PERSONALE_TOT", NA_character_,
  "ASSUN_TOT", "Assunzioni totali", "ASSUNTI", "Valore diretto o somma ASSUN_UOMINI + ASSUN_DONNE", TRUE, "unità", "Flussi occupazionali", NA_character_, NA_character_,
  "CESS_TOT", "Cessazioni totali", "CESSATI", "Valore diretto o somma CESS_UOMINI + CESS_DONNE", TRUE, "unità", "Flussi occupazionali", NA_character_, NA_character_,
  "SALDO_ASSUN_CESS", "Saldo assunzioni-cessazioni", "ASSUNTI; CESSATI", "ASSUN_TOT - CESS_TOT", TRUE, "unità", "Flussi occupazionali", NA_character_, NA_character_,
  "TURNOVER_PERC", "Turnover", "ASSUNTI; CESSATI; OCCUPAZIONE", "100 * (ASSUN_TOT + CESS_TOT) / PERSONALE_TOT", FALSE, "%", "Ricambio del personale", "PERSONALE_TOT", NA_character_,
  "TASSO_CRESCITA_PERC", "Tasso di crescita del personale", "ASSUNTI; CESSATI; OCCUPAZIONE", "100 * (ASSUN_TOT - CESS_TOT) / PERSONALE_TOT", FALSE, "%", "Dinamica del personale", "PERSONALE_TOT", NA_character_,
  "ETA_MEDIA_PA", "Età media", "ETA_MEDIA", "Valore da fonte/master", FALSE, "anni", "Struttura per età", NA_character_, NA_character_,
  "QUOTA_UNDER35_PERC", "Quota under 35", "ETA_MEDIA", "100 * UNDER35 / PERSONALE_TOT_ETA", FALSE, "%", "Ricambio generazionale", "PERSONALE_TOT_ETA", NA_character_,
  "QUOTA_OVER55_PERC", "Quota over 55", "ETA_MEDIA", "100 * OVER55 / PERSONALE_TOT_ETA", FALSE, "%", "Struttura per età", "PERSONALE_TOT_ETA", NA_character_,
  "QUOTA_OVER65_PERC", "Quota over 65", "ETA_MEDIA", "100 * OVER65 / PERSONALE_TOT_ETA", FALSE, "%", "Struttura per età", "PERSONALE_TOT_ETA", NA_character_,
  "INDICE_RICAMBIO_GENERAZIONALE", "Indice di ricambio generazionale", "ETA_MEDIA", "UNDER35 / OVER55", FALSE, "rapporto", "Ricambio generazionale", "OVER55", NA_character_,
  "QUOTA_LAUREA_PERC", "Quota laurea", "TITOLI_STUDIO", "100 * LAUREA_TOT / PERS_TOT_TITOLI", FALSE, "%", "Titoli di studio", "PERS_TOT_TITOLI", NA_character_,
  "QUOTA_DIPLOMA_PERC", "Quota diploma", "TITOLI_STUDIO", "100 * DIPLOMA_TOT / PERS_TOT_TITOLI", FALSE, "%", "Titoli di studio", "PERS_TOT_TITOLI", NA_character_,
  "PERS_FORM_TOT", "Personale formato", "FORMAZIONE", "Valore diretto o somma PERS_FORM_UOMINI + PERS_FORM_DONNE", TRUE, "unità", "Formazione del personale", NA_character_, NA_character_,
  "PERC_PERSONALE_FORMATO", "Incidenza personale formato", "FORMAZIONE; OCCUPAZIONE", "100 * PERS_FORM_TOT / PERSONALE_TOT", FALSE, "%", "Formazione del personale", "PERSONALE_TOT", NA_character_,
  "GIORNI_FORM_TOT", "Giorni di formazione", "FORMAZIONE", "Valore diretto o somma GIORNI_FORM_UOMINI + GIORNI_FORM_DONNE", TRUE, "giorni", "Formazione del personale", NA_character_, NA_character_,
  "GIORNI_FORM_PER_DIPENDENTE", "Giorni formazione per dipendente", "FORMAZIONE; OCCUPAZIONE", "GIORNI_FORM_TOT / PERSONALE_TOT", FALSE, "giorni per dipendente", "Formazione del personale", "PERSONALE_TOT", NA_character_,
  "SPESA_FORMAZIONE_L020", "Spesa per formazione", "FORMAZIONE", "Valore da fonte/master", TRUE, "euro", "Spesa per formazione", NA_character_, NA_character_,
  "SPESA_FORMAZIONE_PER_DIPENDENTE", "Spesa formazione per dipendente", "FORMAZIONE; OCCUPAZIONE", "SPESA_FORMAZIONE_L020 / PERSONALE_TOT", FALSE, "euro per dipendente", "Spesa per formazione", "PERSONALE_TOT", NA_character_,
  "INCIDENZA_SPESA_FORMAZIONE_PERC", "Incidenza spesa formazione", "FORMAZIONE", "100 * SPESA_FORMAZIONE_L020 / TOTALE_SPESA", FALSE, "%", "Spesa per formazione", "TOTALE_SPESA", NA_character_,
  "GG_ASSENZA_PER_DIP", "Giorni di assenza per dipendente", "ASSENZE", "ASSENZE_TOT / PERSONALE_TOT", FALSE, "giorni per dipendente", "Assenze", "PERSONALE_TOT", NA_character_,
  "GG_ASSENZA_MALATTIA_PER_DIP", "Giorni assenza per malattia per dipendente", "ASSENZE", "ASSENZE_MALATTIA_TOT / PERSONALE_TOT", FALSE, "giorni per dipendente", "Assenze", "PERSONALE_TOT", NA_character_,
  "PASSAGGI_QUALIFICA_TOT", "Passaggi di qualifica", "PASSAGGI_QUALIFICA", "Valore da fonte/master", TRUE, "unità", "Carriere e progressioni", NA_character_, NA_character_,
  "TASSO_PASSAGGI_QUALIFICA_PERC", "Tasso passaggi di qualifica", "PASSAGGI_QUALIFICA; OCCUPAZIONE", "100 * PASSAGGI_QUALIFICA_TOT / PERSONALE_TOT", FALSE, "%", "Carriere e progressioni", "PERSONALE_TOT", NA_character_,
  "ANZIANITA_MEDIA_PA", "Anzianità media", "ANZIANITA", "Valore da fonte/master", FALSE, "anni/valore medio", "Anzianità di servizio", NA_character_, NA_character_,
  "QUOTA_ANZIANITA_BREVE_PERC", "Quota anzianità breve", "ANZIANITA", "100 * PERS_ANZIANITA_BREVE / PERS_TOT_ANZIANITA", FALSE, "%", "Anzianità di servizio", "PERS_TOT_ANZIANITA", NA_character_,
  "QUOTA_ANZIANITA_LUNGA_PERC", "Quota anzianità lunga", "ANZIANITA", "100 * PERS_ANZIANITA_LUNGA / PERS_TOT_ANZIANITA", FALSE, "%", "Anzianità di servizio", "PERS_TOT_ANZIANITA", NA_character_,
  "QUOTA_PRECARI_PERC", "Quota PERS_PRECARIO_TOT", "LAVORO_FLESSIBILE", "100 * PERS_PRECARIO_TOT / PERSONALE_TOT", FALSE, "%", "Rapporti di lavoro", "PERSONALE_TOT", "Etichetta provvisoria allineata al nome tecnico della variabile di fonte/master."
)

MET_INDICATORS_CA <- indicator_base %>%
  dplyr::left_join(indicator_dictionary, by = "indicatore_id") %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    dataset_id = dplyr::coalesce(.data$tabella_input, "master_CA_multianno"),
    nome_variabile_originale = .data$indicatore_id,
    nome_variabile_standardizzato = .data$indicatore_id,
    tipo_dato_import = "numeric",
    unita_di_misura = dplyr::coalesce(.data$unita_di_misura, indicator_format(.data$indicatore_id)),
    descrizione = dplyr::coalesce(.data$nome_indicatore, label_from_name(.data$indicatore_id)),
    formula = dplyr::coalesce(.data$formula, "Valore presente nel master CA"),
    fenomeno_osservabile = dplyr::coalesce(.data$fenomeno_osservabile, "Da classificare"),
    indicatore_derivato = !stringr::str_detect(.data$formula, "Valore presente nel master CA|Valore da fonte/master"),
    fonte_origine = .data$dataset_id,
    note = dplyr::coalesce(.data$note, NA_character_),
    run_id = RUN_ID
  ) %>%
  dplyr::select(
    fonte, dataset_id,
    nome_variabile_originale,
    nome_variabile_standardizzato,
    tipo_dato_import,
    unita_di_misura,
    descrizione,
    formula,
    fenomeno_osservabile,
    indicatore_derivato,
    fonte_origine,
    n_missing,
    pct_missing,
    n_valori_distinti,
    esempi_valori,
    note,
    run_id
  )

MET_FILTERS_CA <- tibble::tribble(
  ~filtro, ~label, ~tabella, ~colonna, ~tipo_controllo, ~multiselezione, ~applica_fact, ~applica_dim_enti, ~descrizione, ~note,
  "anno", "Anno", "FACT_CA_DASHBOARD", "anno", "selectize", TRUE, TRUE, FALSE, "Anno di riferimento del dato.", NA_character_,
  "regione", "Regione", "FACT_CA_DASHBOARD", "regione_bdap", "selectize", TRUE, TRUE, TRUE, "Regione associata all'amministrazione.", NA_character_,
  "forma_giuridica", "Forma giuridica", "FACT_CA_DASHBOARD", "desc_fg", "selectize", TRUE, TRUE, TRUE, "Forma giuridica dell'amministrazione.", NA_character_,
  "ateco", "ATECO", "FACT_CA_DASHBOARD", "ateco_bdap", "selectize", TRUE, TRUE, TRUE, "Codice ATECO associato all'amministrazione, se disponibile.", NA_character_,
  "tipologia_istituzione", "Tipologia istituzione", "FACT_CA_DASHBOARD", "desc_tipo_istituzione_ca", "selectize", TRUE, TRUE, TRUE, "Tipologia di istituzione secondo il Conto Annuale, se disponibile.", NA_character_,
  "pa", "Amministrazione", "FACT_CA_DASHBOARD", "codice_fiscale", "selectize", FALSE, TRUE, TRUE, "Selezione della singola amministrazione.", "Da visualizzare con ragione sociale e codice fiscale.",
  "tabella_aggregata", "Tabella aggregata", "DASH_VIEWS", "tabella_aggregata", "radio", FALSE, FALSE, FALSE, "Selezione della tabella da mostrare nella scheda Viste aggregate.", "Filtro virtuale della dashboard."
) %>%
  dplyr::mutate(fonte = "Conto Annuale", run_id = RUN_ID, .before = 1)

message("MET_VARIABLES_CA: ", nrow(MET_VARIABLES_CA), " righe.")
message("MET_INDICATORS_CA: ", nrow(MET_INDICATORS_CA), " righe.")
message("MET_FILTERS_CA: ", nrow(MET_FILTERS_CA), " righe.")

# 7) CATALOGO DASHBOARD -------------------------------------------------------

message("[5/11] Costruzione catalogo dashboard DASH_*...")

DASH_SECTIONS_CA <- tibble::tribble(
  ~scheda_id, ~scheda_label, ~ordine, ~descrizione, ~visibile,
  "copertura_mpa_ca", "Copertura MPA-Conto Annuale", 1L, "Copertura del perimetro MPA rispetto alla fonte Conto Annuale.", TRUE,
  "overview_nazionale", "Overview nazionale", 2L, "Quadro sintetico nazionale per anno e filtri generali.", TRUE,
  "struttura_ricambio", "Struttura, composizione e ricambio del personale", 3L, "Composizione, età e ricambio generazionale del personale.", TRUE,
  "formazione", "Formazione del personale", 4L, "Intensità e investimento in formazione.", TRUE,
  "organizzazione_lavoro", "Organizzazione del lavoro", 5L, "Modalità organizzative e rapporti di lavoro. Scheda predisposta per successive verifiche metodologiche.", FALSE,
  "assenze_carriere", "Assenze e carriere", 6L, "Assenze, passaggi di qualifica e progressioni.", TRUE,
  "trend", "Trend", 7L, "Evoluzione temporale degli indicatori 2021-2023.", TRUE,
  "dettaglio_pa", "Dettaglio PA", 8L, "Consultazione puntuale per singola amministrazione.", TRUE,
  "viste_aggregate", "Viste aggregate", 9L, "Tabelle comparative per regione, forma giuridica e tipologia amministrativa.", TRUE
) %>%
  dplyr::mutate(fonte = "Conto Annuale", run_id = RUN_ID, .before = 1)

common_filters <- c("anno", "regione", "forma_giuridica", "ateco", "tipologia_istituzione")
trend_filters <- c("regione", "forma_giuridica", "ateco", "tipologia_istituzione")

DASH_FILTERS_CA <- tibble::tribble(
  ~scheda_id, ~filtro, ~ordine, ~default_value, ~visibile,
  "copertura_mpa_ca", "anno", 1L, "max", TRUE,
  "copertura_mpa_ca", "regione", 2L, "Tutte", TRUE,
  "copertura_mpa_ca", "forma_giuridica", 3L, "Tutte", TRUE,
  "copertura_mpa_ca", "ateco", 4L, "Tutte", TRUE,
  "copertura_mpa_ca", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "overview_nazionale", "anno", 1L, "max", TRUE,
  "overview_nazionale", "regione", 2L, "Tutte", TRUE,
  "overview_nazionale", "forma_giuridica", 3L, "Tutte", TRUE,
  "overview_nazionale", "ateco", 4L, "Tutte", TRUE,
  "overview_nazionale", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "struttura_ricambio", "anno", 1L, "max", TRUE,
  "struttura_ricambio", "regione", 2L, "Tutte", TRUE,
  "struttura_ricambio", "forma_giuridica", 3L, "Tutte", TRUE,
  "struttura_ricambio", "ateco", 4L, "Tutte", TRUE,
  "struttura_ricambio", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "formazione", "anno", 1L, "max", TRUE,
  "formazione", "regione", 2L, "Tutte", TRUE,
  "formazione", "forma_giuridica", 3L, "Tutte", TRUE,
  "formazione", "ateco", 4L, "Tutte", TRUE,
  "formazione", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "organizzazione_lavoro", "anno", 1L, "max", TRUE,
  "organizzazione_lavoro", "regione", 2L, "Tutte", TRUE,
  "organizzazione_lavoro", "forma_giuridica", 3L, "Tutte", TRUE,
  "organizzazione_lavoro", "ateco", 4L, "Tutte", TRUE,
  "organizzazione_lavoro", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "assenze_carriere", "anno", 1L, "max", TRUE,
  "assenze_carriere", "regione", 2L, "Tutte", TRUE,
  "assenze_carriere", "forma_giuridica", 3L, "Tutte", TRUE,
  "assenze_carriere", "ateco", 4L, "Tutte", TRUE,
  "assenze_carriere", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "trend", "regione", 1L, "Tutte", TRUE,
  "trend", "forma_giuridica", 2L, "Tutte", TRUE,
  "trend", "ateco", 3L, "Tutte", TRUE,
  "trend", "tipologia_istituzione", 4L, "Tutte", TRUE,
  "dettaglio_pa", "pa", 1L, NA_character_, TRUE,
  "dettaglio_pa", "anno", 2L, "max", TRUE,
  "dettaglio_pa", "regione", 3L, "Tutte", TRUE,
  "dettaglio_pa", "forma_giuridica", 4L, "Tutte", TRUE,
  "dettaglio_pa", "ateco", 5L, "Tutte", TRUE,
  "dettaglio_pa", "tipologia_istituzione", 6L, "Tutte", TRUE,
  "viste_aggregate", "anno", 1L, "max", TRUE,
  "viste_aggregate", "regione", 2L, "Tutte", TRUE,
  "viste_aggregate", "forma_giuridica", 3L, "Tutte", TRUE,
  "viste_aggregate", "ateco", 4L, "Tutte", TRUE,
  "viste_aggregate", "tipologia_istituzione", 5L, "Tutte", TRUE,
  "viste_aggregate", "tabella_aggregata", 6L, "regione", TRUE
) %>%
  dplyr::left_join(MET_FILTERS_CA, by = "filtro") %>%
  dplyr::transmute(
    fonte = "Conto Annuale",
    run_id = RUN_ID,
    scheda_id,
    filtro,
    ordine,
    default_value,
    visibile,
    label_filtro = label,
    tabella,
    colonna,
    tipo_controllo,
    multiselezione,
    applica_fact,
    applica_dim_enti,
    descrizione,
    note
  )

DASH_INDICATORS_SEED <- tibble::tribble(
  ~nome_variabile, ~nome_indicatore, ~scheda_id, ~sezione, ~ordine, ~visibile, ~default, ~tooltip,
  "IN_FONTE_CA", "Presenza in Conto Annuale", "copertura_mpa_ca", "Copertura", 1L, TRUE, TRUE, "Flag di copertura della PA nella fonte Conto Annuale.",
  "PERSONALE_TOT", "Personale totale", "overview_nazionale", "KPI", 1L, TRUE, TRUE, "Totale del personale rilevato.",
  "PERSONALE_UOMINI", "Uomini", "overview_nazionale", "KPI", 2L, TRUE, FALSE, "Personale maschile.",
  "PERSONALE_DONNE", "Donne", "overview_nazionale", "KPI", 3L, TRUE, FALSE, "Personale femminile.",
  "ETA_MEDIA_PA", "Età media", "overview_nazionale", "KPI", 4L, TRUE, TRUE, "Età media del personale.",
  "QUOTA_UNDER35_PERC", "Quota under 35", "overview_nazionale", "KPI", 5L, TRUE, TRUE, "Incidenza del personale under 35.",
  "QUOTA_OVER55_PERC", "Quota over 55", "overview_nazionale", "KPI", 6L, TRUE, TRUE, "Incidenza del personale over 55.",
  "ASSUN_TOT", "Assunzioni totali", "overview_nazionale", "KPI", 7L, TRUE, TRUE, "Assunzioni registrate nell'anno.",
  "CESS_TOT", "Cessazioni totali", "overview_nazionale", "KPI", 8L, TRUE, TRUE, "Cessazioni registrate nell'anno.",
  "SALDO_ASSUN_CESS", "Saldo assunzioni-cessazioni", "overview_nazionale", "KPI", 9L, TRUE, TRUE, "Differenza tra assunzioni e cessazioni.",
  "TURNOVER_PERC", "Turnover", "overview_nazionale", "KPI", 10L, TRUE, TRUE, "Rapporto tra flussi in entrata/uscita e personale totale.",
  "INDICE_RICAMBIO_GENERAZIONALE", "Indice ricambio generazionale", "overview_nazionale", "KPI", 11L, TRUE, TRUE, "Rapporto tra personale under 35 e over 55.",
  "ETA_MEDIA_PA", "Età media", "struttura_ricambio", "Età e ricambio", 1L, TRUE, TRUE, "Età media del personale.",
  "QUOTA_UNDER35_PERC", "Quota under 35", "struttura_ricambio", "Età e ricambio", 2L, TRUE, TRUE, "Incidenza del personale under 35.",
  "QUOTA_OVER55_PERC", "Quota over 55", "struttura_ricambio", "Età e ricambio", 3L, TRUE, TRUE, "Incidenza del personale over 55.",
  "QUOTA_OVER65_PERC", "Quota over 65", "struttura_ricambio", "Età e ricambio", 4L, TRUE, FALSE, "Incidenza del personale over 65.",
  "INDICE_RICAMBIO_GENERAZIONALE", "Indice ricambio generazionale", "struttura_ricambio", "Età e ricambio", 5L, TRUE, TRUE, "Rapporto tra personale under 35 e over 55.",
  "QUOTA_DONNE_PERC", "Quota donne", "struttura_ricambio", "Composizione", 6L, TRUE, FALSE, "Incidenza del personale femminile.",
  "PERC_PART_TIME", "Incidenza part-time", "struttura_ricambio", "Composizione", 7L, TRUE, FALSE, "Incidenza del personale part-time.",
  "QUOTA_LAUREA_PERC", "Quota laurea", "struttura_ricambio", "Titoli di studio", 8L, TRUE, FALSE, "Incidenza del personale con laurea.",
  "PERS_FORM_TOT", "Personale formato", "formazione", "KPI", 1L, TRUE, FALSE, "Totale del personale formato.",
  "PERC_PERSONALE_FORMATO", "Incidenza personale formato", "formazione", "KPI", 2L, TRUE, TRUE, "Quota di personale formato sul totale.",
  "GIORNI_FORM_TOT", "Giorni di formazione", "formazione", "KPI", 3L, TRUE, TRUE, "Totale dei giorni di formazione.",
  "GIORNI_FORM_PER_DIPENDENTE", "Giorni formazione per dipendente", "formazione", "KPI", 4L, TRUE, TRUE, "Giorni medi di formazione per dipendente.",
  "SPESA_FORMAZIONE_L020", "Spesa per formazione", "formazione", "KPI", 5L, TRUE, TRUE, "Spesa per attività di formazione.",
  "SPESA_FORMAZIONE_PER_DIPENDENTE", "Spesa formazione per dipendente", "formazione", "KPI", 6L, TRUE, TRUE, "Spesa media per formazione per dipendente.",
  "INCIDENZA_SPESA_FORMAZIONE_PERC", "Incidenza spesa formazione", "formazione", "KPI", 7L, TRUE, TRUE, "Incidenza della spesa per formazione sulla spesa totale.",
  "QUOTA_LAVORO_AGILE_PERC", "Quota lavoro agile", "organizzazione_lavoro", "Modalità di lavoro", 1L, FALSE, FALSE, "Indicatore predisposto per successive verifiche metodologiche.",
  "QUOTA_TELE_LAVORO_PERC", "Quota telelavoro", "organizzazione_lavoro", "Modalità di lavoro", 2L, FALSE, FALSE, "Indicatore predisposto per successive verifiche metodologiche.",
  "QUOTA_MOD_FLESSIBILE_PERC", "Quota modalità flessibili", "organizzazione_lavoro", "Modalità di lavoro", 3L, FALSE, FALSE, "Indicatore predisposto per successive verifiche metodologiche.",
  "QUOTA_PRECARI_PERC", "Quota PERS_PRECARIO_TOT", "organizzazione_lavoro", "Rapporti di lavoro", 4L, FALSE, FALSE, "Indicatore predisposto per successive verifiche metodologiche.",
  "GG_ASSENZA_PER_DIP", "Giorni di assenza per dipendente", "assenze_carriere", "Assenze", 1L, TRUE, TRUE, "Giorni di assenza per dipendente.",
  "GG_ASSENZA_MALATTIA_PER_DIP", "Giorni assenza malattia per dipendente", "assenze_carriere", "Assenze", 2L, TRUE, FALSE, "Giorni di assenza per malattia per dipendente.",
  "PASSAGGI_QUALIFICA_TOT", "Passaggi di qualifica", "assenze_carriere", "Carriere", 3L, TRUE, TRUE, "Numero di passaggi di qualifica.",
  "TASSO_PASSAGGI_QUALIFICA_PERC", "Tasso passaggi qualifica", "assenze_carriere", "Carriere", 4L, TRUE, TRUE, "Passaggi di qualifica rapportati al personale totale.",
  "TASSO_PROGRESSIONE_PERC", "Tasso progressioni", "assenze_carriere", "Carriere", 5L, TRUE, FALSE, "Progressioni rapportate al personale totale.",
  "PERSONALE_TOT", "Trend personale totale", "trend", "Dinamica del personale", 1L, TRUE, TRUE, "Andamento del personale totale.",
  "ASSUN_TOT", "Trend assunzioni", "trend", "Dinamica del personale", 2L, TRUE, TRUE, "Andamento delle assunzioni.",
  "CESS_TOT", "Trend cessazioni", "trend", "Dinamica del personale", 3L, TRUE, TRUE, "Andamento delle cessazioni.",
  "SALDO_ASSUN_CESS", "Trend saldo", "trend", "Dinamica del personale", 4L, TRUE, TRUE, "Andamento del saldo assunzioni-cessazioni.",
  "TURNOVER_PERC", "Trend turnover", "trend", "Dinamica del personale", 5L, TRUE, TRUE, "Andamento del turnover.",
  "TASSO_CRESCITA_PERC", "Trend crescita", "trend", "Dinamica del personale", 6L, TRUE, TRUE, "Andamento del tasso di crescita.",
  "GIORNI_FORM_PER_DIPENDENTE", "Trend giorni formazione per dipendente", "trend", "Formazione", 7L, TRUE, TRUE, "Andamento dei giorni di formazione per dipendente.",
  "SPESA_FORMAZIONE_PER_DIPENDENTE", "Trend spesa formazione per dipendente", "trend", "Formazione", 8L, TRUE, TRUE, "Andamento della spesa di formazione per dipendente."
)

DASH_INDICATORS_CA <- DASH_INDICATORS_SEED %>%
  dplyr::filter(.data$nome_variabile %in% names(FACT_CA_DASHBOARD)) %>%
  dplyr::transmute(
    fonte = "Conto Annuale",
    run_id = RUN_ID,
    scheda_id = .data$scheda_id,
    sezione = .data$sezione,
    nome_variabile = .data$nome_variabile,
    nome_indicatore = .data$nome_indicatore,
    ordine = .data$ordine,
    visibile = .data$visibile,
    default = .data$default,
    tipo_visualizzazione = dplyr::case_when(
      .data$scheda_id == "trend" ~ "line",
      .data$scheda_id == "viste_aggregate" ~ "table",
      .data$scheda_id == "dettaglio_pa" ~ "table",
      .data$sezione == "KPI" ~ "value_box",
      TRUE ~ "value_box"
    ),
    formato = indicator_format(.data$nome_variabile),
    decimali = indicator_decimals(indicator_format(.data$nome_variabile)),
    aggregazione = indicator_aggregation(.data$nome_variabile),
    tooltip = .data$tooltip
  )

message("DASH_SECTIONS_CA: ", nrow(DASH_SECTIONS_CA), " righe.")
message("DASH_FILTERS_CA: ", nrow(DASH_FILTERS_CA), " righe.")
message("DASH_INDICATORS_CA: ", nrow(DASH_INDICATORS_CA), " righe.")

# 8) FACT AGGREGATI DASHBOARD -------------------------------------------------

message("[6/11] Costruzione fact aggregati per dashboard...")

FACT_CA_COVERAGE <- FACT_CA_DASHBOARD %>%
  dplyr::group_by(.data$anno, .data$regione_bdap, .data$desc_fg, .data$desc_tipo_istituzione_ca) %>%
  dplyr::summarise(
    n_pa_monitorate = dplyr::n_distinct(.data$codice_fiscale),
    n_pa_con_dato_ca = dplyr::n_distinct(.data$codice_fiscale[.data$IN_FONTE_CA == 1]),
    copertura_ca_perc = sim_safe_div(n_pa_con_dato_ca, n_pa_monitorate, 100),
    .groups = "drop"
  )

FACT_CA_REGIONE <- agg_ca(FACT_CA_DASHBOARD, c("anno", "regione_bdap", "zona_geografica"))
FACT_CA_ZONA <- agg_ca(FACT_CA_DASHBOARD, c("anno", "zona_geografica"))
FACT_CA_FG <- agg_ca(FACT_CA_DASHBOARD, c("anno", "desc_fg"))
FACT_CA_TIPOLOGIA <- agg_ca(FACT_CA_DASHBOARD, c("anno", "desc_tipo_istituzione_ca"))
FACT_CA_TREND <- agg_ca(FACT_CA_DASHBOARD, c("anno"))

message("FACT_CA_COVERAGE: ", nrow(FACT_CA_COVERAGE), " righe.")
message("FACT_CA_REGIONE: ", nrow(FACT_CA_REGIONE), " righe.")
message("FACT_CA_ZONA: ", nrow(FACT_CA_ZONA), " righe.")
message("FACT_CA_FG: ", nrow(FACT_CA_FG), " righe.")
message("FACT_CA_TIPOLOGIA: ", nrow(FACT_CA_TIPOLOGIA), " righe.")
message("FACT_CA_TREND: ", nrow(FACT_CA_TREND), " righe.")
message("Dettaglio PA: usare direttamente FACT_CA_DASHBOARD nella dashboard.")

# 9) CONTROLLI ----------------------------------------------------------------

message("[7/11] Controlli finali catalogo SIM...")

indicatori_non_documentati <- tibble::tibble(
  indicatore_id = unique(INDICATORS_CA_LONG$indicatore_id)
) %>%
  dplyr::anti_join(
    MET_INDICATORS_CA %>% dplyr::select(indicatore_id = nome_variabile_standardizzato),
    by = "indicatore_id"
  )

if (nrow(indicatori_non_documentati) > 0) {
  warning(
    "Indicatori CA non documentati in MET_INDICATORS_CA: ",
    paste(indicatori_non_documentati$indicatore_id, collapse = ", ")
  )
} else {
  message("Controllo superato: tutti gli indicatori del long sono presenti in MET_INDICATORS_CA.")
}

filtri_non_presenti <- MET_FILTERS_CA %>%
  dplyr::filter(.data$tabella == "FACT_CA_DASHBOARD") %>%
  dplyr::filter(!.data$colonna %in% names(FACT_CA_DASHBOARD))

if (nrow(filtri_non_presenti) > 0) {
  warning(
    "Alcune colonne filtro non sono presenti nel FACT_CA_DASHBOARD: ",
    paste(filtri_non_presenti$colonna, collapse = ", ")
  )
} else {
  message("Controllo superato: tutte le colonne filtro documentate sono presenti nel FACT_CA_DASHBOARD.")
}

dash_filters_non_presenti <- DASH_FILTERS_CA %>%
  dplyr::filter(.data$visibile == TRUE, .data$applica_fact == TRUE) %>%
  dplyr::filter(.data$tabella == "FACT_CA_DASHBOARD") %>%
  dplyr::filter(!.data$colonna %in% names(FACT_CA_DASHBOARD))

if (nrow(dash_filters_non_presenti) > 0) {
  warning(
    "Alcune colonne filtro DASH non sono presenti nel FACT_CA_DASHBOARD: ",
    paste(dash_filters_non_presenti$colonna, collapse = ", ")
  )
} else {
  message("Controllo superato: tutti i filtri DASH applicabili al FACT sono presenti nel FACT_CA_DASHBOARD.")
}

indicatori_dashboard_non_presenti <- DASH_INDICATORS_CA %>%
  dplyr::filter(!.data$nome_variabile %in% names(FACT_CA_DASHBOARD))

if (nrow(indicatori_dashboard_non_presenti) > 0) {
  warning("Indicatori dashboard non presenti nel FACT_CA_DASHBOARD.")
} else {
  message("Controllo superato: tutti gli indicatori dashboard sono presenti nel FACT_CA_DASHBOARD.")
}

# 10) EXPORT LOCALE E UPLOAD DRIVE -------------------------------------------

message("[8/11] Export locale e upload su Drive...")

tryCatch({
  
  DRIVE_DOC_VARIABLES_RUN <- file.path(DRIVE_CA_VARIABLES_MET, RUN_ID)
  DRIVE_DOC_INDICATORS_RUN <- file.path(DRIVE_CA_INDICATORS_MET, RUN_ID)
  DRIVE_DASHBOARD_RUN <- file.path(DRIVE_CA_INDICATORS, "Dashboard", RUN_ID)
  DRIVE_CA_LOGS_RUN <- file.path(DRIVE_CA_LOGS, RUN_ID)
  
  DIR_CA_CATALOGO_LOCAL <- file.path(
    "07_Temp", "Conto_annuale", "Catalogo_SIM", RUN_ID
  )
  
  DIR_DOC_LOCAL <- file.path(DIR_CA_CATALOGO_LOCAL, "Documentation")
  DIR_DASH_LOCAL <- file.path(DIR_CA_CATALOGO_LOCAL, "Dashboard")
  
  dir.create(DIR_DOC_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_DASH_LOCAL, recursive = TRUE, showWarnings = FALSE)
  
  message("Cartella locale catalogo CA: ", DIR_CA_CATALOGO_LOCAL)
  message("Cartella Drive metadati variabili CA: ", DRIVE_DOC_VARIABLES_RUN)
  message("Cartella Drive metadati indicatori CA: ", DRIVE_DOC_INDICATORS_RUN)
  message("Cartella Drive dashboard CA: ", DRIVE_DASHBOARD_RUN)
  
  files_doc_variables <- c(
    save_table_local(MET_VARIABLES_CA, file.path(DIR_DOC_LOCAL, "MET_VARIABLES_CA"))
  )
  
  files_doc_indicators <- c(
    save_table_local(MET_INDICATORS_CA, file.path(DIR_DOC_LOCAL, "MET_INDICATORS_CA")),
    save_table_local(MET_FILTERS_CA, file.path(DIR_DOC_LOCAL, "MET_FILTERS_CA"))
  )
  
  files_dashboard <- c(
    save_table_local(DASH_SECTIONS_CA, file.path(DIR_DASH_LOCAL, "DASH_SECTIONS_CA")),
    save_table_local(DASH_FILTERS_CA, file.path(DIR_DASH_LOCAL, "DASH_FILTERS_CA")),
    save_table_local(DASH_INDICATORS_CA, file.path(DIR_DASH_LOCAL, "DASH_INDICATORS_CA")),
    save_table_local(FACT_CA_COVERAGE, file.path(DIR_DASH_LOCAL, "FACT_CA_COVERAGE")),
    save_table_local(FACT_CA_REGIONE, file.path(DIR_DASH_LOCAL, "FACT_CA_REGIONE")),
    save_table_local(FACT_CA_ZONA, file.path(DIR_DASH_LOCAL, "FACT_CA_ZONA")),
    save_table_local(FACT_CA_FG, file.path(DIR_DASH_LOCAL, "FACT_CA_FG")),
    save_table_local(FACT_CA_TIPOLOGIA, file.path(DIR_DASH_LOCAL, "FACT_CA_TIPOLOGIA")),
    save_table_local(FACT_CA_TREND, file.path(DIR_DASH_LOCAL, "FACT_CA_TREND"))
  )
  
  message("Upload metadati variabili...")
  upload_files(files_doc_variables, DRIVE_DOC_VARIABLES_RUN)
  
  message("Upload metadati indicatori/filtri...")
  upload_files(files_doc_indicators, DRIVE_DOC_INDICATORS_RUN)
  
  message("Upload catalogo dashboard e fact aggregati...")
  upload_files(files_dashboard, DRIVE_DASHBOARD_RUN)
  
  files_to_remove <- c(files_doc_variables, files_doc_indicators, files_dashboard)
  files_to_remove <- files_to_remove[file.exists(files_to_remove)]
  
  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
    message("Pulizia file temporanei del run completata.")
  }
  
  status_run <- "completed"
  
}, error = function(e) {
  
  status_run <<- "failed"
  
  message(
    "ERRORE costruzione catalogo SIM CA: ",
    conditionMessage(e)
  )
  
  stop(e)
  
}, finally = {
  
  message("[9/11] Chiusura console log e upload log su Drive...")
  
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
  
  DRIVE_CA_LOGS_RUN <- file.path(DRIVE_CA_LOGS, RUN_ID)
  
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
  "--- Catalogo SIM CA terminato. RUN_ID: ",
  RUN_ID,
  " | status: ",
  status_run,
  " ---"
)
