# ============================================================ #
# Script: 02_raccordo_padigitale2026_lista.R
# Fonte: PA digitale 2026 - Open data
#
# Obiettivo:
#   1. leggere i dati PA digitale 2026 processed
#   2. importare la master list da Drive
#   3. raccordare candidature finanziate alla master list
#   4. produrre log di qualità del raccordo
#   5. produrre indicatori per dashboard
#
# NOTE:
# - Il valore aggiunto è leggere PA digitale 2026 per tipologia PA,
#   macro-gruppo PA, perimetro S13/MPA e territorio della master list.
# ============================================================ #


# 0) Pulizia ambiente ---------------------------------------------------------

rm(list = ls())


# 1) Configurazione e helper --------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")


# 2) Pacchetti ---------------------------------------------------------------

{library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(janitor)
  library(googledrive)
  library(openxlsx)
  library(lubridate)
  library(jsonlite)}


# 3) Autenticazione Drive --------------------------------------------------------

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# 4) Parametri del run ---------------------------------------------------------------

# parametro per pulire la cartella temp alla fine del run
delete_local_temp <- FALSE

RUN_ID_IMPORT <- "20260624_162127"  # da copiare dall'output dello script 01
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")

message("RUN_ID_IMPORT: ", RUN_ID_IMPORT)
message("RUN_ID raccordo: ", RUN_ID)


#  5) Directory locali e remote -----------------------------------------------
{
  # Input processati prodotti dallo script 01.
  DIR_PAD26_PROCESSED_INPUT_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_IMPORT)
  DIR_PAD26_PROCESSED_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID)
  DIR_PAD26_OUTPUT_LOCAL    <- file.path(DIR_TEMP, "PADigitale2026", "Output", RUN_ID)
  DIR_PAD26_LOGS_LOCAL      <- file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID)
  DIR_PAD26_METADATA_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID)
  DIR_PAD26_INDICATORI_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Indicatori", RUN_ID)
  
  # Percorsi Drive.
  DRIVE_PAD26_PROCESSED_INPUT <- file.path(DRIVE_DIR_PROCESSED_PAD26, RUN_ID_IMPORT)
  DRIVE_PAD26_PROCESSED <- file.path(DRIVE_DIR_PROCESSED_PAD26, RUN_ID)
  DRIVE_PAD26_OUTPUT <- file.path(DRIVE_DIR_OUTPUT_PAD26, RUN_ID)
  DRIVE_PAD26_LOGS <- file.path(DRIVE_DIR_LOGS_PAD26, RUN_ID)
  DRIVE_PAD26_METADATA <- file.path(DRIVE_DIR_SOURCE_MET_PAD26, RUN_ID)
  DRIVE_PAD26_INDICATORI <- file.path(DRIVE_DIR_INDICATORS_PAD26, RUN_ID)
}

# 6) Creazione directory locali ----------------------------------------------
{
  dir.create(DIR_PAD26_PROCESSED_INPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_PROCESSED_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_OUTPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_INDICATORI_LOCAL, recursive = TRUE, showWarnings = FALSE)
}   

# 7) Avvio console log --------------------------------------------------------
console_log <- start_console_log(
  log_dir = DIR_PAD26_LOGS_LOCAL,
  run_id = RUN_ID,
  script_name = "02_raccordo_PAdigitale2026"
)

message("Console log locale: ", console_log$path)
message("Cartella log Drive: ", DRIVE_PAD26_LOGS)


# 8) Download input -----------------------------------------------------------

file_candidature <- file.path(
  DIR_PAD26_PROCESSED_INPUT_LOCAL,
  "candidature_finanziate_padigitale2026.rds"
)

file_avvisi <- file.path(
  DIR_PAD26_PROCESSED_INPUT_LOCAL,
  "avvisi_padigitale2026.csv"
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_PROCESSED_INPUT, "candidature_finanziate_padigitale2026.rds"),
  local_path = file_candidature
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_PROCESSED_INPUT, "avvisi_padigitale2026.csv"),
  local_path = file_avvisi
)


if (!file.exists(file_candidature)) {
  stop("File candidature PA digitale 2026 non trovato: ", file_candidature)
}

if (!file.exists(file_avvisi)) {
  stop("File avvisi PA digitale 2026 non trovato: ", file_avvisi)
}


# 10) FUNZIONI ---------------------------------------------------------------

normalizza_testo <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("’", "'") %>%
    stringr::str_replace_all("`", "'") %>%
    stringr::str_squish() %>%
    dplyr::na_if("")
}

normalizza_codice_fiscale <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[^A-Z0-9]", "") %>%
    dplyr::na_if("")
}

normalizza_codice_regione <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    dplyr::na_if("") %>%
    stringr::str_pad(width = 2, pad = "0")
}


normalizza_chiave_avviso <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[–—−]", "-") %>%
    stringr::str_replace_all("’|`", "'") %>%
    stringr::str_replace_all("\\s*/\\s*", "/") %>%
    stringr::str_replace_all("\\s*-\\s*", " - ") %>%
    stringr::str_squish() %>%
    dplyr::na_if("")
}


# 11) IMPORT DATI PA DIGITALE 2026 -------------------------------------------

candidature_pad26 <- readRDS(file_candidature) %>%
  janitor::clean_names()

avvisi <- readr::read_csv(file_avvisi, show_col_types = FALSE) %>%
  janitor::clean_names()

# Check nomi colonne
message("Colonne candidature:")
print(names(candidature_pad26))

message("Colonne avvisi:")
print(names(avvisi))

# summary(as.factor(candidature_pad26$dataset_id))


# 11.1) INTEGRAZIONE CANDIDATURE-AVVISI E CONTROLLI ---------------------------

# Chiavi normalizzate.
candidature_check <- candidature_pad26 %>%
  dplyr::mutate(
    avviso = as.character(avviso),
    avviso_key = normalizza_chiave_avviso(avviso)
  )

avvisi_check <- avvisi %>%
  dplyr::mutate(
    titolo = as.character(titolo),
    avviso_key = normalizza_chiave_avviso(titolo)
  )

# Il file avvisi deve avere una sola riga per chiave normalizzata.
duplicati_avvisi <- avvisi_check %>%
  dplyr::count(avviso_key, sort = TRUE, name = "n") %>%
  dplyr::filter(!is.na(avviso_key), n > 1L)

if (nrow(duplicati_avvisi) > 0L) {
  stop(
    "Il dataset avvisi contiene chiavi duplicate. Verificare il foglio ",
    "'duplicati_avvisi' nel workbook dei controlli."
  )
}

# Controllo preliminare prima del raccordo manuale.
check_match_avvisi_pre <- candidature_check %>%
  dplyr::distinct(avviso, avviso_key) %>%
  dplyr::left_join(
    avvisi_check %>%
      dplyr::distinct(
        avviso_key,
        titolo,
        misura,
        data_inizio_bando,
        data_fine_bando,
        stato,
        totale_importo_stanziato,
        totale_importo_misura,
        soggetti_destinatari
      ),
    by = "avviso_key",
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    match_avviso = !is.na(titolo)
  )

sintesi_match_avvisi_pre <- check_match_avvisi_pre %>%
  dplyr::count(match_avviso, name = "n_titoli")

non_match_avvisi_pre <- check_match_avvisi_pre %>%
  dplyr::filter(!match_avviso) %>%
  dplyr::select(avviso, avviso_key)

# Raccordi espliciti e documentati:
# nelle candidature la quota è inclusa nel titolo, mentre nel dataset avvisi
# il titolo è disponibile soltanto al livello generale.
raccordo_avvisi_manual <- tibble::tribble(
  ~avviso_candidatura,
  ~titolo_avviso_ufficiale,
  ~motivo_raccordo,
  
  "1.1 Infrastrutture digitali - ASL/AO - giugno 2025 (quota 1.1)",
  "1.1 Infrastrutture digitali - ASL/AO - giugno 2025",
  "La candidatura distingue la quota 1.1; il dataset avvisi riporta il titolo generale.",
  
  "1.1 Infrastrutture digitali - ASL/AO - giugno 2025 (quota 1.2)",
  "1.1 Infrastrutture digitali - ASL/AO - giugno 2025",
  "La candidatura distingue la quota 1.2; il dataset avvisi riporta il titolo generale.",
  
  "1.1 Infrastrutture digitali - ASL/AO - ottobre 2025 (quota 1.1)",
  "1.1 Infrastrutture digitali - ASL/AO - ottobre 2025",
  "La candidatura distingue la quota 1.1; il dataset avvisi riporta il titolo generale.",
  
  "1.1 Infrastrutture digitali - ASL/AO - ottobre 2025 (quota 1.2)",
  "1.1 Infrastrutture digitali - ASL/AO - ottobre 2025",
  "La candidatura distingue la quota 1.2; il dataset avvisi riporta il titolo generale."
) %>%
  dplyr::mutate(
    avviso_key_candidatura =
      normalizza_chiave_avviso(avviso_candidatura),
    avviso_key_ufficiale =
      normalizza_chiave_avviso(titolo_avviso_ufficiale)
  )

# Applica il raccordo manuale soltanto alle quattro chiavi note.
candidature_check <- candidature_check %>%
  dplyr::left_join(
    raccordo_avvisi_manual %>%
      dplyr::select(
        avviso_key_candidatura,
        avviso_key_ufficiale,
        motivo_raccordo
      ),
    by = c("avviso_key" = "avviso_key_candidatura"),
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    avviso_key_join = dplyr::coalesce(
      avviso_key_ufficiale,
      avviso_key
    ),
    raccordo_avviso_manuale = !is.na(avviso_key_ufficiale)
  )


# 12) STANDARDIZZAZIONE PA DIGITALE 2026 -------------------------------------

avvisi_std <- avvisi_check %>%
  dplyr::mutate(
    data_inizio_bando = as.Date(data_inizio_bando),
    data_fine_bando = as.Date(data_fine_bando),
    totale_importo_stanziato =
      readr::parse_number(as.character(totale_importo_stanziato)),
    totale_importo_misura =
      readr::parse_number(as.character(totale_importo_misura))
  ) %>%
  dplyr::transmute(
    avviso_key,
    titolo_avviso = titolo,
    misura = as.character(misura),
    data_inizio_bando,
    data_fine_bando,
    anno_inizio_bando = as.integer(format(data_inizio_bando, "%Y")),
    anno_fine_bando = as.integer(format(data_fine_bando, "%Y")),
    stato_avviso = as.character(stato),
    totale_importo_stanziato,
    totale_importo_misura,
    soggetti_destinatari = as.character(soggetti_destinatari)
  ) %>%
  dplyr::distinct(avviso_key, .keep_all = TRUE)

stopifnot(!anyDuplicated(avvisi_std$avviso_key))

# Dataset candidature arricchito con le informazioni ufficiali degli avvisi.
pad26_std <- candidature_check %>%
  dplyr::mutate(
    ente_key = normalizza_testo(ente),
    codice_ipa_key = normalizza_testo(codice_ipa),
    
    cod_regione = normalizza_codice_regione(cod_regione),
    cod_provincia = stringr::str_pad(
      as.character(cod_provincia),
      3,
      pad = "0"
    ),
    cod_comune = as.character(cod_comune),
    
    regione_key = normalizza_testo(regione),
    provincia_key = normalizza_testo(provincia),
    comune_key = normalizza_testo(comune),
    tipologia_ente_key = normalizza_testo(tipologia_ente),
    stato_candidatura_key = normalizza_testo(stato_candidatura),
    
    importo_finanziamento =
      readr::parse_number(as.character(importo_finanziamento))
  ) %>%
  dplyr::left_join(
    avvisi_std,
    by = c("avviso_key_join" = "avviso_key"),
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    match_avviso = !is.na(titolo_avviso),
    titolo_avviso = dplyr::coalesce(titolo_avviso, avviso),
    motivo_non_match_avviso = dplyr::case_when(
      match_avviso ~ NA_character_,
      TRUE ~ "Titolo candidatura non presente nel dataset avvisi"
    )
  )

stopifnot(nrow(pad26_std) == nrow(candidature_pad26))

# Controlli successivi al raccordo.
check_join_avvisi <- pad26_std %>%
  dplyr::summarise(
    n_candidature = dplyr::n(),
    n_match_avviso = sum(match_avviso, na.rm = TRUE),
    n_senza_match_avviso = sum(!match_avviso, na.rm = TRUE),
    quota_match_avviso = n_match_avviso / n_candidature,
    n_raccordi_manuali =
      sum(raccordo_avviso_manuale, na.rm = TRUE)
  )

check_match_avvisi_post <- pad26_std %>%
  dplyr::distinct(
    avviso,
    avviso_key,
    avviso_key_join,
    raccordo_avviso_manuale,
    motivo_raccordo,
    titolo_avviso,
    misura,
    match_avviso
  ) %>%
  dplyr::arrange(match_avviso, avviso)

non_match_avvisi_post <- check_match_avvisi_post %>%
  dplyr::filter(!match_avviso)

if (nrow(non_match_avvisi_post) > 0L) {
  warning(
    "Restano ",
    nrow(non_match_avvisi_post),
    " titoli candidatura senza raccordo al dataset avvisi."
  )
}

message(
  "Raccordo candidature-avvisi: ",
  check_join_avvisi$n_match_avviso,
  "/",
  check_join_avvisi$n_candidature,
  " candidature abbinate; raccordi manuali: ",
  check_join_avvisi$n_raccordi_manuali
)

# Dimensione ufficiale degli avvisi. Gli importi stanziati restano qui,
# perché non devono essere sommati sulle righe-candidatura.
dim_avvisi_padigitale2026 <- avvisi_std %>%
  dplyr::arrange(misura, data_inizio_bando, titolo_avviso)

# 13) IMPORT MASTER LIST DA DRIVE --------------------------------------------

file_lista_local <- file.path(
  DIR_TEMP,
  basename(DRIVE_FILE_LISTA_RACCORDO_SIM_RDS)
)

drive_download_from_path(
  drive_file_rel = DRIVE_FILE_LISTA_RACCORDO_SIM_RDS,
  local_path = file_lista_local
)

lista <- readRDS(file_lista_local) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()


# 14) PREPARAZIONE MASTER LIST ------------------------------------------------

names(lista)
str(lista)

lista_base <- lista %>%
  mutate(
    lista_row_id = row_number(),
    lista_ind = 1,
    
    codice_fiscale_key = normalizza_codice_fiscale(codice_fiscale),
    codice_ipa_key = normalizza_testo(codice_ente_ipa),
    codice_siope_key = normalizza_testo(codice_ente_siope),
    ragione_sociale_key = normalizza_testo(ragione_sociale),
    
    codice_regione = normalizza_codice_regione(codice_reg),
    codice_provincia = stringr::str_pad(as.character(codice_provincia), 3, pad = "0"),
    codice_comune = as.character(codice_comune),
    
    presente_mpa = as.integer(presente_mpa == "1"),
    presente_s13 = as.integer(presente_s13 == "1"),
    presente_bdap = as.integer(presente_bdap == "1"),
    bdap_record_storicizzato = as.integer(bdap_record_storicizzato == "1"),
    bdap_storicizzazione_ambigua = as.integer(bdap_storicizzazione_ambigua == "1"),
    bdap_n_righe_originali = suppressWarnings(as.integer(bdap_n_righe_originali)),
    
    fg = as.character(fg),
    desc_fg = as.character(desc_fg),
    ateco_bdap = as.character(ateco_bdap),
    descr_ateco_bdap = as.character(descr_ateco_bdap)
  )

# # Controlli preliminari di copertura
# 
# check_pad26_ipa <- pad26_std %>%
#   summarise(
#     n_righe_pad26 = n(),
#     n_enti_pad26 = n_distinct(ente_key, na.rm = TRUE),
#     n_codici_ipa_distinti = n_distinct(codice_ipa_key, na.rm = TRUE),
#     n_codici_ipa_missing = sum(is.na(codice_ipa_key) | codice_ipa_key == ""),
#     quota_codici_ipa_missing = n_codici_ipa_missing / n_righe_pad26
#   )
# 
# check_lista_ipa <- lista_base %>%
#   summarise(
#     n_enti_lista = n(),
#     n_codici_ipa_distinti = n_distinct(codice_ipa_key, na.rm = TRUE),
#     n_codici_ipa_missing = sum(is.na(codice_ipa_key) | codice_ipa_key == ""),
#     quota_codici_ipa_missing = n_codici_ipa_missing / n_enti_lista
#   )
# 
# check_pad26_in_lista <- pad26_std %>%
#   distinct(codice_ipa_key, ente_key) %>%
#   mutate(has_codice_ipa = !is.na(codice_ipa_key) & codice_ipa_key != "") %>%
#   left_join(
#     lista_base %>%
#       distinct(codice_ipa_key) %>%
#       mutate(in_lista = TRUE),
#     by = "codice_ipa_key"
#   ) %>%
#   summarise(
#     n_enti_pad26 = n(),
#     n_enti_pad26_con_ipa = sum(has_codice_ipa),
#     n_enti_pad26_in_lista = sum(in_lista %in% TRUE, na.rm = TRUE),
#     quota_enti_pad26_in_lista = n_enti_pad26_in_lista / n_enti_pad26_con_ipa
#   )
# 
# check_lista_in_pad26 <- lista_base %>%
#   distinct(codice_ipa_key, ragione_sociale) %>%
#   mutate(has_codice_ipa = !is.na(codice_ipa_key) & codice_ipa_key != "") %>%
#   left_join(
#     pad26_std %>%
#       distinct(codice_ipa_key) %>%
#       mutate(in_pad26 = TRUE),
#     by = "codice_ipa_key"
#   ) %>%
#   summarise(
#     n_enti_lista = n(),
#     n_enti_lista_con_ipa = sum(has_codice_ipa),
#     n_enti_lista_in_pad26 = sum(in_pad26 %in% TRUE, na.rm = TRUE),
#     quota_enti_lista_in_pad26 = n_enti_lista_in_pad26 / n_enti_lista_con_ipa
#   )

# write_csv(check_pad26_ipa, file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_codici_ipa.csv"))
# write_csv(check_lista_ipa, file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_codici_ipa.csv"))
# write_csv(check_pad26_in_lista, file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_in_lista.csv"))
# write_csv(check_lista_in_pad26, file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_in_pad26.csv"))
# 
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_codici_ipa.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_codici_ipa.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_in_lista.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_in_pad26.csv"), DRIVE_PAD26_LOGS)

# Crea un log dei duplicati IPA
ipa_ambigui_lista <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  distinct(codice_ipa_key, lista_row_id) %>%
  count(codice_ipa_key, name = "n_enti_lista") %>%
  filter(n_enti_lista > 1)

local_log_ipa_ambigui <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "pad26_codici_ipa_ambigui_lista.csv"
)

# write_csv(ipa_ambigui_lista, local_log_ipa_ambigui)
# drive_upload_or_update(local_log_ipa_ambigui, DRIVE_PAD26_LOGS)

lista_match_ipa <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  anti_join(ipa_ambigui_lista, by = "codice_ipa_key")


# Match per codice IPA, se disponibile
lista_match_ipa <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  distinct(codice_ipa_key, .keep_all = TRUE)

# Match per ragione sociale
lista_keys_den <- lista_base %>%
  transmute(
    lista_row_id,
    chiave_lista_tipo = "ragione_sociale",
    ente_key = ragione_sociale_key
  ) %>%
  filter(!is.na(ente_key), ente_key != "") %>%
  distinct()

chiavi_ambigue_lista_den <- lista_keys_den %>%
  distinct(ente_key, lista_row_id) %>%
  count(ente_key, name = "n_enti_lista") %>%
  filter(n_enti_lista > 1)

local_log_chiavi_ambigue <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "pad26_chiavi_ambigue_lista_ragione_sociale.csv"
)

# write_csv(chiavi_ambigue_lista_den, local_log_chiavi_ambigue)
# drive_upload_or_update(local_log_chiavi_ambigue, DRIVE_PAD26_LOGS)

lista_match_den <- lista_keys_den %>%
  anti_join(chiavi_ambigue_lista_den, by = "ente_key") %>%
  distinct(ente_key, .keep_all = TRUE) %>%
  left_join(lista_base, by = "lista_row_id")

# write_csv(
#   chiavi_ambigue_lista_den,
#   file.path(DIR_LOGS, "pad26_chiavi_ambigue_lista_denominazione.csv")
# )

# local_log_chiavi_ambigue <- file.path(
#   DIR_PAD26_LOGS_LOCAL,
#   "pad26_chiavi_ambigue_lista_denominazione.csv"
# )

# write_csv(chiavi_ambigue_lista_den, local_log_chiavi_ambigue)

# drive_upload_or_update(
#   local_path = local_log_chiavi_ambigue,
#   drive_folder_rel = DRIVE_PAD26_LOGS
# )

lista_match_den <- lista_keys_den %>%
  anti_join(chiavi_ambigue_lista_den, by = "ente_key") %>%
  left_join(lista_base, by = "lista_row_id")

# Porta tutte le variabili della lista nel raccordo.
# Le variabili usate come chiavi o indicatori tecnici non vengono rinominate;
# tutte le altre ricevono il prefisso "lista_" per evitare collisioni con PAD26.

colonne_lista_tecniche <- c(
  "lista_row_id",
  "lista_ind",
  "codice_ipa_key",
  "ragione_sociale_key"
)

# lista_base_match <- lista_base %>%
#   dplyr::rename_with(
#     .fn = ~ paste0("lista_", .x),
#     .cols = -dplyr::any_of(colonne_lista_tecniche)
#   )
# 
# names(lista_base_match)

#==============================================================================#
####       14.1 PREPARAZIONE PAD26 E LOOKUP DELLA LISTA                    ----
#==============================================================================#

# La lista è il database principale:
# - i nomi delle sue colonne rimangono invariati;
# - tutte le colonne provenienti da PAD26 ricevono il prefisso "pad26_".

stopifnot(
  all(c(
    "lista_row_id",
    "lista_ind",
    "codice_ipa_key",
    "ragione_sociale_key"
  ) %in% names(lista_base))
)

stopifnot(
  dplyr::n_distinct(lista_base$lista_row_id) ==
    nrow(lista_base)
)


#------------------------------------------------------------------------------#
# 14.1.1 PAD26 con nomi prefissati
#------------------------------------------------------------------------------#

pad26_base <- pad26_std %>%
  dplyr::mutate(
    pad26_row_id = dplyr::row_number()
  ) %>%
  dplyr::rename_with(
    .fn = ~ paste0("pad26_", .x),
    .cols = -dplyr::any_of("pad26_row_id")
  )

# Esempi:
# codice_ipa_key        -> pad26_codice_ipa_key
# ente_key              -> pad26_ente_key
# dataset_id            -> pad26_dataset_id
# importo_finanziamento -> pad26_importo_finanziamento

stopifnot(
  nrow(pad26_base) == nrow(pad26_std)
)

stopifnot(
  all(c(
    "pad26_row_id",
    "pad26_codice_ipa_key",
    "pad26_ente_key",
    "pad26_dataset_id",
    "pad26_tipo_file_candidatura"
  ) %in% names(pad26_base))
)


#------------------------------------------------------------------------------#
# 14.1.2 Codici IPA ambigui nella lista
#------------------------------------------------------------------------------#

ipa_ambigui_lista <- lista_base %>%
  dplyr::filter(
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  dplyr::distinct(
    codice_ipa_key,
    lista_row_id
  ) %>%
  dplyr::count(
    codice_ipa_key,
    name = "n_enti_lista"
  ) %>%
  dplyr::filter(n_enti_lista > 1L) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_lista),
    codice_ipa_key
  )

#------------------------------------------------------------------------------#
# Lookup univoca della lista per codice fiscale
#------------------------------------------------------------------------------#

cf_ambigui_lista <- lista_base %>%
  dplyr::filter(
    !is.na(codice_fiscale_key),
    codice_fiscale_key != ""
  ) %>%
  dplyr::distinct(
    codice_fiscale_key,
    lista_row_id
  ) %>%
  dplyr::count(
    codice_fiscale_key,
    name = "n_enti_lista"
  ) %>%
  dplyr::filter(n_enti_lista > 1L) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_lista),
    codice_fiscale_key
  )



#------------------------------------------------------------------------------#
# 14.1.3 Lookup univoca della lista per codice IPA
#------------------------------------------------------------------------------#

lista_match_ipa <- lista_base %>%
  dplyr::filter(
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  dplyr::anti_join(
    ipa_ambigui_lista,
    by = "codice_ipa_key"
  ) %>%
  dplyr::distinct(
    codice_ipa_key,
    .keep_all = TRUE
  ) %>%
  dplyr::mutate(
    # Chiave testuale parallela, utile per rendere compatibili
    # le colonne dei due rami del raccordo.
    ente_key = ragione_sociale_key,
    chiave_lista_tipo = "codice_ipa"
  )

stopifnot(
  !anyDuplicated(lista_match_ipa$codice_ipa_key)
)

lista_match_cf <- lista_base %>%
  dplyr::filter(
    !is.na(codice_fiscale_key),
    codice_fiscale_key != ""
  ) %>%
  dplyr::anti_join(
    cf_ambigui_lista,
    by = "codice_fiscale_key"
  ) %>%
  dplyr::distinct(
    codice_fiscale_key,
    .keep_all = TRUE
  ) %>%
  dplyr::mutate(
    ente_key = ragione_sociale_key,
    chiave_lista_tipo = "codice_fiscale"
  )

stopifnot(
  !anyDuplicated(lista_match_cf$codice_fiscale_key)
)

stopifnot(
  "pad26_codice_fiscale_ipa_key" %in%
    names(pad26_base)
)
#------------------------------------------------------------------------------#
# 14.1.4 Chiavi testuali della lista
#------------------------------------------------------------------------------#

lista_keys_den <- lista_base %>%
  dplyr::transmute(
    lista_row_id,
    ente_key = ragione_sociale_key
  ) %>%
  dplyr::filter(
    !is.na(ente_key),
    ente_key != ""
  ) %>%
  dplyr::distinct()


chiavi_ambigue_lista_den <- lista_keys_den %>%
  dplyr::distinct(
    ente_key,
    lista_row_id
  ) %>%
  dplyr::count(
    ente_key,
    name = "n_enti_lista"
  ) %>%
  dplyr::filter(n_enti_lista > 1L) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_lista),
    ente_key
  )


#------------------------------------------------------------------------------#
# 14.1.5 Lookup univoca della lista per denominazione
#------------------------------------------------------------------------------#

lista_match_den <- lista_keys_den %>%
  dplyr::anti_join(
    chiavi_ambigue_lista_den,
    by = "ente_key"
  ) %>%
  dplyr::distinct(
    ente_key,
    .keep_all = TRUE
  ) %>%
  dplyr::left_join(
    lista_base,
    by = "lista_row_id"
  ) %>%
  dplyr::mutate(
    chiave_lista_tipo = "ragione_sociale"
  )

stopifnot(
  !anyDuplicated(lista_match_den$ente_key)
)


#==============================================================================#
####                     14.2 DIAGNOSTICA IPA                              ----
#==============================================================================#

diag_pad26_ipa <- pad26_base %>%
  dplyr::distinct(
    pad26_codice_ipa_key,
    pad26_ente_key,
    pad26_codice_ipa,
    pad26_ente,
    pad26_tipologia_ente,
    pad26_cod_regione,
    pad26_cod_provincia,
    pad26_cod_comune,
    pad26_regione,
    pad26_provincia,
    pad26_comune
  ) %>%
  dplyr::mutate(
    has_ipa_pad26 =
      !is.na(pad26_codice_ipa_key) &
      pad26_codice_ipa_key != ""
  )


diag_lista_ipa <- lista_base %>%
  dplyr::distinct(
    codice_ipa_key,
    codice_ente_ipa,
    ragione_sociale_key,
    ragione_sociale,
    codice_regione,
    codice_provincia,
    codice_comune,
    fg,
    desc_fg,
    presente_mpa,
    presente_s13,
    presente_bdap
  ) %>%
  dplyr::mutate(
    has_ipa_lista =
      !is.na(codice_ipa_key) &
      codice_ipa_key != ""
  )


# IPA presenti in PAD26 ma non nella lista.
log_ipa_pad26_non_in_lista <- diag_pad26_ipa %>%
  dplyr::filter(has_ipa_pad26) %>%
  dplyr::anti_join(
    diag_lista_ipa %>%
      dplyr::filter(has_ipa_lista) %>%
      dplyr::distinct(codice_ipa_key),
    by = c(
      "pad26_codice_ipa_key" = "codice_ipa_key"
    )
  ) %>%
  dplyr::count(
    pad26_tipologia_ente,
    pad26_regione,
    pad26_provincia,
    name = "n_enti_pad26"
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_pad26)
  )


# Stesso codice IPA ma denominazioni molto diverse.
log_ipa_match_denom_differenti <- diag_pad26_ipa %>%
  dplyr::filter(has_ipa_pad26) %>%
  dplyr::inner_join(
    diag_lista_ipa %>%
      dplyr::filter(has_ipa_lista),
    by = c(
      "pad26_codice_ipa_key" = "codice_ipa_key"
    )
  ) %>%
  dplyr::mutate(
    dist_jw = stringdist::stringdist(
      pad26_ente_key,
      ragione_sociale_key,
      method = "jw"
    ),
    score_jw = 1 - dist_jw
  ) %>%
  dplyr::filter(
    score_jw < 0.90 |
      is.na(score_jw)
  ) %>%
  dplyr::select(
    pad26_codice_ipa_key,
    pad26_codice_ipa,
    codice_ente_ipa,
    pad26_ente,
    ragione_sociale,
    pad26_tipologia_ente,
    desc_fg,
    pad26_regione,
    pad26_provincia,
    pad26_comune,
    score_jw
  ) %>%
  dplyr::arrange(score_jw)

log_ipa_incoerenze_territoriali <- diag_pad26_ipa %>%
  dplyr::filter(has_ipa_pad26) %>%
  dplyr::inner_join(
    diag_lista_ipa %>%
      dplyr::filter(has_ipa_lista),
    by = c(
      "pad26_codice_ipa_key" = "codice_ipa_key"
    )
  ) %>%
  dplyr::mutate(
    # Confronti effettuati solo quando entrambe le fonti
    # contengono una variabile valorizzata.
    regione_confrontabile =
      !is.na(pad26_cod_regione) &
      pad26_cod_regione != "" &
      !is.na(codice_regione) &
      codice_regione != "",
    
    provincia_confrontabile =
      !is.na(pad26_cod_provincia) &
      pad26_cod_provincia != "" &
      !is.na(codice_provincia) &
      codice_provincia != "",
    
    comune_confrontabile =
      !is.na(pad26_cod_comune) &
      pad26_cod_comune != "" &
      !is.na(codice_comune) &
      codice_comune != "",
    
    regione_coerente = dplyr::case_when(
      !regione_confrontabile ~ NA,
      pad26_cod_regione == codice_regione ~ TRUE,
      TRUE ~ FALSE
    ),
    
    provincia_coerente = dplyr::case_when(
      !provincia_confrontabile ~ NA,
      pad26_cod_provincia == codice_provincia ~ TRUE,
      TRUE ~ FALSE
    ),
    
    comune_coerente = dplyr::case_when(
      !comune_confrontabile ~ NA,
      pad26_cod_comune == codice_comune ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::filter(
    regione_coerente %in% FALSE |
      provincia_coerente %in% FALSE |
      comune_coerente %in% FALSE
  ) %>%
  dplyr::select(
    pad26_codice_ipa_key,
    pad26_codice_ipa,
    codice_ente_ipa,
    pad26_ente,
    ragione_sociale,
    pad26_tipologia_ente,
    desc_fg,
    
    pad26_cod_regione,
    codice_regione,
    regione_coerente,
    
    pad26_cod_provincia,
    codice_provincia,
    provincia_coerente,
    
    pad26_cod_comune,
    codice_comune,
    comune_coerente
  ) %>%
  dplyr::arrange(
    regione_coerente,
    provincia_coerente,
    comune_coerente
  )

#==============================================================================#
####          15. RACCORDO: PRIMA IPA, POI DENOMINAZIONE                  ----
#==============================================================================#

#------------------------------------------------------------------------------#
# 15.1 Match forte per codice IPA
#------------------------------------------------------------------------------#

pad26_match_ipa <- pad26_base %>%
  dplyr::left_join(
    lista_match_ipa,
    by = c(
      "pad26_codice_ipa_key" = "codice_ipa_key"
    ),
    keep = TRUE
  ) %>%
  dplyr::mutate(
    match_lista_ipa = dplyr::if_else(
      !is.na(lista_ind),
      1L,
      0L
    )
  )

# keep = TRUE conserva:
# - pad26_codice_ipa_key: codice proveniente da PAD26;
# - codice_ipa_key: codice proveniente dalla lista.

stopifnot(
  nrow(pad26_match_ipa) ==
    nrow(pad26_base)
)


#------------------------------------------------------------------------------#
# 15.2 Match per codice fiscale sui record non abbinati via IPA
#------------------------------------------------------------------------------#

pad26_non_match_ipa <- pad26_match_ipa %>%
  dplyr::filter(match_lista_ipa == 0L) %>%
  dplyr::select(
    dplyr::all_of(names(pad26_base))
  )


pad26_match_cf <- pad26_non_match_ipa %>%
  dplyr::left_join(
    lista_match_cf,
    by = c(
      "pad26_codice_fiscale_ipa_key" =
        "codice_fiscale_key"
    ),
    keep = TRUE
  ) %>%
  dplyr::mutate(
    match_lista_ipa = 0L,
    
    match_lista_cf = dplyr::if_else(
      !is.na(lista_ind),
      1L,
      0L
    )
  )

stopifnot(
  nrow(pad26_match_cf) ==
    nrow(pad26_non_match_ipa)
)

#------------------------------------------------------------------------------#
# 15.3 Match per denominazione sui record non abbinati via IPA o CF
#------------------------------------------------------------------------------#

pad26_non_match_cf <- pad26_match_cf %>%
  dplyr::filter(match_lista_cf == 0L) %>%
  dplyr::select(
    dplyr::all_of(names(pad26_base))
  )


pad26_match_den <- pad26_non_match_cf %>%
  dplyr::left_join(
    lista_match_den,
    by = c(
      "pad26_ente_key" = "ente_key"
    ),
    keep = TRUE
  ) %>%
  dplyr::mutate(
    match_lista_ipa = 0L,
    match_lista_cf = 0L,
    
    match_lista_den = dplyr::if_else(
      !is.na(lista_ind),
      1L,
      0L
    )
  )

stopifnot(
  nrow(pad26_match_den) ==
    nrow(pad26_non_match_cf)
)

#------------------------------------------------------------------------------#
# 15.4 Ricomposizione
#------------------------------------------------------------------------------#

pad26_match_ipa_ok <- pad26_match_ipa %>%
  dplyr::filter(match_lista_ipa == 1L) %>%
  dplyr::mutate(
    match_lista_cf = 0L,
    match_lista_den = 0L
  )


pad26_match_cf_ok <- pad26_match_cf %>%
  dplyr::filter(match_lista_cf == 1L) %>%
  dplyr::mutate(
    match_lista_den = 0L
  )


pad26_raccordato <- dplyr::bind_rows(
  pad26_match_ipa_ok,
  pad26_match_cf_ok,
  pad26_match_den
) %>%
  dplyr::arrange(pad26_row_id) %>%
  dplyr::mutate(
    pad26_ind = 1L,
    
    match_lista = dplyr::if_else(
      !is.na(lista_ind),
      1L,
      0L
    ),
    
    tipo_match = dplyr::case_when(
      match_lista_ipa == 1L ~
        "match_codice_ipa",
      
      match_lista_cf == 1L ~
        "match_codice_fiscale",
      
      match_lista_den == 1L ~
        "match_ragione_sociale",
      
      TRUE ~
        "no_match"
    ),
    
    ente_pad26_diag_key = dplyr::case_when(
      !is.na(pad26_codice_ipa_key) &
        pad26_codice_ipa_key != "" ~
        paste0("IPA:", pad26_codice_ipa_key),
      
      !is.na(pad26_ente_key) &
        pad26_ente_key != "" ~
        paste0("DEN:", pad26_ente_key),
      
      TRUE ~
        paste0("ROW:", pad26_row_id)
    )
  )

#------------------------------------------------------------------------------#
# 15.3.1 Controlli strutturali
#------------------------------------------------------------------------------#

stopifnot(
  nrow(pad26_raccordato) ==
    nrow(pad26_std)
)

stopifnot(
  dplyr::n_distinct(
    pad26_raccordato$pad26_row_id
  ) == nrow(pad26_std)
)

stopifnot(
  all(
    pad26_raccordato$match_lista %in%
      c(0L, 1L)
  )
)

stopifnot(
  all(
    pad26_raccordato$tipo_match %in%
      c(
        "match_codice_ipa",
        "match_codice_fiscale",
        "match_ragione_sociale",
        "no_match"
      )
  )
)

# Tutte le colonne originali della lista devono essere presenti
# con il loro nome originale.
colonne_lista_mancanti <- setdiff(
  names(lista_base),
  names(pad26_raccordato)
)

if (length(colonne_lista_mancanti) > 0L) {
  warning(
    "Colonne della lista non presenti nel raccordo: ",
    paste(
      colonne_lista_mancanti,
      collapse = ", "
    )
  )
}


# Tutte le colonne di PAD26 devono essere presenti con prefisso pad26_.
colonne_pad26_attese <- c(
  "pad26_row_id",
  paste0(
    "pad26_",
    names(pad26_std)
  )
)

colonne_pad26_mancanti <- setdiff(
  colonne_pad26_attese,
  names(pad26_raccordato)
)

if (length(colonne_pad26_mancanti) > 0L) {
  warning(
    "Colonne PAD26 non presenti nel raccordo: ",
    paste(
      colonne_pad26_mancanti,
      collapse = ", "
    )
  )
}



#==============================================================================#
#### 15.3.2 SINTESI DEI TIPI DI MATCH                                     ----
#==============================================================================#

# Questa tabella è solo diagnostica.
# Conta quante candidature vengono abbinate tramite:
# - codice IPA;
# - codice fiscale;
# - denominazione;
# - nessun match.

log_tipo_match <- pad26_raccordato %>%
  dplyr::count(
    pad26_dataset_id,
    tipo_match,
    name = "n_candidature"
  ) %>%
  dplyr::group_by(
    pad26_dataset_id
  ) %>%
  dplyr::mutate(
    quota_candidature =
      n_candidature / sum(n_candidature)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    pad26_dataset_id,
    tipo_match
  )


# Controllo specifico sul contributo del codice fiscale.
check_match_codice_fiscale <- pad26_raccordato %>%
  dplyr::summarise(
    n_candidature_totali =
      dplyr::n(),
    
    n_match_codice_ipa =
      sum(
        tipo_match == "match_codice_ipa",
        na.rm = TRUE
      ),
    
    n_match_codice_fiscale =
      sum(
        tipo_match == "match_codice_fiscale",
        na.rm = TRUE
      ),
    
    n_match_denominazione =
      sum(
        tipo_match == "match_ragione_sociale",
        na.rm = TRUE
      ),
    
    n_non_match =
      sum(
        tipo_match == "no_match",
        na.rm = TRUE
      )
  )

print(log_tipo_match)
print(check_match_codice_fiscale)


#==============================================================================#
#### 15.3.3 DATABASE FINALE: LISTA CON TUTTE LE CANDIDATURE PAD26         ----
#==============================================================================#

# Seleziona dal raccordo soltanto:
# - identificativo della riga della lista;
# - tutte le variabili PAD26;
# - indicatori del match.
#
# Non portiamo di nuovo le variabili della lista perché verranno prese
# direttamente da lista_base nel left_join successivo.

pad26_per_lista_long <- pad26_raccordato %>%
  dplyr::filter(
    match_lista == 1L,
    !is.na(lista_row_id)
  ) %>%
  dplyr::select(
    lista_row_id,
    
    dplyr::starts_with("pad26_"),
    
    match_lista_ipa,
    match_lista_cf,
    match_lista_den,
    match_lista,
    tipo_match,
    
    ente_pad26_diag_key
  )


# Database finale desiderato:
# parte da tutte le osservazioni della lista e aggiunge le candidature PAD26.
#
# - un ente con 10 candidature avrà 10 righe;
# - un ente senza candidature manterrà una riga con variabili PAD26 mancanti.

lista_pad26_long <- lista_base %>%
  dplyr::left_join(
    pad26_per_lista_long,
    by = "lista_row_id",
    relationship = "one-to-many"
  ) %>%
  dplyr::mutate(
    in_pad26 = dplyr::if_else(
      !is.na(pad26_row_id),
      1L,
      0L
    )
  ) %>%
  dplyr::arrange(
    lista_row_id,
    pad26_dataset_id,
    pad26_data_invio_candidatura
  )


#------------------------------------------------------------------------------#
# Controlli sul database finale
#------------------------------------------------------------------------------#

# Tutti gli enti della lista devono essere ancora presenti.
stopifnot(
  dplyr::n_distinct(
    lista_pad26_long$lista_row_id
  ) ==
    nrow(lista_base)
)

# Il database long non può avere meno righe della lista.
stopifnot(
  nrow(lista_pad26_long) >=
    nrow(lista_base)
)

# Ogni riga della lista deve comparire almeno una volta.
check_presenza_lista_nel_long <- lista_base %>%
  dplyr::anti_join(
    lista_pad26_long %>%
      dplyr::distinct(lista_row_id),
    by = "lista_row_id"
  )

stopifnot(
  nrow(check_presenza_lista_nel_long) == 0L
)


message(
  "Enti nella lista: ",
  nrow(lista_base)
)

message(
  "Righe nel database lista-PAD26 long: ",
  nrow(lista_pad26_long)
)

message(
  "Enti della lista con almeno una candidatura PAD26: ",
  dplyr::n_distinct(
    lista_pad26_long$lista_row_id[
      lista_pad26_long$in_pad26 == 1L
    ],
    na.rm = TRUE
  )
)


#==============================================================================#
#### 15.3.4 PAD26 NON MATCHATE: TUTTE LE VARIABILI                        ----
#==============================================================================#

# Mantiene ogni candidatura PAD26 non abbinata alla lista.
# Non è una tabella aggregata: contiene tutte le variabili disponibili,
# comprese quelle aggiunte tramite l'anagrafica IPA nello script 01.

log_pad26_non_match_full <- pad26_raccordato %>%
  dplyr::filter(
    match_lista == 0L
  ) %>%
  dplyr::arrange(
    pad26_dataset_id,
    pad26_codice_categoria_ipa,
    pad26_tipologia_ente,
    pad26_ente,
    pad26_avviso
  )


# Controllo: deve contenere lo stesso numero di candidature non matchate
# rilevato nel raccordo.

stopifnot(
  nrow(log_pad26_non_match_full) ==
    sum(
      pad26_raccordato$match_lista == 0L,
      na.rm = TRUE
    )
)


# Sintesi delle PAD26 non matchate per categoria ufficiale IPA.
# Serve per individuare famiglie prevalenti, per esempio:
# unioni di comuni, consorzi, aziende di servizi alla persona, ecc.

log_pad26_non_match_per_categoria <- log_pad26_non_match_full %>%
  dplyr::group_by(
    pad26_codice_categoria_ipa,
    pad26_nome_categoria_ipa,
    pad26_tipologia_ente
  ) %>%
  dplyr::summarise(
    n_candidature =
      dplyr::n(),
    
    n_enti_pad26 =
      dplyr::n_distinct(
        ente_pad26_diag_key,
        na.rm = TRUE
      ),
    
    n_codici_ipa =
      dplyr::n_distinct(
        pad26_codice_ipa_key,
        na.rm = TRUE
      ),
    
    n_codici_fiscali =
      dplyr::n_distinct(
        pad26_codice_fiscale_ipa_key,
        na.rm = TRUE
      ),
    
    importo_totale =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_pad26),
    dplyr::desc(n_candidature)
  )




#==============================================================================#
#### 15.3.2 CANDIDATI PER COMUNI NON MATCHATI                              ----
#==============================================================================#

pad26_comuni_non_match <- pad26_raccordato %>%
  dplyr::filter(
    match_lista == 0L,
    pad26_tipologia_ente == "Comuni"
  ) %>%
  dplyr::distinct(
    pad26_codice_ipa_key,
    pad26_codice_ipa,
    pad26_ente,
    pad26_ente_key,
    pad26_cod_regione,
    pad26_cod_provincia,
    pad26_cod_comune,
    pad26_regione,
    pad26_provincia,
    pad26_comune
  )


lista_comuni_candidati <- lista_base %>%
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(
        dplyr::coalesce(desc_fg, "")
      ),
      "\\bcomune\\b"
    )
  ) %>%
  dplyr::select(
    lista_row_id,
    codice_ipa_key,
    codice_ente_ipa,
    ragione_sociale,
    ragione_sociale_key,
    codice_regione,
    codice_provincia,
    codice_comune,
    fg,
    desc_fg
  )


candidati_match_comuni <- pad26_comuni_non_match %>%
  dplyr::inner_join(
    lista_comuni_candidati,
    by = c(
      "pad26_cod_regione" = "codice_regione"
    ),
    suffix = c("_pad26", "_lista")
  ) %>%
  dplyr::filter(
    # Se entrambe le province sono valorizzate, devono coincidere.
    is.na(pad26_cod_provincia) |
      pad26_cod_provincia == "" |
      is.na(codice_provincia) |
      codice_provincia == "" |
      pad26_cod_provincia == codice_provincia
  ) %>%
  dplyr::mutate(
    score_jw = 1 - stringdist::stringdist(
      pad26_ente_key,
      ragione_sociale_key,
      method = "jw"
    )
  ) %>%
  dplyr::group_by(
    pad26_codice_ipa_key,
    pad26_ente
  ) %>%
  dplyr::arrange(
    dplyr::desc(score_jw),
    .by_group = TRUE
  ) %>%
  dplyr::mutate(
    rank_candidato = dplyr::row_number(),
    score_secondo = dplyr::lead(score_jw),
    margine_primo_secondo = score_jw - score_secondo
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(rank_candidato <= 5L) %>%
  dplyr::select(
    pad26_codice_ipa_key,
    pad26_codice_ipa,
    pad26_ente,
    pad26_regione,
    pad26_provincia,
    pad26_comune,
    pad26_cod_comune,
    
    lista_row_id,
    codice_ipa_key,
    codice_ente_ipa,
    ragione_sociale,
    codice_provincia,
    codice_comune,
    
    score_jw,
    rank_candidato,
    margine_primo_secondo
  )

raccordo_manual_comuni <- tibble::tribble(
  ~pad26_codice_ipa_key, ~lista_row_id, ~motivo_raccordo
  # "vecchio_codice",    123L,          "fusione comuni"
)


#==============================================================================#
#### 15.4 PARTECIPAZIONE DELLA LISTA                                      ----
#==============================================================================#

# Sintesi delle candidature per ciascuna riga della lista.

partecipazione_pad26_per_lista <- pad26_raccordato %>%
  dplyr::filter(
    match_lista == 1L,
    !is.na(lista_row_id)
  ) %>%
  dplyr::group_by(
    lista_row_id
  ) %>%
  dplyr::summarise(
    in_pad26 = 1L,
    
    n_candidature_pad26 =
      dplyr::n(),
    
    n_misure_pad26 =
      dplyr::n_distinct(
        pad26_avviso,
        na.rm = TRUE
      ),
    
    n_dataset_pad26 =
      dplyr::n_distinct(
        pad26_dataset_id,
        na.rm = TRUE
      ),
    
    importo_finanziato_pad26 =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    tipi_match_pad26 = paste(
      sort(unique(tipo_match)),
      collapse = " | "
    ),
    
    .groups = "drop"
  )


# Una sola riga per ogni osservazione della lista.

lista_pad26_master <- lista_base %>%
  dplyr::left_join(
    partecipazione_pad26_per_lista,
    by = "lista_row_id"
  ) %>%
  dplyr::mutate(
    has_codice_ipa_lista =
      !is.na(codice_ipa_key) &
      codice_ipa_key != "",
    
    in_pad26 =
      tidyr::replace_na(in_pad26, 0L),
    
    n_candidature_pad26 =
      tidyr::replace_na(n_candidature_pad26, 0L),
    
    n_misure_pad26 =
      tidyr::replace_na(n_misure_pad26, 0L),
    
    n_dataset_pad26 =
      tidyr::replace_na(n_dataset_pad26, 0L),
    
    importo_finanziato_pad26 =
      tidyr::replace_na(
        importo_finanziato_pad26,
        0
      )
  )


stopifnot(
  nrow(lista_pad26_master) ==
    nrow(lista_base)
)



log_partecipazione_lista_pad26 <- lista_pad26_master %>%
  dplyr::summarise(
    n_enti_lista = dplyr::n(),
    
    n_enti_lista_con_codice_ipa =
      sum(
        has_codice_ipa_lista,
        na.rm = TRUE
      ),
    
    n_enti_lista_senza_codice_ipa =
      sum(
        !has_codice_ipa_lista,
        na.rm = TRUE
      ),
    
    n_enti_lista_in_pad26 =
      sum(
        in_pad26 == 1L,
        na.rm = TRUE
      ),
    
    n_enti_lista_non_in_pad26 =
      sum(
        in_pad26 == 0L,
        na.rm = TRUE
      ),
    
    quota_enti_lista_in_pad26_su_totale =
      n_enti_lista_in_pad26 /
      n_enti_lista,
    
    quota_enti_lista_in_pad26_su_enti_con_ipa =
      dplyr::if_else(
        n_enti_lista_con_codice_ipa > 0,
        n_enti_lista_in_pad26 /
          n_enti_lista_con_codice_ipa,
        NA_real_
      )
  )


# Tutti gli enti della lista che non hanno trovato candidature PAD26,
# mantenendo tutte le variabili originarie della lista.

log_lista_non_in_pad26_full <- lista_pad26_master %>%
  dplyr::filter(
    in_pad26 == 0L
  ) %>%
  dplyr::arrange(
    desc_fg,
    ragione_sociale
  )

log_lista_senza_codice_ipa <- lista_base %>%
  dplyr::filter(
    is.na(codice_ipa_key) |
      codice_ipa_key == ""
  ) %>%
  dplyr::select(
    lista_row_id,
    codice_fiscale,
    ragione_sociale,
    fg,
    desc_fg,
    codice_reg,
    presente_mpa,
    presente_s13,
    presente_bdap,
    fonte_ragione_sociale,
    fonte_fg
  ) %>%
  dplyr::arrange(ragione_sociale)


#==============================================================================#
#### 15.4.1 COPERTURA DELLA LISTA PER FORMA GIURIDICA                     ----
#==============================================================================#



# IPA presenti nella lista ma non in PAD26.
log_copertura_lista_per_desc_fg <- lista_pad26_master %>%
  dplyr::group_by(desc_fg) %>%
  dplyr::summarise(
    n_enti_lista =
      dplyr::n(),
    
    n_enti_lista_matchati =
      sum(in_pad26 == 1L, na.rm = TRUE),
    
    n_enti_lista_non_matchati =
      sum(in_pad26 == 0L, na.rm = TRUE),
    
    quota_enti_lista_matchati =
      n_enti_lista_matchati /
      n_enti_lista,
    
    n_enti_mpa =
      sum(presente_mpa == 1L, na.rm = TRUE),
    
    n_enti_mpa_matchati =
      sum(
        presente_mpa == 1L &
          in_pad26 == 1L,
        na.rm = TRUE
      ),
    
    quota_enti_mpa_matchati =
      dplyr::if_else(
        n_enti_mpa > 0,
        n_enti_mpa_matchati /
          n_enti_mpa,
        NA_real_
      ),
    
    n_enti_s13 =
      sum(presente_s13 == 1L, na.rm = TRUE),
    
    n_enti_s13_matchati =
      sum(
        presente_s13 == 1L &
          in_pad26 == 1L,
        na.rm = TRUE
      ),
    
    n_enti_bdap =
      sum(presente_bdap == 1L, na.rm = TRUE),
    
    n_enti_bdap_matchati =
      sum(
        presente_bdap == 1L &
          in_pad26 == 1L,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("quota_"),
      ~ round(.x, 4)
    )
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_enti_lista)
  )


#==============================================================================#
####          15.5 COPERTURA COMPLESSIVA PAD26–LISTA                       ----
#==============================================================================#

log_copertura_pad26_lista <- pad26_raccordato %>%
  dplyr::summarise(
    n_candidature_pad26 =
      dplyr::n(),
    
    n_candidature_match_lista =
      sum(
        match_lista == 1L,
        na.rm = TRUE
      ),
    
    n_candidature_non_match_lista =
      sum(
        match_lista == 0L,
        na.rm = TRUE
      ),
    
    quota_candidature_match_lista =
      n_candidature_match_lista /
      n_candidature_pad26,
    
    n_enti_pad26 =
      dplyr::n_distinct(
        ente_pad26_diag_key,
        na.rm = TRUE
      ),
    
    n_enti_pad26_match_lista =
      dplyr::n_distinct(
        ente_pad26_diag_key[
          match_lista == 1L
        ],
        na.rm = TRUE
      ),
    
    n_enti_pad26_non_match_lista =
      dplyr::n_distinct(
        ente_pad26_diag_key[
          match_lista == 0L
        ],
        na.rm = TRUE
      ),
    
    quota_enti_pad26_match_lista =
      n_enti_pad26_match_lista /
      n_enti_pad26,
    
    importo_totale_pad26 =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    importo_match_lista =
      sum(
        pad26_importo_finanziamento[
          match_lista == 1L
        ],
        na.rm = TRUE
      ),
    
    importo_non_match_lista =
      sum(
        pad26_importo_finanziamento[
          match_lista == 0L
        ],
        na.rm = TRUE
      ),
    
    quota_importo_match_lista =
      dplyr::if_else(
        importo_totale_pad26 > 0,
        importo_match_lista /
          importo_totale_pad26,
        NA_real_
      )
  )


#==============================================================================#
####             15.6 COPERTURA PAD26 PER DATASET_ID                       ----
#==============================================================================#

log_copertura_pad26_per_dataset <- pad26_raccordato %>%
  dplyr::group_by(
    pad26_tipo_file_candidatura,
    pad26_dataset_id
  ) %>%
  dplyr::summarise(
    n_candidature =
      dplyr::n(),
    
    n_candidature_match =
      sum(
        match_lista == 1L,
        na.rm = TRUE
      ),
    
    n_candidature_non_match =
      sum(
        match_lista == 0L,
        na.rm = TRUE
      ),
    
    quota_candidature_match =
      n_candidature_match /
      n_candidature,
    
    n_candidature_match_ipa =
      sum(
        match_lista_ipa == 1L,
        na.rm = TRUE
      ),
    
    n_candidature_match_cf =
      sum(match_lista_cf == 1L, 
          na.rm = TRUE
      ),
    
    n_candidature_match_den =
      sum(
        match_lista_den == 1L,
        na.rm = TRUE
      ),
    
    quota_match_ipa_su_match =
      dplyr::if_else(
        n_candidature_match > 0,
        n_candidature_match_ipa /
          n_candidature_match,
        NA_real_
      ),
    
    quota_match_cf_su_match =
      dplyr::if_else(
        n_candidature_match > 0,
        n_candidature_match_cf /
          n_candidature_match,
        NA_real_
      ),
    
    quota_match_den_su_match =
      dplyr::if_else(
        n_candidature_match > 0,
        n_candidature_match_den /
          n_candidature_match,
        NA_real_
      ),
    
    n_enti_pad26 =
      dplyr::n_distinct(
        ente_pad26_diag_key,
        na.rm = TRUE
      ),
    
    n_enti_match_lista =
      dplyr::n_distinct(
        ente_pad26_diag_key[
          match_lista == 1L
        ],
        na.rm = TRUE
      ),
    
    n_enti_non_match_lista =
      dplyr::n_distinct(
        ente_pad26_diag_key[
          match_lista == 0L
        ],
        na.rm = TRUE
      ),
    
    quota_enti_match_lista =
      n_enti_match_lista /
      n_enti_pad26,
    
    n_codici_ipa_pad26 =
      dplyr::n_distinct(
        pad26_codice_ipa_key[
          !is.na(pad26_codice_ipa_key) &
            pad26_codice_ipa_key != ""
        ],
        na.rm = TRUE
      ),
    
    n_codici_ipa_match =
      dplyr::n_distinct(
        pad26_codice_ipa_key[
          match_lista == 1L &
            !is.na(pad26_codice_ipa_key) &
            pad26_codice_ipa_key != ""
        ],
        na.rm = TRUE
      ),
    
    quota_codici_ipa_match =
      dplyr::if_else(
        n_codici_ipa_pad26 > 0,
        n_codici_ipa_match /
          n_codici_ipa_pad26,
        NA_real_
      ),
    
    importo_totale =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    importo_match =
      sum(
        pad26_importo_finanziamento[
          match_lista == 1L
        ],
        na.rm = TRUE
      ),
    
    importo_non_match =
      sum(
        pad26_importo_finanziamento[
          match_lista == 0L
        ],
        na.rm = TRUE
      ),
    
    quota_importo_match =
      dplyr::if_else(
        importo_totale > 0,
        importo_match /
          importo_totale,
        NA_real_
      ),
    
    tipi_match = paste(
      sort(unique(tipo_match)),
      collapse = " | "
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("quota_"),
      ~ round(.x, 4)
    )
  ) %>%
  dplyr::arrange(
    pad26_tipo_file_candidatura,
    pad26_dataset_id
  )


#==============================================================================#
####              15.7 DETTAGLIO NON-MATCH PER DATASET                     ----
#==============================================================================#

collapse_examples <- function(x, n_max = 5L) {
  
  x <- sort(
    unique(
      stats::na.omit(x)
    )
  )
  
  if (length(x) == 0L) {
    return(NA_character_)
  }
  
  paste(
    utils::head(
      x,
      n_max
    ),
    collapse = " | "
  )
}


log_non_match_per_dataset <- pad26_raccordato %>%
  dplyr::filter(match_lista == 0L) %>%
  dplyr::group_by(
    pad26_tipo_file_candidatura,
    pad26_dataset_id,
    pad26_codice_ipa,
    pad26_codice_ipa_key,
    pad26_ente_key,
    pad26_tipologia_ente,
    pad26_comune,
    pad26_provincia,
    pad26_regione,
    pad26_cod_comune,
    pad26_cod_provincia,
    pad26_cod_regione
  ) %>%
  dplyr::summarise(
    n_candidature =
      dplyr::n(),
    
    n_misure =
      dplyr::n_distinct(
        pad26_avviso,
        na.rm = TRUE
      ),
    
    importo_totale =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    esempi_ente =
      collapse_examples(
        pad26_ente,
        n_max = 5L
      ),
    
    avvisi =
      collapse_examples(
        pad26_avviso,
        n_max = 10L
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    pad26_tipo_file_candidatura,
    pad26_dataset_id,
    dplyr::desc(importo_totale),
    dplyr::desc(n_candidature)
  )


log_pad26_non_match_lista <-
  log_non_match_per_dataset

#==============================================================================#
#### 15.7.1 ENTI PAD26 NON MATCHATI: DETTAGLIO                            ----
#==============================================================================#

log_enti_pad26_non_match_dettaglio <- pad26_raccordato %>%
  dplyr::filter(match_lista == 0L) %>%
  dplyr::group_by(
    pad26_dataset_id,
    pad26_codice_ipa_key,
    pad26_codice_ipa,
    pad26_ente,
    pad26_ente_key,
    pad26_tipologia_ente,
    pad26_regione,
    pad26_provincia,
    pad26_comune,
    pad26_cod_regione,
    pad26_cod_provincia,
    pad26_cod_comune
  ) %>%
  dplyr::summarise(
    n_candidature = dplyr::n(),
    
    n_misure =
      dplyr::n_distinct(
        pad26_avviso,
        na.rm = TRUE
      ),
    
    importo_totale =
      sum(
        pad26_importo_finanziamento,
        na.rm = TRUE
      ),
    
    avvisi =
      collapse_examples(
        pad26_avviso,
        n_max = 10L
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    pad26_dataset_id,
    pad26_tipologia_ente,
    dplyr::desc(importo_totale),
    pad26_ente
  )

#==============================================================================#
####                  9. SINTESI MATCH PER DATASET                         ----
#==============================================================================#

log_match_pad26 <- log_copertura_pad26_per_dataset %>%
  dplyr::transmute(
    tipo_file_candidatura =
      pad26_tipo_file_candidatura,
    
    dataset_id =
      pad26_dataset_id,
    
    n_candidature,
    n_candidature_match,
    n_candidature_non_match,
    
    quota_match =
      quota_candidature_match,
    
    quota_match_pct =
      round(
        100 * quota_candidature_match,
        1
      ),
    
    n_enti_pad26,
    
    n_enti_match =
      n_enti_match_lista,
    
    quota_enti_match_pct =
      round(
        100 * quota_enti_match_lista,
        1
      ),
    
    importo_totale,
    importo_match,
    importo_non_match,
    
    quota_importo_match_pct =
      round(
        100 * quota_importo_match,
        1
      ),
    
    n_candidature_match_ipa,
    n_candidature_match_den,
    tipi_match
  )


#==============================================================================#
#### 16) OUTPUT PROCESSED, CONTROLLI E METADATI                         ----
#==============================================================================#

# Il file canonico del raccordo è lista_pad26_long:
# - una riga per ente-candidatura;
# - tutti gli enti della lista sono mantenuti;
# - le informazioni ufficiali degli avvisi sono prefissate pad26_.
#
# lista_pad26_master e dim_avvisi_padigitale2026 sono dimensioni di supporto.

oggetti_output_attesi <- c(
  "lista_pad26_long",
  "lista_pad26_master",
  "dim_avvisi_padigitale2026",
  "log_pad26_non_match_full",
  "log_lista_non_in_pad26_full"
)

oggetti_output_mancanti <- oggetti_output_attesi[
  !vapply(
    oggetti_output_attesi,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(oggetti_output_mancanti) > 0L) {
  stop(
    "Oggetti output mancanti prima del salvataggio: ",
    paste(oggetti_output_mancanti, collapse = ", ")
  )
}

salva_processed <- function(
    obj,
    base_filename,
    formati = c("rds", "json")
) {
  paths <- character()
  
  if ("rds" %in% formati) {
    p <- file.path(
      DIR_PAD26_PROCESSED_LOCAL,
      paste0(base_filename, ".rds")
    )
    saveRDS(obj, p)
    paths <- c(paths, p)
  }
  
  if ("json" %in% formati) {
    p <- file.path(
      DIR_PAD26_PROCESSED_LOCAL,
      paste0(base_filename, ".json")
    )
    jsonlite::write_json(
      x = obj,
      path = p,
      dataframe = "rows",
      na = "null",
      null = "null",
      pretty = FALSE,
      auto_unbox = TRUE,
      digits = NA,
      Date = "ISO8601",
      POSIXt = "ISO8601"
    )
    paths <- c(paths, p)
  }
  
  if ("csv" %in% formati) {
    p <- file.path(
      DIR_PAD26_PROCESSED_LOCAL,
      paste0(base_filename, ".csv")
    )
    readr::write_csv(obj, p, na = "")
    paths <- c(paths, p)
  }
  
  if (!all(file.exists(paths))) {
    stop(
      "Errore nel salvataggio di ",
      base_filename,
      ": ",
      paste(paths[!file.exists(paths)], collapse = ", ")
    )
  }
  
  purrr::walk(
    paths,
    ~ drive_upload_or_update(
      local_path = .x,
      drive_folder_rel = DRIVE_PAD26_PROCESSED
    )
  )
  
  message("Salvato processed: ", base_filename)
  invisible(paths)
}

# Dataset canonico e dimensioni di supporto.
paths_lista_long <- salva_processed(
  lista_pad26_long,
  "lista_pad26_long",
  formati = c("rds", "json")
)

paths_lista_master <- salva_processed(
  lista_pad26_master,
  "lista_pad26_master",
  formati = c("rds", "json")
)

paths_dim_avvisi <- salva_processed(
  dim_avvisi_padigitale2026,
  "dim_avvisi_padigitale2026",
  formati = c("rds", "json", "csv")
)

# File diagnostici di dettaglio, mantenuti nella stessa cartella processed.
paths_pad26_non_match <- salva_processed(
  log_pad26_non_match_full,
  "pad26_non_match_lista",
  formati = c("rds")
)

paths_lista_non_pad26 <- salva_processed(
  log_lista_non_in_pad26_full,
  "lista_non_in_pad26",
  formati = c("rds")
)


#==============================================================================#
#### 16.1 WORKBOOK UNICO DEI CONTROLLI                                    ----
#==============================================================================#

# Controlli specifici sul raccordo avvisi.
sintesi_raccordo_avvisi <- check_join_avvisi %>%
  dplyr::mutate(
    quota_match_avviso_pct =
      round(100 * quota_match_avviso, 2)
  )

# Controlli residuali sul codice fiscale.
check_cf_residui <- log_pad26_non_match_full %>%
  dplyr::filter(
    !is.na(pad26_codice_fiscale_ipa_key),
    pad26_codice_fiscale_ipa_key != ""
  ) %>%
  dplyr::mutate(
    cf_presente_in_lista =
      pad26_codice_fiscale_ipa_key %in%
      lista_base$codice_fiscale_key,
    cf_ambiguo_in_lista =
      pad26_codice_fiscale_ipa_key %in%
      cf_ambigui_lista$codice_fiscale_key
  ) %>%
  dplyr::count(
    cf_presente_in_lista,
    cf_ambiguo_in_lista,
    name = "n_candidature"
  )

residui_cf_matchabili <- log_pad26_non_match_full %>%
  dplyr::filter(
    !is.na(pad26_codice_fiscale_ipa_key),
    pad26_codice_fiscale_ipa_key != ""
  ) %>%
  dplyr::inner_join(
    lista_match_cf %>%
      dplyr::select(
        codice_fiscale_key,
        lista_row_id,
        ragione_sociale
      ),
    by = c(
      "pad26_codice_fiscale_ipa_key" =
        "codice_fiscale_key"
    )
  )

log_list <- list(
  "01_avvisi_sintesi" =
    sintesi_raccordo_avvisi,
  
  "02_avvisi_match_pre" =
    sintesi_match_avvisi_pre,
  
  "03_avvisi_nonmatch_pre" =
    non_match_avvisi_pre,
  
  "04_avvisi_match_post" =
    check_match_avvisi_post,
  
  "05_avvisi_nonmatch_post" =
    non_match_avvisi_post,
  
  "06_avvisi_racc_manual" =
    raccordo_avvisi_manual,
  
  "07_avvisi_duplicati" =
    duplicati_avvisi,
  
  "08_tipo_match_lista" =
    log_tipo_match,
  
  "09_check_match_cf" =
    check_match_codice_fiscale,
  
  "10_cf_residui" =
    check_cf_residui,
  
  "11_cf_residui_match" =
    residui_cf_matchabili,
  
  "12_partecip_lista" =
    log_partecipazione_lista_pad26,
  
  "13_copertura_tot" =
    log_copertura_pad26_lista,
  
  "14_copertura_dataset" =
    log_copertura_pad26_per_dataset,
  
  "15_nonmatch_aggregato" =
    log_non_match_per_dataset,
  
  "16_pad26_nonmatch_full" =
    log_pad26_non_match_full,
  
  "17_nonmatch_categoria" =
    log_pad26_non_match_per_categoria,
  
  "18_lista_senza_ipa" =
    log_lista_senza_codice_ipa,
  
  "19_lista_non_pad26" =
    log_lista_non_in_pad26_full,
  
  "20_copertura_desc_fg" =
    log_copertura_lista_per_desc_fg,
  
  "21_ipa_pad26_non_lista" =
    log_ipa_pad26_non_in_lista,
  
  "22_denom_diff_ipa" =
    log_ipa_match_denom_differenti,
  
  "23_incoer_territ_ipa" =
    log_ipa_incoerenze_territoriali,
  
  "24_candidati_comuni" =
    candidati_match_comuni,
  
  "25_enti_nonmatch_det" =
    log_enti_pad26_non_match_dettaglio
)

oggetti_log_null <- names(log_list)[
  vapply(log_list, is.null, logical(1))
]

if (length(oggetti_log_null) > 0L) {
  stop(
    "Oggetti log NULL: ",
    paste(oggetti_log_null, collapse = ", ")
  )
}

local_log_excel <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "controlli_raccordo_padigitale2026_Lista_raccordo_SIM.xlsx"
)

openxlsx::write.xlsx(
  x = log_list,
  file = local_log_excel,
  overwrite = TRUE
)

if (!file.exists(local_log_excel)) {
  stop("Workbook dei controlli non creato: ", local_log_excel)
}

drive_upload_or_update(
  local_path = local_log_excel,
  drive_folder_rel = DRIVE_PAD26_METADATA
)


#==============================================================================#
#### 16.2 METADATI SOURCE/PROCESSED                                        ----
#==============================================================================#

# Questi metadati documentano le variabili create o arricchite nello script 02.
# I metadati degli indicatori e delle formule della dashboard restano invece
# nello script 03_indicatori_padigitale2026.

metadata_dataset_processed <- tibble::tribble(
  ~dataset, ~ruolo, ~granularita, ~descrizione, ~fonte, ~run_id,
  
  "lista_pad26_long",
  "dataset canonico processed",
  "una riga per ente della lista e candidatura PAD26; una riga con PAD26 vuoto per gli enti senza candidature",
  "Raccordo tra Lista SIM, candidature finanziate PA digitale 2026, IPA e dataset avvisi.",
  "PA digitale 2026; IPA; Lista raccordo SIM",
  RUN_ID,
  
  "lista_pad26_master",
  "dimensione enti",
  "una riga per ente della Lista SIM",
  "Sintesi della partecipazione degli enti alle candidature PA digitale 2026.",
  "Lista raccordo SIM; PA digitale 2026",
  RUN_ID,
  
  "dim_avvisi_padigitale2026",
  "dimensione avvisi",
  "una riga per avviso ufficiale",
  "Anagrafica degli avvisi con misura, periodo, stato, destinatari e importi stanziati.",
  "PA digitale 2026 - dataset avvisi",
  RUN_ID
)

metadata_variabili_raccordo <- tibble::tribble(
  ~dataset, ~variabile, ~tipo, ~descrizione, ~provenienza, ~regola,
  
  "lista_pad26_long",
  "pad26_avviso",
  "character",
  "Titolo dell'avviso così come riportato nella candidatura.",
  "candidature finanziate",
  "Valore originale; può includere una quota più specifica del titolo ufficiale.",
  
  "lista_pad26_long",
  "pad26_titolo_avviso",
  "character",
  "Titolo ufficiale dell'avviso dopo il raccordo con il dataset avvisi.",
  "dataset avvisi",
  "Join many-to-one tramite chiave normalizzata; per quattro titoli è applicato un raccordo manuale documentato.",
  
  "lista_pad26_long",
  "pad26_misura",
  "character",
  "Misura PA digitale 2026 associata all'avviso.",
  "dataset avvisi",
  "Ereditata dal titolo ufficiale dell'avviso.",
  
  "lista_pad26_long",
  "pad26_data_inizio_bando",
  "Date",
  "Data di apertura del bando.",
  "dataset avvisi",
  "Ereditata dal titolo ufficiale dell'avviso.",
  
  "lista_pad26_long",
  "pad26_data_fine_bando",
  "Date",
  "Data di chiusura del bando.",
  "dataset avvisi",
  "Ereditata dal titolo ufficiale dell'avviso.",
  
  "lista_pad26_long",
  "pad26_anno_inizio_bando",
  "integer",
  "Anno della data di apertura del bando.",
  "derivata",
  "Anno estratto da pad26_data_inizio_bando.",
  
  "lista_pad26_long",
  "pad26_anno_fine_bando",
  "integer",
  "Anno della data di chiusura del bando.",
  "derivata",
  "Anno estratto da pad26_data_fine_bando.",
  
  "lista_pad26_long",
  "pad26_stato_avviso",
  "character",
  "Stato amministrativo dell'avviso.",
  "dataset avvisi",
  "Non coincide necessariamente con lo stato di avanzamento del progetto.",
  
  "lista_pad26_long",
  "pad26_soggetti_destinatari",
  "character",
  "Categorie di soggetti destinatari dell'avviso.",
  "dataset avvisi",
  "Campo descrittivo dell'avviso; non identifica direttamente il singolo ente.",
  
  "lista_pad26_long",
  "pad26_totale_importo_stanziato",
  "numeric",
  "Importo stanziato per l'avviso.",
  "dataset avvisi",
  "Ripetuto sulle candidature: non deve essere sommato a livello candidatura.",
  
  "lista_pad26_long",
  "pad26_totale_importo_misura",
  "numeric",
  "Importo complessivo associato alla misura.",
  "dataset avvisi",
  "Ripetuto sulle candidature: non deve essere sommato a livello candidatura.",
  
  "lista_pad26_long",
  "pad26_match_avviso",
  "logical",
  "Indica se la candidatura è stata raccordata al dataset avvisi.",
  "derivata",
  "TRUE quando è disponibile un titolo ufficiale associato.",
  
  "lista_pad26_long",
  "pad26_raccordo_avviso_manuale",
  "logical",
  "Indica se il raccordo candidatura-avviso è stato risolto tramite tabella manuale.",
  "derivata",
  "TRUE per le candidature con quota 1.1 o 1.2 dei bandi ASL/AO di giugno e ottobre 2025.",
  
  "lista_pad26_long",
  "pad26_motivo_raccordo",
  "character",
  "Motivazione del raccordo manuale candidatura-avviso.",
  "raccordo manuale",
  "Documenta la differenza tra titolo candidatura e titolo ufficiale.",
  
  "lista_pad26_long",
  "pad26_motivo_non_match_avviso",
  "character",
  "Motivo dell'eventuale mancato raccordo con il dataset avvisi.",
  "derivata",
  "Valorizzata soltanto per i titoli non presenti nel dataset avvisi.",
  
  "lista_pad26_long",
  "in_pad26",
  "integer",
  "Indica se l'ente della Lista SIM ha almeno una candidatura nella riga.",
  "derivata",
  "1 se pad26_row_id è valorizzato, altrimenti 0."
)

metadata_raccordo_avvisi <- tibble::tibble(
  campo = c(
    "chiave_candidature",
    "chiave_avvisi",
    "cardinalita",
    "normalizzazione",
    "raccordi_manuali",
    "quota_match_finale"
  ),
  valore = c(
    "avviso",
    "titolo",
    "molte candidature -> un avviso",
    "maiuscole, apostrofi, trattini, slash e spazi normalizzati",
    as.character(check_join_avvisi$n_raccordi_manuali),
    as.character(check_join_avvisi$quota_match_avviso)
  )
)

local_metadata_excel <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "metadati_processed_padigitale2026.xlsx"
)

openxlsx::write.xlsx(
  x = list(
    "Dataset processed" = metadata_dataset_processed,
    "Variabili raccordo" = metadata_variabili_raccordo,
    "Metodo join avvisi" = metadata_raccordo_avvisi,
    "Raccordi manuali" = raccordo_avvisi_manual
  ),
  file = local_metadata_excel,
  overwrite = TRUE
)

local_metadata_variabili_csv <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "metadata_variabili_raccordo_padigitale2026.csv"
)

local_metadata_variabili_json <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "metadata_variabili_raccordo_padigitale2026.json"
)

readr::write_csv(
  metadata_variabili_raccordo,
  local_metadata_variabili_csv,
  na = ""
)

jsonlite::write_json(
  metadata_variabili_raccordo,
  local_metadata_variabili_json,
  dataframe = "rows",
  na = "null",
  null = "null",
  pretty = TRUE,
  auto_unbox = TRUE
)

metadata_paths <- c(
  local_metadata_excel,
  local_metadata_variabili_csv,
  local_metadata_variabili_json
)

if (!all(file.exists(metadata_paths))) {
  stop(
    "Uno o più file di metadati non sono stati creati: ",
    paste(metadata_paths[!file.exists(metadata_paths)], collapse = ", ")
  )
}

purrr::walk(
  metadata_paths,
  ~ drive_upload_or_update(
    local_path = .x,
    drive_folder_rel = DRIVE_PAD26_METADATA
  )
)


#==============================================================================#
#### 17) CHIUSURA RUN                                                      ----
#==============================================================================#

message("Raccordo PA digitale 2026 completato.")
message("RUN_ID_IMPORT: ", RUN_ID_IMPORT)
message("RUN_ID raccordo: ", RUN_ID)
message("- Processed: ", DRIVE_PAD26_PROCESSED)
message("- Metadati e controlli: ", DRIVE_PAD26_METADATA)
message("- Log: ", DRIVE_PAD26_LOGS)

console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

drive_upload_or_update(
  local_path = console_log_path,
  drive_folder_rel = DRIVE_PAD26_LOGS
)

if (delete_local_temp) {
  unlink(
    file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_IMPORT),
    recursive = TRUE
  )
  unlink(
    file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID),
    recursive = TRUE
  )
  unlink(
    file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID),
    recursive = TRUE
  )
  unlink(
    file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID),
    recursive = TRUE
  )
  unlink(
    file.path(DIR_TEMP, "Lista_raccordo_SIM.xlsx"),
    recursive = FALSE
  )
}

message(
  "--- 02_raccordo_padigitale2026_lista completato. RUN_ID: ",
  RUN_ID,
  " ---"
)
