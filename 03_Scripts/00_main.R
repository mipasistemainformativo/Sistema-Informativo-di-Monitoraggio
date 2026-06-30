#..............................................................................#
# SCRIPT: costruzione Lista_raccordo_SIM.xlsx
# PROGETTO: Monitoraggio-PNRR / MIPA
#
# SCOPO DELLO SCRIPT
# Costruire la lista di riferimento delle amministrazioni a partire dalla lista MPA.
# La lista MPA definisce il perimetro operativo del progetto: ogni record finale
# deve appartenere a MPA. Le altre fonti servono ad arricchire, controllare o
# documentare la lista, ma non ad ampliarne il perimetro.
#
# ARCHITETTURA DEL PROGETTO
# - Gli script sono versionati in GitHub nella cartella 03_Scripts.
# - I dati e gli output sono salvati su Google Drive nella repository
#   Monitoraggio-PNRR.
# - I file vengono scaricati localmente in 07_Temp, elaborati, poi caricati
#   su Drive nelle cartelle corrette.
# - La cartella 07_Temp è solo una cache tecnica locale: può essere cancellata
#   a fine esecuzione.
#
# INPUT DRIVE
# - 01_Dataset/Lists/11 05 2026 Lista MPA_2025.xlsx
# - 01_Dataset/Lists/11 05 2026 Lista S13_2025.xlsx
# - 01_Dataset/Lists/Anagrafe-Enti-BDAP.xlsx
#
# OUTPUT DRIVE
# - 01_Dataset/Lists/Lista_raccordo_SIM.xlsx
#   Lista finale pulita da usare nei raccordi e nelle dashboard.
#
# - 05_Logs/lista/lista_audit_<RUN_ID>.xlsx
#   File di audit con controlli, conflitti, duplicati e copertura.
#
# - 02_Metadata/lista/metadata_lista_<RUN_ID>.xlsx
#   Metadati delle variabili della lista finale.
#
# Nota:
# tutti gli output collegati alla costruzione della lista, ad eccezione
# del file operativo Lista_raccordo_SIM.xlsx, vengono salvati in un sottfolder "lista".
#
# SCELTE METODOLOGICHE SINTETICHE
# 1. MPA è il perimetro della master list.
# 2. S13 e BDAP sono fonti di arricchimento e controllo.
# 3. Le colonne duplicate tra fonti non vengono tenute nella lista finale:
#    vengono confrontate e documentate nei log.
# 4. I conflitti tra fonti vengono registrati prima di applicare regole
#    di priorità.
# 5. La lista finale contiene solo variabili pulite, stabili e documentate.
#..............................................................................#

#..............................................................................#
rm(list=ls())

#..............................................................................#
#                                 IMPORT                                    ####
#..............................................................................#
source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")

library(readxl) 
library(googledrive)
library(dplyr)
library(writexl)
library(purrr)
library(stringr)
library(tibble)
library(jsonlite)

#..............................................................................#
#                             CONFIGURATIONS                                ####
#..............................................................................#
# Autenticazione a Google Drive.
# Serve per scaricare i file sorgente e ricaricare gli output finali.
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# Parametro operativo:
# - FALSE: mantiene i file locali in 07_Temp per ispezione/debug;
# - TRUE: cancella i file locali temporanei a fine esecuzione.
delete_local_temp <- FALSE

# Identificativo univoco del run.
# Serve per nominare log e audit in modo tracciabile.
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID costruzione lista: ", RUN_ID)

DRIVE_DIR_METADATA_LISTA <- file.path(DRIVE_DIR_METADATA, "Lists_met")
DRIVE_DIR_LOGS_LISTA <- file.path(DRIVE_DIR_LOGS, "lists")

DIR_LOGS_LISTA_LOCAL <- file.path(DIR_TEMP, "Lists", "Logs", RUN_ID)

#..............................................................................#
#                     PATH DI INPUT E OUTPUT                                ####
#..............................................................................#

# Nomi dei file sorgente su Drive.
file_mpa_name  <- "11 05 2026 Lista MPA_2025.xlsx"
file_s13_name  <- "11 05 2026 Lista S13_2025.xlsx"
file_bdap_name <- "Anagrafe-Enti-BDAP.xlsx"

# Path relativi su Drive.
# Questi path usano le variabili definite in 00_config.R.
drive_file_mpa  <- file.path(DRIVE_DIR_LISTS, file_mpa_name)
drive_file_s13  <- file.path(DRIVE_DIR_LISTS, file_s13_name)
drive_file_bdap <- file.path(DRIVE_DIR_LISTS, file_bdap_name)

# Path locali temporanei.
local_file_mpa  <- file.path(DIR_TEMP, file_mpa_name)
local_file_s13  <- file.path(DIR_TEMP, file_s13_name)
local_file_bdap <- file.path(DIR_TEMP, file_bdap_name)

# Output locali temporanei.
local_lista_xlsx    <- file.path(DIR_TEMP, "Lista_raccordo_SIM.xlsx")
local_lista_rds <- file.path(DIR_TEMP, "Lista_raccordo_SIM.rds")
local_lista_json <- file.path(DIR_TEMP, "Lista_raccordo_SIM.json")

local_integrazione_qualita_file    <- file.path(DIR_TEMP, paste0("lista_integrazione_qualita_", RUN_ID, ".xlsx"))
local_metadata_file <- file.path(DIR_TEMP, paste0("metadata_lista_", RUN_ID, ".xlsx"))




# Avvio console log ...........................................................

console_log <- start_console_log(
  log_dir = DIR_LOGS_LISTA_LOCAL,
  run_id = RUN_ID,
  script_name = "00_main"
)


#..............................................................................#
#                              FUNCTIONS                                    ####
#..............................................................................#

# Controlla se una chiave identifica univocamente le righe di una fonte.
# Se ci sono duplicati, il merge può moltiplicare i record.
check_keys <- function(df, keys, source_name) {
  missing_keys <- setdiff(keys, names(df))
  
  if (length(missing_keys) > 0) {
    stop(
      "Nella fonte ", source_name, " mancano queste chiavi: ",
      paste(missing_keys, collapse = ", ")
    )
  }
  
  df %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1) %>%
    mutate(source = source_name, .before = 1)
}


# Aggiunge un suffisso di fonte a tutte le colonne non chiave.
# Evita suffissi automatici come .x/.y e rende leggibile la provenienza.
add_source_suffix <- function(df, source_suffix, keys) {
  df %>%
    rename_with(
      .fn = ~ paste0(.x, "_", source_suffix),
      .cols = -all_of(keys)
    )
}


# Trova variabili presenti in due fonti con la stessa radice.
# Esempio: RAGIONE_SOCIALE_mpa e RAGIONE_SOCIALE_bdap.
find_duplicate_pairs_by_suffix <- function(df, suffix_a, suffix_b) {
  names_df <- names(df)
  
  base_a <- names_df[stringr::str_detect(names_df, paste0("_", suffix_a, "$"))] %>%
    stringr::str_remove(paste0("_", suffix_a, "$"))
  
  base_b <- names_df[stringr::str_detect(names_df, paste0("_", suffix_b, "$"))] %>%
    stringr::str_remove(paste0("_", suffix_b, "$"))
  
  common_base <- intersect(base_a, base_b)
  
  tibble(
    variable_base = common_base,
    var_a = paste0(common_base, "_", suffix_a),
    var_b = paste0(common_base, "_", suffix_b),
    source_a = suffix_a,
    source_b = suffix_b
  )
}


# Registra i conflitti tra due colonne omologhe.
# Un conflitto esiste quando entrambe le fonti hanno un valore non mancante
# e i valori differiscono.
log_conflicts_one_pair <- function(df, id_cols, var_a, var_b, source_a, source_b) {
  df %>%
    mutate(
      value_a = as.character(.data[[var_a]]),
      value_b = as.character(.data[[var_b]]),
      value_a_clean = stringr::str_squish(value_a),
      value_b_clean = stringr::str_squish(value_b),
      conflict = case_when(
        is.na(value_a_clean) & is.na(value_b_clean) ~ FALSE,
        is.na(value_a_clean) | is.na(value_b_clean) ~ FALSE,
        value_a_clean != value_b_clean ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(conflict) %>%
    transmute(
      across(all_of(id_cols)),
      variable = stringr::str_remove(var_a, paste0("_", source_a, "$")),
      source_a = toupper(source_a),
      var_a = var_a,
      value_a = value_a,
      source_b = toupper(source_b),
      var_b = var_b,
      value_b = value_b
    )
}


# Applica il log dei conflitti a tutte le coppie duplicate trovate.
make_conflict_log <- function(df, pairs, id_cols) {
  if (nrow(pairs) == 0) {
    return(tibble())
  }
  
  purrr::pmap_dfr(
    pairs,
    function(variable_base, var_a, var_b, source_a, source_b) {
      log_conflicts_one_pair(
        df = df,
        id_cols = id_cols,
        var_a = var_a,
        var_b = var_b,
        source_a = source_a,
        source_b = source_b
      )
    }
  )
}


# Seleziona il primo valore disponibile secondo un ordine di priorità.
# Usa solo colonne effettivamente presenti nel dataset, così lo script è più robusto
# a piccole differenze nei nomi tra versioni dei file.
coalesce_existing <- function(df, cols) {
  cols <- intersect(cols, names(df))
  
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- rep(NA_character_, nrow(df))
  
  for (col in cols) {
    value <- as.character(df[[col]])
    out <- ifelse(is.na(out) & !is.na(value), value, out)
  }
  
  out
}


# Registra la fonte da cui proviene il valore finale selezionato.
source_existing <- function(df, cols, sources) {
  source_map <- tibble(col = cols, source = sources) %>%
    filter(col %in% names(df))
  
  if (nrow(source_map) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- rep(NA_character_, nrow(df))
  
  for (i in seq_len(nrow(source_map))) {
    col <- source_map$col[i]
    src <- source_map$source[i]
    value <- as.character(df[[col]])
    
    out <- ifelse(is.na(out) & !is.na(value), src, out)
  }
  
  out
}


#..............................................................................#
#                              IMPORT LISTS                                 ####
#..............................................................................#

#file temporanei locali
drive_download_from_path(drive_file_mpa,  local_file_mpa,  overwrite = TRUE)
drive_download_from_path(drive_file_s13,  local_file_s13,  overwrite = TRUE)
drive_download_from_path(drive_file_bdap, local_file_bdap, overwrite = TRUE)

#caricamento su R
MPA_raw <- readxl::read_excel(local_file_mpa)
s13_raw <- readxl::read_excel(local_file_s13)
BDAP_raw <- readxl::read_excel(local_file_bdap)
names(MPA_raw)
names(s13_raw)
names(BDAP_raw)

#..............................................................................#
#                       STANDARDIZZAZIONE MINIMA FONTI                      ####
#..............................................................................#

# Chiave operativa di raccordo.
#
# SCELTA OPERATIVA SULLE CHIAVI
# Per il raccordo tra MPA, S13 e BDAP usiamo CODICE_FISCALE come chiave principale.
# CODICE_REG non viene usato nel merge: viene mantenuto come variabile descrittiva/
# territoriale e come controllo di coerenza tra fonti.
#
# Implicazione metodologica:
# il codice fiscale deve identificare univocamente il record MPA.
# Se MPA presenta più righe per lo stesso CODICE_FISCALE, la lista non è a livello
# di ente ma a livello di unità/record MPA, e la chiave va rivalutata.
keys_main <- c("CODICE_FISCALE")

# Creiamo indicatori di presenza nelle fonti.
# Questi indicatori servono a sapere da quali fonti arriva ogni record.
MPA_raw$presente_mpa <- 1
s13_raw$presente_s13 <- 1
BDAP_raw$presente_bdap <- 1

# Uniformiamo i nomi delle chiavi BDAP ai nomi usati nelle altre fonti.
# In BDAP il codice fiscale è CF e il codice regione è Codice_Regione.
BDAP_raw$CODICE_FISCALE <- BDAP_raw$CF
BDAP_raw$CODICE_REG <- BDAP_raw$Codice_Regione


#..............................................................................#
#                   CONTROLLI DI QUALITÀ PRIMA DEI MERGE                    ####
#..............................................................................#

# Controlliamo se le chiavi sono univoche nelle tre fonti.
# Questo log è cruciale: se ci sono duplicati, il join può aumentare il numero
# di righe finali.
duplicate_keys_mpa <- check_keys(MPA_raw, keys_main, "MPA")
duplicate_keys_s13 <- check_keys(s13_raw, keys_main, "S13")
duplicate_keys_bdap <- check_keys(BDAP_raw, keys_main, "BDAP")

duplicate_keys_log <- bind_rows(
  duplicate_keys_mpa,
  duplicate_keys_s13,
  duplicate_keys_bdap
)


# Elenco delle variabili originarie per fonte.
# Utile per completare il dizionario dati.
source_variables <- bind_rows(
  tibble(source = "MPA", variable_original = names(MPA_raw)),
  tibble(source = "S13", variable_original = names(s13_raw)),
  tibble(source = "BDAP", variable_original = names(BDAP_raw))
)


#..............................................................................#
#              GESTIONE BDAP: RECORD STORICIZZATI E DUPLICATI              ####
#..............................................................................#

# BDAP può contenere più righe per lo stesso CODICE_FISCALE.
# Per evitare che il join moltiplichi le righe MPA, BDAP viene ridotta
# a una sola riga per CODICE_FISCALE prima del merge.
#
# Regola adottata:
# - se esiste una sola riga attiva, viene selezionata quella;
# - una riga è attiva se Data_Cessazione è mancante o vuota;
# - se non esiste nessuna riga attiva, il caso viene segnalato;
# - se esistono più righe attive, il caso viene segnalato come ambiguo.
#
# Le righe non selezionate non entrano nel merge operativo ma vengono salvate
# nel file audit.

BDAP_active_raw <- BDAP_raw %>%
  mutate(
    data_cessazione_clean = stringr::str_squish(as.character(Data_Cessazione)),
    is_active_bdap = is.na(data_cessazione_clean) | data_cessazione_clean == ""
  )

bdap_duplicate_keys <- BDAP_active_raw %>%
  count(CODICE_FISCALE, name = "n_righe_bdap") %>%
  filter(n_righe_bdap > 1)

bdap_duplicate_keys_mpa <- bdap_duplicate_keys %>%
  inner_join(
    MPA_raw %>% distinct(CODICE_FISCALE),
    by = "CODICE_FISCALE"
  )

bdap_active_check <- BDAP_active_raw %>%
  semi_join(
    bdap_duplicate_keys_mpa,
    by = "CODICE_FISCALE"
  ) %>%
  group_by(CODICE_FISCALE) %>%
  summarise(
    n_righe_bdap = n(),
    n_attive = sum(is_active_bdap),
    n_cessate = sum(!is_active_bdap),
    denominazione_values = paste(sort(unique(na.omit(as.character(Denominazione)))), collapse = " | "),
    codice_reg_values = paste(sort(unique(na.omit(as.character(CODICE_REG)))), collapse = " | "),
    ateco_values = paste(sort(unique(na.omit(as.character(Codice_ATECO)))), collapse = " | "),
    forma_giuridica_values = paste(sort(unique(na.omit(as.character(Descr_Forma_Giuridica)))), collapse = " | "),
    data_cessazione_values = paste(sort(unique(na.omit(data_cessazione_clean))), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    bdap_duplicate_case = case_when(
      n_attive == 1 ~ "una_attiva",
      n_attive == 0 ~ "nessuna_attiva",
      n_attive > 1 ~ "multiple_attive",
      TRUE ~ "controllare"
    )
  )

bdap_active_problems <- bdap_active_check %>%
  filter(bdap_duplicate_case != "una_attiva")
bdap_duplicate_case_summary <- bdap_active_check %>%
  count(bdap_duplicate_case, name = "n_codici_fiscali")

if (nrow(bdap_active_problems) > 0) {
  stop(
    "BDAP contiene codici fiscali duplicati con zero o più di una riga attiva. ",
    "Controllare bdap_active_problems prima di procedere."
  )
}

BDAP_ranked_raw <- BDAP_active_raw %>%
  group_by(CODICE_FISCALE) %>%
  arrange(
    desc(is_active_bdap),
    Data_Cessazione,
    .by_group = TRUE
  ) %>%
  mutate(
    row_selected_bdap = row_number() == 1,
    n_righe_bdap_originali = n(),
    n_righe_bdap_attive = sum(is_active_bdap),
    bdap_record_storicizzato = as.integer(n_righe_bdap_originali > 1)
  ) %>%
  ungroup()

BDAP_for_merge_raw <- BDAP_ranked_raw %>%
  filter(row_selected_bdap)

bdap_rows_excluded_by_dedup <- BDAP_ranked_raw %>%
  filter(!row_selected_bdap)

bdap_dedup_rule_log <- BDAP_ranked_raw %>%
  group_by(CODICE_FISCALE) %>%
  summarise(
    n_righe_bdap_originali = n(),
    n_righe_attive = sum(is_active_bdap),
    n_righe_cessate = sum(!is_active_bdap),
    selected_is_active = any(row_selected_bdap & is_active_bdap),
    
    n_denominazioni_distinte = n_distinct(Denominazione, na.rm = TRUE),
    denominazione_values = paste(sort(unique(na.omit(as.character(Denominazione)))), collapse = " | "),
    
    n_codici_reg_distinti = n_distinct(CODICE_REG, na.rm = TRUE),
    codice_reg_values = paste(sort(unique(na.omit(as.character(CODICE_REG)))), collapse = " | "),
    
    n_ateco_distinti = n_distinct(Codice_ATECO, na.rm = TRUE),
    ateco_values_originali = paste(sort(unique(na.omit(as.character(Codice_ATECO)))), collapse = " | "),
    
    n_forme_giuridiche_distinte = n_distinct(Descr_Forma_Giuridica, na.rm = TRUE),
    forma_giuridica_values_originali = paste(sort(unique(na.omit(as.character(Descr_Forma_Giuridica)))), collapse = " | "),
    
    data_cessazione_values = paste(sort(unique(na.omit(data_cessazione_clean))), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_righe_bdap_originali > 1) %>%
  mutate(
    bdap_storicizzazione_ambigua = case_when(
      n_denominazioni_distinte > 1 & n_forme_giuridiche_distinte > 1 ~ 1,
      n_denominazioni_distinte > 1 & n_ateco_distinti > 1 ~ 1,
      n_codici_reg_distinti > 1 ~ 1,
      TRUE ~ 0
    )
  )

bdap_storicizzazioni_ambigue <- bdap_dedup_rule_log %>%
  filter(bdap_storicizzazione_ambigua == 1)

bdap_storicizzazione_flags <- bdap_dedup_rule_log %>%
  transmute(
    CODICE_FISCALE,
    bdap_storicizzazione_ambigua = bdap_storicizzazione_ambigua
  )

BDAP_for_merge_raw <- BDAP_for_merge_raw %>%
  left_join(
    bdap_storicizzazione_flags,
    by = "CODICE_FISCALE"
  ) %>%
  mutate(
    bdap_storicizzazione_ambigua = if_else(
      is.na(bdap_storicizzazione_ambigua),
      0,
      bdap_storicizzazione_ambigua
    )
  )


#..............................................................................#
#                 PREPARAZIONE DELLE FONTI PER IL MERGE ####
#..............................................................................#

# Aggiungiamo suffissi espliciti alle colonne non chiave.

MPA <- MPA_raw %>%
  add_source_suffix(source_suffix = "mpa", keys = keys_main)

s13 <- s13_raw %>%
  add_source_suffix(source_suffix = "s13", keys = keys_main)

BDAP <- BDAP_for_merge_raw %>%
  add_source_suffix(source_suffix = "bdap", keys = keys_main)
# 
# BDAP <- BDAP_raw %>%
#   add_source_suffix(source_suffix = "bdap", keys = keys_main)


#..............................................................................#
#                      COSTRUZIONE DELLA MASTER LIST                        ####
#..............................................................................#
# BDAP_raw
# ↓
# creo is_active_bdap da Data_Cessazione
# ↓
# identifico duplicati su CODICE_FISCALE
# ↓
# loggo duplicati e casi problematici
# ↓
# scelgo riga per merge:
#   1. se esiste una sola riga attiva, tengo quella
# 2. se non esistono righe attive, tengo la più recente per Data_Cessazione e flaggo problema
# 3. se esistono più righe attive, non scelgo automaticamente oppure mi fermo
# ↓
# BDAP_for_merge_raw
# ↓
# merge con MPA

# SCELTA METODOLOGICA:
# MPA è il perimetro della lista.
# Partiamo quindi da MPA e aggiungiamo S13 e BDAP.
# S13 e BDAP vengono agganciati a MPA, ma non aggiungono nuove righe
# se contengono enti assenti da MPA.
#
# Nota:
# se dopo il join il numero di righe cresce, il problema non è il left_join
# in sé, ma la presenza di duplicati nelle chiavi di raccordo.

n_mpa_before_join <- nrow(MPA)

MPA_S13 <- MPA %>%
  left_join(s13, by = keys_main)

n_after_s13_join <- nrow(MPA_S13)

MPA_S13_BDAP <- MPA_S13 %>%
  left_join(BDAP, by = keys_main)

n_after_bdap_join <- nrow(MPA_S13_BDAP)


# Log per verificare se i join hanno moltiplicato le righe.
join_row_count_log <- tibble(
  step = c(
    "MPA iniziale",
    "Dopo join con S13",
    "Dopo join con BDAP"
  ),
  n_rows = c(
    n_mpa_before_join,
    n_after_s13_join,
    n_after_bdap_join
  ),
  run_id = RUN_ID
)


#..............................................................................#
#           ENTI PRESENTI IN ALTRE FONTI MA FUORI DAL PERIMETRO MPA         ####
#..............................................................................#

# Questi enti non entrano nella lista finale perché MPA è il perimetro.
# Li salviamo però in audit, perché sono informativamente utili:
# indicano copertura differenziale tra fonti.

s13_fuori_perimetro_mpa <- s13 %>%
  anti_join(MPA, by = keys_main)

bdap_fuori_perimetro_mpa <- BDAP %>%
  anti_join(MPA, by = keys_main)


#..............................................................................#
#       IDENTIFICAZIONE e LOG DELLE VARIABILI DUPLICATE TRA FONTI           ####
#..............................................................................#

# Individuiamo variabili omologhe tra fonti.
# Esempio: RAGIONE_SOCIALE_mpa e RAGIONE_SOCIALE_s13.
pairs_mpa_s13 <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "mpa", "s13")
pairs_mpa_bdap <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "mpa", "bdap")
pairs_s13_bdap <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "s13", "bdap")

duplicate_pairs_log <- bind_rows(
  pairs_mpa_s13,
  pairs_mpa_bdap,
  pairs_s13_bdap
)

# Registriamo i conflitti prima di applicare le regole di priorità.
conflict_log <- bind_rows(
  make_conflict_log(MPA_S13_BDAP, pairs_mpa_s13, keys_main),
  make_conflict_log(MPA_S13_BDAP, pairs_mpa_bdap, keys_main),
  make_conflict_log(MPA_S13_BDAP, pairs_s13_bdap, keys_main)
) %>%
  mutate(run_id = RUN_ID, .before = 1)


#..............................................................................#
#                   COSTRUZIONE DELLA LISTA FINALE PULITA                   ####
#..............................................................................#

# Costruiamo variabili finali senza suffissi tecnici.
#
# Per ogni variabile finale, scegliamo i valori secondo una priorità esplicita.
# Manteniamo anche alcune colonne "fonte_*" per sapere da quale fonte arriva
# il valore finale selezionato.
#
# Importante:
# le colonne originarie con suffissi _mpa, _s13, _bdap restano nel file audit,
# ma non nella lista finale.


# Regole di priorità adottate in questa versione:
#
# - Perimetro: MPA.
# - Codice fiscale: MPA, perché definiscono il record finale.
# - Ragione sociale: MPA > BDAP > S13.
#   Questa scelta è coerente con MPA come lista di riferimento.
#   BDAP resta fonte di controllo e fallback.
# - codice_reg: MPA > BDAP > S13
# - FG: MPA > S13 > BDAP.
# - desc_fg: MPA > S13 > BDAP, se disponibile
# - Codici fonte-specifici: dalla rispettiva fonte.
# - ATECO: BDAP.
#
# Le colonne originali con suffissi _mpa, _s13, _bdap non vengono tenute nella
# lista finale, ma restano documentate nel file audit.


#..............................................................................
# COSTRUZIONE LISTA FINALE 
#..............................................................................
# La lista finale è una anagrafica centrale arricchita:
# - MPA definisce il perimetro delle unità incluse;
# - S13 integra/confronta alcune variabili comuni;
# - OpenBDAP arricchisce la lista con codici di raccordo e informazioni
#   anagrafiche/amministrative, senza modificare il perimetro MPA.
#
# Nota metodologica:
# FG e Codice_Forma_Giuridica di BDAP sono mantenuti separati.
# FG deriva da MPA/S13; la forma giuridica BDAP è conservata come
# informazione aggiuntiva, ma non usata per costruire FG.
#..............................................................................

lista <- MPA_S13_BDAP %>%
  mutate(
    #..............................................................................
    # Identificativi principali
    #..............................................................................
    codice_fiscale = CODICE_FISCALE,
    
    codice_reg = coalesce_existing(
      .,
      c("CODICE_REG_mpa", "Codice_Regione_bdap", "CODICE_REG_s13")
    ),
    fonte_codice_reg = source_existing(
      .,
      cols = c("CODICE_REG_mpa", "Codice_Regione_bdap", "CODICE_REG_s13"),
      sources = c("MPA", "BDAP", "S13")
    ),
    
    ragione_sociale = coalesce_existing(
      .,
      c("RAGIONE_SOCIALE_mpa", "Denominazione_bdap", "RAGIONE_SOCIALE_s13")
    ),
    fonte_ragione_sociale = source_existing(
      .,
      cols = c("RAGIONE_SOCIALE_mpa", "Denominazione_bdap", "RAGIONE_SOCIALE_s13"),
      sources = c("MPA", "BDAP", "S13")
    ),
    
    #..............................................................................
    # Classificazione FG: solo MPA/S13
    #..............................................................................
    fg = coalesce_existing(
      .,
      c("FG_mpa", "FG_s13")
    ),
    fonte_fg = source_existing(
      .,
      cols = c("FG_mpa", "FG_s13"),
      sources = c("MPA", "S13")
    ),
    
    desc_fg = coalesce_existing(
      .,
      c("DESC_FG_mpa", "DESCRIZIONE_FORMA_GIURIDICA_s13")
    ),
    fonte_desc_fg = source_existing(
      .,
      cols = c("DESC_FG_mpa", "DESCRIZIONE_FORMA_GIURIDICA_s13"),
      sources = c("MPA", "S13")
    ),
    
    #..............................................................................
    # Codici di raccordo MPA/S13/BDAP
    #..............................................................................
    codice_unita_mpa = coalesce_existing(., c("CODICE_UNITA_UG_mpa")),
    codice_unita_s13 = coalesce_existing(., c("CODICE_UNITA_s13")),
    
    id_ente_bdap = coalesce_existing(., c("Id_Ente_bdap")),
    codice_ente_ipa = coalesce_existing(., c("Codice_Ente_IPA_bdap")),
    codice_ente_siope = coalesce_existing(., c("Codice_Ente_SIOPE_bdap")),
    
    #..............................................................................
    # Informazioni territoriali BDAP
    #..............................................................................
    codice_istat_comune = coalesce_existing(., c("Codice_ISTAT_Comune_bdap")),
    codice_comune = coalesce_existing(., c("Codice_Comune_bdap")),
    comune = coalesce_existing(., c("Dizione_Comune_bdap")),
    
    codice_provincia = coalesce_existing(., c("Codice_Provincia_bdap")),
    sigla_provincia = coalesce_existing(., c("Sigla_Provincia_bdap")),
    provincia = coalesce_existing(., c("Dizione_Provincia_bdap")),
    
    codice_regione_bdap = coalesce_existing(., c("Codice_Regione_bdap")),
    regione_bdap = coalesce_existing(., c("Dizione_Regione_bdap")),
    
    #..............................................................................
    # Classificazioni BDAP mantenute separate
    #..............................................................................
    ateco_bdap = coalesce_existing(., c("Codice_ATECO_bdap")),
    descr_ateco_bdap = coalesce_existing(., c("Descr_Codice_ATECO_bdap")),
    
    codice_forma_giuridica_bdap = coalesce_existing(
      .,
      c("Codice_Forma_Giuridica_bdap")
    ),
    descr_forma_giuridica_bdap = coalesce_existing(
      .,
      c("Descr_Forma_Giuridica_bdap")
    ),
    
    codice_tipologia_siope_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_SIOPE_bdap")
    ),
    descr_tipologia_siope_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_SIOPE_bdap")
    ),
    
    codice_categoria_ipa_bdap = coalesce_existing(
      .,
      c("Codice_Categoria_IPA_bdap")
    ),
    descr_categoria_ipa_bdap = coalesce_existing(
      .,
      c("Descr_Categoria_IPA_bdap")
    ),
    
    codice_tipologia_ipa_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_IPA_bdap")
    ),
    descr_tipologia_ipa_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_IPA_bdap")
    ),
    
    codice_tipologia_mtur_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_MTUR_bdap")
    ),
    descr_tipologia_mtur_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_MTUR_bdap")
    ),
    
    codice_tipologia_dt_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_DT_bdap")
    ),
    descr_tipologia_dt_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_DT_bdap")
    ),
    
    codice_tipologia_istat_s13_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_ISTAT_S13_bdap")
    ),
    descr_tipologia_istat_s13_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_ISTAT_S13_bdap")
    ),
    
    codice_tipologia_dlgs_118_2011_bdap = coalesce_existing(
      .,
      c("Codice_Tipologia_DLGS_118_2011_bdap")
    ),
    descr_tipologia_dlgs_118_2011_bdap = coalesce_existing(
      .,
      c("Descr_Tipologia_DLGS_118_2011_bdap")
    ),
    
    #..............................................................................
    # Date BDAP
    #..............................................................................
    data_istituzione_bdap = coalesce_existing(., c("Data_Istituzione_bdap")),
    data_cessazione_bdap = coalesce_existing(., c("Data_Cessazione_bdap")),
    
    data_inclusione_siope_bdap = coalesce_existing(
      .,
      c("Data_Inclusione_SIOPE_bdap")
    ),
    data_esclusione_siope_bdap = coalesce_existing(
      .,
      c("Data_Esclusione_SIOPE_bdap")
    ),
    
    data_inclusione_ipa_bdap = coalesce_existing(
      .,
      c("Data_Inclusione_IPA_bdap")
    ),
    data_esclusione_ipa_bdap = coalesce_existing(
      .,
      c("Data_Esclusione_IPA_bdap")
    ),
    
    data_inclusione_istat_s13_bdap = coalesce_existing(
      .,
      c("Data_Inclusione_ISTAT_S13_bdap")
    ),
    data_esclusione_istat_s13_bdap = coalesce_existing(
      .,
      c("Data_Esclusione_ISTAT_S13_bdap")
    ),
    
    data_inclusione_dlgs_118_2011_bdap = coalesce_existing(
      .,
      c("Data_Inclusione_DLGS_118_2011_bdap")
    ),
    data_esclusione_dlgs_118_2011_bdap = coalesce_existing(
      .,
      c("Data_Esclusione_DLGS_118_2011_bdap")
    ),
    
    #..............................................................................
    # Informazioni anagrafiche e di contatto BDAP
    #..............................................................................
    url_bdap = coalesce_existing(., c("URL_bdap")),
    telefono_bdap = coalesce_existing(., c("Telefono_bdap")),
    fax_bdap = coalesce_existing(., c("FAX_bdap")),
    indirizzo_bdap = coalesce_existing(., c("Indirizzo_bdap")),
    cap_bdap = coalesce_existing(., c("CAP_bdap")),
    
    nome_resp_bdap = coalesce_existing(., c("Nome_Resp_bdap")),
    cogn_resp_bdap = coalesce_existing(., c("Cogn_Resp_bdap")),
    titolo_resp_bdap = coalesce_existing(., c("Titolo_Resp_bdap")),
    
    #..............................................................................
    # Indicatori di presenza fonte
    #..............................................................................
    presente_mpa = as.integer(coalesce_existing(., c("presente_mpa_mpa"))),
    presente_s13 = as.integer(coalesce_existing(., c("presente_s13_s13"))),
    presente_bdap = as.integer(coalesce_existing(., c("presente_bdap_bdap"))),
    
    #..............................................................................
    # Indicatori di storicizzazione BDAP
    #..............................................................................
    bdap_record_storicizzato = as.integer(
      coalesce_existing(., c("bdap_record_storicizzato_bdap"))
    ),
    bdap_storicizzazione_ambigua = as.integer(
      coalesce_existing(., c("bdap_storicizzazione_ambigua_bdap"))
    ),
    bdap_n_righe_originali = coalesce_existing(
      .,
      c("n_righe_bdap_originali_bdap")
    ),
    
    run_id = RUN_ID
  ) %>%
  select(
    # Identificativi e variabili principali
    codice_fiscale,
    codice_reg,
    fonte_codice_reg,
    ragione_sociale,
    fonte_ragione_sociale,
    fg,
    fonte_fg,
    desc_fg,
    fonte_desc_fg,
    
    # Codici di raccordo
    codice_unita_mpa,
    codice_unita_s13,
    id_ente_bdap,
    codice_ente_ipa,
    codice_ente_siope,
    
    # Territorio
    codice_istat_comune,
    codice_comune,
    comune,
    codice_provincia,
    sigla_provincia,
    provincia,
    codice_regione_bdap,
    regione_bdap,
    
    # Classificazioni BDAP
    ateco_bdap,
    descr_ateco_bdap,
    codice_forma_giuridica_bdap,
    descr_forma_giuridica_bdap,
    codice_tipologia_siope_bdap,
    descr_tipologia_siope_bdap,
    codice_categoria_ipa_bdap,
    descr_categoria_ipa_bdap,
    codice_tipologia_ipa_bdap,
    descr_tipologia_ipa_bdap,
    codice_tipologia_mtur_bdap,
    descr_tipologia_mtur_bdap,
    codice_tipologia_dt_bdap,
    descr_tipologia_dt_bdap,
    codice_tipologia_istat_s13_bdap,
    descr_tipologia_istat_s13_bdap,
    codice_tipologia_dlgs_118_2011_bdap,
    descr_tipologia_dlgs_118_2011_bdap,
    
    # Date BDAP
    data_istituzione_bdap,
    data_cessazione_bdap,
    data_inclusione_siope_bdap,
    data_esclusione_siope_bdap,
    data_inclusione_ipa_bdap,
    data_esclusione_ipa_bdap,
    data_inclusione_istat_s13_bdap,
    data_esclusione_istat_s13_bdap,
    data_inclusione_dlgs_118_2011_bdap,
    data_esclusione_dlgs_118_2011_bdap,
    
    # Contatti BDAP
    url_bdap,
    telefono_bdap,
    fax_bdap,
    indirizzo_bdap,
    cap_bdap,
    nome_resp_bdap,
    cogn_resp_bdap,
    titolo_resp_bdap,
    
    # Indicatori di copertura e qualità
    presente_mpa,
    presente_s13,
    presente_bdap,
    bdap_record_storicizzato,
    bdap_storicizzazione_ambigua,
    bdap_n_righe_originali,
    
    run_id
  )


#..............................................................................#
#                  LOG DI COPERTURA DELLA LISTA FINALE                      ####
#..............................................................................#

# Questo log dà una sintesi della qualità e completezza della lista finale.
# È utile per capire rapidamente quanti record MPA sono stati arricchiti
# con informazioni S13 e BDAP.
coverage_log <- tibble(
  run_id = RUN_ID,
  n_record_lista = nrow(lista),
  
  n_codice_fiscale_mancante = sum(is.na(lista$codice_fiscale) | lista$codice_fiscale == ""),
  n_codice_reg_mancante = sum(is.na(lista$codice_reg) | lista$codice_reg == ""),
  n_ragione_sociale_mancante = sum(is.na(lista$ragione_sociale) | lista$ragione_sociale == ""),
  n_fg_mancante = sum(is.na(lista$fg) | lista$fg == ""),
  n_desc_fg_mancante = sum(is.na(lista$desc_fg) | lista$desc_fg == ""),
  
  n_presenti_in_mpa = sum(lista$presente_mpa == 1, na.rm = TRUE),
  n_presenti_anche_in_s13 = sum(lista$presente_s13 == 1, na.rm = TRUE),
  n_presenti_anche_in_bdap = sum(lista$presente_bdap == 1, na.rm = TRUE),
  
  n_codice_unita_mpa_valorizzato = sum(!is.na(lista$codice_unita_mpa) & lista$codice_unita_mpa != ""),
  n_codice_unita_s13_valorizzato = sum(!is.na(lista$codice_unita_s13) & lista$codice_unita_s13 != ""),
  n_id_ente_bdap_valorizzato = sum(!is.na(lista$id_ente_bdap) & lista$id_ente_bdap != ""),
  n_codice_ente_ipa_valorizzato = sum(!is.na(lista$codice_ente_ipa) & lista$codice_ente_ipa != ""),
  n_codice_ente_siope_valorizzato = sum(!is.na(lista$codice_ente_siope) & lista$codice_ente_siope != ""),
  
  n_ateco_bdap_valorizzato = sum(!is.na(lista$ateco_bdap) & lista$ateco_bdap != ""),
  n_forma_giuridica_bdap_valorizzata = sum(
    !is.na(lista$codice_forma_giuridica_bdap) &
      lista$codice_forma_giuridica_bdap != ""
  ),
  n_tipologia_ipa_bdap_valorizzata = sum(
    !is.na(lista$codice_tipologia_ipa_bdap) &
      lista$codice_tipologia_ipa_bdap != ""
  ),
  n_tipologia_siope_bdap_valorizzata = sum(
    !is.na(lista$codice_tipologia_siope_bdap) &
      lista$codice_tipologia_siope_bdap != ""
  ),
  n_tipologia_istat_s13_bdap_valorizzata = sum(
    !is.na(lista$codice_tipologia_istat_s13_bdap) &
      lista$codice_tipologia_istat_s13_bdap != ""
  ),
  
  n_bdap_record_storicizzati = sum(lista$bdap_record_storicizzato == 1, na.rm = TRUE),
  n_bdap_storicizzazioni_ambigue = sum(lista$bdap_storicizzazione_ambigua == 1, na.rm = TRUE)
)

technical_suffix_cols <- names(lista)[
  stringr::str_detect(names(lista), "\\.x$|\\.y$|_x$|_y$")
]

merge_quality_check <- tibble(
  check = c(
    "Lista finale conserva numero righe MPA",
    "Nessun duplicato chiave in MPA",
    "Nessuna moltiplicazione dopo join S13",
    "Nessuna moltiplicazione dopo join BDAP",
    "Tutti i record finali hanno presente_mpa",
    "Lista finale senza suffissi tecnici automatici"
  ),
  esito = c(
    nrow(lista) == nrow(MPA_raw),
    nrow(duplicate_keys_mpa) == 0,
    n_after_s13_join == n_mpa_before_join,
    n_after_bdap_join == n_mpa_before_join,
    sum(is.na(lista$presente_mpa)) == 0,
    length(technical_suffix_cols) == 0
  )
)


if (any(!merge_quality_check$esito)) {
  warning("Alcuni controlli di qualità del merge non sono superati. Controllare merge_quality_check nel file audit.")
}


#..............................................................................
# LOG DI SOVRAPPOSIZIONE TRA FONTI ####
#..............................................................................
# Descrive come le unità del perimetro MPA si distribuiscono rispetto
# alla presenza/assenza nelle fonti di arricchimento S13 e OpenBDAP.
# Utile per documentare dove la lista finale dispone di informazione completa
# e dove invece alcune informazioni di fonte S13 o BDAP non sono disponibili.

source_overlap_log <- lista %>%
  mutate(
    presenza_fonti = case_when(
      presente_mpa == 1 & presente_s13 == 1 & presente_bdap == 1 ~ "MPA + S13 + OpenBDAP",
      presente_mpa == 1 & presente_s13 == 1 & (is.na(presente_bdap) | presente_bdap != 1) ~ "MPA + S13, non OpenBDAP",
      presente_mpa == 1 & (is.na(presente_s13) | presente_s13 != 1) & presente_bdap == 1 ~ "MPA + OpenBDAP, non S13",
      presente_mpa == 1 & (is.na(presente_s13) | presente_s13 != 1) & (is.na(presente_bdap) | presente_bdap != 1) ~ "Solo MPA",
      TRUE ~ "Altro controllo"
    ),
    presenza_fonti = factor(
      presenza_fonti,
      levels = c(
        "MPA + S13 + OpenBDAP",
        "MPA + S13, non OpenBDAP",
        "MPA + OpenBDAP, non S13",
        "Solo MPA",
        "Altro controllo"
      )
    )
  ) %>%
  count(presenza_fonti, name = "n_record", .drop = FALSE) %>%
  filter(n_record > 0) %>%
  mutate(
    totale_lista = sum(n_record),
    quota_record = n_record / totale_lista,
    quota_percentuale = round(100 * quota_record, 2)
  )

mpa_solo <- lista %>%
  filter(
    presente_mpa == 1,
    is.na(presente_s13) | presente_s13 != 1,
    is.na(presente_bdap) | presente_bdap != 1
  ) %>%
  arrange(codice_reg, ragione_sociale)

mpa_s13_non_bdap <- lista %>%
  filter(
    presente_mpa == 1,
    presente_s13 == 1,
    is.na(presente_bdap) | presente_bdap != 1
  ) %>%
  arrange(codice_reg, ragione_sociale)

mpa_bdap_non_s13 <- lista %>%
  filter(
    presente_mpa == 1,
    presente_bdap == 1,
    is.na(presente_s13) | presente_s13 != 1
  ) %>%
  arrange(codice_reg, ragione_sociale)

#..............................................................................#
#                     METADATI DELLA LISTA FINALE                           ####
#..............................................................................#

# Primo dizionario delle variabili finali.
#
# Questo non sostituisce la documentazione metodologica completa, ma crea già
# una base strutturata: per ogni variabile finale indichiamo fonte, contenuto,
# regola di costruzione e note di qualità.

metadata_variabili <- tribble(
  ~nome_variabile, ~descrizione, ~fonte_prevalente, ~criterio_costruzione, ~uso_previsto,
  
  "codice_fiscale",
  "Codice fiscale dell'amministrazione. È la chiave principale utilizzata per il raccordo tra le fonti.",
  "MPA",
  "Deriva dalla variabile CODICE_FISCALE della lista MPA. La lista finale mantiene il perimetro MPA.",
  "Chiave primaria di raccordo tra fonti e dataset successivi.",
  
  "codice_reg",
  "Codice regione associato all'amministrazione.",
  "MPA",
  "Valore selezionato con priorità MPA, poi OpenBDAP, poi S13.",
  "Classificazioni territoriali e controlli di coerenza.",
  
  "fonte_codice_reg",
  "Fonte da cui deriva il valore finale di codice_reg.",
  "Derivata",
  "Assume il nome della prima fonte valorizzata secondo la priorità MPA > OpenBDAP > S13.",
  "Tracciabilità della fonte del valore selezionato.",
  
  "ragione_sociale",
  "Denominazione principale dell'amministrazione nella lista finale.",
  "MPA",
  "Valore selezionato con priorità MPA, poi OpenBDAP, poi S13.",
  "Identificazione leggibile dell'unità istituzionale.",
  
  "fonte_ragione_sociale",
  "Fonte da cui deriva il valore finale di ragione_sociale.",
  "Derivata",
  "Assume il nome della prima fonte valorizzata secondo la priorità MPA > OpenBDAP > S13.",
  "Tracciabilità della fonte del valore selezionato.",
  
  "fg",
  "Codice della forma giuridica/classificazione FG utilizzata nella lista MPA/S13.",
  "MPA",
  "Valore selezionato con priorità MPA, poi S13. La classificazione BDAP è mantenuta separatamente.",
  "Classificazione principale dell'unità nel perimetro MPA.",
  
  "fonte_fg",
  "Fonte da cui deriva il valore finale di fg.",
  "Derivata",
  "Assume il nome della prima fonte valorizzata secondo la priorità MPA > S13.",
  "Tracciabilità della fonte del valore selezionato.",
  
  "desc_fg",
  "Descrizione testuale della classificazione FG.",
  "MPA",
  "Valore selezionato con priorità MPA, poi S13. La descrizione della forma giuridica BDAP è mantenuta separatamente.",
  "Lettura e interpretazione della classificazione FG.",
  
  "fonte_desc_fg",
  "Fonte da cui deriva il valore finale di desc_fg.",
  "Derivata",
  "Assume il nome della prima fonte valorizzata secondo la priorità MPA > S13.",
  "Tracciabilità della fonte del valore selezionato.",
  
  "codice_unita_mpa",
  "Codice unità presente nella lista MPA.",
  "MPA",
  "Deriva da CODICE_UNITA_UG della lista MPA.",
  "Raccordo con elaborazioni o classificazioni interne basate sulla lista MPA.",
  
  "codice_unita_s13",
  "Codice unità presente nella lista S13.",
  "S13",
  "Deriva da CODICE_UNITA della lista S13, quando disponibile dopo il raccordo per codice fiscale.",
  "Raccordo con la lista S13 e controlli di coerenza.",
  
  "id_ente_bdap",
  "Identificativo dell'ente in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Id_Ente di OpenBDAP, dopo selezione del record attivo in caso di storicizzazione.",
  "Raccordo con OpenBDAP e fonti amministrative collegate.",
  
  "codice_ente_ipa",
  "Codice ente IPA disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Ente_IPA di OpenBDAP.",
  "Raccordo con IPA e dataset che utilizzano codici IPA.",
  
  "codice_ente_siope",
  "Codice ente SIOPE disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Ente_SIOPE di OpenBDAP.",
  "Raccordo con fonti SIOPE e dati contabili/amministrativi.",
  
  "codice_istat_comune",
  "Codice ISTAT del comune associato all'ente in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_ISTAT_Comune di OpenBDAP.",
  "Analisi territoriali e raccordi con classificazioni comunali ISTAT.",
  
  "codice_comune",
  "Codice comune disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Comune di OpenBDAP.",
  "Informazione territoriale e raccordo con fonti comunali.",
  
  "comune",
  "Denominazione del comune disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Dizione_Comune di OpenBDAP.",
  "Descrizione territoriale.",
  
  "codice_provincia",
  "Codice provincia disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Provincia di OpenBDAP.",
  "Analisi territoriali provinciali.",
  
  "sigla_provincia",
  "Sigla della provincia disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Sigla_Provincia di OpenBDAP.",
  "Descrizione territoriale e visualizzazioni.",
  
  "provincia",
  "Denominazione della provincia disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Dizione_Provincia di OpenBDAP.",
  "Descrizione territoriale.",
  
  "codice_regione_bdap",
  "Codice regione riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Regione di OpenBDAP.",
  "Controllo e confronto con il codice regione selezionato nella lista finale.",
  
  "regione_bdap",
  "Denominazione della regione disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Dizione_Regione di OpenBDAP.",
  "Descrizione territoriale.",
  
  "ateco_bdap",
  "Codice ATECO dell'ente riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_ATECO di OpenBDAP.",
  "Classificazione economica e possibili analisi settoriali.",
  
  "descr_ateco_bdap",
  "Descrizione del codice ATECO riportata in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Codice_ATECO di OpenBDAP.",
  "Interpretazione della classificazione ATECO.",
  
  "codice_forma_giuridica_bdap",
  "Codice forma giuridica dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Forma_Giuridica di OpenBDAP. È mantenuto separato da fg.",
  "Classificazione amministrativa BDAP e confronto con FG MPA/S13.",
  
  "descr_forma_giuridica_bdap",
  "Descrizione della forma giuridica dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Forma_Giuridica di OpenBDAP. È mantenuta separata da desc_fg.",
  "Interpretazione della classificazione amministrativa BDAP.",
  
  "codice_tipologia_siope_bdap",
  "Codice tipologia SIOPE dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_SIOPE di OpenBDAP.",
  "Raccordo con classificazioni SIOPE.",
  
  "descr_tipologia_siope_bdap",
  "Descrizione della tipologia SIOPE dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_SIOPE di OpenBDAP.",
  "Interpretazione della classificazione SIOPE.",
  
  "codice_categoria_ipa_bdap",
  "Codice categoria IPA dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Categoria_IPA di OpenBDAP.",
  "Raccordo con classificazioni IPA.",
  
  "descr_categoria_ipa_bdap",
  "Descrizione della categoria IPA dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Categoria_IPA di OpenBDAP.",
  "Interpretazione della classificazione IPA.",
  
  "codice_tipologia_ipa_bdap",
  "Codice tipologia IPA dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_IPA di OpenBDAP.",
  "Raccordo con classificazioni IPA.",
  
  "descr_tipologia_ipa_bdap",
  "Descrizione della tipologia IPA dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_IPA di OpenBDAP.",
  "Interpretazione della classificazione IPA.",
  
  "codice_tipologia_mtur_bdap",
  "Codice tipologia MTUR dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_MTUR di OpenBDAP.",
  "Raccordo con classificazioni MTUR, se rilevanti.",
  
  "descr_tipologia_mtur_bdap",
  "Descrizione della tipologia MTUR dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_MTUR di OpenBDAP.",
  "Interpretazione della classificazione MTUR.",
  
  "codice_tipologia_dt_bdap",
  "Codice tipologia DT dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_DT di OpenBDAP.",
  "Raccordo con classificazioni DT, se rilevanti.",
  
  "descr_tipologia_dt_bdap",
  "Descrizione della tipologia DT dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_DT di OpenBDAP.",
  "Interpretazione della classificazione DT.",
  
  "codice_tipologia_istat_s13_bdap",
  "Codice tipologia ISTAT S13 riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_ISTAT_S13 di OpenBDAP.",
  "Confronto con il perimetro S13 e classificazioni collegate.",
  
  "descr_tipologia_istat_s13_bdap",
  "Descrizione della tipologia ISTAT S13 riportata in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_ISTAT_S13 di OpenBDAP.",
  "Interpretazione della classificazione ISTAT S13 riportata in BDAP.",
  
  "codice_tipologia_dlgs_118_2011_bdap",
  "Codice tipologia secondo D.Lgs. 118/2011 riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Codice_Tipologia_DLGS_118_2011 di OpenBDAP.",
  "Raccordo con classificazioni amministrativo-contabili.",
  
  "descr_tipologia_dlgs_118_2011_bdap",
  "Descrizione della tipologia secondo D.Lgs. 118/2011 riportata in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Descr_Tipologia_DLGS_118_2011 di OpenBDAP.",
  "Interpretazione della classificazione D.Lgs. 118/2011.",
  
  "data_istituzione_bdap",
  "Data di istituzione dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Istituzione di OpenBDAP.",
  "Informazione storica/anagrafica.",
  
  "data_cessazione_bdap",
  "Data di cessazione dell'ente secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Cessazione di OpenBDAP.",
  "Identificazione di record attivi o cessati.",
  
  "data_inclusione_siope_bdap",
  "Data di inclusione dell'ente in SIOPE secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Inclusione_SIOPE di OpenBDAP.",
  "Tracciabilità della classificazione SIOPE.",
  
  "data_esclusione_siope_bdap",
  "Data di esclusione dell'ente da SIOPE secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Esclusione_SIOPE di OpenBDAP.",
  "Tracciabilità della classificazione SIOPE.",
  
  "data_inclusione_ipa_bdap",
  "Data di inclusione dell'ente in IPA secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Inclusione_IPA di OpenBDAP.",
  "Tracciabilità della classificazione IPA.",
  
  "data_esclusione_ipa_bdap",
  "Data di esclusione dell'ente da IPA secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Esclusione_IPA di OpenBDAP.",
  "Tracciabilità della classificazione IPA.",
  
  "data_inclusione_istat_s13_bdap",
  "Data di inclusione dell'ente nella tipologia ISTAT S13 secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Inclusione_ISTAT_S13 di OpenBDAP.",
  "Tracciabilità della classificazione ISTAT S13 in BDAP.",
  
  "data_esclusione_istat_s13_bdap",
  "Data di esclusione dell'ente dalla tipologia ISTAT S13 secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Esclusione_ISTAT_S13 di OpenBDAP.",
  "Tracciabilità della classificazione ISTAT S13 in BDAP.",
  
  "data_inclusione_dlgs_118_2011_bdap",
  "Data di inclusione dell'ente nella classificazione D.Lgs. 118/2011 secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Inclusione_DLGS_118_2011 di OpenBDAP.",
  "Tracciabilità della classificazione D.Lgs. 118/2011.",
  
  "data_esclusione_dlgs_118_2011_bdap",
  "Data di esclusione dell'ente dalla classificazione D.Lgs. 118/2011 secondo OpenBDAP.",
  "OpenBDAP",
  "Deriva da Data_Esclusione_DLGS_118_2011 di OpenBDAP.",
  "Tracciabilità della classificazione D.Lgs. 118/2011.",
  
  "url_bdap",
  "URL dell'ente disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da URL di OpenBDAP.",
  "Informazione anagrafica e possibile supporto alla consultazione.",
  
  "telefono_bdap",
  "Numero di telefono dell'ente disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Telefono di OpenBDAP.",
  "Informazione di contatto.",
  
  "fax_bdap",
  "Numero di fax dell'ente disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da FAX di OpenBDAP.",
  "Informazione di contatto.",
  
  "indirizzo_bdap",
  "Indirizzo dell'ente disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Indirizzo di OpenBDAP.",
  "Informazione anagrafica.",
  
  "cap_bdap",
  "CAP dell'ente disponibile in OpenBDAP.",
  "OpenBDAP",
  "Deriva da CAP di OpenBDAP.",
  "Informazione territoriale/anagrafica.",
  
  "nome_resp_bdap",
  "Nome del responsabile riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Nome_Resp di OpenBDAP.",
  "Informazione anagrafica di contatto, da usare con cautela perché potenzialmente soggetta ad aggiornamenti.",
  
  "cogn_resp_bdap",
  "Cognome del responsabile riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Cogn_Resp di OpenBDAP.",
  "Informazione anagrafica di contatto, da usare con cautela perché potenzialmente soggetta ad aggiornamenti.",
  
  "titolo_resp_bdap",
  "Titolo/ruolo del responsabile riportato in OpenBDAP.",
  "OpenBDAP",
  "Deriva da Titolo_Resp di OpenBDAP.",
  "Informazione anagrafica di contatto, da usare con cautela perché potenzialmente soggetta ad aggiornamenti.",
  
  "presente_mpa",
  "Indicatore di presenza dell'ente nella lista MPA.",
  "Derivata",
  "Assume valore 1 per tutti i record della lista finale, poiché MPA definisce il perimetro.",
  "Controllo del perimetro della master list.",
  
  "presente_s13",
  "Indicatore di presenza dell'ente nella lista S13.",
  "Derivata",
  "Assume valore 1 se il codice fiscale MPA trova corrispondenza nella lista S13.",
  "Controllo di copertura tra MPA e S13.",
  
  "presente_bdap",
  "Indicatore di presenza dell'ente in OpenBDAP.",
  "Derivata",
  "Assume valore 1 se il codice fiscale MPA trova corrispondenza in OpenBDAP dopo la deduplicazione dei record BDAP.",
  "Controllo di copertura tra MPA e OpenBDAP.",
  
  "bdap_record_storicizzato",
  "Indicatore di presenza di più record OpenBDAP originari associati allo stesso codice fiscale.",
  "Derivata da OpenBDAP",
  "Assume valore 1 se in OpenBDAP erano presenti più righe per lo stesso codice fiscale prima della deduplicazione.",
  "Tracciabilità dei casi storicizzati in OpenBDAP.",
  
  "bdap_storicizzazione_ambigua",
  "Indicatore di potenziale ambiguità nella storicizzazione OpenBDAP.",
  "Derivata da OpenBDAP",
  "Assume valore 1 se, tra i record BDAP storicizzati dello stesso codice fiscale, emergono variazioni potenzialmente rilevanti di denominazione, forma giuridica, ATECO o territorio.",
  "Identificazione di casi da considerare con cautela nelle analisi.",
  
  "bdap_n_righe_originali",
  "Numero di righe OpenBDAP originarie associate allo stesso codice fiscale.",
  "Derivata da OpenBDAP",
  "Calcolata prima della selezione del record BDAP attivo.",
  "Documentazione della deduplicazione BDAP.",
  
  "run_id",
  "Identificativo della run di produzione della lista.",
  "Derivata",
  "Generato automaticamente a partire da data e ora di esecuzione dello script.",
  "Tracciabilità della versione prodotta."
) %>%
  mutate(run_id = RUN_ID, .before = 1)



#..............................................................................#
#                     EXPORT LOCALE IN 07_TEMP                              ####
#..............................................................................#

# Excel per consultazione umana.
writexl::write_xlsx(
  lista,
  path = local_lista_xlsx
)

# RDS come formato operativo per gli script R e la dashboard.
saveRDS(
  object = lista,
  file = local_lista_rds,
  compress = "xz"
)

# JSON opzionale per interoperabilità.
jsonlite::write_json(
  x = lista,
  path = local_lista_json,
  dataframe = "rows",
  na = "null",
  null = "null",
  pretty = FALSE,
  auto_unbox = TRUE,
  digits = NA,
  Date = "ISO8601",
  POSIXt = "ISO8601"
)

## File audit completo.
writexl::write_xlsx(
  list(
    lista = lista,
    metadata_variabili = metadata_variabili,
    coverage_log = coverage_log,
    source_overlap_log = source_overlap_log,
    merge_quality_check = merge_quality_check,
    join_row_count_log = join_row_count_log,
    duplicate_keys_log = duplicate_keys_log,
    duplicate_pairs_log = duplicate_pairs_log,
    conflict_log = conflict_log,
    source_variables = source_variables,
    bdap_duplicate_keys = bdap_duplicate_keys,
    bdap_duplicate_keys_mpa = bdap_duplicate_keys_mpa,
    bdap_active_check = bdap_active_check,
    bdap_duplicate_case_summary = bdap_duplicate_case_summary,
    bdap_active_problems = bdap_active_problems,
    bdap_dedup_rule_log = bdap_dedup_rule_log,
    bdap_storicizzazioni_ambigue = bdap_storicizzazioni_ambigue,
    bdap_rows_excluded_by_dedup = bdap_rows_excluded_by_dedup,
    mpa_solo = mpa_solo,
    mpa_s13_non_bdap = mpa_s13_non_bdap,
    mpa_bdap_non_s13 = mpa_bdap_non_s13,
    s13_fuori_perimetro_mpa = s13_fuori_perimetro_mpa,
    bdap_fuori_perimetro_mpa = bdap_fuori_perimetro_mpa
  ),
  path = local_integrazione_qualita_file
)


# File metadati separato.
writexl::write_xlsx(
  list(
    metadata_variabili = metadata_variabili,
    source_variables = source_variables
  ),
  path = local_metadata_file
)



#..............................................................................#
#                               EXPORT TO DRIVE                             ####
#..............................................................................#
# Lista_raccordo_SIM.xlsx è il file operativo: viene caricato/aggiornato in 01_Dataset/Lists.
drive_upload_or_update(
  local_path = local_lista_xlsx,
  drive_folder_rel = DRIVE_DIR_LISTS,
  drive_name = "Lista_raccordo_SIM.xlsx"
)

drive_upload_or_update(
  local_path = local_lista_rds,
  drive_folder_rel = DRIVE_DIR_LISTS,
  drive_name = "Lista_raccordo_SIM.rds"
)

drive_upload_or_update(
  local_path = local_lista_json,
  drive_folder_rel = DRIVE_DIR_LISTS,
  drive_name = "Lista_raccordo_SIM.json"
)
# Il file audit è run-specific: lo salviamo nei log.
drive_upload_or_update(
  local_path = local_integrazione_qualita_file,
  drive_folder_rel = DRIVE_DIR_METADATA_LISTA,
  drive_name = basename(local_integrazione_qualita_file)
)


# I metadati sono run-specific: li salviamo nella cartella metadata.
drive_upload_or_update(
  local_path = local_metadata_file,
  drive_folder_rel = DRIVE_DIR_METADATA_LISTA,
  drive_name = basename(local_metadata_file)
)


#..............................................................................#
#                          PULIZIA FILE TEMPORANEI                          ####
#..............................................................................#

if (delete_local_temp) {
  files_to_delete <- c(
    local_file_mpa,
    local_file_s13,
    local_file_bdap,
    local_lista_xlsx,
    local_lista_rds,
    local_lista_json,
    local_audit_file,
    local_metadata_file
  )
  
  file.remove(files_to_delete[file.exists(files_to_delete)])
  message("File temporanei cancellati da: ", DIR_TEMP)
} else {
  message("File temporanei mantenuti in: ", DIR_TEMP)
}

#..............................................................................#
technical_suffix_cols <- names(lista)[
  stringr::str_detect(names(lista), "\\.x$|\\.y$|_x$|_y$")
]

merge_quality_check <- tibble(
  check = c(
    "Lista finale conserva numero righe MPA",
    "Nessun duplicato chiave in MPA",
    "Nessuna moltiplicazione dopo join S13",
    "Nessuna moltiplicazione dopo join BDAP",
    "Tutti i record finali hanno presente_mpa",
    "Lista finale senza suffissi tecnici automatici"
  ),
  esito = c(
    nrow(lista) == nrow(MPA_raw),
    nrow(duplicate_keys_mpa) == 0,
    n_after_s13_join == n_mpa_before_join,
    n_after_bdap_join == n_mpa_before_join,
    sum(is.na(lista$presente_mpa)) == 0,
    length(technical_suffix_cols) == 0
  )
)



# Chiude il file e ripristina la console.
console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

# Carica o aggiorna il log nella cartella 05_Logs su Drive.
drive_upload_or_update(
  local_path = console_log_path,
  drive_folder_rel = DRIVE_DIR_LOGS_LISTA
)
