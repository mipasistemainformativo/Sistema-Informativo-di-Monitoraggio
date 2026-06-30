# ..............................................................................
# 05_ca_variable_met.R
# Fonte: Conto Annuale
# Obiettivo: costruire il file metadati del master_CA_multianno prodotto dallo script 02
# ..............................................................................

rm(list = ls())

# 1) SOURCE -------------------------------------------------------------------
source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/Conto_annuale/00_ca_config.R")
source("03_Scripts/helper_console_log.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(writexl)
  library(googledrive)
  library(purrr)
  library(stringr)
  library(tibble)
})

# 2) AUTENTICAZIONE DRIVE ----------------------------------------------------

if (exists("SIM_DRIVE_EMAIL")) {
  options(gargle_oauth_email = SIM_DRIVE_EMAIL)
  
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive",
    cache = TRUE
  )
} else {
  googledrive::drive_auth(
    scopes = "https://www.googleapis.com/auth/drive"
  )
}

# 3) PARAMETRI DEL RUN --------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID metadati CA: ", RUN_ID)

script_name <- "05_ca_variable_met.R"

console_log <- start_console_log(
  log_dir = DRIVE_CA_LOGS,
  run_id = RUN_ID,
  script_name = script_name
)

# 3) RECUPERO VARIABILI LISTA RACCORDO SIM DA ESCLUDERE ----------------------

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

variabili_da_escludere <- setdiff(
  variabili_lista_sim,
  "codice_fiscale"
)

variabili_da_escludere <- c(
  variabili_da_escludere,
  "presente_mpa"
)

message("Variabili escluse dal file metadati CA perché provenienti dalla Lista_raccordo_SIM:")
message(paste(variabili_da_escludere, collapse = ", "))

# 5) RECUPERO ULTIMO MASTER CA MULTIANNO -------------------------------------

processed_dir <- sim_drive_ls_path(DRIVE_CA_PROCESSED, create = FALSE)

file_master <- googledrive::drive_ls(processed_dir) %>%
  filter(str_detect(name, "^master_CA_multianno_.*\\.rds$")) %>%
  arrange(desc(name)) %>%
  slice(1)

if (nrow(file_master) == 0) {
  stop("Nessun file master_CA_multianno_*.rds trovato in: ", DRIVE_CA_PROCESSED)
}

local_master <- file.path("07_Temp", file_master$name[1])

googledrive::drive_download(
  file = file_master,
  path = local_master,
  overwrite = TRUE
)

master_ca_mpa <- readr::read_rds(local_master)

variabili_da_escludere <- c(
  variabili_lista_sim,
  "presente_MPA"
)

message("Variabili escluse dal file metadati CA perché provenienti dalla Lista_raccordo_SIM:")
message(paste(variabili_da_escludere, collapse = ", "))

# 3) DIZIONARIO DESCRITTIVO MANUALE ------------------------------------------

dizionario_ca <- tribble(
  ~nome_variabile_standardizzato, ~descrizione, ~unita_di_misura, ~formula, ~fenomeno_osservabile, ~indicatore_derivato, ~fonte_origine, ~note,
  
  "anno", "Anno di riferimento del dato", "anno", "", "Identificazione temporale", "Anno", "Lista SIM / Conto Annuale", "",
  "codice_fiscale", "Codice fiscale della PA, normalizzato a 11 caratteri", "codice", "", "Identificazione PA", "Chiave PA", "Lista SIM / Anagrafica CA", "",
  "presente_mpa", "Flag di appartenenza al perimetro MPA nella lista di raccordo SIM", "flag 0/1", "", "Perimetro di osservazione", "Presenza MPA", "Lista_raccordo_SIM", "",
  "presente_MPA", "Flag di appartenenza al perimetro MPA riportato nel master finale", "flag 0/1", "", "Perimetro di osservazione", "Presenza MPA", "Lista_raccordo_SIM", "",
  "fonte_conto_annuale", "Flag che indica la presenza di dati Conto Annuale per la PA e anno", "flag 0/1", "1 se n_istituzioni_ca non è NA; 0 altrimenti", "Copertura fonte", "Presenza dati CA", "Conto Annuale", "",
  "n_istituzioni_ca", "Numero di istituzioni CA aggregate sullo stesso codice fiscale", "conteggio", "n_distinct(istituzione)", "Raccordo anagrafico", "Numero istituzioni CA", "Anagrafica CA", "",
  "istituzioni_ca", "Elenco delle chiavi istituzione CA aggregate sul codice fiscale", "testo", "paste(unique(istituzione), collapse='|')", "Raccordo anagrafico", "Istituzioni CA aggregate", "Anagrafica CA", "",
  "desc_tipo_istituzione_ca", "Descrizione del tipo di istituzione nel Conto Annuale", "testo", "", "Classificazione PA", "Tipo istituzione CA", "Anagrafica CA", "",
  "desc_istituzione_ca", "Denominazione dell'istituzione nel Conto Annuale", "testo", "", "Identificazione PA", "Denominazione CA", "Anagrafica CA", "",
  
  "TEMPO_PIENO_UOMINI", "Personale a tempo pieno uomini", "persone", "Somma personale_tempo_pieno_uomini", "Occupazione", "Tempo pieno uomini", "OCCUPAZIONE", "",
  "TEMPO_PIENO_DONNE", "Personale a tempo pieno donne", "persone", "Somma personale_tempo_pieno_donne", "Occupazione", "Tempo pieno donne", "OCCUPAZIONE", "",
  "TEMPO_PIENO_TOT", "Personale a tempo pieno totale", "persone", "TEMPO_PIENO_UOMINI + TEMPO_PIENO_DONNE", "Occupazione", "Tempo pieno totale", "OCCUPAZIONE", "",
  
  "PART_TIME_UOMINI", "Personale part-time uomini", "persone", "PART_TIME_INF50_UOMINI + PART_TIME_SUP50_UOMINI", "Occupazione", "Part-time uomini", "OCCUPAZIONE", "",
  "PART_TIME_DONNE", "Personale part-time donne", "persone", "PART_TIME_INF50_DONNE + PART_TIME_SUP50_DONNE", "Occupazione", "Part-time donne", "OCCUPAZIONE", "",
  "TOT_PART_TIME", "Personale part-time totale", "persone", "PART_TIME_UOMINI + PART_TIME_DONNE", "Occupazione", "Part-time totale", "OCCUPAZIONE", "",
  "PERC_PART_TIME", "Quota percentuale di personale part-time sul personale totale", "percentuale", "TOT_PART_TIME / PERSONALE_TOT * 100", "Occupazione", "Quota part-time", "OCCUPAZIONE", "",
  
  "PERSONALE_UOMINI", "Personale totale uomini", "persone", "TEMPO_PIENO_UOMINI + PART_TIME_UOMINI", "Occupazione", "Personale uomini", "OCCUPAZIONE", "",
  "PERSONALE_DONNE", "Personale totale donne", "persone", "TEMPO_PIENO_DONNE + PART_TIME_DONNE", "Occupazione", "Personale donne", "OCCUPAZIONE", "",
  "PERSONALE_TOT", "Personale totale", "persone", "PERSONALE_UOMINI + PERSONALE_DONNE", "Occupazione", "Personale totale", "OCCUPAZIONE", "",
  
  "ASSUN_UOMINI", "Assunti uomini", "persone", "Somma assunti uomini", "Flussi del personale", "Assunti uomini", "ASSUNTI", "",
  "ASSUN_DONNE", "Assunti donne", "persone", "Somma assunti donne", "Flussi del personale", "Assunti donne", "ASSUNTI", "",
  "ASSUN_TOT", "Assunti totali", "persone", "ASSUN_UOMINI + ASSUN_DONNE", "Flussi del personale", "Assunti totali", "ASSUNTI", "",
  
  "CESS_UOMINI", "Cessati uomini", "persone", "Somma cessati uomini", "Flussi del personale", "Cessati uomini", "CESSATI", "",
  "CESS_DONNE", "Cessati donne", "persone", "Somma cessati donne", "Flussi del personale", "Cessati donne", "CESSATI", "",
  "CESS_TOT", "Cessati totali", "persone", "CESS_UOMINI + CESS_DONNE", "Flussi del personale", "Cessati totali", "CESSATI", "",
  
  "PERSONALE_TOT_ETA", "Personale considerato nel dataset età media", "persone", "uomini + donne per fascia di età", "Struttura anagrafica", "Personale base età", "ETA_MEDIA", "",
  "ETA_MEDIA_PA", "Età media ponderata del personale della PA", "anni", "media ponderata di media_uomini e media_donne con pesi uomini e donne", "Struttura anagrafica", "Età media", "ETA_MEDIA", "",
  
  "UNDER35_UOMINI", "Personale uomini con meno di 35 anni", "persone", "Somma uomini nelle fasce E0, E20, E25, E30", "Struttura anagrafica", "Under 35 uomini", "ETA_MEDIA", "",
  "UNDER35_DONNE", "Personale donne con meno di 35 anni", "persone", "Somma donne nelle fasce E0, E20, E25, E30", "Struttura anagrafica", "Under 35 donne", "ETA_MEDIA", "",
  "UNDER35", "Personale totale con meno di 35 anni", "persone", "UNDER35_UOMINI + UNDER35_DONNE", "Struttura anagrafica", "Under 35 totale", "ETA_MEDIA", "",
  "QUOTA_UNDER35_PERC", "Quota percentuale di personale under 35 sul personale totale età", "percentuale", "UNDER35 / PERSONALE_TOT_ETA * 100", "Struttura anagrafica", "Quota under 35", "ETA_MEDIA", "",
  
  "QUOTA_UNDER35_UOMINI_PERC", "Quota percentuale uomini tra il personale under 35", "percentuale", "UNDER35_UOMINI / UNDER35 * 100", "Struttura anagrafica", "Composizione uomini under 35", "ETA_MEDIA", "",
  "QUOTA_UNDER35_DONNE_PERC", "Quota percentuale donne tra il personale under 35", "percentuale", "UNDER35_DONNE / UNDER35 * 100", "Struttura anagrafica", "Composizione donne under 35", "ETA_MEDIA", "",
  
  "OVER55_UOMINI", "Personale uomini con più di 55 anni", "persone", "Somma uomini nelle fasce E55, E60, E65, E68", "Struttura anagrafica", "Over 55 uomini", "ETA_MEDIA", "",
  "OVER55_DONNE", "Personale donne con più di 55 anni", "persone", "Somma donne nelle fasce E55, E60, E65, E68", "Struttura anagrafica", "Over 55 donne", "ETA_MEDIA", "",
  "OVER55", "Personale totale con più di 55 anni", "persone", "OVER55_UOMINI + OVER55_DONNE", "Struttura anagrafica", "Over 55 totale", "ETA_MEDIA", "",
  "QUOTA_OVER55_PERC", "Quota percentuale di personale over 55 sul personale totale età", "percentuale", "OVER55 / PERSONALE_TOT_ETA * 100", "Struttura anagrafica", "Quota over 55", "ETA_MEDIA", "",
  
  "OVER65_UOMINI", "Personale uomini con più di 65 anni", "persone", "Somma uomini nelle fasce E65, E68", "Struttura anagrafica", "Over 65 uomini", "ETA_MEDIA", "",
  "OVER65_DONNE", "Personale donne con più di 65 anni", "persone", "Somma donne nelle fasce E65, E68", "Struttura anagrafica", "Over 65 donne", "ETA_MEDIA", "",
  "OVER65", "Personale totale con più di 65 anni", "persone", "OVER65_UOMINI + OVER65_DONNE", "Struttura anagrafica", "Over 65 totale", "ETA_MEDIA", "",
  "QUOTA_OVER65_PERC", "Quota percentuale di personale over 65 sul personale totale età", "percentuale", "OVER65 / PERSONALE_TOT_ETA * 100", "Struttura anagrafica", "Quota over 65", "ETA_MEDIA", "",
  
  "INDICE_RICAMBIO_GENERAZIONALE", "Rapporto tra personale under 35 e personale over 55", "rapporto", "UNDER35 / OVER55", "Ricambio generazionale", "Indice ricambio generazionale", "ETA_MEDIA", "",
  
  "GIORNI_FORM_UOMINI", "Giorni complessivi di formazione fruiti dagli uomini", "giorni", "Somma form_uomini", "Formazione", "Giorni formazione uomini", "FORMAZIONE", "",
  "GIORNI_FORM_DONNE", "Giorni complessivi di formazione fruiti dalle donne", "giorni", "Somma form_donne", "Formazione", "Giorni formazione donne", "FORMAZIONE", "",
  "GIORNI_FORM_TOT", "Giorni complessivi di formazione", "giorni", "GIORNI_FORM_UOMINI + GIORNI_FORM_DONNE", "Formazione", "Giorni formazione totale", "FORMAZIONE", "",
  "GIORNI_FORM_PER_DIPENDENTE", "Giorni medi di formazione per dipendente", "giorni per dipendente", "GIORNI_FORM_TOT / PERSONALE_TOT", "Formazione", "Giorni formazione per dipendente", "FORMAZIONE / OCCUPAZIONE", "",
  
  "FORM_MEDIA_UOMINI_CA", "Media dei valori FORM_MEDIA_UOMINI disponibili nel CA", "giorni medi", "mean(FORM_MEDIA_UOMINI, na.rm = TRUE)", "Formazione", "Formazione media uomini CA", "FORMAZIONE", "Variabile da interpretare con cautela perché calcolata come media dei record CA aggregati.",
  "FORM_MEDIA_DONNE_CA", "Media dei valori FORM_MEDIA_DONNE disponibili nel CA", "giorni medi", "mean(FORM_MEDIA_DONNE, na.rm = TRUE)", "Formazione", "Formazione media donne CA", "FORMAZIONE", "Variabile da interpretare con cautela perché calcolata come media dei record CA aggregati.",
  
  "TOTALE_SPESA", "Totale spesa del personale rilevata nel dataset costo lavoro", "euro", "Somma totale_spesa", "Costo del lavoro", "Totale spesa", "COSTO_LAVORO", "",
  "SPESA_FORMAZIONE_L020", "Spesa per formazione identificata dalla voce L020", "euro", "Somma totale_spesa dove voce_spesa == L020", "Formazione", "Spesa formazione", "COSTO_LAVORO", "",
  "SPESA_FORMAZIONE_PER_DIPENDENTE", "Spesa per formazione per dipendente", "euro per dipendente", "SPESA_FORMAZIONE_L020 / PERSONALE_TOT", "Formazione", "Spesa formazione per dipendente", "COSTO_LAVORO / OCCUPAZIONE", "",
  "INCIDENZA_SPESA_FORMAZIONE_PERC", "Incidenza percentuale della spesa per formazione sulla spesa totale", "percentuale", "SPESA_FORMAZIONE_L020 / TOTALE_SPESA * 100", "Formazione", "Incidenza spesa formazione", "COSTO_LAVORO", "",
  "VOCE_SPESA_STR", "Elenco delle voci di spesa aggregate per PA e anno", "testo", "paste(unique(voce_spesa), collapse='|')", "Costo del lavoro", "Voci spesa disponibili", "COSTO_LAVORO", ""
)

# 4) ESTRAZIONE METADATI AUTOMATICA ------------------------------------------

estrai_metadati_master <- function(df, nome_ds = "master_CA_multianno") {
  tibble(
    fonte = "Conto Annuale",
    dataset_id = nome_ds,
    nome_variabile_originale = names(df),
    nome_variabile_standardizzato = names(df),
    tipo_dato_dopo_import = map_chr(df, ~ class(.x)[1]),
    n_missing = map_dbl(df, ~ sum(is.na(.x))),
    pct_missing = map_dbl(df, ~ round(mean(is.na(.x)) * 100, 2)),
    n_valori_distinti = map_dbl(df, ~ n_distinct(.x)),
    esempi_valori = map_chr(df, function(x) {
      esempi <- head(unique(na.omit(x)), 3)
      if (length(esempi) == 0) return("Tutti NA")
      paste(as.character(esempi), collapse = ", ")
    })
  )
}

metadati_auto <- estrai_metadati_master(
  master_ca_mpa %>%
    dplyr::select(-dplyr::any_of(variabili_da_escludere))
)

report_metadati_ca <- metadati_auto %>%
  left_join(
    dizionario_ca,
    by = "nome_variabile_standardizzato"
  ) %>%
  mutate(
    descrizione = if_else(is.na(descrizione), "Variabile presente nel master CA; descrizione da completare", descrizione),
    unita_di_misura = if_else(is.na(unita_di_misura), "n.d.", unita_di_misura),
    formula = if_else(is.na(formula), "", formula),
    fenomeno_osservabile = if_else(is.na(fenomeno_osservabile), "n.d.", fenomeno_osservabile),
    indicatore_derivato = if_else(is.na(indicatore_derivato), "n.d.", indicatore_derivato),
    fonte_origine = if_else(is.na(fonte_origine), "n.d.", fonte_origine),
    note = if_else(is.na(note), "", note)
  ) %>%
  select(
    fonte,
    dataset_id,
    nome_variabile_originale,
    nome_variabile_standardizzato,
    tipo_dato_dopo_import,
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
    note
  )

# 5) CONTROLLO VARIABILI NON DOCUMENTATE -------------------------------------

variabili_non_documentate <- report_metadati_ca %>%
  filter(descrizione == "Variabile presente nel master CA; descrizione da completare") %>%
  pull(nome_variabile_standardizzato)

if (length(variabili_non_documentate) > 0) {
  warning(
    "Ci sono variabili nel master non presenti nel dizionario_ca: ",
    paste(variabili_non_documentate, collapse = ", ")
  )
} else {
  message(
    "Controllo dizionario CA superato: tutte le variabili presenti nel master sono documentate."
  )
}

# 6) EXPORT LOCALE ------------------------------------------------------------

# dir.create("02_Metadata/Conto_annuale", recursive = TRUE, showWarnings = FALSE)
# 
# file_excel <- file.path("02_Metadata/Conto_annuale", "CA_variables.xlsx")
# file_csv   <- file.path("02_Metadata/Conto_annuale", "CA_variables.csv")
# 
# writexl::write_xlsx(report_metadati_ca, file_excel)
# readr::write_excel_csv2(report_metadati_ca, file_csv)
# 
# message("Report metadati CA generato localmente:")
# message(" - ", file_excel)
# message(" - ", file_csv)

# 7) EXPORT SU DRIVE ----------------------------------------------------------

metadata_dir <- sim_drive_ls_path(DRIVE_CA_VARIABLES_MET, create = TRUE)

googledrive::drive_put(
  media = file_excel,
  path = metadata_dir,
  name = "CA_variables.xlsx"
)

googledrive::drive_put(
  media = file_csv,
  path = metadata_dir,
  name = "CA_variables.csv"
)

message("Upload metadati CA completato su Drive:")

message(" - ", DRIVE_CA_VARIABLES_MET,"/",basename(file_excel))

message(" - ", DRIVE_CA_VARIABLES_MET,"/", basename(file_csv))

# 8) PULIZIA TEMP -------------------------------------------------------------

file.remove(local_master, file_excel, file_csv)

message("Pulizia file temporanei completata.")

message("Fine costruzione metadati master CA.")

# 9) CHIUSURA LOG ------------------------------------------------------------

console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

message(
  "Log generato: ",
  basename(console_log_path)
)

message(
  "Percorso locale log: ",
  console_log_path
)

sim_drive_upload(
  local_file = console_log_path,
  drive_dir = sim_drive_ls_path(DRIVE_CA_LOGS, create = TRUE)
)

message("Log caricato su Drive: ", DRIVE_CA_LOGS,"/",basename(console_log_path))