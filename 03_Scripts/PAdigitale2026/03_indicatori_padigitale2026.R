# ============================================================ #
# Script: 03_indicatori_padigitale2026_dashboard.R
# Fonte: PA digitale 2026 - Open data
#
# Obiettivo:
#   1. mantenere l'output long EAV condiviso dal gruppo;
#   2. produrre una fact table a livello candidatura, adatta a filtri multipli;
#   3. produrre una dimensione enti per copertura e normalizzazioni;
#   4. produrre metadati operativi per dashboard e download.
#
# Output indicatori:
#   - INDICATORS_PADIGITALE2026.*
#   - FACT_PADIGITALE2026_DASHBOARD.*
#   - DIM_ENTI_PADIGITALE2026.*
#   - DIM_AVVISI_PADIGITALE2026.*
#
# Output metadati:
#   - MET_INDICATORS_PADIGITALE2026.*
#   - MET_FILTERS_PADIGITALE2026.*
#   - MET_VARIABLES_PADIGITALE2026.*
#
# Nota metodologica:
#   Il long EAV resta il formato canonico comune alle fonti.
#   Per la dashboard con filtri simultanei si usa la fact table, che conserva
#   tutte le dimensioni in colonne separate.
# ============================================================ #

rm(list = ls())

# 1) Configurazione e helper --------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/00_spatial_helpers.R")
source("03_Scripts/helper_console_log.R")

# 2) Pacchetti ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(janitor)
  library(googledrive)
  library(jsonlite)
  library(openxlsx)
})

# 3) Autenticazione ----------------------------------------------------------

googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive"
)

# 4) Parametri ---------------------------------------------------------------

delete_local_temp <- FALSE

# RUN_ID prodotto dallo script 02_raccordo_padigitale2026_lista.
RUN_ID_RACCORDO <- "20260623_021242"

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")

NOME_FILE_LONG <- "lista_pad26_long.json"
NOME_FILE_MASTER <- "lista_pad26_master.json"
NOME_FILE_AVVISI <- "dim_avvisi_padigitale2026.json"

FONTE_LABEL <- "PA digitale 2026"
ENTE_EDITORE <- "Presidenza del Consiglio dei ministri - Dipartimento per la trasformazione digitale"

message("RUN_ID_RACCORDO: ", RUN_ID_RACCORDO)
message("RUN_ID indicatori: ", RUN_ID)

# 5) Directory ---------------------------------------------------------------

DIR_PAD26_PROCESSED_INPUT_LOCAL <- file.path(
  DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_RACCORDO
)

DIR_PAD26_INDICATORS_LOCAL <- file.path(
  DIR_TEMP, "PADigitale2026", "Indicators", RUN_ID
)

DIR_PAD26_METADATA_LOCAL <- file.path(
  DIR_TEMP, "PADigitale2026", "Indicators_met", RUN_ID
)

DIR_PAD26_LOGS_LOCAL <- file.path(
  DIR_TEMP, "PADigitale2026", "Logs", RUN_ID
)

dir.create(DIR_PAD26_PROCESSED_INPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_INDICATORS_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)

DRIVE_PAD26_PROCESSED_INPUT <- file.path(
  DRIVE_DIR_PROCESSED_PAD26,
  RUN_ID_RACCORDO
)

DRIVE_PAD26_INDICATORS <- file.path(
  DRIVE_DIR_INDICATORS_PAD26,
  RUN_ID
)

DRIVE_PAD26_METADATA <- file.path(
  DRIVE_DIR_INDICATORS_MET_PAD26,
  RUN_ID
)

DRIVE_PAD26_LOGS <- file.path(
  DRIVE_DIR_LOGS_PAD26,
  RUN_ID
)

# 7) Log ---------------------------------------------------------------------

console_log <- start_console_log(
  log_dir = DIR_PAD26_LOGS_LOCAL,
  run_id = RUN_ID,
  script_name = "03_indicatori_padigitale2026_dashboard"
)

# 8) Funzioni ----------------------------------------------------------------

leggi_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "json") {
    return(jsonlite::fromJSON(path, simplifyDataFrame = TRUE) %>% tibble::as_tibble())
  }
  
  if (ext == "rds") {
    return(readRDS(path) %>% tibble::as_tibble())
  }
  
  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE) %>% tibble::as_tibble())
  }
  
  stop("Formato non supportato: ", ext)
}

scarica_input <- function(nome_file) {
  local_path <- file.path(DIR_PAD26_PROCESSED_INPUT_LOCAL, nome_file)
  
  drive_download_from_path(
    drive_file_rel = file.path(DRIVE_PAD26_PROCESSED_INPUT, nome_file),
    local_path = local_path
  )
  
  if (!file.exists(local_path)) {
    stop("Input non trovato dopo il download: ", local_path)
  }
  
  local_path
}

ensure_cols <- function(df, cols, value = NA_character_) {
  for (nm in cols) {
    if (!nm %in% names(df)) {
      df[[nm]] <- value
    }
  }
  df
}

safe_date <- function(x) {
  suppressWarnings(as.Date(x))
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_int <- function(x) {
  suppressWarnings(as.integer(x))
}

safe_divide <- function(num, den) {
  dplyr::if_else(
    !is.na(den) & den != 0,
    num / den,
    NA_real_
  )
}

safe_id <- function(...) {
  vals <- list(...)
  vals <- lapply(vals, as.character)
  
  out <- Reduce(
    function(x, y) dplyr::coalesce(x, y),
    vals
  )
  
  out
}

salva_output <- function(
    obj,
    base_filename,
    local_dir,
    drive_dir,
    formati = c("rds", "csv", "json"),
    pretty_json = FALSE
) {
  paths <- character()
  
  if ("rds" %in% formati) {
    p <- file.path(local_dir, paste0(base_filename, ".rds"))
    saveRDS(obj, p)
    paths <- c(paths, p)
  }
  
  if ("csv" %in% formati) {
    p <- file.path(local_dir, paste0(base_filename, ".csv"))
    readr::write_csv(obj, p, na = "")
    paths <- c(paths, p)
  }
  
  if ("json" %in% formati) {
    p <- file.path(local_dir, paste0(base_filename, ".json"))
    
    jsonlite::write_json(
      x = obj,
      path = p,
      dataframe = "rows",
      na = "null",
      null = "null",
      pretty = pretty_json,
      auto_unbox = TRUE,
      digits = NA,
      Date = "ISO8601",
      POSIXt = "ISO8601"
    )
    
    paths <- c(paths, p)
  }
  
  missing <- paths[!file.exists(paths)]
  
  if (length(missing) > 0L) {
    stop(
      "File non creati per ",
      base_filename,
      ": ",
      paste(missing, collapse = ", ")
    )
  }
  
  purrr::walk(
    paths,
    ~ drive_upload_or_update(
      local_path = .x,
      drive_folder_rel = drive_dir
    )
  )
  
  message("Salvato: ", base_filename)
  invisible(paths)
}

# 9) Input -------------------------------------------------------------------

file_long_local <- scarica_input(NOME_FILE_LONG)
file_master_local <- scarica_input(NOME_FILE_MASTER)
file_avvisi_local <- scarica_input(NOME_FILE_AVVISI)

lista_long <- leggi_file(file_long_local) %>%
  janitor::clean_names()

lista_master <- leggi_file(file_master_local) %>%
  janitor::clean_names()

dim_avvisi_input <- leggi_file(file_avvisi_local) %>%
  janitor::clean_names()

message("lista_pad26_long: ", nrow(lista_long), " righe")
message("lista_pad26_master: ", nrow(lista_master), " righe")
message("dim_avvisi_padigitale2026: ", nrow(dim_avvisi_input), " righe")

# 10) Colonne richieste -------------------------------------------------------

required_long <- c(
  "lista_row_id",
  "codice_fiscale",
  "codice_ente_ipa",
  "ragione_sociale",
  "codice_reg",
  "codice_provincia",
  "codice_comune",
  "regione_bdap",
  "provincia",
  "comune",
  "desc_fg",
  "ateco_bdap",
  "descr_ateco_bdap",
  "presente_mpa",
  "presente_s13",
  "presente_bdap",
  "in_pad26",
  "pad26_row_id",
  "pad26_avviso",
  "pad26_titolo_avviso",
  "pad26_misura",
  "pad26_data_inizio_bando",
  "pad26_data_fine_bando",
  "pad26_anno_inizio_bando",
  "pad26_anno_fine_bando",
  "pad26_stato_avviso",
  "pad26_soggetti_destinatari",
  "pad26_stato_candidatura",
  "pad26_data_invio_candidatura",
  "pad26_data_finanziamento",
  "pad26_importo_finanziamento",
  "pad26_match_avviso",
  "pad26_raccordo_avviso_manuale"
)

lista_long <- ensure_cols(lista_long, required_long)

required_master <- c(
  "lista_row_id",
  "codice_fiscale",
  "codice_ente_ipa",
  "ragione_sociale",
  "codice_reg",
  "codice_provincia",
  "codice_comune",
  "regione_bdap",
  "provincia",
  "comune",
  "desc_fg",
  "ateco_bdap",
  "descr_ateco_bdap",
  "presente_mpa",
  "presente_s13",
  "presente_bdap",
  "in_pad26",
  "n_candidature_pad26",
  "n_misure_pad26",
  "importo_finanziato_pad26"
)

lista_master <- ensure_cols(lista_master, required_master)

# 11) Dimensione enti ---------------------------------------------------------

dim_enti <- lista_master %>%
  dplyr::transmute(
    lista_row_id = safe_int(lista_row_id),
    
    pa = safe_id(
      codice_fiscale,
      codice_ente_ipa,
      paste0("LISTA_", lista_row_id)
    ),
    
    codice_fiscale = as.character(codice_fiscale),
    codice_ipa = as.character(codice_ente_ipa),
    nome_ente = as.character(ragione_sociale),
    
    codice_regione = normalizza_codice_regione(codice_reg),
    regione = dplyr::coalesce(
      as.character(regione_bdap),
      NA_character_
    ),
    
    codice_provincia = normalizza_codice_provincia(codice_provincia),
    provincia = as.character(provincia),
    
    codice_comune = as.character(codice_comune),
    comune = as.character(comune),
    
    forma_giuridica = as.character(desc_fg),
    ateco = as.character(ateco_bdap),
    descrizione_ateco = as.character(descr_ateco_bdap),
    
    presente_mpa = safe_int(presente_mpa),
    presente_s13 = safe_int(presente_s13),
    presente_bdap = safe_int(presente_bdap),
    
    in_pad26 = safe_int(in_pad26),
    n_candidature_pad26 = safe_int(n_candidature_pad26),
    n_misure_pad26 = safe_int(n_misure_pad26),
    importo_finanziato_pad26 = safe_num(importo_finanziato_pad26),
    
    fonte = FONTE_LABEL,
    run_id = RUN_ID
  ) %>%
  dplyr::distinct(lista_row_id, .keep_all = TRUE)

stopifnot(
  nrow(dim_enti) == nrow(lista_master),
  !anyDuplicated(dim_enti$lista_row_id)
)

# 12) Fact candidature per filtri multipli -----------------------------------

fact_dashboard <- lista_long %>%
  dplyr::filter(
    safe_int(in_pad26) == 1L,
    !is.na(pad26_row_id)
  ) %>%
  dplyr::transmute(
    candidatura_id = as.character(pad26_row_id),
    lista_row_id = safe_int(lista_row_id),
    
    pa = safe_id(
      codice_fiscale,
      codice_ente_ipa,
      paste0("LISTA_", lista_row_id)
    ),
    
    codice_fiscale = as.character(codice_fiscale),
    codice_ipa = as.character(codice_ente_ipa),
    nome_ente = as.character(ragione_sociale),
    
    # Dimensioni territoriali
    codice_regione = normalizza_codice_regione(codice_reg),
    regione = as.character(regione_bdap),
    codice_provincia = normalizza_codice_provincia(codice_provincia),
    provincia = as.character(provincia),
    codice_comune = as.character(codice_comune),
    comune = as.character(comune),
    
    # Dimensioni strutturali
    forma_giuridica = as.character(desc_fg),
    ateco = as.character(ateco_bdap),
    descrizione_ateco = as.character(descr_ateco_bdap),
    
    # Dimensioni specifiche PA digitale 2026
    avviso_originale = as.character(pad26_avviso),
    titolo_avviso = as.character(pad26_titolo_avviso),
    misura = as.character(pad26_misura),
    
    data_inizio_bando = safe_date(pad26_data_inizio_bando),
    data_fine_bando = safe_date(pad26_data_fine_bando),
    anno_inizio_bando = safe_int(pad26_anno_inizio_bando),
    anno_fine_bando = safe_int(pad26_anno_fine_bando),
    
    stato_avviso = as.character(pad26_stato_avviso),
    soggetti_destinatari = as.character(pad26_soggetti_destinatari),
    
    stato_candidatura = as.character(pad26_stato_candidatura),
    data_invio_candidatura = safe_date(pad26_data_invio_candidatura),
    anno_invio_candidatura = safe_int(format(safe_date(pad26_data_invio_candidatura), "%Y")),
    
    data_finanziamento = safe_date(pad26_data_finanziamento),
    anno_finanziamento = safe_int(format(safe_date(pad26_data_finanziamento), "%Y")),
    
    match_avviso = as.logical(pad26_match_avviso),
    raccordo_avviso_manuale = as.logical(pad26_raccordo_avviso_manuale),
    
    # Misure additive
    candidature_finanziate = 1L,
    importo_finanziato = safe_num(pad26_importo_finanziamento),
    
    fonte = FONTE_LABEL,
    run_id = RUN_ID
  ) %>%
  dplyr::arrange(misura, titolo_avviso, nome_ente)

stopifnot(
  nrow(fact_dashboard) ==
    sum(safe_int(lista_long$in_pad26) == 1L, na.rm = TRUE)
)

# 13) Dimensione avvisi -------------------------------------------------------

dim_avvisi <- dim_avvisi_input %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    run_id = RUN_ID
  ) %>%
  dplyr::distinct(avviso_key, .keep_all = TRUE)

# 14) Long EAV compatibile con lo standard -----------------------------------

# Il long EAV resta disponibile per integrazione con altre fonti.
# La dashboard con filtri multipli deve invece usare fact_dashboard.

base_eav <- fact_dashboard %>%
  dplyr::transmute(
    pa,
    misura,
    titolo_avviso,
    anno_finanziamento,
    codice_regione,
    codice_provincia,
    codice_comune,
    forma_giuridica,
    ateco,
    candidature_finanziate,
    importo_finanziato
  )

emit_fil <- function(df, fil_name, fil_col) {
  df %>%
    dplyr::filter(
      !is.na(.data[[fil_col]]),
      .data[[fil_col]] != ""
    ) %>%
    dplyr::group_by(
      pa,
      .data[[fil_col]]
    ) %>%
    dplyr::summarise(
      ind1 = sum(candidature_finanziate, na.rm = TRUE),
      ind2 = sum(importo_finanziato, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      c(ind1, ind2),
      names_to = "ind",
      values_to = "ind_val"
    ) %>%
    dplyr::transmute(
      pa,
      fil = fil_name,
      fil_val = as.character(.data[[fil_col]]),
      sub_fil = NA_character_,
      sub_fil_val = NA_character_,
      ind,
      ind_val
    )
}

indicatori_long <- dplyr::bind_rows(
  emit_fil(base_eav, "fil_misura", "misura"),
  emit_fil(base_eav, "fil_avviso", "titolo_avviso"),
  emit_fil(base_eav, "fil_anno", "anno_finanziamento"),
  emit_fil(base_eav, "fil_reg", "codice_regione"),
  emit_fil(base_eav, "fil_prov", "codice_provincia"),
  emit_fil(base_eav, "fil_com", "codice_comune"),
  emit_fil(base_eav, "fil_fg", "forma_giuridica"),
  emit_fil(base_eav, "fil_ateco", "ateco")
) %>%
  dplyr::arrange(ind, fil, fil_val, pa)

# 15) Metadati indicatori -----------------------------------------------------

metadata_indicatori <- tibble::tribble(
  ~indicatore, ~label, ~tabella_input, ~formula, ~additivo,
  ~unita_misura, ~descrizione, ~denominatore, ~note,
  
  "candidature_finanziate",
  "Candidature finanziate",
  "FACT_PADIGITALE2026_DASHBOARD",
  "SUM(candidature_finanziate)",
  TRUE,
  "numero",
  "Numero di candidature finanziate dopo l'applicazione dei filtri.",
  NA_character_,
  "Può essere sommato dopo aver applicato i filtri.",
  
  "importo_finanziato",
  "Importo finanziato",
  "FACT_PADIGITALE2026_DASHBOARD",
  "SUM(importo_finanziato)",
  TRUE,
  "euro",
  "Importo complessivo finanziato alle candidature selezionate.",
  NA_character_,
  "Può essere sommato dopo aver applicato i filtri.",
  
  "enti_finanziati",
  "Enti finanziati",
  "FACT_PADIGITALE2026_DASHBOARD",
  "N_DISTINCT(pa)",
  FALSE,
  "numero",
  "Numero distinto di enti con almeno una candidatura nel sottoinsieme filtrato.",
  NA_character_,
  "Calcolare dopo l'applicazione dei filtri.",
  
  "enti_perimetro",
  "Enti del perimetro",
  "DIM_ENTI_PADIGITALE2026",
  "N_DISTINCT(pa)",
  FALSE,
  "numero",
  "Numero distinto di enti nel perimetro di riferimento.",
  NA_character_,
  "Il denominatore dipende dai filtri territoriali e strutturali applicati alla dimensione enti.",
  
  "copertura_perc",
  "Copertura degli enti",
  "FACT + DIM_ENTI",
  "100 * enti_finanziati / enti_perimetro",
  FALSE,
  "%",
  "Quota di enti del perimetro con almeno una candidatura finanziata.",
  "enti_perimetro",
  "I filtri misura/destinatario non restringono automaticamente il denominatore senza una mappa di eleggibilità.",
  
  "importo_medio_candidatura",
  "Importo medio per candidatura",
  "FACT_PADIGITALE2026_DASHBOARD",
  "SUM(importo_finanziato) / SUM(candidature_finanziate)",
  FALSE,
  "euro",
  "Importo medio delle candidature selezionate.",
  "candidature_finanziate",
  "Calcolare dopo l'applicazione dei filtri.",
  
  "importo_medio_ente",
  "Importo medio per ente finanziato",
  "FACT_PADIGITALE2026_DASHBOARD",
  "SUM(importo_finanziato) / N_DISTINCT(pa)",
  FALSE,
  "euro",
  "Importo medio per ente finanziato.",
  "enti_finanziati",
  "Calcolare dopo l'applicazione dei filtri.",
  
  "candidature_per_ente",
  "Candidature per ente finanziato",
  "FACT_PADIGITALE2026_DASHBOARD",
  "SUM(candidature_finanziate) / N_DISTINCT(pa)",
  FALSE,
  "rapporto",
  "Numero medio di candidature per ente finanziato.",
  "enti_finanziati",
  "Calcolare dopo l'applicazione dei filtri.",
  
  "importo_per_ente_perimetro",
  "Importo per ente del perimetro",
  "FACT + DIM_ENTI",
  "SUM(importo_finanziato) / enti_perimetro",
  FALSE,
  "euro",
  "Importo finanziato normalizzato per il numero di enti nel perimetro.",
  "enti_perimetro",
  "Utile per i confronti territoriali.",
  
  "candidature_per_100_enti",
  "Candidature per 100 enti del perimetro",
  "FACT + DIM_ENTI",
  "100 * SUM(candidature_finanziate) / enti_perimetro",
  FALSE,
  "numero per 100 enti",
  "Numero di candidature normalizzato per la dimensione del perimetro.",
  "enti_perimetro",
  "Utile per i confronti territoriali."
) %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    ente_editore = ENTE_EDITORE,
    run_id = RUN_ID,
    .before = 1
  )

# 16) Metadati filtri multipli -----------------------------------------------

metadata_filtri <- tibble::tribble(
  ~filtro, ~label, ~tabella, ~colonna, ~tipo_controllo,
  ~multiselezione, ~applica_fact, ~applica_dim_enti, ~descrizione, ~note,
  
  "misura", "Misura", "FACT_PADIGITALE2026_DASHBOARD", "misura",
  "selectize", TRUE, TRUE, FALSE,
  "Misura PA digitale 2026 associata all'avviso.",
  "Non modifica automaticamente il denominatore di copertura.",
  
  "titolo_avviso", "Avviso", "FACT_PADIGITALE2026_DASHBOARD", "titolo_avviso",
  "selectize", TRUE, TRUE, FALSE,
  "Titolo ufficiale dell'avviso.",
  "Usare per il dettaglio; per confronti sintetici preferire misura.",
  
  "anno_inizio_bando", "Anno apertura bando", "FACT_PADIGITALE2026_DASHBOARD", "anno_inizio_bando",
  "selectize", TRUE, TRUE, FALSE,
  "Anno di apertura del bando.",
  NA_character_,
  
  "anno_finanziamento", "Anno finanziamento", "FACT_PADIGITALE2026_DASHBOARD", "anno_finanziamento",
  "selectize", TRUE, TRUE, FALSE,
  "Anno della data di finanziamento della candidatura.",
  NA_character_,
  
  "soggetti_destinatari", "Soggetti destinatari", "FACT_PADIGITALE2026_DASHBOARD", "soggetti_destinatari",
  "selectize", TRUE, TRUE, FALSE,
  "Categorie destinatarie dichiarate nell'avviso.",
  "Campo descrittivo; può contenere più categorie nello stesso valore.",
  
  "stato_avviso", "Stato avviso", "FACT_PADIGITALE2026_DASHBOARD", "stato_avviso",
  "selectize", TRUE, TRUE, FALSE,
  "Stato amministrativo dell'avviso.",
  "Non equivale allo stato di avanzamento del progetto.",
  
  "stato_candidatura", "Stato candidatura", "FACT_PADIGITALE2026_DASHBOARD", "stato_candidatura",
  "selectize", TRUE, TRUE, FALSE,
  "Stato della candidatura.",
  NA_character_,
  
  "codice_regione", "Regione", "FACT + DIM_ENTI", "codice_regione",
  "selectize", TRUE, TRUE, TRUE,
  "Codice regione dell'ente.",
  "Filtro valido sia per numeratore sia per denominatore.",
  
  "codice_provincia", "Provincia", "FACT + DIM_ENTI", "codice_provincia",
  "selectize", TRUE, TRUE, TRUE,
  "Codice provincia dell'ente.",
  "Filtro valido sia per numeratore sia per denominatore.",
  
  "codice_comune", "Comune", "FACT + DIM_ENTI", "codice_comune",
  "selectize", TRUE, TRUE, TRUE,
  "Codice comune dell'ente.",
  "Filtro valido sia per numeratore sia per denominatore.",
  
  "forma_giuridica", "Forma giuridica", "FACT + DIM_ENTI", "forma_giuridica",
  "selectize", TRUE, TRUE, TRUE,
  "Forma giuridica dell'ente.",
  "Filtro valido sia per numeratore sia per denominatore.",
  
  "ateco", "ATECO", "FACT + DIM_ENTI", "ateco",
  "selectize", TRUE, TRUE, TRUE,
  "Codice ATECO BDAP dell'ente.",
  "Filtro valido sia per numeratore sia per denominatore."
) %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    run_id = RUN_ID,
    .before = 1
  )

# 17) Metadati variabili dei nuovi file --------------------------------------

metadata_variabili <- dplyr::bind_rows(
  tibble::tribble(
    ~dataset, ~variabile, ~tipo, ~ruolo, ~descrizione,
    
    "FACT_PADIGITALE2026_DASHBOARD", "candidatura_id", "character", "chiave",
    "Identificativo della riga candidatura.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "pa", "character", "chiave ente",
    "Identificativo dell'ente usato per conteggi distinti e raccordi.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "nome_ente", "character", "label",
    "Denominazione leggibile dell'ente.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "misura", "character", "filtro",
    "Misura PA digitale 2026 associata all'avviso.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "titolo_avviso", "character", "filtro",
    "Titolo ufficiale dell'avviso.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "anno_inizio_bando", "integer", "filtro",
    "Anno di apertura del bando.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "anno_finanziamento", "integer", "filtro",
    "Anno di finanziamento della candidatura.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "soggetti_destinatari", "character", "filtro",
    "Categorie destinatarie dichiarate nell'avviso.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "stato_avviso", "character", "filtro",
    "Stato amministrativo dell'avviso.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "stato_candidatura", "character", "filtro",
    "Stato della candidatura.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "codice_regione", "character", "filtro",
    "Codice regione dell'ente.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "forma_giuridica", "character", "filtro",
    "Forma giuridica dell'ente.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "ateco", "character", "filtro",
    "Codice ATECO BDAP dell'ente.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "candidature_finanziate", "integer", "misura",
    "Valore additivo pari a 1 per ciascuna candidatura.",
    
    "FACT_PADIGITALE2026_DASHBOARD", "importo_finanziato", "numeric", "misura",
    "Importo finanziato associato alla candidatura.",
    
    "DIM_ENTI_PADIGITALE2026", "pa", "character", "chiave ente",
    "Identificativo univoco dell'ente.",
    
    "DIM_ENTI_PADIGITALE2026", "nome_ente", "character", "label",
    "Denominazione leggibile dell'ente.",
    
    "DIM_ENTI_PADIGITALE2026", "presente_mpa", "integer", "perimetro",
    "Indica se l'ente appartiene al perimetro MPA.",
    
    "DIM_ENTI_PADIGITALE2026", "presente_s13", "integer", "perimetro",
    "Indica se l'ente appartiene al perimetro S13.",
    
    "DIM_ENTI_PADIGITALE2026", "presente_bdap", "integer", "perimetro",
    "Indica se l'ente è presente in BDAP."
  )
) %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    run_id = RUN_ID
  )


# 17.1) Export compatibile con lo schema condiviso del gruppo -----------------
#
# Una riga per combinazione indicatore x filtro disponibile.
# Questo file serve come formato comune di documentazione.
# La dashboard multifiltro continua a leggere FACT_PADIGITALE2026_DASHBOARD
# e DIM_ENTI_PADIGITALE2026.

indicatori_con_perimetro <- c(
  "enti_perimetro",
  "copertura_perc",
  "importo_per_ente_perimetro",
  "candidature_per_100_enti"
)

# Selezioniamo e rinominiamo esplicitamente i campi necessari prima del
# crossing. In questo modo non si creano suffissi .x/.y e non restano
# riferimenti fragili come label.y.
metadata_indicatori_per_standard <- metadata_indicatori %>%
  dplyr::transmute(
    indicatore,
    label_indicatore = label,
    tabella_input,
    formula,
    descrizione_indicatore = descrizione,
    note_indicatore = note
  )

metadata_filtri_per_standard <- metadata_filtri %>%
  dplyr::transmute(
    filtro,
    label_filtro = label,
    applica_dim_enti,
    descrizione_filtro = descrizione,
    note_filtro = note
  )

metadata_indicatori_standard <- tidyr::crossing(
  metadata_indicatori_per_standard,
  metadata_filtri_per_standard
) %>%
  dplyr::filter(
    # Gli indicatori che richiedono il denominatore del perimetro sono
    # documentati soltanto per i filtri applicabili anche a DIM_ENTI.
    !(indicatore %in% indicatori_con_perimetro) |
      applica_dim_enti %in% TRUE
  ) %>%
  dplyr::mutate(
    nome_indicatore_standard = paste0(
      "PAD26_",
      stringr::str_to_upper(indicatore),
      "_",
      stringr::str_to_upper(filtro)
    ),
    
    x1_standard = dplyr::case_when(
      indicatore == "candidature_finanziate" ~
        "candidature_finanziate",
      
      indicatore == "importo_finanziato" ~
        "importo_finanziato",
      
      indicatore == "enti_finanziati" ~
        "pa",
      
      indicatore == "enti_perimetro" ~
        "pa",
      
      indicatore == "copertura_perc" ~
        "enti_finanziati",
      
      indicatore == "importo_medio_candidatura" ~
        "importo_finanziato",
      
      indicatore == "importo_medio_ente" ~
        "importo_finanziato",
      
      indicatore == "candidature_per_ente" ~
        "candidature_finanziate",
      
      indicatore == "importo_per_ente_perimetro" ~
        "importo_finanziato",
      
      indicatore == "candidature_per_100_enti" ~
        "candidature_finanziate",
      
      TRUE ~ NA_character_
    ),
    
    x2_standard = dplyr::case_when(
      indicatore == "copertura_perc" ~
        "enti_perimetro",
      
      indicatore == "importo_medio_candidatura" ~
        "candidature_finanziate",
      
      indicatore == "importo_medio_ente" ~
        "enti_finanziati",
      
      indicatore == "candidature_per_ente" ~
        "enti_finanziati",
      
      indicatore == "importo_per_ente_perimetro" ~
        "enti_perimetro",
      
      indicatore == "candidature_per_100_enti" ~
        "enti_perimetro",
      
      TRUE ~ NA_character_
    ),
    
    anno_metadata = paste(
      sort(
        unique(
          stats::na.omit(
            fact_dashboard$anno_finanziamento
          )
        )
      ),
      collapse = " | "
    ),
    
    note_standard = paste(
      descrizione_indicatore,
      note_indicatore,
      paste0("Filtro: ", label_filtro, "."),
      descrizione_filtro,
      note_filtro,
      sep = " | "
    )
  ) %>%
  dplyr::transmute(
    `Dataset Originale` = dplyr::case_when(
      tabella_input == "DIM_ENTI_PADIGITALE2026" ~
        "DIM_ENTI_PADIGITALE2026.json",
      
      tabella_input == "FACT + DIM_ENTI" ~
        paste0(
          "FACT_PADIGITALE2026_DASHBOARD.json + ",
          "DIM_ENTI_PADIGITALE2026.json"
        ),
      
      TRUE ~
        "FACT_PADIGITALE2026_DASHBOARD.json"
    ),
    
    Nome_variabile = indicatore,
    Nome_indicatore = nome_indicatore_standard,
    Nome_filtro = filtro,
    Nome_sub_filtro = NA_character_,
    Formula = formula,
    X1 = x1_standard,
    X2 = x2_standard,
    X3 = NA_character_,
    Anno_di_riferimento = anno_metadata,
    Note = note_standard
  ) %>%
  dplyr::arrange(
    Nome_variabile,
    Nome_filtro
  )

# 18) Controlli ---------------------------------------------------------------

check_output <- tibble::tibble(
  controllo = c(
    "righe_fact",
    "candidature_input",
    "enti_dimensione",
    "enti_input",
    "fact_pa_missing",
    "fact_misura_missing",
    "fact_importo_missing",
    "duplicati_candidatura_id"
  ),
  valore = c(
    nrow(fact_dashboard),
    sum(safe_int(lista_long$in_pad26) == 1L, na.rm = TRUE),
    nrow(dim_enti),
    nrow(lista_master),
    sum(is.na(fact_dashboard$pa) | fact_dashboard$pa == ""),
    sum(is.na(fact_dashboard$misura) | fact_dashboard$misura == ""),
    sum(is.na(fact_dashboard$importo_finanziato)),
    sum(duplicated(fact_dashboard$candidatura_id))
  )
)

if (check_output$valore[check_output$controllo == "duplicati_candidatura_id"] > 0L) {
  warning("Sono presenti candidatura_id duplicate nella fact table.")
}

# 19) Salvataggio dati --------------------------------------------------------

salva_output(
  indicatori_long,
  "INDICATORS_PADIGITALE2026",
  DIR_PAD26_INDICATORS_LOCAL,
  DRIVE_PAD26_INDICATORS
)

salva_output(
  fact_dashboard,
  "FACT_PADIGITALE2026_DASHBOARD",
  DIR_PAD26_INDICATORS_LOCAL,
  DRIVE_PAD26_INDICATORS
)

salva_output(
  dim_enti,
  "DIM_ENTI_PADIGITALE2026",
  DIR_PAD26_INDICATORS_LOCAL,
  DRIVE_PAD26_INDICATORS
)

salva_output(
  dim_avvisi,
  "DIM_AVVISI_PADIGITALE2026",
  DIR_PAD26_INDICATORS_LOCAL,
  DRIVE_PAD26_INDICATORS
)

# 20) Salvataggio metadati ----------------------------------------------------

salva_output(
  metadata_indicatori,
  "MET_INDICATORS_PADIGITALE2026",
  DIR_PAD26_METADATA_LOCAL,
  DRIVE_PAD26_METADATA,
  formati = c("csv", "json"),
  pretty_json = TRUE
)

salva_output(
  metadata_filtri,
  "MET_FILTERS_PADIGITALE2026",
  DIR_PAD26_METADATA_LOCAL,
  DRIVE_PAD26_METADATA,
  formati = c("csv", "json"),
  pretty_json = TRUE
)

salva_output(
  metadata_variabili,
  "MET_VARIABLES_PADIGITALE2026",
  DIR_PAD26_METADATA_LOCAL,
  DRIVE_PAD26_METADATA,
  formati = c("csv", "json"),
  pretty_json = TRUE
)

salva_output(
  check_output,
  "CHECK_INDICATORS_PADIGITALE2026",
  DIR_PAD26_METADATA_LOCAL,
  DRIVE_PAD26_METADATA,
  formati = c("csv", "json"),
  pretty_json = TRUE
)


# Export con lo stesso schema del file Indicators_PagoPA.
salva_output(
  metadata_indicatori_standard,
  "Indicators_PADigitale2026",
  DIR_PAD26_METADATA_LOCAL,
  DRIVE_PAD26_METADATA,
  formati = c("csv"),
  pretty_json = FALSE
)

local_indicators_standard_xlsx <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "Indicators_PADigitale2026.xlsx"
)

openxlsx::write.xlsx(
  x = metadata_indicatori_standard,
  file = local_indicators_standard_xlsx,
  overwrite = TRUE
)

drive_upload_or_update(
  local_path = local_indicators_standard_xlsx,
  drive_folder_rel = DRIVE_PAD26_METADATA
)

# Workbook leggibile.
metadata_xlsx <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "MET_PADIGITALE2026_DASHBOARD.xlsx"
)

openxlsx::write.xlsx(
  x = list(
    "Indicatori" = metadata_indicatori,
    "Indicatori standard" = metadata_indicatori_standard,
    "Filtri" = metadata_filtri,
    "Variabili" = metadata_variabili,
    "Controlli" = check_output
  ),
  file = metadata_xlsx,
  overwrite = TRUE
)

drive_upload_or_update(
  local_path = metadata_xlsx,
  drive_folder_rel = DRIVE_PAD26_METADATA
)

# 21) Chiusura ---------------------------------------------------------------

console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

drive_upload_or_update(
  local_path = console_log_path,
  drive_folder_rel = DRIVE_PAD26_LOGS
)

if (delete_local_temp) {
  unlink(DIR_PAD26_PROCESSED_INPUT_LOCAL, recursive = TRUE)
  unlink(DIR_PAD26_INDICATORS_LOCAL, recursive = TRUE)
  unlink(DIR_PAD26_METADATA_LOCAL, recursive = TRUE)
}

message("Indicatori: ", DRIVE_PAD26_INDICATORS)
message("Metadati: ", DRIVE_PAD26_METADATA)
message("--- Script completato. RUN_ID: ", RUN_ID, " ---")
