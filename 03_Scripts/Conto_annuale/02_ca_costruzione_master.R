# ============== README =====================
# 02_ca_costruzione_master_pa.R
# Fonte: Conto Annuale
# Fase: costruzione master PA/anno raccordato al perimetro lista MPA

# Logica operativa:
#   1. Legge i dati CA da Drive:
#      01_Dataset/Source/Conto_annuale/CA_<anno>/Dati
#   2. Legge l'anagrafica istituzioni da:
#      01_Dataset/Source/Conto_annuale/CA_<anno>/Anagrafiche/
#      TipoIstituzione_Istituzione_<anno>.CSV
#   3. Crea la chiave:
#      istituzione = CODI_TIPO_ISTITUZIONE + CODI_ISTITUZIONE
#   4. Standardizza il codice fiscale a 11 caratteri con padding a sinistra
#   5. Aggrega i dataset CA per anno + istituzione
#   6. Passa da istituzione a codice fiscale tramite anagrafica CA
#   7. Usa Lista_raccordo_SIM con presente_mpa = 1 come base del master
#   8. Arricchisce la base MPA con i dati del Conto Annuale
#   9. Salva output versionati su Drive e log locali nel repository


# 1) SOURCE -------------------------------------------------------------------
rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")
source("03_Scripts/helper_console_log.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/Conto_annuale/00_ca_config.R")


suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(readxl)
  library(janitor)
  library(googledrive)
  library(tibble)
})

# 2 ) AUTENTICAZIONE ----------------------------------------------------------

# Autenticazione Drive: usa l'account con accesso al Drive SIM.
# Se in 00_config.R hai SIM_DRIVE_EMAIL, usa quello. Altrimenti usa la cache.
if (exists("SIM_DRIVE_EMAIL")) {
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

anni_ca <- c(2021, 2022, 2023)

# DIR_LOGS_CA <- file.path("05_Logs", "Conto_annuale")
# if (!dir.exists(DIR_LOGS_CA)) dir.create(DIR_LOGS_CA, recursive = TRUE, showWarnings = FALSE)

# 3) PARAMETRI DEL RUN --------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID import: ", RUN_ID)

# parametro per pulire la cartella temp alla fine del run
delete_local_temp <- FALSE

# 4) AVVIO CONSOLE LOG --------------------------------------------------------

script_name <- "02_ca_costruzione_master_pa.R"
console_log <- start_console_log(
  log_dir = DRIVE_CA_LOGS,
  run_id = RUN_ID,
  script_name = script_name
)

# 5) FUNZIONI STANDARDIZZAZIONE --------------------------------------------

# ca_to_num <- function(x) {
#   x <- as.character(x)
#   x <- stringr::str_replace_all(x, "\\.", "")
#   x <- stringr::str_replace_all(x, ",", ".")
#   suppressWarnings(as.numeric(x))
# }

ca_norm_cf <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\.0$", "") %>%
    stringr::str_replace_all("[^0-9A-Za-z]", "") %>%
    stringr::str_to_upper() %>%
    stringr::str_pad(width = 11, side = "left", pad = "0")
}

ca_pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[1]
}

ca_safe_sum <- function(x) {
  sim_safe_sum(x)
}

ca_weighted_mean <- function(x, w) {
  w <- ifelse(is.na(w), 0, w)
  x <- ifelse(is.na(x), NA_real_, x)

  if (sum(w, na.rm = TRUE) == 0) {
    return(NA_real_)
  }

  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

ca_add_istituzione <- function(df) {
  df <- df %>% janitor::clean_names()

  if ("istituzione" %in% names(df)) {
    return(df %>% dplyr::mutate(istituzione = as.character(istituzione)))
  }

  if (all(c("codi_tipo_istituzione", "codi_istituzione") %in% names(df))) {
    return(
      df %>%
        dplyr::mutate(
          codi_tipo_istituzione = as.character(codi_tipo_istituzione),
          codi_istituzione = as.character(codi_istituzione),
          istituzione = paste0(codi_tipo_istituzione, codi_istituzione)
        )
    )
  }

  stop(
    "Nel dataset ", unique(df$dataset_origine)[1],
    " manca la chiave istituzione. Colonne disponibili: ",
    paste(names(df), collapse = ", ")
  )
}

ca_read_dataset <- function(anno, pattern, dataset_nome) {
  files <- ca_find_file(anno, "Dati", pattern)
  
  if (nrow(files) == 0) {
    warning("Dataset ", dataset_nome, " non trovato per anno ", anno)
    return(tibble::tibble())
  }
  
  ext <- tools::file_ext(files$name[1])
  
  ca_download_read(
    files[1, ],
    local_name = paste0(dataset_nome, "_", anno, ".", ext)
  ) %>%
    dplyr::mutate(
      anno = anno,
      dataset_origine = dataset_nome,
      file_origine = files$name[1],
      .before = 1
    )
}

# 6) ANAGRAFICA ISTITUZIONI -------------------------------------------------

ca_read_anagrafica_istituzioni <- function(anno) {

  message("Lettura anagrafica istituzioni CA anno ", anno)

  files <- ca_find_file(
    anno = anno,
    sottocartella = "Anagrafiche",
    pattern = "^TipoIstituzione_Istituzione"
  )

  if (nrow(files) == 0) {
    stop("File TipoIstituzione_Istituzione non trovato per anno ", anno)
  }

  ext <- tools::file_ext(files$name[1])

  df <- ca_download_read(
    files[1, ],
    local_name = paste0("TipoIstituzione_Istituzione_", anno, ".", ext)
  )

  required <- c(
    "codi_tipo_istituzione",
    "codi_istituzione",
    "desc_tipo_istituzione",
    "desc_istituzione",
    "codi_fiscale"
  )

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Nel file TipoIstituzione_Istituzione_", anno,
      " mancano colonne: ", paste(missing, collapse = ", "),
      ". Colonne disponibili: ", paste(names(df), collapse = ", ")
    )
  }

  df %>%
    dplyr::transmute(
      anno = anno,
      codi_tipo_istituzione = as.character(codi_tipo_istituzione),
      codi_istituzione = as.character(codi_istituzione),
      istituzione = paste0(codi_tipo_istituzione, codi_istituzione),
      desc_tipo_istituzione_ca = as.character(desc_tipo_istituzione),
      desc_istituzione_ca = as.character(desc_istituzione),
      codice_fiscale = ca_norm_cf(codi_fiscale),
      file_anagrafica = files$name[1]
    ) %>%
    dplyr::filter(
      !is.na(istituzione),
      istituzione != "",
      !is.na(codice_fiscale),
      codice_fiscale != ""
    ) %>%
    dplyr::distinct(anno, istituzione, .keep_all = TRUE)
}

# 7) AGGREGAZIONI DATASET CA ------------------------------------------------

ca_agg_occupazione <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())

  df <- ca_add_istituzione(df)

  required <- c(
    "personale_tempo_pieno_uomini",
    "personale_tempo_pieno_donne",
    "part_time_inf50_percent_uomini",
    "part_time_inf50_percent_donne",
    "part_time_sup50_percent_uomini",
    "part_time_sup50_percent_donne"
  )

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Il dataset OCCUPAZIONE non contiene le colonne attese: ",
      paste(missing, collapse = ", "),
      ". Colonne disponibili: ", paste(names(df), collapse = ", ")
    )
  }

  df %>%
    dplyr::mutate(
      personale_tempo_pieno_uomini   = as.numeric(.data$personale_tempo_pieno_uomini),
      personale_tempo_pieno_donne    = as.numeric(.data$personale_tempo_pieno_donne),
      part_time_inf50_percent_uomini = as.numeric(.data$part_time_inf50_percent_uomini),
      part_time_inf50_percent_donne  = as.numeric(.data$part_time_inf50_percent_donne),
      part_time_sup50_percent_uomini = as.numeric(.data$part_time_sup50_percent_uomini),
      part_time_sup50_percent_donne  = as.numeric(.data$part_time_sup50_percent_donne),

      TEMPO_PIENO_UOMINI = personale_tempo_pieno_uomini,
      TEMPO_PIENO_DONNE  = personale_tempo_pieno_donne,
      TEMPO_PIENO_TOT    = TEMPO_PIENO_UOMINI + TEMPO_PIENO_DONNE,

      PART_TIME_INF50_UOMINI = part_time_inf50_percent_uomini,
      PART_TIME_INF50_DONNE  = part_time_inf50_percent_donne,
      PART_TIME_SUP50_UOMINI = part_time_sup50_percent_uomini,
      PART_TIME_SUP50_DONNE  = part_time_sup50_percent_donne,
      PART_TIME_UOMINI = PART_TIME_INF50_UOMINI + PART_TIME_SUP50_UOMINI,
      PART_TIME_DONNE  = PART_TIME_INF50_DONNE + PART_TIME_SUP50_DONNE,
      TOT_PART_TIME    = PART_TIME_UOMINI + PART_TIME_DONNE,

      PERSONALE_UOMINI = TEMPO_PIENO_UOMINI + PART_TIME_UOMINI,
      PERSONALE_DONNE  = TEMPO_PIENO_DONNE  + PART_TIME_DONNE,
      PERSONALE_TOT    = PERSONALE_UOMINI + PERSONALE_DONNE
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      TEMPO_PIENO_UOMINI = ca_safe_sum(TEMPO_PIENO_UOMINI),
      TEMPO_PIENO_DONNE  = ca_safe_sum(TEMPO_PIENO_DONNE),
      TEMPO_PIENO_TOT    = ca_safe_sum(TEMPO_PIENO_TOT),
      PART_TIME_UOMINI   = ca_safe_sum(PART_TIME_UOMINI),
      PART_TIME_DONNE    = ca_safe_sum(PART_TIME_DONNE),
      TOT_PART_TIME      = ca_safe_sum(TOT_PART_TIME),
      PERSONALE_UOMINI   = ca_safe_sum(PERSONALE_UOMINI),
      PERSONALE_DONNE    = ca_safe_sum(PERSONALE_DONNE),
      PERSONALE_TOT      = ca_safe_sum(PERSONALE_TOT),
      .groups = "drop"
    )
}

ca_agg_flusso <- function(df, prefisso) {
  if (nrow(df) == 0) return(tibble::tibble())

  df <- ca_add_istituzione(df)

  prefisso_low <- stringr::str_to_lower(prefisso)

  col_u <- ca_pick_col(
    df,
    c(
      paste0(prefisso_low, "_uomini"),
      paste0(prefisso, "_uomini"),
      "uomini",
      "maschi"
    )
  )

  col_d <- ca_pick_col(
    df,
    c(
      paste0(prefisso_low, "_donne"),
      paste0(prefisso, "_donne"),
      "donne",
      "femmine"
    )
  )

  col_tot <- ca_pick_col(
    df,
    c(
      paste0(prefisso_low, "_tot"),
      paste0(prefisso, "_tot"),
      paste0(prefisso_low, "_totale"),
      paste0(prefisso, "_totale"),
      "totale",
      "tot"
    )
  )

  df %>%
    dplyr::mutate(
      val_uomini = if (!is.null(col_u)) as.numeric(.data[[col_u]]) else NA_real_,
      val_donne  = if (!is.null(col_d)) as.numeric(.data[[col_d]]) else NA_real_,
      val_totale = if (!is.null(col_tot)) as.numeric(.data[[col_tot]]) else val_uomini + val_donne
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      "{prefisso}_UOMINI" := ca_safe_sum(val_uomini),
      "{prefisso}_DONNE"  := ca_safe_sum(val_donne),
      "{prefisso}_TOT"    := ca_safe_sum(val_totale),
      .groups = "drop"
    )
}

ca_agg_eta <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())

  df <- ca_add_istituzione(df)

  required <- c("fascia_eta", "uomini", "donne", "media_uomini", "media_donne")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Il dataset ETA_MEDIA non contiene le colonne attese: ",
      paste(missing, collapse = ", "),
      ". Colonne disponibili: ", paste(names(df), collapse = ", ")
    )
  }

  df %>%
    dplyr::mutate(
      fascia_eta = as.character(fascia_eta),
      uomini = as.numeric(uomini),
      donne  = as.numeric(donne),
      # ETA_MEDIA nel CSV MEF viene letta in anni.
      # Esempio atteso: 57.5, 62.5, 52.5.
      # Non dividere per 10: il controllo sui dati grezzi ha confermato
      # che media_uomini e media_donne sono già nella scala corretta.
      media_uomini = as.numeric(media_uomini),
      media_donne  = as.numeric(media_donne),
      n_eta = uomini + donne,

      is_under35 = fascia_eta %in% c("E0", "E20", "E25", "E30"),
      is_over55  = fascia_eta %in% c("E55", "E60", "E65", "E68"),
      is_over65  = fascia_eta %in% c("E65", "E68")
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERSONALE_TOT_ETA = ca_safe_sum(n_eta),

      ETA_MEDIA_PA = dplyr::if_else(
        PERSONALE_TOT_ETA == 0,
        NA_real_,
        (
          sum(media_uomini * uomini, na.rm = TRUE) +
            sum(media_donne * donne, na.rm = TRUE)
        ) / PERSONALE_TOT_ETA
      ),

      UNDER35_UOMINI = sum(uomini[is_under35], na.rm = TRUE),
      UNDER35_DONNE  = sum(donne[is_under35], na.rm = TRUE),
      UNDER35        = UNDER35_UOMINI + UNDER35_DONNE,

      OVER55_UOMINI = sum(uomini[is_over55], na.rm = TRUE),
      OVER55_DONNE  = sum(donne[is_over55], na.rm = TRUE),
      OVER55        = OVER55_UOMINI + OVER55_DONNE,

      OVER65_UOMINI = sum(uomini[is_over65], na.rm = TRUE),
      OVER65_DONNE  = sum(donne[is_over65], na.rm = TRUE),
      OVER65        = OVER65_UOMINI + OVER65_DONNE,

      QUOTA_UNDER35_PERC = dplyr::if_else(PERSONALE_TOT_ETA == 0, NA_real_, 100 * UNDER35 / PERSONALE_TOT_ETA),
      QUOTA_UNDER35_UOMINI_PERC = dplyr::if_else(UNDER35 == 0, NA_real_, 100 * UNDER35_UOMINI / UNDER35),
      QUOTA_UNDER35_DONNE_PERC  = dplyr::if_else(UNDER35 == 0, NA_real_, 100 * UNDER35_DONNE  / UNDER35),

      QUOTA_OVER55_PERC = dplyr::if_else(PERSONALE_TOT_ETA == 0, NA_real_, 100 * OVER55 / PERSONALE_TOT_ETA),
      QUOTA_OVER65_PERC = dplyr::if_else(PERSONALE_TOT_ETA == 0, NA_real_, 100 * OVER65 / PERSONALE_TOT_ETA),

      INDICE_RICAMBIO_GENERAZIONALE = dplyr::if_else(OVER55 == 0, NA_real_, UNDER35 / OVER55),

      .groups = "drop"
    )
}

ca_agg_formazione <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())

  df <- ca_add_istituzione(df)

  col_u <- ca_pick_col(df, c("form_uomini", "giorni_form_uomini", "uomini"))
  col_d <- ca_pick_col(df, c("form_donne", "giorni_form_donne", "donne"))
  col_mu <- ca_pick_col(df, c("form_media_uomini", "media_uomini"))
  col_md <- ca_pick_col(df, c("form_media_donne", "media_donne"))

  # NOTA: nelle colonne del FORMAZIONE.CSV del CA MEF:
  # - FORM_UOMINI / FORM_DONNE = numero di PERSONE formate (person count)
  # - FORM_MEDIA_UOMINI / FORM_MEDIA_DONNE = giorni medi di formazione per persona
  # GIORNI_FORM totali = person_count x giorni_medi (somma per categoria/qualifica)
  df %>%
    dplyr::mutate(
      form_u  = if (!is.null(col_u))  as.numeric(.data[[col_u]])  else NA_real_,
      form_d  = if (!is.null(col_d))  as.numeric(.data[[col_d]])  else NA_real_,
      form_mu = if (!is.null(col_mu)) as.numeric(.data[[col_mu]]) else NA_real_,
      form_md = if (!is.null(col_md)) as.numeric(.data[[col_md]]) else NA_real_,
      gg_u = form_u * form_mu,   # giorni totali uomini per questa riga
      gg_d = form_d * form_md    # giorni totali donne per questa riga
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      # Persone formate (count)
      PERS_FORM_UOMINI = ca_safe_sum(form_u),
      PERS_FORM_DONNE  = ca_safe_sum(form_d),
      PERS_FORM_TOT    = PERS_FORM_UOMINI + PERS_FORM_DONNE,
      # Giorni totali di formazione (person_count x giorni_medi)
      GIORNI_FORM_UOMINI = ca_safe_sum(gg_u),
      GIORNI_FORM_DONNE  = ca_safe_sum(gg_d),
      GIORNI_FORM_TOT    = GIORNI_FORM_UOMINI + GIORNI_FORM_DONNE,
      # Giorni medi per persona formata (calcolati come ratio aggregato)
      FORM_MEDIA_UOMINI_CA = dplyr::if_else(
        PERS_FORM_UOMINI > 0, GIORNI_FORM_UOMINI / PERS_FORM_UOMINI, NA_real_),
      FORM_MEDIA_DONNE_CA = dplyr::if_else(
        PERS_FORM_DONNE > 0, GIORNI_FORM_DONNE / PERS_FORM_DONNE, NA_real_),
      .groups = "drop"
    )
}

ca_agg_costo_lavoro <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())

  df <- ca_add_istituzione(df)

  voce_col <- names(df)[
    stringr::str_detect(names(df), stringr::regex("^voce_spesa$|codi_voce_spesa|voce", ignore_case = TRUE))
  ][1]

  spesa_col <- names(df)[
    stringr::str_detect(names(df), stringr::regex("^totale_spesa$|totale", ignore_case = TRUE))
  ][1]

  if (is.na(voce_col) || is.na(spesa_col)) {
    warning("COSTO_LAVORO trovato ma colonne voce/spesa non riconosciute. Colonne disponibili: ", paste(names(df), collapse = ", "))
    return(tibble::tibble())
  }

  df %>%
    dplyr::mutate(
      VOCE_SPESA_TMP = as.character(.data[[voce_col]]),
      TOTALE_SPESA_TMP = as.numeric(.data[[spesa_col]])
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      TOTALE_SPESA = ca_safe_sum(TOTALE_SPESA_TMP),
      SPESA_FORMAZIONE_L020 = ca_safe_sum(dplyr::if_else(VOCE_SPESA_TMP == "L020", TOTALE_SPESA_TMP, 0)),
      VOCE_SPESA_STR = paste(sort(unique(VOCE_SPESA_TMP[!is.na(VOCE_SPESA_TMP)])), collapse = "|"),
      .groups = "drop"
    )
}


# ─────────────────────────────────────────────────────────────────────────────
# NUOVE FUNZIONI DI AGGREGAZIONE - PNRR assi: Accesso/Reclutamento,
# Buona Amministrazione, Competenze e Carriere
# ─────────────────────────────────────────────────────────────────────────────

# 1. TITOLI DI STUDIO (fonte: TITOLI_STUDIO_DATI)
# Asse PNRR: Accesso e reclutamento / Competenze e carriere
# Colonne raw: ISTITUZIONE|CONTRATTO|CATEGORIA|QUALIFICA|TITOLO_STUDIO|UOMINI|DONNE
ca_agg_titoli_studio <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  required <- c("titolo_studio", "uomini", "donne")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning("TITOLI_STUDIO_DATI: colonne mancanti: ", paste(missing, collapse = ", "))
    return(tibble::tibble())
  }

  # Mappa codici TITOLO_STUDIO → macrogruppo
  # Codici CA MEF: T01=nessuno/elem, T02=media inf, T03=diploma,
  #                T04=laurea breve, T05=laurea magistrale, T06=dottorato/spec.
  df %>%
    dplyr::mutate(
      uomini = as.numeric(uomini),
      donne  = as.numeric(donne),
      totale = uomini + donne,
      titolo_norm = stringr::str_to_upper(stringr::str_trim(as.character(titolo_studio))),
      gruppo_titolo = dplyr::case_when(
        titolo_norm %in% c("T01", "1", "NESSUN TITOLO", "LICENZA ELEMENTARE", "NESSUNO") ~ "NESSUNO_ELEMENTARE",
        titolo_norm %in% c("T02", "2", "LICENZA MEDIA", "MEDIA INFERIORE")               ~ "MEDIA_INFERIORE",
        titolo_norm %in% c("T03", "3", "DIPLOMA", "MATURITA", "SCUOLA SUPERIORE",
                           "MEDIA SUPERIORE", "DIPLOMA SCUOLA SUPERIORE")                ~ "DIPLOMA",
        titolo_norm %in% c("T04", "4", "LAUREA BREVE", "LAUREA TRIENNALE",
                           "LAUREA I LIVELLO", "TRIENNALE")                              ~ "LAUREA",
        titolo_norm %in% c("T05", "5", "LAUREA", "LAUREA MAGISTRALE",
                           "LAUREA SPECIALISTICA", "LAUREA II LIVELLO", "MAGISTRALE")   ~ "LAUREA",
        titolo_norm %in% c("T06", "6", "DOTTORATO", "SPECIALIZZAZIONE",
                           "MASTER", "DOTTORATO RICERCA")                               ~ "LAUREA",
        TRUE ~ "ALTRO"
      )
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERS_TOT_TITOLI    = ca_safe_sum(totale),
      LAUREA_TOT         = sum(totale[gruppo_titolo == "LAUREA"],             na.rm = TRUE),
      DIPLOMA_TOT        = sum(totale[gruppo_titolo == "DIPLOMA"],            na.rm = TRUE),
      MEDIA_INF_TOT      = sum(totale[gruppo_titolo == "MEDIA_INFERIORE"],    na.rm = TRUE),
      NESSUNO_EL_TOT     = sum(totale[gruppo_titolo == "NESSUNO_ELEMENTARE"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      QUOTA_LAUREA_PERC  = sim_safe_div(LAUREA_TOT,  PERS_TOT_TITOLI, 100),
      QUOTA_DIPLOMA_PERC = sim_safe_div(DIPLOMA_TOT, PERS_TOT_TITOLI, 100)
    )
}

# 2. MODALITA' LAVORO FLESSIBILE: lavoro agile, telelavoro, coworking
# Asse PNRR: Buona amministrazione e semplificazione
# Colonne raw: ISTITUZIONE|CONTRATTO|MACROCATEGORIA|CATEGORIA|
#   TELE_LAVORO_UOMINI|TELE_LAVORO_DONNE|SOGGETTI_TURNAZIONE_UOMINI|
#   SOGGETTI_TURNAZIONE_DONNE|SOGGETTI_REPERIBILITA_UOMINI|SOGGETTI_REPERIBILITA_DONNE|
#   PERS_LAVORO_AGILE_U|PERS_LAVORO_AGILE_D|PERS_COWORKING_U|PERS_COWORKING_D
ca_agg_lavoro_agile <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  col_la_u <- ca_pick_col(df, c("pers_lavoro_agile_u", "lavoro_agile_uomini", "smart_working_uomini"))
  col_la_d <- ca_pick_col(df, c("pers_lavoro_agile_d", "lavoro_agile_donne",  "smart_working_donne"))
  col_tl_u <- ca_pick_col(df, c("tele_lavoro_uomini", "telelavoro_uomini"))
  col_tl_d <- ca_pick_col(df, c("tele_lavoro_donne",  "telelavoro_donne"))
  col_cw_u <- ca_pick_col(df, c("pers_coworking_u", "coworking_uomini"))
  col_cw_d <- ca_pick_col(df, c("pers_coworking_d", "coworking_donne"))

  df %>%
    dplyr::mutate(
      la_u = if (!is.null(col_la_u)) as.numeric(.data[[col_la_u]]) else NA_real_,
      la_d = if (!is.null(col_la_d)) as.numeric(.data[[col_la_d]]) else NA_real_,
      tl_u = if (!is.null(col_tl_u)) as.numeric(.data[[col_tl_u]]) else NA_real_,
      tl_d = if (!is.null(col_tl_d)) as.numeric(.data[[col_tl_d]]) else NA_real_,
      cw_u = if (!is.null(col_cw_u)) as.numeric(.data[[col_cw_u]]) else NA_real_,
      cw_d = if (!is.null(col_cw_d)) as.numeric(.data[[col_cw_d]]) else NA_real_
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERS_LAVORO_AGILE_UOMINI = ca_safe_sum(la_u),
      PERS_LAVORO_AGILE_DONNE  = ca_safe_sum(la_d),
      PERS_LAVORO_AGILE_TOT    = PERS_LAVORO_AGILE_UOMINI + PERS_LAVORO_AGILE_DONNE,
      PERS_TELE_LAVORO_UOMINI  = ca_safe_sum(tl_u),
      PERS_TELE_LAVORO_DONNE   = ca_safe_sum(tl_d),
      PERS_TELE_LAVORO_TOT     = PERS_TELE_LAVORO_UOMINI + PERS_TELE_LAVORO_DONNE,
      PERS_COWORKING_UOMINI    = ca_safe_sum(cw_u),
      PERS_COWORKING_DONNE     = ca_safe_sum(cw_d),
      PERS_COWORKING_TOT       = PERS_COWORKING_UOMINI + PERS_COWORKING_DONNE,
      PERS_MOD_FLESSIBILE_TOT  = PERS_LAVORO_AGILE_TOT + PERS_TELE_LAVORO_TOT + PERS_COWORKING_TOT,
      .groups = "drop"
    )
}

# 3. ASSENZE: giorni totali per causale
# Asse PNRR: Buona amministrazione e semplificazione
# Colonne raw: ISTITUZIONE|CONTRATTO|CATEGORIA|QUALIFICA|CAUSALE_ASSENZA|ASSENZE_UOMINI|ASSENZE_DONNE
# GG_ASSENZA_PER_DIP calcolato nel mutate finale di ca_costruisci_anno (/ PERSONALE_TOT)
ca_agg_assenze <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  col_u <- ca_pick_col(df, c("assenze_uomini", "uomini"))
  col_d <- ca_pick_col(df, c("assenze_donne",  "donne"))
  col_c <- ca_pick_col(df, c("causale_assenza", "causale"))

  if (is.null(col_u) || is.null(col_d)) {
    warning("ASSENZE: colonne uomini/donne non trovate. Colonne: ", paste(names(df), collapse = ", "))
    return(tibble::tibble())
  }

  # Causali malattia: nel CA le cause 01-03 (malattia, ricovero, day hospital)
  # sono le principali assenze per motivi di salute
  causali_malattia <- c("01", "02", "03", "1", "2", "3",
                        "MALATTIA", "RICOVERO", "DAY HOSPITAL")

  df %>%
    dplyr::mutate(
      ass_u   = as.numeric(.data[[col_u]]),
      ass_d   = as.numeric(.data[[col_d]]),
      causale_norm = if (!is.null(col_c))
        stringr::str_to_upper(stringr::str_trim(as.character(.data[[col_c]])))
        else NA_character_,
      is_malattia = causale_norm %in% causali_malattia
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      ASSENZE_UOMINI          = ca_safe_sum(ass_u),
      ASSENZE_DONNE           = ca_safe_sum(ass_d),
      ASSENZE_TOT             = ASSENZE_UOMINI + ASSENZE_DONNE,
      ASSENZE_MALATTIA_UOMINI = sum(ass_u[is_malattia], na.rm = TRUE),
      ASSENZE_MALATTIA_DONNE  = sum(ass_d[is_malattia], na.rm = TRUE),
      ASSENZE_MALATTIA_TOT    = ASSENZE_MALATTIA_UOMINI + ASSENZE_MALATTIA_DONNE,
      .groups = "drop"
    )
}

# 4. PASSAGGI DI QUALIFICA: progressioni di carriera
# Asse PNRR: Competenze e carriere
# Colonne raw: ISTITUZIONE|CONTRATTO|CATEGORIA_PARTENZA|QUALIFICA_PARTENZA|
#              CATEGORIA_ARRIVO|QUALIFICA_ARRIVO|TIPO_PASSAGGIO|NUMERO_PASSAGGI
# TASSO_PROGRESSIONE_PERC calcolato nel mutate finale (/ PERSONALE_TOT)
ca_agg_passaggi_qualifica <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  col_n <- ca_pick_col(df, c("numero_passaggi", "passaggi", "totale", "tot"))
  col_t <- ca_pick_col(df, c("tipo_passaggio", "tipo"))

  if (is.null(col_n)) {
    warning("PASSAGGI_QUALIFICA: colonna numero_passaggi non trovata. Colonne: ",
            paste(names(df), collapse = ", "))
    return(tibble::tibble())
  }

  df %>%
    dplyr::mutate(
      n_pass    = as.numeric(.data[[col_n]]),
      tipo_norm = if (!is.null(col_t))
        stringr::str_to_upper(stringr::str_trim(as.character(.data[[col_t]])))
        else NA_character_
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PASSAGGI_QUALIFICA_TOT         = ca_safe_sum(n_pass),
      PASSAGGI_CONCORSO_TOT          = sum(n_pass[stringr::str_detect(
        tipo_norm, "CONCORSO|SELEZIONE|ESAME", negate = FALSE)], na.rm = TRUE),
      PASSAGGI_PROGRESSIONE_TOT      = sum(n_pass[stringr::str_detect(
        tipo_norm, "PROGRESSIONE|AVANZAMENTO|INTERNO", negate = FALSE)], na.rm = TRUE),
      .groups = "drop"
    )
}

# 5. ANZIANITA': distribuzione per fascia e anzianita' media ponderata
# Asse PNRR: Accesso e reclutamento / Competenze e carriere
# Colonne raw: ISTITUZIONE|CONTRATTO|CATEGORIA|QUALIFICA|FASCIA_ANZIANITA|UOMINI|DONNE
# I punti-medi delle fasce CA (in anni di servizio):
#   A00=0-5 -> 2.5  | A05=6-10 -> 7.5  | A10=11-15 -> 12.5 | A15=16-20 -> 17.5
#   A20=21-25 -> 22.5 | A25=26-30 -> 27.5 | A30=31-35 -> 32.5 | A35=oltre 35 -> 40
ca_agg_anzianita <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  required <- c("fascia_anzianita", "uomini", "donne")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning("ANZIANITA: colonne mancanti: ", paste(missing, collapse = ", "))
    return(tibble::tibble())
  }

  midpoints <- c(
    "A00" = 2.5,  "A05" = 7.5,  "A10" = 12.5, "A15" = 17.5,
    "A20" = 22.5, "A25" = 27.5, "A30" = 32.5, "A35" = 40.0,
    "0"   = 2.5,  "5"   = 7.5,  "10"  = 12.5, "15"  = 17.5,
    "20"  = 22.5, "25"  = 27.5, "30"  = 32.5, "35"  = 40.0
  )

  df %>%
    dplyr::mutate(
      fascia_norm = stringr::str_to_upper(stringr::str_trim(as.character(fascia_anzianita))),
      uomini = as.numeric(uomini),
      donne  = as.numeric(donne),
      n_tot  = uomini + donne,
      midpoint = dplyr::recode(fascia_norm, !!!midpoints, .default = NA_real_),
      # fasce per indicatori di ricambio/anzianita'
      is_breve   = fascia_norm %in% c("A00", "A05", "0", "5"),    # < 10 anni
      is_lunga   = fascia_norm %in% c("A25", "A30", "A35", "25", "30", "35") # > 25 anni
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERS_TOT_ANZIANITA    = ca_safe_sum(n_tot),
      ANZIANITA_MEDIA_PA    = dplyr::if_else(
        ca_safe_sum(n_tot) == 0, NA_real_,
        sum(midpoint * n_tot, na.rm = TRUE) / ca_safe_sum(n_tot)
      ),
      PERS_ANZIANITA_BREVE  = sum(n_tot[is_breve], na.rm = TRUE),   # < 10 anni servizio
      PERS_ANZIANITA_LUNGA  = sum(n_tot[is_lunga], na.rm = TRUE),   # > 25 anni servizio
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      QUOTA_ANZIANITA_BREVE_PERC = sim_safe_div(PERS_ANZIANITA_BREVE, PERS_TOT_ANZIANITA, 100),
      QUOTA_ANZIANITA_LUNGA_PERC = sim_safe_div(PERS_ANZIANITA_LUNGA, PERS_TOT_ANZIANITA, 100)
    )
}

# 6. LAVORO FLESSIBILE: personale a tempo determinato e precario
# Asse PNRR: Buona amministrazione e semplificazione
# Colonne raw: ISTITUZIONE|CONTRATTO|MACROCATEGORIA|CATEGORIA|
#   PERSONALE_TEMPO_DETERMINATO_UOMINI|PERSONALE_TEMPO_DETERMINATO_DONNE|
#   FORMAZIONE_LAVORO_UOMINI|FORMAZIONE_LAVORO_DONNE|
#   INTERINALE_UOMINI|INTERINALE_DONNE|
#   LAVORO_SOCIALMENTE_UTILE_UOMINI|LAVORO_SOCIALMENTE_UTILE_DONNE
ca_agg_lavoro_flessibile <- function(df) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- ca_add_istituzione(df)

  col_td_u <- ca_pick_col(df, c("personale_tempo_determinato_uomini", "tempo_determinato_uomini"))
  col_td_d <- ca_pick_col(df, c("personale_tempo_determinato_donne",  "tempo_determinato_donne"))
  col_fl_u <- ca_pick_col(df, c("formazione_lavoro_uomini"))
  col_fl_d <- ca_pick_col(df, c("formazione_lavoro_donne"))
  col_in_u <- ca_pick_col(df, c("interinale_uomini"))
  col_in_d <- ca_pick_col(df, c("interinale_donne"))
  col_ls_u <- ca_pick_col(df, c("lavoro_socialmente_utile_uomini", "lsu_uomini"))
  col_ls_d <- ca_pick_col(df, c("lavoro_socialmente_utile_donne",  "lsu_donne"))

  df %>%
    dplyr::mutate(
      td_u = if (!is.null(col_td_u)) as.numeric(.data[[col_td_u]]) else NA_real_,
      td_d = if (!is.null(col_td_d)) as.numeric(.data[[col_td_d]]) else NA_real_,
      fl_u = if (!is.null(col_fl_u)) as.numeric(.data[[col_fl_u]]) else NA_real_,
      fl_d = if (!is.null(col_fl_d)) as.numeric(.data[[col_fl_d]]) else NA_real_,
      in_u = if (!is.null(col_in_u)) as.numeric(.data[[col_in_u]]) else NA_real_,
      in_d = if (!is.null(col_in_d)) as.numeric(.data[[col_in_d]]) else NA_real_,
      ls_u = if (!is.null(col_ls_u)) as.numeric(.data[[col_ls_u]]) else NA_real_,
      ls_d = if (!is.null(col_ls_d)) as.numeric(.data[[col_ls_d]]) else NA_real_
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERS_TD_UOMINI    = ca_safe_sum(td_u),
      PERS_TD_DONNE     = ca_safe_sum(td_d),
      PERS_TD_TOT       = PERS_TD_UOMINI + PERS_TD_DONNE,
      PERS_FL_TOT       = ca_safe_sum(fl_u) + ca_safe_sum(fl_d),
      PERS_INTERINALE_TOT = ca_safe_sum(in_u) + ca_safe_sum(in_d),
      PERS_LSU_TOT      = ca_safe_sum(ls_u) + ca_safe_sum(ls_d),
      PERS_PRECARIO_TOT = PERS_TD_TOT + PERS_FL_TOT + PERS_INTERINALE_TOT + PERS_LSU_TOT,
      .groups = "drop"
    )
}

ca_log_match_dataset_anagrafica <- function(dataset_pa, anagrafica, anno_rif, dataset_nome) {
  dataset_pa %>%
    dplyr::distinct(anno, istituzione) %>%
    dplyr::left_join(
      anagrafica %>% dplyr::select(anno, istituzione, codice_fiscale),
      by = c("anno", "istituzione")
    ) %>%
    dplyr::summarise(
      anno = unique(anno_rif),
      dataset = unique(dataset_nome),
      n_istituzioni_dataset = dplyr::n_distinct(istituzione),
      n_match_anagrafica = dplyr::n_distinct(istituzione[!is.na(codice_fiscale)]),
      n_senza_anagrafica = dplyr::n_distinct(istituzione[is.na(codice_fiscale)]),
      quota_match_anagrafica = n_match_anagrafica / n_istituzioni_dataset
    )
}

ca_fmt_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

ca_fmt_pct <- function(x, digits = 1) {
  paste0(round(100 * x, digits), "%")
}

ca_print_log_match_dataset_anagrafica <- function(log_tbl) {
  if (is.null(log_tbl) || nrow(log_tbl) == 0) {
    message("Nessun controllo dataset-anagrafica da stampare.")
    return(invisible(NULL))
  }

  message("")
  message(strrep("-", 80))
  message("CONTROLLO MATCH DATASET CA -> ANAGRAFICA CA")
  message(strrep("-", 80))

  for (i in seq_len(nrow(log_tbl))) {
    message(
      "Anno: ", log_tbl$anno[i],
      " | Dataset: ", log_tbl$dataset[i],
      " | Istituzioni dataset: ", ca_fmt_int(log_tbl$n_istituzioni_dataset[i]),
      " | Match anagrafica: ", ca_fmt_int(log_tbl$n_match_anagrafica[i]),
      " | Senza anagrafica: ", ca_fmt_int(log_tbl$n_senza_anagrafica[i]),
      " | Copertura: ", ca_fmt_pct(log_tbl$quota_match_anagrafica[i])
    )
  }

  message(strrep("-", 80))
  message("")
  invisible(NULL)
}

ca_print_log_lista_sim_anagrafica <- function(log_tbl) {
  if (is.null(log_tbl) || nrow(log_tbl) == 0) {
    message("Nessun controllo Lista SIM-anagrafica CA da stampare.")
    return(invisible(NULL))
  }

  message("")
  message(strrep("-", 80))
  message("CONTROLLO MATCH LISTA SIM -> ANAGRAFICA CONTO ANNUALE")
  message(strrep("-", 80))

  for (i in seq_len(nrow(log_tbl))) {
    message(
      "Anno: ", log_tbl$anno[i],
      " | PA MPA: ", ca_fmt_int(log_tbl$n_pa_mpa[i]),
      " | Con anagrafica CA: ", ca_fmt_int(log_tbl$n_pa_mpa_con_anagrafica_ca[i]),
      " | Senza anagrafica CA: ", ca_fmt_int(log_tbl$n_pa_mpa_senza_anagrafica_ca[i]),
      " | Copertura: ", ca_fmt_pct(log_tbl$quota_mpa_con_anagrafica_ca[i])
    )
  }

  message(strrep("-", 80))
  message("")
  invisible(NULL)
}

ca_print_log_copertura_mpa <- function(log_tbl) {
  if (is.null(log_tbl) || nrow(log_tbl) == 0) {
    message("Nessun controllo copertura MPA da stampare.")
    return(invisible(NULL))
  }

  message("")
  message(strrep("-", 80))
  message("COPERTURA CONTO ANNUALE RISPETTO AL PERIMETRO MPA")
  message(strrep("-", 80))

  for (i in seq_len(nrow(log_tbl))) {
    message(
      "Anno: ", log_tbl$anno[i],
      " | PA MPA: ", ca_fmt_int(log_tbl$n_pa_mpa[i]),
      " | Con dati CA: ", ca_fmt_int(log_tbl$n_pa_mpa_con_dati_ca[i]),
      " | Senza dati CA: ", ca_fmt_int(log_tbl$n_pa_mpa_senza_dati_ca[i]),
      " | Copertura: ", ca_fmt_pct(log_tbl$quota_mpa_con_dati_ca[i])
    )
  }

  message(strrep("-", 80))
  message("")
  invisible(NULL)
}

ca_costruisci_anno <- function(anno) {
  message("Costruzione master CA anno ", anno)

  anagrafica <- ca_read_anagrafica_istituzioni(anno)

  assunti      <- ca_read_dataset(anno, "^ASSUNT",                   "ASSUNTI")
  cessati      <- ca_read_dataset(anno, "^CESS",                     "CESSATI")
  occupazione  <- ca_read_dataset(anno, "^OCCUPAZIONE",              "OCCUPAZIONE")
  eta          <- ca_read_dataset(anno, "^ETA_MEDIA",                "ETA_MEDIA")
  formazione   <- ca_read_dataset(anno, "^FORMAZIONE",               "FORMAZIONE")
  costo_lavoro <- ca_read_dataset(anno, "^COSTO",                    "COSTO_LAVORO")
  # --- Nuovi dataset PNRR (Accesso/Reclutamento, Buona Amm., Competenze) ----
  titoli_studio  <- ca_read_dataset(anno, "^TITOLI_STUDIO_DATI",     "TITOLI_STUDIO_DATI")
  lavoro_agile   <- ca_read_dataset(anno, "^MODALITA_LAVORO_FLESS",  "MODALITA_LAVORO_FLESSIBILE")
  assenze        <- ca_read_dataset(anno, "^ASSENZE$|^ASSENZE_\\d", "ASSENZE")
  pass_qualifica <- ca_read_dataset(anno, "^PASSAGGI_QUALIFICA",     "PASSAGGI_QUALIFICA")
  anzianita      <- ca_read_dataset(anno, "^ANZIANITA$|^ANZIANITA_\\d", "ANZIANITA")
  lav_flessibile <- ca_read_dataset(anno, "^LAVORO_FLESSIBILE",      "LAVORO_FLESSIBILE")

  occupazione_pa   <- ca_agg_occupazione(occupazione)
  assunti_pa       <- ca_agg_flusso(assunti, "ASSUN")
  cessati_pa       <- ca_agg_flusso(cessati, "CESS")
  eta_pa           <- ca_agg_eta(eta)
  formazione_pa    <- ca_agg_formazione(formazione)
  costo_lavoro_pa  <- ca_agg_costo_lavoro(costo_lavoro)
  # --- Aggregazioni nuovi dataset PNRR --------------------------------------
  titoli_pa        <- ca_agg_titoli_studio(titoli_studio)
  lavoro_agile_pa  <- ca_agg_lavoro_agile(lavoro_agile)
  assenze_pa       <- ca_agg_assenze(assenze)
  pass_qual_pa     <- ca_agg_passaggi_qualifica(pass_qualifica)
  anzianita_pa     <- ca_agg_anzianita(anzianita)
  lav_fless_pa     <- ca_agg_lavoro_flessibile(lav_flessibile)

  log_dataset_anagrafica <- dplyr::bind_rows(
    ca_log_match_dataset_anagrafica(occupazione_pa,  anagrafica, anno, "OCCUPAZIONE"),
    ca_log_match_dataset_anagrafica(assunti_pa,      anagrafica, anno, "ASSUNTI"),
    ca_log_match_dataset_anagrafica(cessati_pa,      anagrafica, anno, "CESSATI"),
    ca_log_match_dataset_anagrafica(eta_pa,          anagrafica, anno, "ETA_MEDIA"),
    ca_log_match_dataset_anagrafica(formazione_pa,   anagrafica, anno, "FORMAZIONE"),
    ca_log_match_dataset_anagrafica(costo_lavoro_pa, anagrafica, anno, "COSTO_LAVORO"),
    ca_log_match_dataset_anagrafica(titoli_pa,       anagrafica, anno, "TITOLI_STUDIO_DATI"),
    ca_log_match_dataset_anagrafica(lavoro_agile_pa, anagrafica, anno, "MODALITA_LAVORO_FLESSIBILE"),
    ca_log_match_dataset_anagrafica(assenze_pa,      anagrafica, anno, "ASSENZE"),
    ca_log_match_dataset_anagrafica(pass_qual_pa,    anagrafica, anno, "PASSAGGI_QUALIFICA"),
    ca_log_match_dataset_anagrafica(anzianita_pa,    anagrafica, anno, "ANZIANITA"),
    ca_log_match_dataset_anagrafica(lav_fless_pa,    anagrafica, anno, "LAVORO_FLESSIBILE")
  )

  master_istituzione <- purrr::reduce(
    list(
      occupazione_pa, assunti_pa, cessati_pa, eta_pa,
      formazione_pa, costo_lavoro_pa,
      # nuovi dataset PNRR - full_join: NA per PA non coperte da ogni fonte
      titoli_pa, lavoro_agile_pa, assenze_pa,
      pass_qual_pa, anzianita_pa, lav_fless_pa
    ),
    dplyr::full_join,
    by = c("anno", "istituzione")
  ) %>%
    dplyr::left_join(anagrafica, by = c("anno", "istituzione")) %>%
    dplyr::filter(!is.na(codice_fiscale), codice_fiscale != "")

  if (nrow(master_istituzione) == 0) {
    warning("Master istituzione vuoto per anno ", anno)
    return(list(
      master_cf = tibble::tibble(),
      anagrafica = anagrafica,
      log_dataset_anagrafica = log_dataset_anagrafica
    ))
  }

  eta_cf <- master_istituzione %>%
    dplyr::group_by(anno, codice_fiscale) %>%
    dplyr::summarise(
      ETA_MEDIA_PA = ca_weighted_mean(ETA_MEDIA_PA, PERSONALE_TOT_ETA),
      .groups = "drop"
    )

  master_cf <- master_istituzione %>%
    dplyr::group_by(anno, codice_fiscale) %>%
    dplyr::summarise(
      n_istituzioni_ca = dplyr::n_distinct(istituzione),
      istituzioni_ca = paste(sort(unique(istituzione)), collapse = "|"),
      desc_tipo_istituzione_ca = dplyr::first(stats::na.omit(desc_tipo_istituzione_ca)),
      desc_istituzione_ca = dplyr::first(stats::na.omit(desc_istituzione_ca)),

      TEMPO_PIENO_UOMINI = ca_safe_sum(TEMPO_PIENO_UOMINI),
      TEMPO_PIENO_DONNE  = ca_safe_sum(TEMPO_PIENO_DONNE),
      TEMPO_PIENO_TOT    = ca_safe_sum(TEMPO_PIENO_TOT),
      PART_TIME_UOMINI   = ca_safe_sum(PART_TIME_UOMINI),
      PART_TIME_DONNE    = ca_safe_sum(PART_TIME_DONNE),
      TOT_PART_TIME      = ca_safe_sum(TOT_PART_TIME),
      PERSONALE_UOMINI = ca_safe_sum(PERSONALE_UOMINI),
      PERSONALE_DONNE  = ca_safe_sum(PERSONALE_DONNE),
      PERSONALE_TOT    = ca_safe_sum(PERSONALE_TOT),

      ASSUN_UOMINI = ca_safe_sum(ASSUN_UOMINI),
      ASSUN_DONNE  = ca_safe_sum(ASSUN_DONNE),
      ASSUN_TOT    = ca_safe_sum(ASSUN_TOT),

      CESS_UOMINI = ca_safe_sum(CESS_UOMINI),
      CESS_DONNE  = ca_safe_sum(CESS_DONNE),
      CESS_TOT    = ca_safe_sum(CESS_TOT),

      PERSONALE_TOT_ETA = ca_safe_sum(PERSONALE_TOT_ETA),
      UNDER35_UOMINI = ca_safe_sum(UNDER35_UOMINI),
      UNDER35_DONNE  = ca_safe_sum(UNDER35_DONNE),
      UNDER35        = ca_safe_sum(UNDER35),
      OVER55_UOMINI  = ca_safe_sum(OVER55_UOMINI),
      OVER55_DONNE   = ca_safe_sum(OVER55_DONNE),
      OVER55         = ca_safe_sum(OVER55),
      OVER65_UOMINI  = ca_safe_sum(OVER65_UOMINI),
      OVER65_DONNE   = ca_safe_sum(OVER65_DONNE),
      OVER65         = ca_safe_sum(OVER65),

      GIORNI_FORM_UOMINI = ca_safe_sum(GIORNI_FORM_UOMINI),
      GIORNI_FORM_DONNE  = ca_safe_sum(GIORNI_FORM_DONNE),
      GIORNI_FORM_TOT    = ca_safe_sum(GIORNI_FORM_TOT),
      PERS_FORM_UOMINI   = ca_safe_sum(PERS_FORM_UOMINI),
      PERS_FORM_DONNE    = ca_safe_sum(PERS_FORM_DONNE),
      PERS_FORM_TOT      = ca_safe_sum(PERS_FORM_TOT),

      TOTALE_SPESA          = ca_safe_sum(TOTALE_SPESA),
      SPESA_FORMAZIONE_L020 = ca_safe_sum(SPESA_FORMAZIONE_L020),
      VOCE_SPESA_STR = paste(sort(unique(VOCE_SPESA_STR[!is.na(VOCE_SPESA_STR)])), collapse = "|"),

      FORM_MEDIA_UOMINI_CA = mean(FORM_MEDIA_UOMINI_CA, na.rm = TRUE),
      FORM_MEDIA_DONNE_CA  = mean(FORM_MEDIA_DONNE_CA, na.rm = TRUE),

      # --- Titoli di studio --------------------------------------------------
      LAUREA_TOT         = ca_safe_sum(LAUREA_TOT),
      DIPLOMA_TOT        = ca_safe_sum(DIPLOMA_TOT),
      MEDIA_INF_TOT      = ca_safe_sum(MEDIA_INF_TOT),
      NESSUNO_EL_TOT     = ca_safe_sum(NESSUNO_EL_TOT),
      PERS_TOT_TITOLI    = ca_safe_sum(PERS_TOT_TITOLI),

      # --- Modalita' lavoro flessibile (lavoro agile, telelavoro, coworking) -
      PERS_LAVORO_AGILE_TOT   = ca_safe_sum(PERS_LAVORO_AGILE_TOT),
      PERS_TELE_LAVORO_TOT    = ca_safe_sum(PERS_TELE_LAVORO_TOT),
      PERS_COWORKING_TOT      = ca_safe_sum(PERS_COWORKING_TOT),
      PERS_MOD_FLESSIBILE_TOT = ca_safe_sum(PERS_MOD_FLESSIBILE_TOT),

      # --- Assenze -----------------------------------------------------------
      ASSENZE_TOT          = ca_safe_sum(ASSENZE_TOT),
      ASSENZE_MALATTIA_TOT = ca_safe_sum(ASSENZE_MALATTIA_TOT),

      # --- Passaggi di qualifica ---------------------------------------------
      PASSAGGI_QUALIFICA_TOT    = ca_safe_sum(PASSAGGI_QUALIFICA_TOT),
      PASSAGGI_CONCORSO_TOT     = ca_safe_sum(PASSAGGI_CONCORSO_TOT),
      PASSAGGI_PROGRESSIONE_TOT = ca_safe_sum(PASSAGGI_PROGRESSIONE_TOT),

      # --- Anzianita' di servizio --------------------------------------------
      PERS_TOT_ANZIANITA     = ca_safe_sum(PERS_TOT_ANZIANITA),
      PERS_ANZIANITA_BREVE   = ca_safe_sum(PERS_ANZIANITA_BREVE),
      PERS_ANZIANITA_LUNGA   = ca_safe_sum(PERS_ANZIANITA_LUNGA),

      # --- Personale precario ------------------------------------------------
      PERS_TD_TOT           = ca_safe_sum(PERS_TD_TOT),
      PERS_FL_TOT           = ca_safe_sum(PERS_FL_TOT),
      PERS_INTERINALE_TOT   = ca_safe_sum(PERS_INTERINALE_TOT),
      PERS_LSU_TOT          = ca_safe_sum(PERS_LSU_TOT),
      PERS_PRECARIO_TOT     = ca_safe_sum(PERS_PRECARIO_TOT),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      FORM_MEDIA_UOMINI_CA = ifelse(is.nan(FORM_MEDIA_UOMINI_CA), NA_real_, FORM_MEDIA_UOMINI_CA),
      FORM_MEDIA_DONNE_CA  = ifelse(is.nan(FORM_MEDIA_DONNE_CA),  NA_real_, FORM_MEDIA_DONNE_CA),

      # --- Indicatori esistenti (invariati) ----------------------------------
      PERC_PART_TIME                  = sim_safe_div(TOT_PART_TIME, PERSONALE_TOT, 100),
      GIORNI_FORM_PER_DIPENDENTE      = sim_safe_div(GIORNI_FORM_TOT, PERSONALE_TOT, 1),
      SPESA_FORMAZIONE_PER_DIPENDENTE = sim_safe_div(SPESA_FORMAZIONE_L020, PERSONALE_TOT, 1),
      INCIDENZA_SPESA_FORMAZIONE_PERC = sim_safe_div(SPESA_FORMAZIONE_L020, TOTALE_SPESA, 100),
      QUOTA_UNDER35_PERC              = sim_safe_div(UNDER35, PERSONALE_TOT_ETA, 100),
      QUOTA_UNDER35_UOMINI_PERC       = sim_safe_div(UNDER35_UOMINI, UNDER35, 100),
      QUOTA_UNDER35_DONNE_PERC        = sim_safe_div(UNDER35_DONNE, UNDER35, 100),
      QUOTA_OVER55_PERC               = sim_safe_div(OVER55, PERSONALE_TOT_ETA, 100),
      QUOTA_OVER65_PERC               = sim_safe_div(OVER65, PERSONALE_TOT_ETA, 100),
      INDICE_RICAMBIO_GENERAZIONALE   = sim_safe_div(UNDER35, OVER55, 1),

      # --- NUOVI indicatori derivati PNRR ------------------------------------

      # Asse: Competenze e carriere
      # % personale formato (citato esplicitamente nel quadro logico)
      QUOTA_PERSONALE_FORMATO_PERC    = sim_safe_div(PERS_FORM_TOT, PERSONALE_TOT, 100),
      QUOTA_PERS_FORM_DONNE_PERC      = sim_safe_div(PERS_FORM_DONNE, PERS_FORM_TOT, 100),

      # Asse: Accesso e reclutamento / Competenze e carriere
      # Distribuzione titoli di studio
      QUOTA_LAUREA_PERC               = sim_safe_div(LAUREA_TOT, PERS_TOT_TITOLI, 100),
      QUOTA_DIPLOMA_PERC              = sim_safe_div(DIPLOMA_TOT, PERS_TOT_TITOLI, 100),

      # Progressioni di carriera (tasso per 100 dipendenti)
      TASSO_PROGRESSIONE_PERC         = sim_safe_div(PASSAGGI_QUALIFICA_TOT, PERSONALE_TOT, 100),

      # Anzianita' di servizio
      # ANZIANITA_MEDIA_PA e' aggiunta tramite join con anzianita_cf (vedi sotto)
      QUOTA_ANZIANITA_BREVE_PERC      = sim_safe_div(PERS_ANZIANITA_BREVE, PERS_TOT_ANZIANITA, 100),
      QUOTA_ANZIANITA_LUNGA_PERC      = sim_safe_div(PERS_ANZIANITA_LUNGA, PERS_TOT_ANZIANITA, 100),

      # Asse: Buona amministrazione e semplificazione
      # Lavoro agile / smart working
      QUOTA_LAVORO_AGILE_PERC         = sim_safe_div(PERS_LAVORO_AGILE_TOT, PERSONALE_TOT, 100),
      QUOTA_TELE_LAVORO_PERC          = sim_safe_div(PERS_TELE_LAVORO_TOT, PERSONALE_TOT, 100),
      QUOTA_MOD_FLESSIBILE_PERC       = sim_safe_div(PERS_MOD_FLESSIBILE_TOT, PERSONALE_TOT, 100),

      # Assenteismo (giorni assenza per dipendente)
      GG_ASSENZA_PER_DIP              = sim_safe_div(ASSENZE_TOT, PERSONALE_TOT, 1),
      GG_ASSENZA_MALATTIA_PER_DIP     = sim_safe_div(ASSENZE_MALATTIA_TOT, PERSONALE_TOT, 1),

      # Personale precario (% su totale)
      QUOTA_PRECARI_PERC              = sim_safe_div(PERS_PRECARIO_TOT, PERSONALE_TOT, 100),
      QUOTA_TEMPO_DET_PERC            = sim_safe_div(PERS_TD_TOT, PERSONALE_TOT, 100)
    ) %>%
    dplyr::left_join(eta_cf, by = c("anno", "codice_fiscale")) %>%
    # Anzianita' media ponderata per codice_fiscale (stessa logica di ETA_MEDIA_PA)
    dplyr::left_join(
      master_istituzione %>%
        dplyr::group_by(anno, codice_fiscale) %>%
        dplyr::summarise(
          ANZIANITA_MEDIA_PA = ca_weighted_mean(ANZIANITA_MEDIA_PA, PERS_TOT_ANZIANITA),
          .groups = "drop"
        ),
      by = c("anno", "codice_fiscale")
    )

  list(
    master_cf = master_cf,
    anagrafica = anagrafica,
    log_dataset_anagrafica = log_dataset_anagrafica
  )
}

# 8) MASTER GREZZO MULTIANNO ------------------------------------------------

risultati_anni <- purrr::map(anni_ca, ca_costruisci_anno)

master_ca_raw <- purrr::map_dfr(risultati_anni, "master_cf")
anagrafiche_ca <- purrr::map_dfr(risultati_anni, "anagrafica")
log_match_dataset_anagrafica <- purrr::map_dfr(risultati_anni, "log_dataset_anagrafica")

if (nrow(master_ca_raw) == 0) {
  stop(
    "Il master CA grezzo è vuoto. Controlla: ",
    "1) nomi file in Dati, ",
    "2) file TipoIstituzione_Istituzione in Anagrafiche, ",
    "3) chiave istituzione."
  )
}

ca_print_log_match_dataset_anagrafica(log_match_dataset_anagrafica)

# 9) LISTA RACCORDO SIM / PERIMETRO MPA ------------------------------------

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

if (!"codice_fiscale" %in% names(lista_sim)) {
  stop("Nella Lista_raccordo_SIM.xlsx manca la colonna codice_fiscale.")
}

if (!"presente_mpa" %in% names(lista_sim)) {
  stop("Nella Lista_raccordo_SIM.xlsx manca la colonna presente_mpa.")
}

lista_mpa <- lista_sim %>%
  mutate(
    codice_fiscale = ca_norm_cf(codice_fiscale),
    presente_mpa = suppressWarnings(as.numeric(presente_mpa))
  ) %>%
  filter(presente_mpa == 1) %>%
  distinct(codice_fiscale, .keep_all = TRUE)

# 10) MASTER FINALE: BASE MPA + ARRICCHIMENTO CA -----------------------------

base_mpa_anni <- tidyr::expand_grid(
  anno = anni_ca,
  lista_mpa
)

log_match_anagrafica_lista_sim <- base_mpa_anni %>%
  dplyr::left_join(
    anagrafiche_ca %>%
      dplyr::distinct(anno, codice_fiscale) %>%
      dplyr::mutate(match_conto_annuale_anagrafica = 1),
    by = c("anno", "codice_fiscale")
  ) %>%
  dplyr::group_by(anno) %>%
  dplyr::summarise(
    n_pa_mpa = dplyr::n_distinct(codice_fiscale),
    n_pa_mpa_con_anagrafica_ca =
      dplyr::n_distinct(codice_fiscale[match_conto_annuale_anagrafica == 1]),
    n_pa_mpa_senza_anagrafica_ca =
      n_pa_mpa - n_pa_mpa_con_anagrafica_ca,
    quota_mpa_con_anagrafica_ca =
      n_pa_mpa_con_anagrafica_ca / n_pa_mpa,
    .groups = "drop"
  )

ca_print_log_lista_sim_anagrafica(log_match_anagrafica_lista_sim)

master_ca_mpa <- base_mpa_anni %>%
  dplyr::left_join(
    master_ca_raw,
    by = c("anno", "codice_fiscale")
  ) %>%
  dplyr::mutate(
    fonte_conto_annuale = dplyr::if_else(!is.na(n_istituzioni_ca), 1, 0),
    presente_MPA = presente_mpa
  )

log_copertura_mpa <- master_ca_mpa %>%
  dplyr::group_by(anno) %>%
  dplyr::summarise(
    n_pa_mpa = dplyr::n_distinct(codice_fiscale),
    n_pa_mpa_con_dati_ca =
      dplyr::n_distinct(codice_fiscale[fonte_conto_annuale == 1]),
    n_pa_mpa_senza_dati_ca =
      n_pa_mpa - n_pa_mpa_con_dati_ca,
    quota_mpa_con_dati_ca =
      n_pa_mpa_con_dati_ca / n_pa_mpa,
    .groups = "drop"
  )

ca_print_log_copertura_mpa(log_copertura_mpa)

# 11) OUTPUT SU DRIVE ---------------------------------------------------------

# Il RUN_ID identifica la versione del processo.
# I file mantengono un nome stabile dentro la cartella del run:
# 01_Dataset/Processed/Conto_annuale/<RUN_ID>/master_CA_multianno.rds
# 01_Dataset/Processed/Conto_annuale/<RUN_ID>/master_CA_multianno.csv
DRIVE_CA_PROCESSED_RUN <- file.path(
  DRIVE_CA_PROCESSED,
  RUN_ID
)

DIR_CA_PROCESSED_LOCAL <- file.path(
  DIR_TEMP,
  "Conto_annuale",
  "Processed",
  RUN_ID
)

dir.create(
  DIR_CA_PROCESSED_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

filename_master_rds <- "master_CA_multianno.rds"
filename_master_csv <- "master_CA_multianno.csv"

local_master_rds <- file.path(
  DIR_CA_PROCESSED_LOCAL,
  filename_master_rds
)

local_master_csv <- file.path(
  DIR_CA_PROCESSED_LOCAL,
  filename_master_csv
)

message("Cartella locale output master CA: ", DIR_CA_PROCESSED_LOCAL)
message("Cartella Drive output master CA: ", DRIVE_CA_PROCESSED_RUN)
message("File RDS master CA: ", filename_master_rds)
message("File CSV master CA: ", filename_master_csv)

message("Controllo finale master_ca_mpa:")
message(" - righe: ", nrow(master_ca_mpa))
message(" - colonne: ", ncol(master_ca_mpa))
message(" - ETA_MEDIA_PA:")
print(summary(master_ca_mpa$ETA_MEDIA_PA))

# Salvataggio locale.
# RDS = formato operativo principale per gli script R.
# CSV = formato di consultazione/interoperabilità.
# JSON non viene generato perché il master è grande e non serve al flusso Shiny.
saveRDS(
  master_ca_mpa,
  local_master_rds
)

readr::write_csv(
  master_ca_mpa,
  local_master_csv
)

# Upload su Drive nella cartella RUN_ID.
drive_upload_or_update(
  local_path = local_master_rds,
  drive_folder_rel = DRIVE_CA_PROCESSED_RUN
)

drive_upload_or_update(
  local_path = local_master_csv,
  drive_folder_rel = DRIVE_CA_PROCESSED_RUN
)

message("Master CA multianno caricato su Drive:")
message(" - ", DRIVE_CA_PROCESSED_RUN, "/", filename_master_rds)
message(" - ", DRIVE_CA_PROCESSED_RUN, "/", filename_master_csv)


# 12) CLEAN TEMP DEL RUN -------------------------------------------------------

files_to_remove <- c(
  local_master_rds,
  local_master_csv
)

files_to_remove <- files_to_remove[file.exists(files_to_remove)]

if (length(files_to_remove) > 0) {
  file.remove(files_to_remove)
  message("Pulizia file temporanei del run completata:")
  message(" - ", paste(files_to_remove, collapse = "\n - "))
} else {
  message("Nessun file temporaneo del run da eliminare.")
}


# 13) CHIUSURA LOG ------------------------------------------------------------

# Chiude il file log e ripristina la console.
console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

message(
  "Log generato: ",
  basename(console_log_path),
  " | Percorso locale: ",
  console_log_path
)

# Carica o aggiorna il log nella cartella Drive del run.
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

message(
  "--- Costruzione master CA terminata. RUN_ID: ",
  RUN_ID,
  " | status: completed ---"
)
