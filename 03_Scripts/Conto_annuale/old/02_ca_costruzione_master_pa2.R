# ============================================================
# 02_ca_costruzione_master_pa.R
# Fonte: Conto Annuale
# Fase: costruzione master PA/anno raccordato al perimetro MPA
# ============================================================
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
# ============================================================

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")

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

# Autenticazione Drive: usa l'account con accesso al Drive SIM.
# Se in 00_config.R hai SIM_DRIVE_EMAIL, usa quello. Altrimenti usa la cache.
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

anni_ca <- c(2021, 2022, 2023)

DIR_LOGS_CA <- file.path("05_Logs", "Conto_annuale")
dir.create(DIR_LOGS_CA, recursive = TRUE, showWarnings = FALSE)

# 1) FUNZIONI DRIVE ---------------------------------------------------------

ca_drive_files <- function(anno, sottocartella) {
  dir <- sim_drive_ls_path(
    file.path(DRIVE_DIR_SOURCE, "Conto_annuale", paste0("CA_", anno), sottocartella),
    create = FALSE
  )
  googledrive::drive_ls(dir)
}

ca_download_read <- function(file, local_name) {
  local <- sim_drive_download_to_temp(
    file,
    local_name = local_name,
    overwrite = TRUE
  )
  df <- sim_read_any_table(local) %>%
    janitor::clean_names()
  unlink(local)
  df
}

ca_find_file <- function(anno, sottocartella, pattern) {
  files <- ca_drive_files(anno, sottocartella)

  files %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(.data$name)
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

# Upload versionato: non sostituisce e non cestina file esistenti.
# Serve a evitare errori 403 quando l'account non può eliminare file già presenti.
ca_save_rds_upload_versioned <- function(obj, drive_path, filename) {
  dir_drive <- sim_drive_mkdir_path(drive_path)
  local_file <- file.path(DIR_TEMP, filename)
  saveRDS(obj, local_file)
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename
  )
  unlink(local_file)
}

ca_write_csv_upload_versioned <- function(obj, drive_path, filename) {
  dir_drive <- sim_drive_mkdir_path(drive_path)
  local_file <- file.path(DIR_TEMP, filename)
  readr::write_csv(obj, local_file)
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename
  )
  unlink(local_file)
}

# 2) FUNZIONI STANDARDIZZAZIONE --------------------------------------------

ca_to_num <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.", "")
  x <- stringr::str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

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

# 3) ANAGRAFICA ISTITUZIONI -------------------------------------------------

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

# 4) AGGREGAZIONI DATASET CA ------------------------------------------------

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
      personale_tempo_pieno_uomini   = ca_to_num(.data$personale_tempo_pieno_uomini),
      personale_tempo_pieno_donne    = ca_to_num(.data$personale_tempo_pieno_donne),
      part_time_inf50_percent_uomini = ca_to_num(.data$part_time_inf50_percent_uomini),
      part_time_inf50_percent_donne  = ca_to_num(.data$part_time_inf50_percent_donne),
      part_time_sup50_percent_uomini = ca_to_num(.data$part_time_sup50_percent_uomini),
      part_time_sup50_percent_donne  = ca_to_num(.data$part_time_sup50_percent_donne),

      PERSONALE_UOMINI =
        personale_tempo_pieno_uomini +
        part_time_inf50_percent_uomini +
        part_time_sup50_percent_uomini,

      PERSONALE_DONNE =
        personale_tempo_pieno_donne +
        part_time_inf50_percent_donne +
        part_time_sup50_percent_donne,

      PERSONALE_TOT = PERSONALE_UOMINI + PERSONALE_DONNE
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERSONALE_UOMINI = ca_safe_sum(PERSONALE_UOMINI),
      PERSONALE_DONNE  = ca_safe_sum(PERSONALE_DONNE),
      PERSONALE_TOT    = ca_safe_sum(PERSONALE_TOT),
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
      val_uomini = if (!is.null(col_u)) ca_to_num(.data[[col_u]]) else NA_real_,
      val_donne  = if (!is.null(col_d)) ca_to_num(.data[[col_d]]) else NA_real_,
      val_totale = if (!is.null(col_tot)) ca_to_num(.data[[col_tot]]) else val_uomini + val_donne
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
      uomini = ca_to_num(uomini),
      donne  = ca_to_num(donne),
      media_uomini = ca_to_num(media_uomini),
      media_donne  = ca_to_num(media_donne),
      n_eta = uomini + donne,

      is_under35 = fascia_eta %in% c("E0", "E20", "E25", "E30"),
      is_over55  = fascia_eta %in% c("E55", "E60", "E65", "E68"),
      is_over65  = fascia_eta %in% c("E65", "E68")
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      PERSONALE_ETA = ca_safe_sum(n_eta),

      ETA_MEDIA_PA = dplyr::if_else(
        PERSONALE_ETA == 0,
        NA_real_,
        (
          sum(media_uomini * uomini, na.rm = TRUE) +
            sum(media_donne * donne, na.rm = TRUE)
        ) / PERSONALE_ETA
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

      QUOTA_UNDER35_PERC = dplyr::if_else(PERSONALE_ETA == 0, NA_real_, 100 * UNDER35 / PERSONALE_ETA),
      QUOTA_UNDER35_UOMINI_PERC = dplyr::if_else(UNDER35 == 0, NA_real_, 100 * UNDER35_UOMINI / UNDER35),
      QUOTA_UNDER35_DONNE_PERC  = dplyr::if_else(UNDER35 == 0, NA_real_, 100 * UNDER35_DONNE  / UNDER35),

      QUOTA_OVER55_PERC = dplyr::if_else(PERSONALE_ETA == 0, NA_real_, 100 * OVER55 / PERSONALE_ETA),
      QUOTA_OVER65_PERC = dplyr::if_else(PERSONALE_ETA == 0, NA_real_, 100 * OVER65 / PERSONALE_ETA),

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

  df %>%
    dplyr::mutate(
      form_u = if (!is.null(col_u)) ca_to_num(.data[[col_u]]) else NA_real_,
      form_d = if (!is.null(col_d)) ca_to_num(.data[[col_d]]) else NA_real_,
      form_mu = if (!is.null(col_mu)) ca_to_num(.data[[col_mu]]) else NA_real_,
      form_md = if (!is.null(col_md)) ca_to_num(.data[[col_md]]) else NA_real_
    ) %>%
    dplyr::group_by(anno, istituzione) %>%
    dplyr::summarise(
      GIORNI_FORM_UOMINI = ca_safe_sum(form_u),
      GIORNI_FORM_DONNE  = ca_safe_sum(form_d),
      GIORNI_FORM_TOT    = GIORNI_FORM_UOMINI + GIORNI_FORM_DONNE,
      FORM_MEDIA_UOMINI_CA = mean(form_mu, na.rm = TRUE),
      FORM_MEDIA_DONNE_CA  = mean(form_md, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      FORM_MEDIA_UOMINI_CA = ifelse(is.nan(FORM_MEDIA_UOMINI_CA), NA_real_, FORM_MEDIA_UOMINI_CA),
      FORM_MEDIA_DONNE_CA  = ifelse(is.nan(FORM_MEDIA_DONNE_CA), NA_real_, FORM_MEDIA_DONNE_CA)
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

ca_costruisci_anno <- function(anno) {
  message("Costruzione master CA anno ", anno)

  anagrafica <- ca_read_anagrafica_istituzioni(anno)

  assunti <- ca_read_dataset(anno, "^ASSUNT", "ASSUNTI")
  cessati <- ca_read_dataset(anno, "^CESS", "CESSATI")
  occupazione <- ca_read_dataset(anno, "^OCCUPAZIONE", "OCCUPAZIONE")
  eta <- ca_read_dataset(anno, "^ETA_MEDIA", "ETA_MEDIA")
  formazione <- ca_read_dataset(anno, "^FORMAZIONE", "FORMAZIONE")

  occupazione_pa <- ca_agg_occupazione(occupazione)
  assunti_pa     <- ca_agg_flusso(assunti, "ASSUN")
  cessati_pa     <- ca_agg_flusso(cessati, "CESS")
  eta_pa         <- ca_agg_eta(eta)
  formazione_pa  <- ca_agg_formazione(formazione)

  log_dataset_anagrafica <- dplyr::bind_rows(
    ca_log_match_dataset_anagrafica(occupazione_pa, anagrafica, anno, "OCCUPAZIONE"),
    ca_log_match_dataset_anagrafica(assunti_pa, anagrafica, anno, "ASSUNTI"),
    ca_log_match_dataset_anagrafica(cessati_pa, anagrafica, anno, "CESSATI"),
    ca_log_match_dataset_anagrafica(eta_pa, anagrafica, anno, "ETA_MEDIA"),
    ca_log_match_dataset_anagrafica(formazione_pa, anagrafica, anno, "FORMAZIONE")
  )

  master_istituzione <- purrr::reduce(
    list(occupazione_pa, assunti_pa, cessati_pa, eta_pa, formazione_pa),
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
      ETA_MEDIA_PA = ca_weighted_mean(ETA_MEDIA_PA, PERSONALE_ETA),
      .groups = "drop"
    )

  master_cf <- master_istituzione %>%
    dplyr::group_by(anno, codice_fiscale) %>%
    dplyr::summarise(
      n_istituzioni_ca = dplyr::n_distinct(istituzione),
      istituzioni_ca = paste(sort(unique(istituzione)), collapse = "|"),
      desc_tipo_istituzione_ca = dplyr::first(stats::na.omit(desc_tipo_istituzione_ca)),
      desc_istituzione_ca = dplyr::first(stats::na.omit(desc_istituzione_ca)),

      PERSONALE_UOMINI = ca_safe_sum(PERSONALE_UOMINI),
      PERSONALE_DONNE  = ca_safe_sum(PERSONALE_DONNE),
      PERSONALE_TOT    = ca_safe_sum(PERSONALE_TOT),

      ASSUN_UOMINI = ca_safe_sum(ASSUN_UOMINI),
      ASSUN_DONNE  = ca_safe_sum(ASSUN_DONNE),
      ASSUN_TOT    = ca_safe_sum(ASSUN_TOT),

      CESS_UOMINI = ca_safe_sum(CESS_UOMINI),
      CESS_DONNE  = ca_safe_sum(CESS_DONNE),
      CESS_TOT    = ca_safe_sum(CESS_TOT),

      PERSONALE_ETA = ca_safe_sum(PERSONALE_ETA),
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

      FORM_MEDIA_UOMINI_CA = mean(FORM_MEDIA_UOMINI_CA, na.rm = TRUE),
      FORM_MEDIA_DONNE_CA  = mean(FORM_MEDIA_DONNE_CA, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      FORM_MEDIA_UOMINI_CA = ifelse(is.nan(FORM_MEDIA_UOMINI_CA), NA_real_, FORM_MEDIA_UOMINI_CA),
      FORM_MEDIA_DONNE_CA  = ifelse(is.nan(FORM_MEDIA_DONNE_CA), NA_real_, FORM_MEDIA_DONNE_CA),

      QUOTA_UNDER35_PERC = sim_safe_div(UNDER35, PERSONALE_ETA, 100),
      QUOTA_UNDER35_UOMINI_PERC = sim_safe_div(UNDER35_UOMINI, UNDER35, 100),
      QUOTA_UNDER35_DONNE_PERC  = sim_safe_div(UNDER35_DONNE, UNDER35, 100),
      QUOTA_OVER55_PERC = sim_safe_div(OVER55, PERSONALE_ETA, 100),
      QUOTA_OVER65_PERC = sim_safe_div(OVER65, PERSONALE_ETA, 100),
      INDICE_RICAMBIO_GENERAZIONALE = sim_safe_div(UNDER35, OVER55, 1)
    ) %>%
    dplyr::left_join(eta_cf, by = c("anno", "codice_fiscale"))

  list(
    master_cf = master_cf,
    anagrafica = anagrafica,
    log_dataset_anagrafica = log_dataset_anagrafica
  )
}

# 5) MASTER GREZZO MULTIANNO ------------------------------------------------

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

readr::write_csv(
  log_match_dataset_anagrafica,
  file.path(
    DIR_LOGS_CA,
    paste0(
      "log_match_dataset_anagrafica_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )
)

# 6) LISTA RACCORDO SIM / PERIMETRO MPA ------------------------------------

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
  dplyr::mutate(
    codice_fiscale = ca_norm_cf(codice_fiscale),
    presente_mpa = suppressWarnings(as.numeric(presente_mpa))
  ) %>%
  dplyr::filter(presente_mpa == 1) %>%
  dplyr::select(dplyr::any_of(c(
    "codice_fiscale",
    "presente_mpa",
    "ragione_sociale",
    "denominazione",
    "codice_unita_s13",
    "codice_unita_mpa",
    "codice_regione",
    "dizione_regione",
    "codice_provincia",
    "dizione_provincia",
    "codice_comune",
    "dizione_comune",
    "descr_tipologia_istat_s13",
    "s13_ind",
    "mpa_ind"
  ))) %>%
  dplyr::distinct(codice_fiscale, .keep_all = TRUE)

# 7) MASTER FINALE: BASE MPA + ARRICCHIMENTO CA -----------------------------

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

readr::write_csv(
  log_match_anagrafica_lista_sim,
  file.path(
    DIR_LOGS_CA,
    paste0(
      "log_match_anagrafica_lista_sim_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )
)

master_ca_mpa <- base_mpa_anni %>%
  dplyr::left_join(
    master_ca_raw,
    by = c("anno", "codice_fiscale")
  ) %>%
  dplyr::mutate(
    fonte_conto_annuale = dplyr::if_else(!is.na(n_istituzioni_ca), 1, 0),
    presente_MPA = presente_mpa
  )

log_match <- master_ca_mpa %>%
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

readr::write_csv(
  log_match,
  file.path(
    DIR_LOGS_CA,
    paste0(
      "log_copertura_mpa_conto_annuale_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )
)

# 8) OUTPUT SU DRIVE --------------------------------------------------------

timestamp_output <- format(Sys.time(), "%Y%m%d_%H%M%S")

filename_master_rds <- paste0(
  "master_CA_MPA_multianno_",
  timestamp_output,
  ".rds"
)

filename_master_csv <- paste0(
  "master_CA_MPA_multianno_",
  timestamp_output,
  ".csv"
)

ca_save_rds_upload_versioned(
  master_ca_mpa,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_master_rds
)

ca_write_csv_upload_versioned(
  master_ca_mpa,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_master_csv
)

message("Master CA-MPA multianno caricato su Drive:")
message(" - ", filename_master_rds)
message(" - ", filename_master_csv)

print(log_match)
print(log_match_anagrafica_lista_sim)
