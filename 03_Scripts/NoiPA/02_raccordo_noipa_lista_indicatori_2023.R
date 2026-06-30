# ============================================================ #
# Script: 02_raccordo_noipa_lista_indicatori_2023.R
# Fonte: NoiPA - Dati stipendi
#
# Obiettivo:
#   1. leggere i dataset NoiPA processed 2023
#   2. armonizzare variabili comuni
#   3. raccordare NoiPA alla master list S13+/MPA/BDAP
#   4. creare macro-gruppi PA da tipologia ISTAT S13
#   5. produrre log di copertura/match
#   6. produrre indicatori per dashboard/Shiny
#
# NOTE:
# - Questo script NON modifica la master list comune.
# - Il raccordo è esplorativo e basato sulla denominazione normalizzata.
# - La dashboard Shiny va tenuta in uno script separato.
# ============================================================ #

rm(list = ls())

# 1) PACCHETTI ---------------------------------------------------------------

{library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)
library(janitor)
library(googledrive)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(stringdist)
}
# opzionali per output spaziali
# library(sf)
# library(giscoR)
# library(leaflet)

# 2) PARAMETRI ---------------------------------------------------------------

source("03_Scripts/00_config.R")

anno_riferimento <- 2023

DIR_NOIPA_PROCESSED <- file.path(DIR_PROCESSED, "NoiPA")
DIR_NOIPA_OUTPUT    <- file.path(DIR_OUTPUT, "NoiPA")
DIR_CLASSIFICATION  <- file.path(DIR_SOURCE, "Classification")

dir.create(DIR_NOIPA_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_NOIPA_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_CLASSIFICATION, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TEMP, recursive = TRUE, showWarnings = FALSE)

# 3) FUNZIONI DI SUPPORTO ---------------------------------------------------

normalizza_testo <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_squish()
}

standardizza_noipa <- function(df) {
  
  df <- df %>%
    janitor::clean_names()
  
  get_chr <- function(col) {
    if (col %in% names(df)) {
      as.character(df[[col]])
    } else {
      rep(NA_character_, nrow(df))
    }
  }
  
  get_num <- function(col) {
    if (col %in% names(df)) {
      suppressWarnings(as.numeric(df[[col]]))
    } else {
      rep(NA_real_, nrow(df))
    }
  }
  
  regione_raw <- dplyr::coalesce(
    get_chr("regione_residenza"),
    get_chr("regione_sede")
  )
  
  provincia_raw <- dplyr::coalesce(
    get_chr("provincia_di_residenza"),
    get_chr("provincia_della_sede")
  )
  
  comune_raw <- get_chr("comune_della_sede")
  
  valore_raw <- dplyr::coalesce(
    get_num("numero"),
    get_num("numerosita"),
    get_num("numero_cedolini"),
    get_num("numero_amministrati"),
    get_num("numero_unita_organizzative"),
    get_num("numero_rapporti_lavoro"),
    get_num("reddito_relativo_anno_corrente")
  )
  
  df %>%
    mutate(
      amministrazione_key = normalizza_testo(get_chr("amministrazione")),
      regione_key = normalizza_testo(regione_raw),
      provincia_key = normalizza_testo(provincia_raw),
      comune_key = normalizza_testo(comune_raw),
      
      eta_min_std = get_num("eta_min"),
      eta_max_std = get_num("eta_max"),
      sesso_key = normalizza_testo(get_chr("sesso")),
      comparto_key = normalizza_testo(get_chr("comparto")),
      inquadramento_key = normalizza_testo(get_chr("inquadramento")),
      
      modalita_pagamento_key = normalizza_testo(get_chr("modalita_pagamento")),
      motivo_assunzione_key = normalizza_testo(get_chr("motivo_assunzione")),
      motivo_cessazione_key = normalizza_testo(get_chr("motivo_cessazione")),
      stesso_comune_key = normalizza_testo(get_chr("stesso_comune")),
      
      distance_min_km_std = get_num("distance_min_km"),
      distance_max_km_std = get_num("distance_max_km"),
      
      imponibili_previdenziali_std = get_num("imponibili_previdenziali"),
      reddito_anno_corrente_std = get_num("reddito_relativo_anno_corrente"),
      reddito_anni_precedenti_std = get_num("reddito_relativo_anni_precedenti"),
      ritenute_comunali_std = get_num("ritenute_comunali"),
      ritenute_regionali_std = get_num("ritenute_regionali"),
      ritenute_irpef_anno_corrente_std = get_num("ritenute_irpef_anno_corrente"),
      ritenute_irpef_anno_precedente_std = get_num("ritenute_irpef_anno_precedente"),
      totale_detrazioni_std = get_num("totale_detrazioni"),
      previdenza_complementare_std = get_num("previdenza_complementare"),
      
      valore_noipa = valore_raw
    )
}

crea_macro_gruppo_default <- function(x) {
  
  x_key <- normalizza_testo(x)
  
  case_when(
    str_detect(x_key, "PRESIDENZA|MINISTERI|ORGANI COSTITUZIONALI|AGENZIE FISCALI|AUTORITA|AUTORITÀ|ENTI DI REGOLAZIONE") ~
      "Amministrazioni centrali",
    
    str_detect(x_key, "COMUNI|REGIONI|PROVINCE|CITTA|CITTÀ|UNIONI DI COMUNI|COMUNITA|COMUNITÀ|CAMERE DI COMMERCIO|ENTI REGIONALI|AGENZIE REGIONALI|CONSORZI|PROVINCE AUTONOME") ~
      "Amministrazioni territoriali",
    
    str_detect(x_key, "AZIENDE SANITARIE|AZIENDE OSPEDALIERE|IRCCS|SANITARIE|SERVIZI SANITARI|ISTITUTI DI RICOVERO") ~
      "Sanità",
    
    str_detect(x_key, "UNIVERSITA|UNIVERSITÀ|POLITECNICI|RICERCA|ISTITUTI DI ISTRUZIONE UNIVERSITARIA") ~
      "Università e ricerca",
    
    str_detect(x_key, "SCUOLA|ISTITUZIONI SCOLASTICHE|ISTRUZIONE") ~
      "Istruzione e formazione",
    
    str_detect(x_key, "PREVIDENZA|ASSISTENZA|ASSISTENZIALI") ~
      "Previdenza e assistenza",
    
    is.na(x_key) ~
      "Non raccordato / non classificato",
    
    TRUE ~
      "Altro / enti speciali"
  )
}

# 4) IMPORT MASTER LIST DA DRIVE --------------------------------------------

drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")

cartella_lists <- drive_get(as_id(DRIVE_LISTS_ID))

file_lista <- drive_find(
  pattern = "^lista\\.xlsx$",
  q = paste0("'", cartella_lists$id, "' in parents"),
  n_max = 1
)

if (nrow(file_lista) == 0) {
  stop("File Lista_raccordo_SIM.xlsx non trovato nella cartella Lists.")
}

drive_download(
  file = file_lista,
  path = file.path(DIR_TEMP, "Lista_raccordo_SIM.xlsx"),
  overwrite = TRUE
)

lista <- readxl::read_excel(file.path(DIR_TEMP, "Lista_raccordo_SIM.xlsx")) %>%
  janitor::clean_names()

# 5) IMPORT DATASET NOIPA PROCESSED -----------------------------------------

file_noipa <- list.files(
  path = DIR_NOIPA_PROCESSED,
  pattern = paste0("_", anno_riferimento, "\\.csv$"),
  full.names = TRUE
)

if (length(file_noipa) == 0) {
  stop("Nessun file NoiPA processed trovato in: ", DIR_NOIPA_PROCESSED)
}

leggi_noipa <- function(path) {
  
  df <- readr::read_csv(path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  if (!"dataset_id" %in% names(df)) {
    df <- df %>%
      mutate(
        dataset_id = stringr::str_remove(
          basename(path),
          paste0("_", anno_riferimento, "\\.csv$")
        ),
        .before = 1
      )
  }
  
  df %>%
    mutate(file_origine = basename(path), .before = 1)
}

noipa_list <- purrr::map(file_noipa, leggi_noipa)

names(noipa_list) <- file_noipa %>%
  basename() %>%
  stringr::str_remove(paste0("_", anno_riferimento, "\\.csv$"))

noipa_std_list <- purrr::map(noipa_list, standardizza_noipa)

noipa_all <- dplyr::bind_rows(noipa_std_list, .id = "dataset_nome")

# 6) CONTROLLI TECNICI PRE-RACCORDO -----------------------------------------

check_missing_noipa <- noipa_all %>%
  group_by(dataset_nome, dataset_id) %>%
  summarise(
    n = n(),
    pct_amministrazione_key = mean(is.na(amministrazione_key)) * 100,
    pct_regione_key = mean(is.na(regione_key)) * 100,
    pct_provincia_key = mean(is.na(provincia_key)) * 100,
    pct_comune_key = mean(is.na(comune_key)) * 100,
    pct_valore_noipa = mean(is.na(valore_noipa)) * 100,
    .groups = "drop"
  )

write_csv(
  check_missing_noipa,
  file.path(DIR_LOGS, paste0("check_missing_noipa_", anno_riferimento, ".csv"))
)

# controllo opzionale: EntryAmministrati ricostruibile da EntryAccreditoStipendi
if (all(c("EntryAccreditoStipendi", "EntryAmministrati") %in% unique(noipa_all$dataset_id))) {
  
  check_amministrati_da_accredito <- noipa_all %>%
    filter(dataset_id == "EntryAccreditoStipendi") %>%
    group_by(anno, mese, comune_della_sede, eta_min, eta_max, sesso) %>%
    summarise(
      numero_da_accredito = sum(numero, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    full_join(
      noipa_all %>%
        filter(dataset_id == "EntryAmministrati") %>%
        select(
          anno, mese, comune_della_sede, eta_min, eta_max, sesso,
          numero_amministrati = numero
        ),
      by = c("anno", "mese", "comune_della_sede", "eta_min", "eta_max", "sesso")
    ) %>%
    mutate(
      diff = numero_da_accredito - numero_amministrati
    )
  
  write_csv(
    check_amministrati_da_accredito,
    file.path(DIR_LOGS, paste0("check_amministrati_da_accredito_", anno_riferimento, ".csv"))
  )
}

# 7) RACCORDO ESPLORATIVO CON MASTER LIST -----------------------------------

lista_base <- lista %>%
  mutate(
    lista_row_id = row_number(),
    lista_ind = 1
  )

lista_keys <- bind_rows(
  lista_base %>%
    transmute(
      lista_row_id,
      chiave_lista_tipo = "ragione_sociale",
      amministrazione_key = normalizza_testo(ragione_sociale)
    ),
  lista_base %>%
    transmute(
      lista_row_id,
      chiave_lista_tipo = "denominazione",
      amministrazione_key = normalizza_testo(denominazione)
    )
) %>%
  filter(!is.na(amministrazione_key), amministrazione_key != "") %>%
  distinct()

# chiavi ambigue: stessa denominazione normalizzata associata a più enti
chiavi_ambigue_lista <- lista_keys %>%
  distinct(amministrazione_key, lista_row_id) %>%
  count(amministrazione_key, name = "n_enti_lista") %>%
  filter(n_enti_lista > 1)

write_csv(
  chiavi_ambigue_lista,
  file.path(DIR_LOGS, paste0("chiavi_ambigue_lista_", anno_riferimento, ".csv"))
)

lista_match <- lista_keys %>%
  anti_join(chiavi_ambigue_lista, by = "amministrazione_key") %>%
  left_join(lista_base, by = "lista_row_id") %>%
  select(
    any_of(c(
      "amministrazione_key",
      "chiave_lista_tipo",
      "lista_ind",
      
      "codice_fiscale",
      "p_iva",
      "ragione_sociale",
      "denominazione",
      "codice_unita_s13",
      "codice_unita_mpa",
      "codice_ente_istat_s13",
      "codice_ente_ipa",
      "codice_ente_siope",
      "codice_ente_ssn",
      "codice_ente_miur",
      
      "id_tipologia_istat_s13",
      "codice_tipologia_istat_s13",
      "descr_tipologia_istat_s13",
      "id_tipologia_ipa",
      "codice_tipologia_ipa",
      "descr_tipologia_ipa",
      "id_tipologia_siope",
      "codice_tipologia_siope",
      "descr_tipologia_siope",
      "id_tipologia_miur",
      "codice_tipologia_miur",
      "descr_tipologia_miur",
      "id_tipologia_dlgs_118_2011",
      "codice_tipologia_dlgs_118_2011",
      "descr_tipologia_dlgs_118_2011",
      
      "codice_istat_comune",
      "codice_comune",
      "dizione_comune",
      "codice_catastale",
      "codice_provincia",
      "sigla_provincia",
      "dizione_provincia",
      "codice_regione",
      "dizione_regione",
      "codice_zona",
      "dizione_zona",
      
      "s13_ind",
      "mpa_ind"
    ))
  )



# 7.1) RACCORDO MANUALE NOIPA-LISTA -----------------------------------------

file_raccordo_noipa_lista <- file.path(
  DIR_SOURCE, "NoiPA_stipendi",
  "noipa_lista_raccordo.csv"
)

if (file.exists(file_raccordo_noipa_lista)) {
  
  raccordo_noipa_lista <- readr::read_csv(
    file_raccordo_noipa_lista,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(
      noipa = normalizza_testo(noipa),
      lista = normalizza_testo(lista)
    ) %>%
    filter(
      !is.na(noipa),
      !is.na(lista),
      noipa != "",
      lista != ""
    ) %>%
    distinct(noipa, .keep_all = TRUE)
  
  # Controllo: le chiavi di destinazione devono esistere nella master list
  raccordo_non_trovato_in_lista <- raccordo_noipa_lista %>%
    anti_join(
      lista_match %>%
        distinct(amministrazione_key),
      by = c("lista" = "amministrazione_key")
    )
  
  if (nrow(raccordo_non_trovato_in_lista) > 0) {
    
    readr::write_csv(
      raccordo_non_trovato_in_lista,
      file.path(DIR_LOGS, paste0("raccordo_noipa_lista_non_trovato_", anno_riferimento, ".csv"))
    )
    
    warning(
      "Alcune chiavi del raccordo manuale non sono state trovate nella master list. ",
      "Controlla il file: ",
      file.path(DIR_LOGS, paste0("raccordo_noipa_lista_non_trovato_", anno_riferimento, ".csv"))
    )
  }
  
  # Applico solo i raccordi con destinazione valida nella lista
  raccordo_noipa_lista_valido <- raccordo_noipa_lista %>%
    semi_join(
      lista_match %>%
        distinct(amministrazione_key),
      by = c("lista" = "amministrazione_key")
    )
  
  noipa_all <- noipa_all %>%
    mutate(
      amministrazione_key_originale = amministrazione_key
    ) %>%
    left_join(
      raccordo_noipa_lista_valido,
      by = c("amministrazione_key" = "noipa")
    ) %>%
    mutate(
      match_raccordo_manuale = if_else(!is.na(lista), 1, 0),
      amministrazione_key = if_else(
        match_raccordo_manuale == 1,
        lista,
        amministrazione_key
      )
    ) %>%
    select(-lista)
  
} else {
  
  noipa_all <- noipa_all %>%
    mutate(
      amministrazione_key_originale = amministrazione_key,
      match_raccordo_manuale = 0
    )
}


noipa_raccordato <- noipa_all %>%
  left_join(
    lista_match,
    by = "amministrazione_key"
  ) %>%
  mutate(
    noipa_ind = 1,
    match_lista = if_else(!is.na(lista_ind), 1, 0),
    tipo_match = case_when(
      is.na(amministrazione_key_originale) ~ "senza_amministrazione",
      match_lista == 1 & match_raccordo_manuale == 1 ~ paste0("match_raccordo_manuale_", chiave_lista_tipo),
      match_lista == 1 ~ paste0("match_denominazione_", chiave_lista_tipo),
      TRUE ~ "no_match"
    )
  )

log_raccordo_manuale_applicato <- noipa_raccordato %>%
  filter(match_raccordo_manuale == 1) %>%
  group_by(
    dataset_nome,
    dataset_id,
    amministrazione_key_originale,
    amministrazione_key,
    chiave_lista_tipo
  ) %>%
  summarise(
    n_righe = n(),
    valore_totale = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dataset_nome, desc(n_righe))

write_csv(
  log_raccordo_manuale_applicato,
  file.path(DIR_LOGS, paste0("log_raccordo_manuale_noipa_lista_", anno_riferimento, ".csv"))
)

noipa_non_match_post_raccordo <- noipa_raccordato %>%
  filter(
    !is.na(amministrazione_key_originale),
    match_lista == 0
  ) %>%
  group_by(dataset_nome, dataset_id, amministrazione_key_originale) %>%
  summarise(
    n_righe = n(),
    valore_totale = sum(valore_noipa, na.rm = TRUE),
    esempi_amministrazione_noipa = paste(
      sort(unique(na.omit(amministrazione)))[1:min(5, length(unique(na.omit(amministrazione))))],
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  arrange(dataset_nome, desc(n_righe), amministrazione_key_originale)

write_csv(
  noipa_non_match_post_raccordo,
  file.path(DIR_LOGS, paste0("noipa_non_match_post_raccordo_", anno_riferimento, ".csv"))
)

diagnostica_match_dataset <- noipa_raccordato %>%
  group_by(dataset_nome, dataset_id, tipo_match) %>%
  summarise(
    n_righe = n(),
    n_amministrazioni = n_distinct(amministrazione_key, na.rm = TRUE),
    valore_totale = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dataset_nome, tipo_match)

write_csv(
  diagnostica_match_dataset,
  file.path(DIR_LOGS, paste0("diagnostica_match_dataset_", anno_riferimento, ".csv"))
)

# # 7) RACCORDO ESPLORATIVO CON MASTER LIST -----------------------------------
# possibili_colonne_denominazione <- c(
#   "ragione_sociale",
#   "denominazione",
#   "denominazione_soggetto",
#   "nome_ente"
# )
# 
# col_den_lista <- intersect(possibili_colonne_denominazione, names(lista))[1]
# 
# if (is.na(col_den_lista)) {
#   stop("Non trovo nella lista una colonna denominazione/ragione sociale.")
# }
# 
# lista_match <- lista %>%
#   mutate(
#     amministrazione_key = normalizza_testo(.data[[col_den_lista]]),
#     lista_ind = 1
#   ) %>%
#   select(
#     any_of(c(
#       "amministrazione_key",
#       "lista_ind",
#       
#       # identificativi ente
#       "codice_fiscale",
#       "p_iva",
#       "ragione_sociale",
#       "denominazione",
#       "codice_unita_s13",
#       "codice_unita_mpa",
#       "codice_ente_istat_s13",
#       "codice_ente_ipa",
#       "codice_ente_siope",
#       "codice_ente_ssn",
#       "codice_ente_miur",
#       
#       # classificazioni
#       "id_tipologia_istat_s13",
#       "codice_tipologia_istat_s13",
#       "descr_tipologia_istat_s13",
#       "id_tipologia_ipa",
#       "codice_tipologia_ipa",
#       "descr_tipologia_ipa",
#       "id_tipologia_siope",
#       "codice_tipologia_siope",
#       "descr_tipologia_siope",
#       "id_tipologia_miur",
#       "codice_tipologia_miur",
#       "descr_tipologia_miur",
#       "id_tipologia_dlgs_118_2011",
#       "codice_tipologia_dlgs_118_2011",
#       "descr_tipologia_dlgs_118_2011",
#       
#       # territorio ente
#       "codice_istat_comune",
#       "codice_comune",
#       "dizione_comune",
#       "codice_catastale",
#       "codice_provincia",
#       "sigla_provincia",
#       "dizione_provincia",
#       "codice_regione",
#       "dizione_regione",
#       "codice_zona",
#       "dizione_zona",
#       
#       # flag/coperture
#       "s13_ind",
#       "mpa_ind"
#     ))
#   )
# 
# noipa_raccordato <- noipa_all %>%
#   left_join(
#     lista_match,
#     by = "amministrazione_key"
#   ) %>%
#   mutate(
#     noipa_ind = 1,
#     match_lista = if_else(!is.na(lista_ind), 1, 0),
#     tipo_match = case_when(
#       is.na(amministrazione_key) ~ "senza_amministrazione",
#       match_lista == 1 ~ "match_denominazione_esplorativo",
#       TRUE ~ "no_match"
#     )
#   )

# 8) MACRO-CLASSIFICAZIONE PA -----------------------------------------------

file_classificazione_pa <- file.path(
  DIR_CLASSIFICATION,
  "classificazione_macro_gruppi_pa.csv"
)

if (!file.exists(file_classificazione_pa)) {
  
  classificazione_macro_gruppi_pa_init <- lista %>%
    distinct(descr_tipologia_istat_s13) %>%
    filter(!is.na(descr_tipologia_istat_s13)) %>%
    arrange(descr_tipologia_istat_s13) %>%
    mutate(
      descr_tipologia_istat_s13_key = normalizza_testo(descr_tipologia_istat_s13),
      macro_gruppo_pa = crea_macro_gruppo_default(descr_tipologia_istat_s13)
    ) %>%
    select(
      descr_tipologia_istat_s13,
      descr_tipologia_istat_s13_key,
      macro_gruppo_pa
    )
  
  write_csv(
    classificazione_macro_gruppi_pa_init,
    file_classificazione_pa
  )
  
  message(
    "Creato file classificazione macro-gruppi PA: ",
    file_classificazione_pa,
    ". Controllare manualmente la classificazione se necessario."
  )
}

classificazione_macro_gruppi_pa <- read_csv(
  file_classificazione_pa,
  show_col_types = FALSE
) %>%
  mutate(
    descr_tipologia_istat_s13_key = normalizza_testo(descr_tipologia_istat_s13),
    macro_gruppo_pa = as.character(macro_gruppo_pa)
  ) %>%
  select(
    descr_tipologia_istat_s13_key,
    macro_gruppo_pa
  ) %>%
  distinct()

noipa_raccordato <- noipa_raccordato %>%
  mutate(
    descr_tipologia_istat_s13_key = normalizza_testo(descr_tipologia_istat_s13),
    regione_ente = dizione_regione,
    provincia_ente = dizione_provincia,
    codice_regione = stringr::str_pad(as.character(codice_regione), 2, pad = "0"),
    codice_provincia = as.character(codice_provincia),
    classe_eta = case_when(
      !is.na(eta_min_std) & !is.na(eta_max_std) ~ paste0(eta_min_std, "-", eta_max_std),
      !is.na(eta_min_std) & is.na(eta_max_std) ~ paste0(eta_min_std, "+"),
      TRUE ~ "Non disponibile"
    ),
    genere = case_when(
      sesso_key == "F" ~ "Femmine",
      sesso_key == "M" ~ "Maschi",
      is.na(sesso_key) ~ "Non disponibile",
      TRUE ~ sesso_key
    )
  ) %>%
  left_join(
    classificazione_macro_gruppi_pa,
    by = "descr_tipologia_istat_s13_key"
  ) %>%
  mutate(
    macro_gruppo_pa = if_else(
      is.na(macro_gruppo_pa),
      "Non raccordato / non classificato",
      macro_gruppo_pa
    )
  )

check_classificazione_pa <- noipa_raccordato %>%
  distinct(descr_tipologia_istat_s13, macro_gruppo_pa) %>%
  arrange(macro_gruppo_pa, descr_tipologia_istat_s13)

write_csv(
  check_classificazione_pa,
  file.path(DIR_LOGS, paste0("check_classificazione_macro_gruppi_pa_", anno_riferimento, ".csv"))
)

# 9) LOG MATCH E OUTPUT RACCORDATO ------------------------------------------

log_match_noipa <- noipa_raccordato %>%
  group_by(dataset_nome, dataset_id) %>%
  summarise(
    n_righe = n(),
    n_righe_con_amministrazione = sum(!is.na(amministrazione_key)),
    n_righe_match = sum(match_lista == 1, na.rm = TRUE),
    quota_match_su_righe = if_else(n_righe > 0, n_righe_match / n_righe, NA_real_),
    quota_match_su_righe_con_amministrazione = if_else(
      n_righe_con_amministrazione > 0,
      n_righe_match / n_righe_con_amministrazione,
      NA_real_
    ),
    n_amministrazioni_noipa = n_distinct(amministrazione_key, na.rm = TRUE),
    n_amministrazioni_match = n_distinct(amministrazione_key[match_lista == 1], na.rm = TRUE),
    tipi_match = paste(sort(unique(tipo_match)), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    quota_match_su_righe_pct = round(100 * quota_match_su_righe, 1),
    quota_match_su_righe_con_amministrazione_pct =
      round(100 * quota_match_su_righe_con_amministrazione, 1)
  )

write_csv(
  log_match_noipa,
  file.path(DIR_LOGS, paste0("log_match_noipa_lista_", anno_riferimento, ".csv"))
)

write_csv(
  noipa_raccordato,
  file.path(DIR_NOIPA_OUTPUT, paste0("noipa_raccordato_lista_", anno_riferimento, ".csv"))
)

saveRDS(
  noipa_raccordato,
  file.path(DIR_NOIPA_OUTPUT, paste0("noipa_raccordato_lista_", anno_riferimento, ".rds"))
)

# 9.1) DIAGNOSTICA NON-MATCH CON CANDIDATI LISTA -----------------------------


# Amministrazioni NoiPA non matchate, aggregate
noipa_non_match_base <- noipa_raccordato %>%
  filter(
    !is.na(amministrazione_key),
    match_lista == 0
  ) %>%
  group_by(dataset_nome, dataset_id, amministrazione_key) %>%
  summarise(
    n_righe = n(),
    valore_totale = sum(valore_noipa, na.rm = TRUE),
    esempi_amministrazione_noipa = paste(
      sort(unique(na.omit(amministrazione)))[1:min(5, length(unique(na.omit(amministrazione))))],
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  arrange(dataset_nome, desc(n_righe), amministrazione_key)

# Denominazioni disponibili nella lista, in formato long
lista_denominazioni_long <- lista %>%
  mutate(lista_row_id = row_number()) %>%
  select(
    any_of(c(
      "lista_row_id",
      "ragione_sociale",
      "denominazione",
      "codice_fiscale",
      "codice_unita_s13",
      "descr_tipologia_istat_s13",
      "dizione_comune",
      "dizione_provincia",
      "dizione_regione"
    ))
  ) %>%
  pivot_longer(
    cols = any_of(c("ragione_sociale", "denominazione")),
    names_to = "campo_lista",
    values_to = "denominazione_lista"
  ) %>%
  mutate(
    amministrazione_key_lista = normalizza_testo(denominazione_lista)
  ) %>%
  filter(
    !is.na(amministrazione_key_lista),
    amministrazione_key_lista != ""
  ) %>%
  distinct()

# Funzione: per una chiave NoiPA trova le candidate lista più simili
trova_candidati_lista <- function(key_noipa, n_candidati = 5) {
  
  lista_denominazioni_long %>%
    mutate(
      distanza_jw = stringdist::stringdist(
        key_noipa,
        amministrazione_key_lista,
        method = "jw"
      ),
      distanza_lv = stringdist::stringdist(
        key_noipa,
        amministrazione_key_lista,
        method = "lv"
      )
    ) %>%
    arrange(distanza_jw, distanza_lv) %>%
    slice_head(n = n_candidati) %>%
    mutate(
      amministrazione_key_noipa = key_noipa
    )
}

# Per non esplodere, creo i candidati sulle amministrazioni distinte,
# poi riaggancio ai dataset.
candidati_lista_per_non_match <- noipa_non_match_base %>%
  distinct(amministrazione_key) %>%
  pull(amministrazione_key) %>%
  purrr::map_dfr(trova_candidati_lista, n_candidati = 5)

noipa_non_match_con_candidati <- noipa_non_match_base %>%
  left_join(
    candidati_lista_per_non_match,
    by = c("amministrazione_key" = "amministrazione_key_noipa")
  ) %>%
  arrange(dataset_nome, desc(n_righe), amministrazione_key, distanza_jw)

write_csv(
  noipa_non_match_base,
  file.path(DIR_LOGS, paste0("noipa_non_match_base_", anno_riferimento, ".csv"))
)

write_csv(
  noipa_non_match_con_candidati,
  file.path(DIR_LOGS, paste0("noipa_non_match_con_candidati_lista_", anno_riferimento, ".csv"))
)


# 10) CONTROLLI TECNICI SUI DATASET ------------------------------------------

controllo_mensile_dataset <- noipa_raccordato %>%
  group_by(dataset_nome, dataset_id, anno, mese, periodo) %>%
  summarise(
    n_righe = n(),
    valore_totale = sum(valore_noipa, na.rm = TRUE),
    n_amministrazioni = n_distinct(amministrazione_key, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  controllo_mensile_dataset,
  file.path(DIR_NOIPA_OUTPUT, paste0("controllo_mensile_dataset_noipa_", anno_riferimento, ".csv"))
)

# 11) INDICATORI SOSTANTIVI --------------------------------------------------
# Gli indicatori sono calcolati dataset per dataset, senza merge largo.

dimensioni_base <- c(
  "anno",
  "mese",
  "periodo",
  "amministrazione_key",
  "macro_gruppo_pa",
  "regione_ente",
  "provincia_ente",
  "codice_regione",
  "codice_provincia",
  "match_lista",
  "tipo_match",
  "classe_eta",
  "genere"
)

# 11.1 Assunzioni ------------------------------------------------------------

indicatori_assunzioni <- noipa_raccordato %>%
  filter(dataset_id == "EntryMotivoAssunzione") %>%
  group_by(
    across(all_of(dimensioni_base)),
    motivo_assunzione_key
  ) %>%
  summarise(
    n_assunzioni = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_assunzioni,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_assunzioni_noipa_", anno_riferimento, ".csv"))
)

# 11.2 Cessazioni ------------------------------------------------------------

indicatori_cessazioni <- noipa_raccordato %>%
  filter(dataset_id == "EntryMotivoCessazione") %>%
  group_by(
    across(all_of(dimensioni_base)),
    motivo_cessazione_key
  ) %>%
  summarise(
    n_cessazioni = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_cessazioni,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_cessazioni_noipa_", anno_riferimento, ".csv"))
)

# 11.3 Flussi personale: assunzioni, cessazioni, saldo -----------------------

assunzioni_tot <- indicatori_assunzioni %>%
  group_by(across(all_of(dimensioni_base))) %>%
  summarise(
    n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
    .groups = "drop"
  )

cessazioni_tot <- indicatori_cessazioni %>%
  group_by(across(all_of(dimensioni_base))) %>%
  summarise(
    n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
    .groups = "drop"
  )

indicatori_flussi_personale <- full_join(
  assunzioni_tot,
  cessazioni_tot,
  by = dimensioni_base
) %>%
  mutate(
    n_assunzioni = replace_na(n_assunzioni, 0),
    n_cessazioni = replace_na(n_cessazioni, 0),
    saldo_assunzioni_cessazioni = n_assunzioni - n_cessazioni
  )

write_csv(
  indicatori_flussi_personale,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_flussi_personale_noipa_", anno_riferimento, ".csv"))
)

saveRDS(
  indicatori_flussi_personale,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_flussi_personale_noipa_", anno_riferimento, ".rds"))
)

# 11.4 Inquadramenti contrattuali -------------------------------------------

indicatori_inquadramenti <- noipa_raccordato %>%
  filter(dataset_id == "EntryContrattiGestiti") %>%
  group_by(
    across(all_of(dimensioni_base)),
    provincia_key,
    comparto_key,
    inquadramento_key
  ) %>%
  summarise(
    n_inquadramenti = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_inquadramenti,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_inquadramenti_noipa_", anno_riferimento, ".csv"))
)

# 11.5 Modalità accredito stipendi ------------------------------------------

indicatori_accredito_stipendi <- noipa_raccordato %>%
  filter(dataset_id == "EntryAccreditoStipendi") %>%
  group_by(
    across(all_of(dimensioni_base)),
    comune_key,
    modalita_pagamento_key
  ) %>%
  summarise(
    n_accrediti = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_accredito_stipendi,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_accredito_stipendi_noipa_", anno_riferimento, ".csv"))
)

# 11.6 Mobilità / pendolarismo ----------------------------------------------

indicatori_pendolarismo <- noipa_raccordato %>%
  filter(dataset_id == "EntryPendolarismo") %>%
  group_by(
    across(all_of(dimensioni_base)),
    provincia_key,
    comune_key,
    stesso_comune_key,
    distance_min_km_std,
    distance_max_km_std
  ) %>%
  summarise(
    n_amministrati_pendolarismo = sum(valore_noipa, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_pendolarismo,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_pendolarismo_noipa_", anno_riferimento, ".csv"))
)

# 11.7 Struttura organizzativa ----------------------------------------------

indicatori_struttura <- noipa_raccordato %>%
  filter(dataset_id == "EntryStrutturaOrganizzativa") %>%
  group_by(
    across(all_of(dimensioni_base)),
    comune_key
  ) %>%
  summarise(
    numero_unita_organizzative = sum(numero_unita_organizzative, na.rm = TRUE),
    numero_rapporti_lavoro = sum(numero_rapporti_lavoro, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_struttura,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_struttura_noipa_", anno_riferimento, ".csv"))
)

# 11.8 Certificazioni uniche / redditi ---------------------------------------

indicatori_certificazioni_uniche <- noipa_raccordato %>%
  filter(dataset_id == "EntryCertificazioniUniche") %>%
  group_by(
    anno,
    amministrazione_key,
    macro_gruppo_pa,
    regione_ente,
    provincia_ente,
    codice_regione,
    codice_provincia,
    match_lista,
    tipo_match,
    classe_eta,
    genere
  ) %>%
  summarise(
    imponibili_previdenziali = sum(imponibili_previdenziali_std, na.rm = TRUE),
    reddito_anno_corrente = sum(reddito_anno_corrente_std, na.rm = TRUE),
    reddito_anni_precedenti = sum(reddito_anni_precedenti_std, na.rm = TRUE),
    ritenute_irpef_anno_corrente = sum(ritenute_irpef_anno_corrente_std, na.rm = TRUE),
    totale_detrazioni = sum(totale_detrazioni_std, na.rm = TRUE),
    previdenza_complementare = sum(previdenza_complementare_std, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  indicatori_certificazioni_uniche,
  file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_certificazioni_uniche_noipa_", anno_riferimento, ".csv"))
)

# 12) DATASET DASHBOARD ------------------------------------------------------
# Questi dataset sono pensati per Shiny: mantengono le dimensioni filtrabili.

dashboard_flussi_long <- indicatori_flussi_personale %>%
  select(
    all_of(dimensioni_base),
    n_assunzioni,
    n_cessazioni,
    saldo_assunzioni_cessazioni
  ) %>%
  pivot_longer(
    cols = c(n_assunzioni, n_cessazioni),
    names_to = "tipo_flusso",
    values_to = "numero"
  ) %>%
  mutate(
    tipo_flusso = recode(
      tipo_flusso,
      n_assunzioni = "Assunzioni",
      n_cessazioni = "Cessazioni"
    ),
    serie_grafico = paste(macro_gruppo_pa, tipo_flusso, sep = " - ")
  )

write_csv(
  dashboard_flussi_long,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_flussi_long_noipa_", anno_riferimento, ".csv"))
)

saveRDS(
  dashboard_flussi_long,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_flussi_long_noipa_", anno_riferimento, ".rds"))
)

dashboard_assunzioni_motivo <- indicatori_assunzioni %>%
  group_by(
    macro_gruppo_pa,
    classe_eta,
    genere,
    periodo,
    motivo_assunzione_key
  ) %>%
  summarise(
    n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
    .groups = "drop"
  )

dashboard_cessazioni_motivo <- indicatori_cessazioni %>%
  group_by(
    macro_gruppo_pa,
    classe_eta,
    genere,
    periodo,
    motivo_cessazione_key
  ) %>%
  summarise(
    n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
    .groups = "drop"
  )

dashboard_accredito <- indicatori_accredito_stipendi %>%
  group_by(
    macro_gruppo_pa,
    classe_eta,
    genere,
    periodo,
    modalita_pagamento_key
  ) %>%
  summarise(
    n_accrediti = sum(n_accrediti, na.rm = TRUE),
    .groups = "drop"
  )

dashboard_inquadramenti <- indicatori_inquadramenti %>%
  group_by(
    macro_gruppo_pa,
    classe_eta,
    genere,
    periodo,
    comparto_key,
    inquadramento_key
  ) %>%
  summarise(
    n_inquadramenti = sum(n_inquadramenti, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  dashboard_assunzioni_motivo,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_assunzioni_motivo_noipa_", anno_riferimento, ".csv"))
)

write_csv(
  dashboard_cessazioni_motivo,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_cessazioni_motivo_noipa_", anno_riferimento, ".csv"))
)

write_csv(
  dashboard_accredito,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_accredito_noipa_", anno_riferimento, ".csv"))
)

write_csv(
  dashboard_inquadramenti,
  file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_inquadramenti_noipa_", anno_riferimento, ".csv"))
)

# 13) OUTPUT SPAZIALE REGIONALE OPZIONALE -----------------------------------
# La parte spaziale è separata in 00_spatial_helpers.R.
# Questo blocco prova a produrre una mappa solo se i pacchetti necessari esistono.

if (
  file.exists(file.path(DIR_SCRIPTS, "00_spatial_helpers.R")) &&
  requireNamespace("giscoR", quietly = TRUE) &&
  requireNamespace("sf", quietly = TRUE) &&
  requireNamespace("leaflet", quietly = TRUE)
) {
  
  source(file.path(DIR_SCRIPTS, "00_spatial_helpers.R"))
  
  lista_geo <- prepara_lista_geo(lista)
  
  write_csv(
    lista_geo,
    file.path(DIR_OUTPUT, "lista_geo.csv")
  )
  
  mappa_flussi_regione <- indicatori_flussi_personale %>%
    group_by(codice_regione, regione_ente) %>%
    summarise(
      n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
      n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
      saldo_assunzioni_cessazioni = sum(saldo_assunzioni_cessazioni, na.rm = TRUE),
      .groups = "drop"
    )
  
  mappa_flussi_regione_sf <- prepara_mappa_regionale(
    dati_regione = mappa_flussi_regione,
    year = 2024,
    resolution = "10"
  )
  
  mappa_leaflet_flussi <- crea_leaflet_regionale_flussi(mappa_flussi_regione_sf)
  
  htmlwidgets::saveWidget(
    mappa_leaflet_flussi,
    file = file.path(DIR_NOIPA_OUTPUT, paste0("mappa_flussi_regione_noipa_", anno_riferimento, ".html")),
    selfcontained = TRUE
  )
  
} else {
  message(
    "Mappa regionale non generata: controllare presenza di 00_spatial_helpers.R e pacchetti giscoR/sf/leaflet."
  )
}

# 14) HTML STATICO LEGGERO DI CONTROLLO -------------------------------------
# Nota: per filtri con riaggregazione dinamica è meglio usare Shiny.
# Qui produciamo solo una vista rapida.

plot_match <- log_match_noipa %>%
  plot_ly(
    x = ~quota_match_su_righe_con_amministrazione_pct,
    y = ~reorder(dataset_nome, quota_match_su_righe_con_amministrazione_pct),
    type = "bar",
    orientation = "h",
    hoverinfo = "text",
    text = ~paste0(
      "Dataset: ", dataset_nome,
      "<br>Quota match su righe con amministrazione: ",
      quota_match_su_righe_con_amministrazione_pct, "%",
      "<br>Righe con amministrazione: ", n_righe_con_amministrazione,
      "<br>Righe matchate: ", n_righe_match,
      "<br>Amministrazioni NoiPA: ", n_amministrazioni_noipa,
      "<br>Amministrazioni matchate: ", n_amministrazioni_match,
      "<br>Tipi match: ", tipi_match
    )
  ) %>%
  layout(
    title = "NoiPA - Qualità del raccordo con la master list",
    xaxis = list(title = "% righe con amministrazione matchate"),
    yaxis = list(title = ""),
    margin = list(l = 220)
  )

plot_flussi_statico <- dashboard_flussi_long %>%
  group_by(periodo, macro_gruppo_pa, tipo_flusso, serie_grafico) %>%
  summarise(
    numero = sum(numero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  plot_ly(
    x = ~periodo,
    y = ~numero,
    color = ~serie_grafico,
    type = "scatter",
    mode = "lines+markers",
    hoverinfo = "text",
    text = ~paste0(
      "Periodo: ", periodo,
      "<br>Macro-gruppo: ", macro_gruppo_pa,
      "<br>Tipo flusso: ", tipo_flusso,
      "<br>Numero: ", format(numero, big.mark = ".", decimal.mark = ",")
    )
  ) %>%
  layout(
    title = paste0("NoiPA - Assunzioni e cessazioni per macro-gruppo PA (", anno_riferimento, ")"),
    xaxis = list(title = "Mese"),
    yaxis = list(title = "Numero"),
    legend = list(orientation = "h")
  )

dashboard_html <- tagList(
  tags$h1(paste0("NoiPA - Raccordo e indicatori esplorativi ", anno_riferimento)),
  tags$p(
    "Dashboard statica di controllo costruita a partire dai dataset NoiPA raccordati, ove possibile, alla master list S13+/MPA/BDAP. ",
    "Il raccordo è basato sulla denominazione dell'amministrazione e va considerato esplorativo. ",
    "Per filtri dinamici per macro-gruppo, classe di età, genere e periodo usare la dashboard Shiny."
  ),
  tags$h2("1. Qualità del raccordo con la master list"),
  plot_match,
  tags$h2("2. Flussi mensili di assunzione e cessazione"),
  plot_flussi_statico
)

htmltools::save_html(
  dashboard_html,
  file = file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_noipa_controllo_", anno_riferimento, ".html"))
)

message("Script completato.")
message("Output principali:")
message("- ", file.path(DIR_NOIPA_OUTPUT, paste0("noipa_raccordato_lista_", anno_riferimento, ".csv")))
message("- ", file.path(DIR_NOIPA_OUTPUT, paste0("indicatori_flussi_personale_noipa_", anno_riferimento, ".csv")))
message("- ", file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_flussi_long_noipa_", anno_riferimento, ".rds")))
message("- ", file.path(DIR_NOIPA_OUTPUT, paste0("dashboard_noipa_controllo_", anno_riferimento, ".html")))
message("- ", file.path(DIR_LOGS, paste0("log_match_noipa_lista_", anno_riferimento, ".csv")))
