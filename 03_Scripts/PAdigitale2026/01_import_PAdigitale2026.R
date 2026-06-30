# ============================================================ #
# Script: 01_import_PAdigitale2026.R
# Fonte: PA digitale 2026 - Open data
# Obiettivo:
#   1. scaricare i CSV da GitHub
#   2. salvare raw e processed
#   3. creare un dataset unico candidature finanziate
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
  library(janitor)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(googledrive)
  library(openxlsx)
}

# 3) Autenticazione Drive --------------------------------------------------------

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")


# 4) Parametri del run --------------------------------------------------------

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID import: ", RUN_ID)

# parametro per pulire la cartella temp alla fine del run
delete_local_temp <- FALSE


# Documentazione ufficiale delle variabili -------------------------------

url_ipa_metadata_enti <- paste0(
  "https://indicepa.gov.it/ipa-dati/dataset/",
  "5baa3eb8-266e-455a-8de8-b1f434c279b2/resource/",
  "61c36f27-e9a9-47de-a4c5-71bc16a28d63/download/",
  "metadata_enti.json"
)

url_ipa_metadata_categorie <- paste0(
  "https://indicepa.gov.it/ipa-dati/dataset/",
  "1c7034cd-8514-485d-8ff7-c72b5cb09a07/resource/",
  "4007d866-96aa-40cd-8f85-e042538c47b5/download/",
  "metadata_categorie_enti.json"
)

base_format_pad26_url <- paste0(
  "https://raw.githubusercontent.com/",
  "teamdigitale/padigitale2026-opendata/main/format"
)

url_format_pad26_avvisi <- paste0(
  base_format_pad26_url,
  "/format-pad26-avvisi.md"
)

url_format_pad26_candidature <- paste0(
  base_format_pad26_url,
  "/format-pad26-candidature-comuni-finanziate.md"
)

# 5) Directory locali e Drive -------------------------------------------------

{DIR_PAD26_SOURCE_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Source", RUN_ID)
DIR_PAD26_PROCESSED_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID)
DIR_PAD26_METADATA_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID)
DIR_PAD26_LOGS_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID)

DRIVE_PAD26_SOURCE <- file.path(DRIVE_DIR_SOURCE_PAD26, RUN_ID)
DRIVE_PAD26_PROCESSED <- file.path(DRIVE_DIR_PROCESSED_PAD26, RUN_ID)
DRIVE_PAD26_METADATA <- file.path(DRIVE_DIR_SOURCE_MET_PAD26, RUN_ID)
DRIVE_PAD26_LOGS <- file.path(DRIVE_DIR_LOGS_PAD26, RUN_ID)
}
# 6) Creazione directory locali ----------------------------------------------

{  dir.create(DIR_PAD26_SOURCE_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_PROCESSED_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)
}

# 7) Avvio console log --------------------------------------------------------

console_log <- start_console_log(
  log_dir = DIR_PAD26_LOGS_LOCAL,
  run_id = RUN_ID,
  script_name = "01_import_PAdigitale2026"
)

# 8) URL file sorgente ----------------------------------------------------

base_raw_url <- "https://raw.githubusercontent.com/teamdigitale/padigitale2026-opendata/main/data"

# URL PA digitale 2026
base_raw_url <- paste0(
  "https://raw.githubusercontent.com/",
  "teamdigitale/padigitale2026-opendata/main/data"
)


# URL ufficiali IPA
url_ipa_categorie <- paste0(
  "https://indicepa.gov.it/ipa-dati/dataset/",
  "1c7034cd-8514-485d-8ff7-c72b5cb09a07/resource/",
  "84ebb2e7-0e61-427b-a1dd-ab8bb2a84f07/download/",
  "categorie-enti.xlsx"
)

url_ipa_enti <- paste0(
  "https://indicepa.gov.it/ipa-dati/dataset/",
  "5baa3eb8-266e-455a-8de8-b1f434c279b2/resource/",
  "d09adf99-dc10-4349-8c53-27b1e5aa97b6/download/",
  "enti.xlsx"
)

run_info <- tibble::tibble(
  run_id = RUN_ID,
  data_run = Sys.time(),
  fonte = "PA digitale 2026 + classificazione IPA",
  base_raw_url = base_raw_url,
  url_ipa_enti = url_ipa_enti,
  url_ipa_categorie = url_ipa_categorie,
  url_ipa_metadata_enti = url_ipa_metadata_enti,
  url_ipa_metadata_categorie = url_ipa_metadata_categorie,
  url_format_pad26_avvisi = url_format_pad26_avvisi,
  url_format_pad26_candidature = url_format_pad26_candidature
)

local_run_info <- file.path(DIR_PAD26_PROCESSED_LOCAL, "run_info.csv")
write_csv(run_info, local_run_info)
# drive_upload_or_update(local_run_info, DRIVE_PAD26_PROCESSED)
drive_upload_or_update(
  local_path = local_run_info,
  drive_folder_rel = DRIVE_PAD26_PROCESSED
)
# 9) Elenco file sorgente ----------------------------------------------------

file_pad26 <- tibble::tribble(
  ~dataset_id, ~filename,
  "avvisi", "avvisi.csv",
  "candidature_comuni_finanziate", "candidature_comuni_finanziate.csv",
  "candidature_scuole_finanziate", "candidature_scuole_finanziate.csv",
  "candidature_altrienti_finanziate", "candidature_altrienti_finanziate.csv"
)


# 10) Funzioni ----------------------------------------------------------------

scarica_pad26 <- function(dataset_id, filename) {
  
  url <- paste0(base_raw_url, "/", filename)
  raw_path <- file.path(DIR_PAD26_SOURCE_LOCAL, filename)
  
  message("Scarico: ", filename)
  
  download.file(
    url = url,
    destfile = raw_path,
    mode = "wb",
    quiet = FALSE,
    method = "libcurl"
  )
  
  drive_upload_or_update(
    local_path = raw_path,
    drive_folder_rel = DRIVE_PAD26_SOURCE
  )
  
  df <- readr::read_csv(
    raw_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(
      dataset_id = dataset_id,
      file_origine = filename,
      .before = 1
    )
  
  df
}


# 11) Download raw e import ---------------------------------------------------

# Timeout più ampio per download di file grandi o connessioni lente
options(timeout = max(600, getOption("timeout")))

pad26_list <- purrr::map2(
  file_pad26$dataset_id,
  file_pad26$filename,
  scarica_pad26
)

names(pad26_list) <- file_pad26$dataset_id

# summary(as.factor(pad26_list$candidature_scuole_finanziate$dataset_id))
# summary(as.factor(pad26_list$candidature_scuole_finanziate$file_origine))
# 
# summary(as.factor(pad26_list$candidature_altrienti_finanziate$dataset_id))
# summary(as.factor(pad26_list$candidature_altrienti_finanziate$file_origine))
# 
# summary(as.factor(pad26_list$candidature_comuni_finanziate$dataset_id))
# summary(as.factor(pad26_list$candidature_comuni_finanziate$file_origine))
# 
# summary(as.factor(pad26_list$avvisi$dataset_id))
# summary(as.factor(pad26_list$avvisi$file_origine))
# 
# 
# names(pad26_list$avvisi)
# names(pad26_list$candidature_comuni_finanziate)
# names(pad26_list$candidature_scuole_finanziate)
# names(pad26_list$candidature_altrienti_finanziate)


#==============================================================================#
#### 11.1) DOWNLOAD E PREPARAZIONE CLASSIFICAZIONE IPA                    ----
#==============================================================================#

# I file IPA raw vengono salvati insieme agli altri source data PAD26.

local_ipa_categorie_raw <- file.path(
  DIR_PAD26_SOURCE_LOCAL,
  "ipa_categorie_enti.xlsx"
)

local_ipa_enti_raw <- file.path(
  DIR_PAD26_SOURCE_LOCAL,
  "ipa_enti.xlsx"
)


message("Scarico dataset IPA: categorie enti")

download.file(
  url = url_ipa_categorie,
  destfile = local_ipa_categorie_raw,
  mode = "wb",
  quiet = FALSE
)


message("Scarico dataset IPA: enti")

download.file(
  url = url_ipa_enti,
  destfile = local_ipa_enti_raw,
  mode = "wb",
  quiet = FALSE
)


if (!file.exists(local_ipa_categorie_raw)) {
  stop(
    "Dataset IPA categorie non scaricato: ",
    local_ipa_categorie_raw
  )
}

if (!file.exists(local_ipa_enti_raw)) {
  stop(
    "Dataset IPA enti non scaricato: ",
    local_ipa_enti_raw
  )
}


# Upload dei raw IPA nella stessa cartella Source di PAD26.

drive_upload_or_update(
  local_path = local_ipa_categorie_raw,
  drive_folder_rel = DRIVE_PAD26_SOURCE
)

drive_upload_or_update(
  local_path = local_ipa_enti_raw,
  drive_folder_rel = DRIVE_PAD26_SOURCE
)


ipa_categorie <- readxl::read_excel(
  local_ipa_categorie_raw,
  col_types = "text"
) %>%
  janitor::clean_names()


ipa_enti <- readxl::read_excel(
  local_ipa_enti_raw,
  col_types = "text"
) %>%
  janitor::clean_names()

# names(ipa_categorie)
# Controllo delle colonne necessarie
colonne_ipa_categorie_attese <- c(
  "codice_categoria",
  "nome_categoria",
  "tipologia_categoria"
)

# names(ipa_enti)
colonne_ipa_enti_attese <- c(
  "codice_ipa",
  "denominazione_ente",
  "codice_fiscale_ente",
  "tipologia",
  "codice_categoria"
  #   "codice_ateco"            "ente_in_liquidazione"    "codice_miur"             "codice_istat"            "acronimo"                "nome_responsabile"   
)

stopifnot(
  all(
    colonne_ipa_categorie_attese %in%
      names(ipa_categorie)
  )
)

stopifnot(
  all(
    colonne_ipa_enti_attese %in%
      names(ipa_enti)
  )
)


# Normalizzazione specifica del codice IPA
normalizza_codice_ipa <- function(x) {
  
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_to_lower(x)
  
  dplyr::na_if(x, "")
}


ipa_categorie <- ipa_categorie %>%
  mutate(
    codice_categoria =
      stringr::str_trim(codice_categoria),
    
    nome_categoria =
      stringr::str_squish(nome_categoria)
  )


ipa_enti <- ipa_enti %>%
  dplyr::mutate(
    codice_ipa_key =
      normalizza_codice_ipa(codice_ipa),
    
    codice_categoria =
      stringr::str_trim(codice_categoria)
  )

# Normalizzazione specifica del codice fiscale
normalizza_codice_fiscale <- function(x) {
  x <- as.character(x)
  
  x <- stringr::str_trim(x)
  x <- stringr::str_to_upper(x)
  
  x <- stringr::str_replace_all(
    x,
    "[^A-Z0-9]",
    ""
  )
  
  dplyr::na_if(x, "")
}

# Tabella completa degli enti con descrizione della categoria
ipa_enti_classificati <- ipa_enti %>%
  dplyr::left_join(
    ipa_categorie %>%
      dplyr::select(
        codice_categoria,
        nome_categoria,
        tipologia_categoria
      ),
    by = "codice_categoria"
  )


duplicati_ipa_enti <- ipa_enti_classificati %>%
  dplyr::filter(
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  dplyr::count(
    codice_ipa_key,
    name = "n_record"
  ) %>%
  dplyr::filter(n_record > 1L)

if (nrow(duplicati_ipa_enti) > 0L) {
  warning(
    "L'anagrafica IPA contiene ",
    nrow(duplicati_ipa_enti),
    " codici IPA duplicati."
  )
}


ipa_enti_lookup <- ipa_enti_classificati %>%
  dplyr::filter(
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  dplyr::arrange(codice_ipa_key) %>%
  dplyr::distinct(
    codice_ipa_key,
    .keep_all = TRUE
  ) %>%
  dplyr::transmute(
    codice_ipa_key,
    
    codice_ipa_ipa =
      codice_ipa,
    
    denominazione_ipa =
      denominazione_ente,
    
    codice_fiscale_ipa =
      codice_fiscale_ente,
    
    tipologia_ipa =
      tipologia,
    
    codice_categoria_ipa =
      codice_categoria,
    
    nome_categoria_ipa =
      nome_categoria,
    
    tipologia_categoria_ipa =
      tipologia_categoria
  )

# summary(as.factor(ipa_enti_lookup$codice_categoria_ipa))

# Categoria ufficiale degli ordini professionali
codice_categoria_ordini <- "C14"

ipa_ordini_professionali <- ipa_enti_lookup %>%
  dplyr::filter(
    codice_categoria_ipa ==
      codice_categoria_ordini
  ) %>%
  dplyr::mutate(
    ordine_professionale_ipa = TRUE
  )

nome_categoria_ordini_atteso <- paste(
  "Federazioni Nazionali, Ordini, Collegi",
  "e Consigli Professionali"
)


# Controlla che C14 corrisponda effettivamente alla categoria attesa
check_categoria_ordini <- ipa_categorie %>%
  filter(codice_categoria == codice_categoria_ordini)

if (nrow(check_categoria_ordini) != 1L) {
  stop(
    "La categoria IPA C14 non è presente una sola volta ",
    "nel dataset categorie enti."
  )
}

if (
  !stringr::str_detect(
    stringr::str_to_lower(
      check_categoria_ordini$nome_categoria
    ),
    "ordini|collegi|consigli professionali"
  )
) {
  stop(
    "La categoria IPA C14 non sembra più identificare ",
    "gli ordini professionali. Verificare il dataset IPA."
  )
}


# Lookup ufficiale degli ordini professionali
ipa_ordini_professionali <- ipa_enti_classificati %>%
  filter(
    codice_categoria ==
      codice_categoria_ordini
  ) %>%
  transmute(
    codice_ipa_key,
    codice_ipa_ipa = codice_ipa,
    denominazione_ipa = denominazione_ente,
    codice_fiscale_ipa = codice_fiscale_ente,
    codice_categoria_ipa = codice_categoria,
    nome_categoria_ipa = nome_categoria,
    ordine_professionale_ipa = TRUE
  ) %>%
  filter(
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  distinct(
    codice_ipa_key,
    .keep_all = TRUE
  )


# Diagnostiche
check_ipa_ordini <- tibble::tibble(
  run_id = RUN_ID,
  codice_categoria = codice_categoria_ordini,
  nome_categoria =
    check_categoria_ordini$nome_categoria,
  n_ordini_professionali =
    nrow(ipa_ordini_professionali),
  n_codici_ipa_distinti =
    n_distinct(
      ipa_ordini_professionali$codice_ipa_key,
      na.rm = TRUE
    )
)


duplicati_codice_ipa_ordini <- ipa_enti_classificati %>%
  filter(
    codice_categoria ==
      codice_categoria_ordini,
    !is.na(codice_ipa_key),
    codice_ipa_key != ""
  ) %>%
  count(
    codice_ipa_key,
    name = "n_record"
  ) %>%
  filter(n_record > 1L)


print(check_ipa_ordini)
print(duplicati_codice_ipa_ordini)

local_ipa_ordini_rds <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "ipa_ordini_professionali.rds"
)

local_ipa_ordini_xlsx <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "ipa_ordini_professionali.xlsx"
)

local_ipa_check_xlsx <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "check_classificazione_ordini_ipa.xlsx"
)


saveRDS(
  ipa_ordini_professionali,
  local_ipa_ordini_rds
)

openxlsx::write.xlsx(
  ipa_ordini_professionali,
  file = local_ipa_ordini_xlsx,
  overwrite = TRUE
)

openxlsx::write.xlsx(
  list(
    "Sintesi" = check_ipa_ordini,
    "Duplicati codice IPA" =
      duplicati_codice_ipa_ordini,
    "Categoria C14" =
      check_categoria_ordini
  ),
  file = local_ipa_check_xlsx,
  overwrite = TRUE
)


drive_upload_or_update(
  local_path = local_ipa_ordini_rds,
  drive_folder_rel = DRIVE_PAD26_PROCESSED
)

drive_upload_or_update(
  local_path = local_ipa_ordini_xlsx,
  drive_folder_rel = DRIVE_PAD26_PROCESSED
)

drive_upload_or_update(
  local_path = local_ipa_check_xlsx,
  drive_folder_rel = DRIVE_PAD26_METADATA
)

# 12) Creazione dataset processed --------------------------------------------

avvisi <- pad26_list$avvisi

candidature_pad26 <- bind_rows(
  pad26_list$candidature_comuni_finanziate,
  pad26_list$candidature_scuole_finanziate,
  pad26_list$candidature_altrienti_finanziate,
  .id = "tipo_file_candidatura"
)


candidature_pad26 <- candidature_pad26 %>%
  mutate(
    importo_finanziamento = readr::parse_number(importo_finanziamento),
    data_invio_candidatura = lubridate::ymd_hms(data_invio_candidatura, quiet = TRUE),
    data_finanziamento = lubridate::ymd(data_finanziamento, quiet = TRUE),
    data_stato_candidatura = lubridate::ymd(data_stato_candidatura, quiet = TRUE),
    cod_regione = stringr::str_pad(as.character(cod_regione), 2, pad = "0"),
    cod_provincia = stringr::str_pad(as.character(cod_provincia), 3, pad = "0"),
    cod_comune = as.character(cod_comune)
  )

summary(as.factor(candidature_pad26$dataset_id))
summary(as.factor(candidature_pad26$file_origine))


#==============================================================================#
#### 12.1) PULIZIA STRINGHE PER ESPORTAZIONE EXCEL                          ----
#==============================================================================#

# XML 1.0 ammette:
# - tabulazione: 9
# - nuova riga: 10
# - carriage return: 13
# - caratteri da 32 in avanti, con alcune esclusioni Unicode.
#
# Questa funzione elimina i caratteri di controllo non ammessi da Excel/XML
# senza usare una regex contenente \x00, che causa un errore di parsing in R.

clean_xml_string <- function(x) {
  
  if (!is.character(x)) {
    return(x)
  }
  
  # Converte in UTF-8; le sequenze non convertibili vengono rimosse.
  x <- iconv(
    x,
    from = "",
    to = "UTF-8",
    sub = ""
  )
  
  clean_one_string <- function(value) {
    
    if (is.na(value)) {
      return(NA_character_)
    }
    
    codepoints <- utf8ToInt(value)
    
    if (length(codepoints) == 0L) {
      return("")
    }
    
    valid_codepoint <- (
      codepoints %in% c(9L, 10L, 13L) |
        (codepoints >= 32L & codepoints <= 55295L) |
        (codepoints >= 57344L & codepoints <= 65533L) |
        (codepoints >= 65536L & codepoints <= 1114111L)
    )
    
    intToUtf8(codepoints[valid_codepoint])
  }
  
  vapply(
    x,
    clean_one_string,
    character(1),
    USE.NAMES = FALSE
  )
}


# Funzione parallela usata solo per il controllo diagnostico.
has_invalid_xml_chars <- function(x) {
  
  if (!is.character(x)) {
    return(rep(FALSE, length(x)))
  }
  
  vapply(
    x,
    function(value) {
      
      if (is.na(value)) {
        return(FALSE)
      }
      
      value_utf8 <- iconv(
        value,
        from = "",
        to = "UTF-8",
        sub = ""
      )
      
      codepoints <- utf8ToInt(value_utf8)
      
      if (length(codepoints) == 0L) {
        return(FALSE)
      }
      
      valid_codepoint <- (
        codepoints %in% c(9L, 10L, 13L) |
          (codepoints >= 32L & codepoints <= 55295L) |
          (codepoints >= 57344L & codepoints <= 65533L) |
          (codepoints >= 65536L & codepoints <= 1114111L)
      )
      
      any(!valid_codepoint)
    },
    logical(1),
    USE.NAMES = FALSE
  )
}


# Conta le celle problematiche nel dataset completo prima della pulizia.
invalid_xml_summary <- candidature_pad26 %>%
  dplyr::summarise(
    dplyr::across(
      where(is.character),
      ~ sum(has_invalid_xml_chars(.x), na.rm = TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variabile",
    values_to = "n_celle_non_valide"
  ) %>%
  dplyr::filter(n_celle_non_valide > 0L) %>%
  dplyr::arrange(dplyr::desc(n_celle_non_valide))

print(invalid_xml_summary)


#==============================================================================#
#### 12.2) ARRICCHIMENTO IPA E DEFINIZIONE DEL PERIMETRO MPA              ----
#==============================================================================#

candidature_pad26 <- candidature_pad26 %>%
  dplyr::mutate(
    codice_ipa_key =
      normalizza_codice_ipa(codice_ipa)
  ) %>%
  dplyr::left_join(
    ipa_enti_lookup,
    by = "codice_ipa_key"
  ) %>% dplyr::mutate(
    # Chiave fiscale normalizzata utilizzabile nello script 02.
    codice_fiscale_ipa_key =
      normalizza_codice_fiscale(
        codice_fiscale_ipa
      ),
    
    # L'indicatore viene ricavato dalla categoria ufficiale IPA.
    ordine_professionale_ipa =
      !is.na(codice_categoria_ipa) &
      codice_categoria_ipa ==
      codice_categoria_ordini,
    
    esclusione_scuola =
      dataset_id ==
      "candidature_scuole_finanziate",
    
    esclusione_ordine_professionale =
      ordine_professionale_ipa,
    
    fuori_perimetro_mpa =
      esclusione_scuola |
      esclusione_ordine_professionale,
    
    motivo_esclusione_mpa =
      dplyr::case_when(
        esclusione_scuola ~
          "scuola",
        
        esclusione_ordine_professionale ~
          "ordine_professionale_categoria_ipa",
        
        TRUE ~
          NA_character_
      )
  )

# Dataset completo:
# contiene scuole, ordini professionali e tutti gli altri enti,
# insieme ai flag di classificazione.
candidature_pad26_full <- candidature_pad26


# Dataset analitico:
# esclude scuole e ordini professionali.
candidature_pad26_mpa <- candidature_pad26_full %>%
  filter(
    !fuori_perimetro_mpa
  )

check_arricchimento_ipa <- candidature_pad26_full %>%
  dplyr::summarise(
    n_candidature =
      dplyr::n(),
    
    n_con_codice_ipa =
      sum(
        !is.na(codice_ipa_key) &
          codice_ipa_key != ""
      ),
    
    n_match_anagrafica_ipa =
      sum(
        !is.na(codice_ipa_ipa) &
          codice_ipa_ipa != ""
      ),
    
    quota_match_anagrafica_ipa =
      n_match_anagrafica_ipa /
      n_con_codice_ipa,
    
    n_con_codice_fiscale_ipa =
      sum(
        !is.na(codice_fiscale_ipa_key) &
          codice_fiscale_ipa_key != ""
      ),
    
    n_ordini_professionali =
      sum(
        ordine_professionale_ipa,
        na.rm = TRUE
      )
  )

print(check_arricchimento_ipa)

check_arricchimento_ipa_per_dataset <- candidature_pad26_full %>%
  dplyr::group_by(dataset_id) %>%
  dplyr::summarise(
    n_candidature =
      dplyr::n(),
    
    n_con_codice_ipa =
      sum(
        !is.na(codice_ipa_key) &
          codice_ipa_key != ""
      ),
    
    n_match_anagrafica_ipa =
      sum(
        !is.na(codice_ipa_ipa) &
          codice_ipa_ipa != ""
      ),
    
    quota_match_anagrafica_ipa =
      dplyr::if_else(
        n_con_codice_ipa > 0,
        n_match_anagrafica_ipa /
          n_con_codice_ipa,
        NA_real_
      ),
    
    n_con_codice_fiscale_ipa =
      sum(
        !is.na(codice_fiscale_ipa_key) &
          codice_fiscale_ipa_key != ""
      ),
    
    n_ordini_professionali =
      sum(
        ordine_professionale_ipa,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


log_possibili_ordini_non_classificati <- candidature_pad26_full %>%
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(
        dplyr::coalesce(ente, "")
      ),
      "\\b(ordine|collegio|geometr|profession)\\b"
    ),
    !ordine_professionale_ipa
  ) %>%
  dplyr::distinct(
    codice_ipa,
    codice_ipa_key,
    ente,
    tipologia_ente,
    codice_ipa_ipa,
    denominazione_ipa,
    codice_fiscale_ipa,
    codice_categoria_ipa,
    nome_categoria_ipa
  ) %>%
  dplyr::arrange(ente)


ipa_enti_classificati %>%
  dplyr::filter(
    codice_ipa_key %in%
      log_possibili_ordini_non_classificati$
      codice_ipa_key
  ) %>%
  dplyr::select(
    codice_ipa,
    denominazione_ente,
    codice_fiscale_ente,
    codice_categoria,
    nome_categoria,
    tipologia_categoria
  )


# Log sintetico di ciò che viene incluso/escluso.
log_perimetro_pad26 <- candidature_pad26_full %>%
  mutate(
    incluso_perimetro_mpa =
      !fuori_perimetro_mpa,
    
    motivo_perimetro = case_when(
      incluso_perimetro_mpa ~
        "incluso",
      
      !is.na(motivo_esclusione_mpa) ~
        motivo_esclusione_mpa,
      
      TRUE ~
        "escluso_altro"
    )
  ) %>%
  group_by(
    tipo_file_candidatura,
    dataset_id,
    incluso_perimetro_mpa,
    motivo_perimetro
  ) %>%
  summarise(
    n_candidature = n(),
    
    n_enti = n_distinct(
      case_when(
        !is.na(codice_ipa_key) ~
          paste0("IPA:", codice_ipa_key),
        
        !is.na(ente) & ente != "" ~
          paste0("ENTE:", ente),
        
        TRUE ~
          NA_character_
      ),
      na.rm = TRUE
    ),
    
    importo_finanziamento =
      sum(
        importo_finanziamento,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    dataset_id,
    incluso_perimetro_mpa,
    motivo_perimetro
  )

log_ordini_professionali_esclusi <- candidature_pad26_full %>%
  filter(
    esclusione_ordine_professionale
  ) %>%
  group_by(
    dataset_id,
    codice_ipa_key,
    codice_ipa,
    ente,
    codice_ipa_ipa,
    denominazione_ipa,
    codice_categoria_ipa,
    nome_categoria_ipa
  ) %>%
  summarise(
    n_candidature = n(),
    n_misure = n_distinct(avviso, na.rm = TRUE),
    importo_finanziamento =
      sum(importo_finanziamento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(importo_finanziamento),
    ente
  )

log_altrienti_codice_ipa_non_in_ipa <- candidature_pad26_full %>%
  filter(
    dataset_id ==
      "candidature_altrienti_finanziate",
    
    !is.na(codice_ipa_key),
    
    is.na(codice_ipa_ipa)
  ) %>%
  distinct(
    codice_ipa_key,
    codice_ipa,
    ente,
    tipologia_ente,
    regione,
    provincia,
    comune
  ) %>%
  arrange(ente)


# Controlli.
stopifnot(
  nrow(candidature_pad26_full) ==
    nrow(candidature_pad26)
)

stopifnot(
  !any(
    candidature_pad26_mpa$dataset_id ==
      "candidature_scuole_finanziate",
    na.rm = TRUE
  )
)

stopifnot(
  !any(
    candidature_pad26_mpa$
      ordine_professionale_ipa,
    na.rm = TRUE
  )
)

stopifnot(
  all(
    candidature_pad26_mpa$
      fuori_perimetro_mpa %in% FALSE
  )
)


message(
  "Candidature complete: ",
  nrow(candidature_pad26_full)
)

message(
  "Candidature analitiche MPA: ",
  nrow(candidature_pad26_mpa)
)

message(
  "Candidature scolastiche escluse: ",
  sum(
    candidature_pad26_full$
      esclusione_scuola,
    na.rm = TRUE
  )
)

message(
  "Candidature di ordini professionali escluse: ",
  sum(
    candidature_pad26_full$
      esclusione_ordine_professionale,
    na.rm = TRUE
  )
)

message(
  "Ordini professionali distinti esclusi: ",
  n_distinct(
    candidature_pad26_full$
      codice_ipa_key[
        candidature_pad26_full$
          esclusione_ordine_professionale
      ],
    na.rm = TRUE
  )
)

#==============================================================================#
#### 12.3) COPIE PULITE DESTINATE ESCLUSIVAMENTE A EXCEL                   ----
#==============================================================================#

# L'RDS conserva l'oggetto originale.
# La pulizia viene applicata solo alle copie XLSX.

candidature_pad26_full_xlsx <- candidature_pad26_full %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      clean_xml_string
    )
  )

candidature_pad26_mpa_xlsx <- candidature_pad26_mpa %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      clean_xml_string
    )
  )


#==============================================================================#
#### 13) SALVATAGGIO PROCESSED                                               ----
#==============================================================================#

# Assicura che la cartella locale esista.
dir.create(
  DIR_PAD26_PROCESSED_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)


#------------------------------------------------------------------------------#
# 13.1 Avvisi
#------------------------------------------------------------------------------#

local_avvisi <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "avvisi_padigitale2026.csv"
)


#------------------------------------------------------------------------------#
# 13.2 Candidature complete, incluse le scuole
#------------------------------------------------------------------------------#

local_candidature_full_rds <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026_full.rds"
)

local_candidature_full_xlsx <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026_full.xlsx"
)


#------------------------------------------------------------------------------#
# 13.3 Candidature nel perimetro MPA, senza scuole
#------------------------------------------------------------------------------#

# Mantengo anche il vecchio nome del file RDS per rendere più semplice
# l'uso negli script successivi. Questo diventa il dataset analitico.

local_candidature_mpa_rds <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026.rds"
)

local_candidature_mpa_xlsx <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026.xlsx"
)


#------------------------------------------------------------------------------#
# 13.4 Log del perimetro
#------------------------------------------------------------------------------#

local_log_perimetro_xlsx <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "log_perimetro_padigitale2026.xlsx"
)


#------------------------------------------------------------------------------#
# 13.5 Scrittura locale
#------------------------------------------------------------------------------#

# Avvisi
readr::write_csv(
  avvisi,
  local_avvisi,
  na = ""
)


# Dataset completo: formato canonico R
saveRDS(
  candidature_pad26_full,
  local_candidature_full_rds
)


# Dataset completo: copia leggibile in Excel
openxlsx::write.xlsx(
  candidature_pad26_full_xlsx,
  file = local_candidature_full_xlsx,
  overwrite = TRUE,
  asTable = FALSE
)


# Dataset analitico MPA: formato canonico R
saveRDS(
  candidature_pad26_mpa,
  local_candidature_mpa_rds
)


# Dataset analitico MPA: copia leggibile in Excel
openxlsx::write.xlsx(
  candidature_pad26_mpa_xlsx,
  file = local_candidature_mpa_xlsx,
  overwrite = TRUE,
  asTable = FALSE
)


# Log del perimetro e dei caratteri XML rimossi
openxlsx::write.xlsx(
  x = list(
    "Perimetro PAD26" =
      log_perimetro_pad26,
    
    "Copertura IPA" =
      check_arricchimento_ipa,
    
    "Copertura IPA dataset" =
      check_arricchimento_ipa_per_dataset,
    
    "Ordini esclusi" =
      log_ordini_professionali_esclusi,
    
    "Possibili ordini residui" =
      log_possibili_ordini_non_classificati,
    
    "Codici IPA duplicati" =
      duplicati_ipa_enti,
    
    "Caratteri XML non validi" =
      invalid_xml_summary
  ),
  file = local_log_perimetro_xlsx,
  overwrite = TRUE
)

#------------------------------------------------------------------------------#
# 13.6 Upload o aggiornamento su Drive
#------------------------------------------------------------------------------#

drive_upload_or_update(local_avvisi, DRIVE_PAD26_PROCESSED)

drive_upload_or_update(local_candidature_full_rds, DRIVE_PAD26_PROCESSED)

drive_upload_or_update(local_candidature_full_xlsx, DRIVE_PAD26_PROCESSED)

drive_upload_or_update(local_candidature_mpa_rds, DRIVE_PAD26_PROCESSED)

drive_upload_or_update(local_candidature_mpa_xlsx, DRIVE_PAD26_PROCESSED)

drive_upload_or_update(local_log_perimetro_xlsx, DRIVE_PAD26_PROCESSED)


message(
  "Salvataggio completato. Dataset analitico MPA: ",
  local_candidature_mpa_rds
)

message(
  "Archivio completo con scuole: ",
  local_candidature_full_rds
)


# 13.7 Mappatura nomi originali e standardizzati ------------------------------

# La mappatura viene ricostruita direttamente dalle intestazioni dei file raw.
# In questo modo non dobbiamo scrivere manualmente i nomi delle variabili.

leggi_header_csv <- function(file_path, dataset_id, fonte) {
  
  header <- readr::read_csv(
    file_path,
    n_max = 0,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  
  nomi_originali <- names(header)
  
  tibble::tibble(
    fonte = fonte,
    dataset_id_originale = dataset_id,
    nome_variabile_originale = nomi_originali,
    nome_variabile_standardizzato =
      janitor::make_clean_names(nomi_originali)
  )
}


leggi_header_excel <- function(file_path, dataset_id, fonte) {
  
  header <- readxl::read_excel(
    file_path,
    n_max = 0,
    col_types = "text"
  )
  
  nomi_originali <- names(header)
  
  tibble::tibble(
    fonte = fonte,
    dataset_id_originale = dataset_id,
    nome_variabile_originale = nomi_originali,
    nome_variabile_standardizzato =
      janitor::make_clean_names(nomi_originali)
  )
}


metadata_nomi_input <- dplyr::bind_rows(
  
  purrr::map2_dfr(
    file_pad26$dataset_id,
    file_pad26$filename,
    function(dataset_id, filename) {
      
      leggi_header_csv(
        file_path = file.path(
          DIR_PAD26_SOURCE_LOCAL,
          filename
        ),
        dataset_id = dataset_id,
        fonte = "PA digitale 2026"
      )
    }
  ),
  
  leggi_header_excel(
    file_path = local_ipa_categorie_raw,
    dataset_id = "ipa_categorie_raw",
    fonte = "IndicePA - categorie enti"
  ),
  
  leggi_header_excel(
    file_path = local_ipa_enti_raw,
    dataset_id = "ipa_enti_raw",
    fonte = "IndicePA - enti"
  )
)


# 13.8 Acquisizione documentazione ufficiale delle variabili -----------------

# Legge il metadata JSON pubblicato da IndicePA.
leggi_metadata_ipa <- function(
    url,
    dataset_id_originale,
    fonte
) {
  
  metadata <- jsonlite::fromJSON(url)
  
  tibble::as_tibble(metadata) %>%
    dplyr::transmute(
      fonte = fonte,
      dataset_id_originale = dataset_id_originale,
      
      nome_variabile_originale =
        as.character(COLUMN_NAME),
      
      nome_variabile_standardizzato =
        janitor::make_clean_names(COLUMN_NAME),
      
      descrizione_ufficiale =
        as.character(COLUMN_COMMENT),
      
      tipo_dato_dichiarato_fonte =
        as.character(DATA_TYPE),
      
      lunghezza_massima =
        suppressWarnings(
          as.integer(CHARACTER_MAXIMUM_LENGTH)
        ),
      
      formato_dichiarato_fonte =
        NA_character_,
      
      url_documentazione = url,
      
      fonte_descrizione =
        "Metadata ufficiale IndicePA"
    )
}


# Legge una tabella Markdown con struttura:
# Campo | Tipo di dati | Descrizione | Formato
leggi_dizionario_markdown_pad26 <- function(
    url,
    dataset_id_originale
) {
  
  righe <- readLines(
    url,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  # Conserva soltanto le righe della tabella Markdown.
  righe_tabella <- righe[
    stringr::str_detect(
      stringr::str_trim(righe),
      "^\\|"
    )
  ]
  
  # Elimina intestazione e separatore.
  righe_tabella <- righe_tabella[
    !stringr::str_detect(
      stringr::str_to_lower(righe_tabella),
      "^\\|\\s*campo\\s*\\|"
    ) &
      !stringr::str_detect(
        righe_tabella,
        "^\\|\\s*[-:]+"
      )
  ]
  
  if (length(righe_tabella) == 0L) {
    warning(
      "Nessuna tabella di variabili trovata in: ",
      url
    )
    
    return(tibble::tibble())
  }
  
  purrr::map_dfr(
    righe_tabella,
    function(riga) {
      
      campi <- stringr::str_split(
        riga,
        "\\|",
        simplify = FALSE
      )[[1]]
      
      # Il primo e l'ultimo elemento sono normalmente vuoti
      # perché la riga comincia e termina con "|".
      campi <- stringr::str_trim(campi)
      campi <- campi[campi != ""]
      
      # Alcune righe possono essere incomplete.
      campi <- c(
        campi,
        rep(
          NA_character_,
          max(0, 4 - length(campi))
        )
      )
      
      tibble::tibble(
        campo = campi[1],
        tipo_dato = campi[2],
        descrizione = campi[3],
        formato = campi[4]
      )
    }
  ) %>%
    dplyr::filter(
      !is.na(campo),
      campo != ""
    ) %>%
    dplyr::transmute(
      fonte = "PA digitale 2026",
      dataset_id_originale = dataset_id_originale,
      
      nome_variabile_originale = campo,
      
      nome_variabile_standardizzato =
        janitor::make_clean_names(campo),
      
      descrizione_ufficiale = descrizione,
      
      tipo_dato_dichiarato_fonte = tipo_dato,
      
      lunghezza_massima =
        stringr::str_extract(
          formato,
          "\\d+"
        ) %>%
        suppressWarnings() %>%
        as.integer(),
      
      formato_dichiarato_fonte = formato,
      
      url_documentazione = url,
      
      fonte_descrizione =
        "Data dictionary ufficiale PA digitale 2026"
    )
}

# Dizionario ufficiale IndicePA -----------------------------------------------

dizionario_ipa_enti <- leggi_metadata_ipa(
  url = url_ipa_metadata_enti,
  dataset_id_originale = "ipa_enti_raw",
  fonte = "IndicePA - AgID"
)

dizionario_ipa_categorie <- leggi_metadata_ipa(
  url = url_ipa_metadata_categorie,
  dataset_id_originale = "ipa_categorie_raw",
  fonte = "IndicePA - AgID"
)


# Dizionario ufficiale PA digitale 2026 --------------------------------------

dizionario_pad26_avvisi <- leggi_dizionario_markdown_pad26(
  url = url_format_pad26_avvisi,
  dataset_id_originale = "avvisi"
)

dizionario_pad26_candidature_base <-
  leggi_dizionario_markdown_pad26(
    url = url_format_pad26_candidature,
    dataset_id_originale =
      "candidature_comuni_finanziate"
  )


# Lo stesso tracciato è usato per Comuni, Scuole e altri enti.
dizionario_pad26_candidature <-
  dplyr::bind_rows(
    dizionario_pad26_candidature_base,
    
    dizionario_pad26_candidature_base %>%
      dplyr::mutate(
        dataset_id_originale =
          "candidature_scuole_finanziate"
      ),
    
    dizionario_pad26_candidature_base %>%
      dplyr::mutate(
        dataset_id_originale =
          "candidature_altrienti_finanziate"
      )
  )


# Dizionario ufficiale complessivo -------------------------------------------

dizionario_ufficiale_variabili <- dplyr::bind_rows(
  dizionario_pad26_avvisi,
  dizionario_pad26_candidature,
  dizionario_ipa_enti,
  dizionario_ipa_categorie
) %>%
  dplyr::distinct(
    dataset_id_originale,
    nome_variabile_standardizzato,
    .keep_all = TRUE
  )


print(dizionario_ufficiale_variabili)


dizionario_variabili_derivate <- tibble::tribble(
  ~nome_variabile_standardizzato,
  ~descrizione_manuale,
  ~unita_di_misura_manuale,
  ~note_manuali,
  
  "dataset_id",
  "Identificativo tecnico del dataset di provenienza del record.",
  NA_character_,
  "Variabile aggiunta durante l'importazione.",
  
  "file_origine",
  "Nome del file sorgente da cui è stato importato il record.",
  NA_character_,
  "Variabile aggiunta durante l'importazione.",
  
  "tipo_file_candidatura",
  "Tipologia del file di candidature da cui deriva il record.",
  NA_character_,
  "Variabile aggiunta durante l'unione dei file di candidatura.",
  
  "codice_ipa_key",
  "Versione normalizzata del codice IPA utilizzata per i raccordi.",
  NA_character_,
  "Variabile derivata.",
  
  "codice_fiscale_ipa_key",
  "Versione normalizzata del codice fiscale IndicePA utilizzata per il raccordo.",
  NA_character_,
  "Variabile derivata.",
  
  "ordine_professionale_ipa",
  paste(
    "Indicatore che identifica ordini, collegi e consigli",
    "professionali secondo la categoria ufficiale IndicePA."
  ),
  "booleano",
  "Derivato dalla categoria IPA C14.",
  
  "esclusione_scuola",
  "Indicatore delle candidature appartenenti al dataset delle scuole.",
  "booleano",
  "Utilizzato per delimitare il perimetro analitico MPA.",
  
  "esclusione_ordine_professionale",
  "Indicatore delle candidature relative a ordini professionali.",
  "booleano",
  "Utilizzato per delimitare il perimetro analitico MPA.",
  
  "fuori_perimetro_mpa",
  "Indicatore delle candidature escluse dal perimetro analitico MPA.",
  "booleano",
  "Comprende scuole e ordini professionali.",
  
  "motivo_esclusione_mpa",
  "Motivazione dell'esclusione dal perimetro analitico MPA.",
  NA_character_,
  "Variabile derivata."
)



# 14) Metadati tecnici della run ----------------------------------------------

# Dataset da documentare:
# - raw: singoli file scaricati da GitHub
# - processed: dataset prodotti dallo script
dataset_metadata_list <- c(
  pad26_list,
  list(
    ipa_categorie_raw = ipa_categorie,
    ipa_enti_raw = ipa_enti,
    ipa_ordini_professionali_processed =
      ipa_ordini_professionali,
    
    avvisi_processed =
      avvisi,
    
    candidature_complete_processed =
      candidature_pad26_full,
    
    candidature_mpa_processed =
      candidature_pad26_mpa
  )
)

# Associa ogni dataset alla fonte prevalente.

fonte_dataset <- function(dataset_id) {
  
  dplyr::case_when(
    stringr::str_detect(dataset_id, "^ipa_") ~
      "IndicePA - AgID",
    
    TRUE ~
      "PA digitale 2026"
  )
}


# Stabilisce quali dataset raw possono contenere le variabili di ciascun
# dataset processed.

dataset_raw_collegati <- function(dataset_id) {
  
  if (dataset_id == "avvisi_processed") {
    return("avvisi")
  }
  
  if (
    dataset_id %in% c(
      "candidature_complete_processed",
      "candidature_mpa_processed"
    )
  ) {
    return(c(
      "candidature_comuni_finanziate",
      "candidature_scuole_finanziate",
      "candidature_altrienti_finanziate",
      "ipa_enti_raw",
      "ipa_categorie_raw"
    ))
  }
  
  if (dataset_id == "ipa_ordini_professionali_processed") {
    return(c(
      "ipa_enti_raw",
      "ipa_categorie_raw"
    ))
  }
  
  dataset_id
}


# Metadati a livello di dataset.

metadata_file_pad26 <- purrr::imap_dfr(
  dataset_metadata_list,
  function(df, dataset_id) {
    
    livello_dataset <- ifelse(
      stringr::str_detect(dataset_id, "processed"),
      "processed",
      "raw"
    )
    
    tibble::tibble(
      run_id = RUN_ID,
      fonte = fonte_dataset(dataset_id),
      dataset_id = dataset_id,
      livello = livello_dataset,
      
      file_origine = if ("file_origine" %in% names(df)) {
        paste(
          unique(stats::na.omit(df$file_origine)),
          collapse = " | "
        )
      } else {
        NA_character_
      },
      
      n_righe = nrow(df),
      n_colonne = ncol(df),
      variabili = paste(names(df), collapse = " | ")
    )
  }
)


# Metadati tecnici e descrittivi delle variabili.

metadata_variabili_pad26 <- purrr::imap_dfr(
  dataset_metadata_list,
  function(df, dataset_id) {
    
    livello_dataset <- ifelse(
      stringr::str_detect(dataset_id, "processed"),
      "processed",
      "raw"
    )
    
    raw_collegati <- dataset_raw_collegati(dataset_id)
    
    documentazione_dataset <-
      dizionario_ufficiale_variabili %>%
      dplyr::filter(
        dataset_id_originale %in%
          raw_collegati
      )
    
    mappa_dataset <- metadata_nomi_input %>%
      dplyr::filter(
        dataset_id_originale %in% raw_collegati
      )
    
    purrr::map_dfr(
      names(df),
      function(var) {
        
        x <- df[[var]]
        x_chr <- as.character(x)
        
        valori_non_missing <- x_chr[
          !is.na(x_chr) &
            stringr::str_squish(x_chr) != ""
        ]
        
        mapping_var <- mappa_dataset %>%
          dplyr::filter(
            nome_variabile_standardizzato == var
          )
        
        documentazione_var <-
          documentazione_dataset %>%
          dplyr::filter(
            nome_variabile_standardizzato == var
          )
        
        
        descrizione_ufficiale <- if (
          nrow(documentazione_var) > 0L
        ) {
          paste(
            unique(
              stats::na.omit(
                documentazione_var$
                  descrizione_ufficiale
              )
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        
        tipo_dato_dichiarato_fonte <- if (
          nrow(documentazione_var) > 0L
        ) {
          paste(
            unique(
              stats::na.omit(
                documentazione_var$
                  tipo_dato_dichiarato_fonte
              )
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        
        formato_dichiarato_fonte <- if (
          nrow(documentazione_var) > 0L
        ) {
          paste(
            unique(
              stats::na.omit(
                documentazione_var$
                  formato_dichiarato_fonte
              )
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        
        lunghezza_massima <- if (
          nrow(documentazione_var) > 0L
        ) {
          valori_lunghezza <- unique(
            stats::na.omit(
              documentazione_var$lunghezza_massima
            )
          )
          
          if (length(valori_lunghezza) == 0L) {
            NA_integer_
          } else {
            max(valori_lunghezza)
          }
        } else {
          NA_integer_
        }
        
        
        url_documentazione <- if (
          nrow(documentazione_var) > 0L
        ) {
          paste(
            unique(
              stats::na.omit(
                documentazione_var$
                  url_documentazione
              )
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        
        fonte_descrizione <- if (
          nrow(documentazione_var) > 0L
        ) {
          paste(
            unique(
              stats::na.omit(
                documentazione_var$
                  fonte_descrizione
              )
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        nome_originale <- if (nrow(mapping_var) > 0) {
          paste(
            unique(mapping_var$nome_variabile_originale),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        dataset_originale <- if (nrow(mapping_var) > 0) {
          paste(
            unique(mapping_var$dataset_id_originale),
            collapse = " | "
          )
        } else {
          NA_character_
        }
        
        fonte_originale <- if (nrow(mapping_var) > 0) {
          paste(
            unique(mapping_var$fonte),
            collapse = " | "
          )
        } else {
          fonte_dataset(dataset_id)
        }
        
        descrizione_provvisoria <- if (!is.na(nome_originale)) {
          stringr::str_to_sentence(
            stringr::str_replace_all(
              nome_originale,
              "_",
              " "
            )
          )
        } else {
          stringr::str_to_sentence(
            stringr::str_replace_all(
              var,
              "_",
              " "
            )
          )
        }
        
        tipo_originale <- if (
          livello_dataset == "raw"
        ) {
          class(x)[1]
        } else {
          
          raw_objects <- dataset_metadata_list[
            intersect(
              raw_collegati,
              names(dataset_metadata_list)
            )
          ]
          
          tipi_raw <- purrr::map_chr(
            raw_objects,
            function(raw_df) {
              
              if (var %in% names(raw_df)) {
                class(raw_df[[var]])[1]
              } else {
                NA_character_
              }
            }
          )
          
          tipi_raw <- unique(
            stats::na.omit(tipi_raw)
          )
          
          if (length(tipi_raw) == 0) {
            NA_character_
          } else {
            paste(tipi_raw, collapse = " | ")
          }
        }
        
        tibble::tibble(
          run_id = RUN_ID,
          fonte = fonte_originale,
          dataset_id = dataset_id,
          dataset_id_originale = dataset_originale,
          livello = livello_dataset,
          
          file_origine = if (
            "file_origine" %in% names(df)
          ) {
            valori_file_origine <- unique(
              stats::na.omit(
                as.character(df$file_origine)
              )
            )
            
            if (length(valori_file_origine) == 0L) {
              NA_character_
            } else {
              paste(
                valori_file_origine,
                collapse = " | "
              )
            }
          } else {
            NA_character_
          },
          
          nome_variabile_originale =
            nome_originale,
          
          nome_variabile_standardizzato =
            var,
          
          descrizione = dplyr::coalesce(
            descrizione_ufficiale,
            descrizione_provvisoria
          ),
          
          tipo_dato_dichiarato_fonte =
            tipo_dato_dichiarato_fonte,
          
          tipo_dato_originale =
            tipo_originale,
          
          tipo_dato_dopo_import =
            class(x)[1],
          
          formato_dichiarato_fonte =
            formato_dichiarato_fonte,
          
          lunghezza_massima =
            lunghezza_massima,
          
          unita_di_misura =
            NA_character_,
          
          url_documentazione =
            url_documentazione,
          
          fonte_descrizione =
            fonte_descrizione,
          
          n_righe = length(x),
          
          n_missing = sum(
            is.na(x_chr) |
              stringr::str_squish(x_chr) == ""
          ),
          
          pct_missing = round(
            100 * n_missing / n_righe,
            2
          ),
          
          n_valori_distinti =
            dplyr::n_distinct(
              x_chr[
                !is.na(x_chr) &
                  stringr::str_squish(x_chr) != ""
              ]
            ),
          
          esempi_valori = paste(
            head(
              unique(valori_non_missing),
              5
            ),
            collapse = " | "
          ),
          
          note = dplyr::case_when(
            livello_dataset == "processed" &
              is.na(nome_originale) ~
              "Variabile derivata durante il processamento.",
            
            is.na(descrizione_ufficiale) ~
              paste(
                "Descrizione generata dal nome della variabile;",
                "documentazione ufficiale non trovata."
              ),
            
            TRUE ~
              NA_character_
          )
        )
      }
    )
  }
) 

metadata_variabili_pad26 <-
  metadata_variabili_pad26 %>%
  dplyr::left_join(
    dizionario_variabili_derivate,
    by = "nome_variabile_standardizzato"
  ) %>%
  dplyr::mutate(
    descrizione = dplyr::coalesce(
      descrizione_manuale,
      descrizione
    ),
    
    unita_di_misura = dplyr::coalesce(
      unita_di_misura_manuale,
      unita_di_misura
    ),
    
    note = dplyr::case_when(
      !is.na(note_manuali) &
        !is.na(note) ~
        paste(
          note_manuali,
          note,
          sep = " "
        ),
      
      !is.na(note_manuali) ~
        note_manuali,
      
      TRUE ~
        note
    )
  ) %>%
  dplyr::select(
    run_id,
    fonte,
    dataset_id,
    dataset_id_originale,
    livello,
    file_origine,
    nome_variabile_originale,
    nome_variabile_standardizzato,
    descrizione,
    tipo_dato_dichiarato_fonte,
    tipo_dato_originale,
    tipo_dato_dopo_import,
    formato_dichiarato_fonte,
    lunghezza_massima,
    unita_di_misura,
    n_righe,
    n_missing,
    pct_missing,
    n_valori_distinti,
    esempi_valori,
    fonte_descrizione,
    url_documentazione,
    note
  )


metadata_variabili_pad26 <-
  metadata_variabili_pad26 %>%
  dplyr::left_join(
    dizionario_variabili_derivate,
    by = "nome_variabile_standardizzato"
  ) %>%
  dplyr::mutate(
    descrizione = dplyr::coalesce(
      descrizione_manuale,
      descrizione
    ),
    
    unita_di_misura = dplyr::coalesce(
      unita_di_misura_manuale,
      unita_di_misura
    ),
    
    note = dplyr::case_when(
      !is.na(note_manuali) &
        !is.na(note) ~
        paste(note_manuali, note),
      
      !is.na(note_manuali) ~
        note_manuali,
      
      TRUE ~
        note
    )
  ) %>%
  dplyr::select(
    run_id,
    fonte,
    dataset_id,
    dataset_id_originale,
    livello,
    file_origine,
    nome_variabile_originale,
    nome_variabile_standardizzato,
    descrizione,
    tipo_dato_dichiarato_fonte,
    tipo_dato_originale,
    tipo_dato_dopo_import,
    formato_dichiarato_fonte,
    lunghezza_massima,
    unita_di_misura,
    n_righe,
    n_missing,
    pct_missing,
    n_valori_distinti,
    esempi_valori,
    fonte_descrizione,
    url_documentazione,
    note
  )

metadata_fonti_pad26 <- tibble::tribble(
  ~fonte,
  ~ente_titolare,
  ~url_repository,
  ~tipo_fonte,
  ~periodicita,
  ~licenza_condizioni_riuso,
  ~copertura_temporale,
  ~copertura_territoriale,
  ~unita_osservazione,
  ~data_accesso,
  ~note_stabilita,
  
  "PA digitale 2026",
  "Dipartimento per la trasformazione digitale",
  base_raw_url,
  "Open data amministrativi",
  "Aggiornamento periodico",
  NA_character_,
  NA_character_,
  "Italia",
  "Avviso o candidatura finanziata",
  as.Date(Sys.time()),
  "I file sono acquisiti dal repository pubblico della fonte.",
  
  "IndicePA - AgID",
  "Agenzia per l'Italia Digitale",
  paste(
    url_ipa_enti,
    url_ipa_categorie,
    sep = " | "
  ),
  "Anagrafe amministrativa open data",
  "Aggiornamento periodico",
  NA_character_,
  NA_character_,
  "Italia",
  "Ente registrato nell'Indice delle pubbliche amministrazioni",
  as.Date(Sys.time()),
  "Fonte utilizzata per l'arricchimento anagrafico e la classificazione degli enti."
) %>%
  dplyr::mutate(
    run_id = RUN_ID,
    .before = 1
  )


# 15) Salvataggio metadati ----------------------------------------------------

local_metadata_xlsx <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  paste0(
    "metadata_padigitale2026_",
    RUN_ID,
    ".xlsx"
  )
)


openxlsx::write.xlsx(
  list(
    "01_fonti" =
      metadata_fonti_pad26,
    
    "02_dataset" =
      metadata_file_pad26,
    
    "03_variabili" =
      metadata_variabili_pad26,
    
    "04_nomi_input" =
      metadata_nomi_input,
    
    "05_documentazione_ufficiale" =
      dizionario_ufficiale_variabili,
    
    "06_run" =
      run_info
  ),
  file = local_metadata_xlsx,
  overwrite = TRUE
)


drive_upload_or_update(
  local_path = local_metadata_xlsx,
  drive_folder_rel = DRIVE_PAD26_METADATA
)



# 16) Chiusura ---------------------------------------------------------------

message("Import PA digitale 2026 completato.")
message("- RUN_ID: ", RUN_ID)
message("- Drive raw: ", DRIVE_PAD26_SOURCE)
message("- Drive processed: ", DRIVE_PAD26_PROCESSED)
message("- Drive metadata: ", DRIVE_PAD26_METADATA)

if (delete_local_temp) {
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Source", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID), recursive = TRUE)
}


# Chiude il file e ripristina la console.
console_log_path <- stop_console_log(
  console_log,
  status = "completed"
)

# Carica o aggiorna il log nella cartella 05_Logs su Drive.
drive_upload_or_update(
  local_path = console_log_path,
  drive_folder_rel = DRIVE_PAD26_LOGS
)
