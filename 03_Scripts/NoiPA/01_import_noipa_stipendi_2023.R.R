# ============================================================================ #
# IMPORT NOIPA - STIPENDI 

# La fonte NoiPA è stata importata tramite download dei file CSV/ZIP 
# disponibili sul portale open data. I dataset sono mensili; per il prototipo 
# SIM/SII è stato considerato l’anno 2023. I dati vengono conservati sia in forma 
# raw, per mese e dataset, sia in forma processed, aggregata per dataset e annualità. 
# La fonte è utilizzabile per indicatori relativi alla presenza/copertura e 
# all’utilizzo della piattaforma NoiPA, con cautela rispetto alla rappresentatività 
# dell’intero perimetro della PA.

# ............................................................ #
# Script: 01_import_noipa_stipendi_2023.R
# Fonte: NoiPA - Dati stipendi
# Obiettivo: scaricare/importare i dataset mensili NoiPA per il 2023,
#            unirli per dataset e produrre metadata/variabili/log.
# Input: portale open data NoiPA, dataset ZIP/CSV mensili
# Output:
#   - 01_Dataset/Source/NoiPA/*.csv
#   - 01_Dataset/Processed/NoiPA/*_2023.csv
#   - 02_Metadata/Source_met/mappatura_stipendi_noipa_2023.xlsx
#   - 05_Logs/log_import_noipa_2023.csv
# Note: la fonte copre il perimetro gestito da NoiPA, non l’intera PA.
# ............................................................ #
# ============================================================================ #

rm(list = ls())

# 1) PACCHETTI ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(openxlsx)

# 2) PARAMETRI ---------------------------------------------------------------
source("03_Scripts/00_config.R")
anno_riferimento <- 2023
mesi_riferimento <- 1:12
load_data_from_local <- TRUE

# Cartelle progetto
DIR_NOIPA_RAW <- file.path(DIR_SOURCE, "NoiPA")
DIR_NOIPA_PROCESSED <- file.path(DIR_PROCESSED, "NoiPA")
DIR_NOIPA_OUTPUT <- file.path(DIR_OUTPUT, "NoiPA")
DIR_SOURCE_MET <- file.path(DIR_SOURCE, "NoiPA")

dir.create(DIR_NOIPA_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_NOIPA_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_NOIPA_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_SOURCE_MET, recursive = TRUE, showWarnings = FALSE)

# 3) CATALOGO DATASET --------------------------------------------------------

catalogo_noipa_base <- tibble::tibble(
  dataset_name = c(
    "Amministrati per provincia di residenza",
    "Inquadramenti contrattuali per Amministrazione",
    "Mobilità degli Amministrati",
    "Modalità di accredito degli stipendi",
    "Motivi assunzione",
    "Motivi di cessazione",
    "Redditi di lavoro dipendente e assimilati certificati agli Amministrati",
    "Struttura organizzativa Amministrazioni"
  ),
  dataset_id = c(
    "EntryResidenti",
    "EntryContrattiGestiti",
    "EntryPendolarismo",
    "EntryAccreditoStipendi",
    "EntryMotivoAssunzione",
    "EntryMotivoCessazione",
    "EntryCertificazioniUniche",
    "EntryStrutturaOrganizzativa"
  ),
  periodicita = c(
    "mensile",
    "mensile",
    "mensile",
    "mensile",
    "mensile",
    "mensile",
    "annuale",
    "mensile"
  ),
  classe_uso = "core_raccordabile"
)

# Espande i dataset mensili sui 12 mesi del 2023
catalogo_noipa_mensile <- catalogo_noipa_base %>%
  filter(periodicita == "mensile") %>%
  tidyr::crossing(mese = mesi_riferimento) %>%
  mutate(
    anno = anno_riferimento,
    periodo = paste0(anno, sprintf("%02d", mese))
  )

catalogo_noipa_annuale <- catalogo_noipa_base %>%
  filter(periodicita == "annuale") %>%
  mutate(
    anno = anno_riferimento,
    mese = 9,
    periodo = as.character(anno)
  )

catalogo_noipa <- bind_rows(
  catalogo_noipa_mensile,
  catalogo_noipa_annuale
)

# 4) PARAMETRI PORTALE -------------------------------------------------------

noipa_base_url <- "https://dati-noipa.mef.gov.it/cl/web/open-data/dataset"
noipa_portlet_id <- "it_gov_mef_opendata_portlet_NoipaOpendataPortlet_INSTANCE_k0QJbYynlaqN"

build_noipa_csv_url <- function(dataset_id, anno, mese = NA) {
  
  url <- paste0(
    noipa_base_url,
    "?p_p_id=", noipa_portlet_id,
    "&p_p_lifecycle=2",
    "&p_p_state=normal",
    "&p_p_mode=view",
    "&p_p_cacheability=cacheLevelPage",
    "&_", noipa_portlet_id, "_anno=", anno,
    "&_", noipa_portlet_id, "_formato=CSV"
  )
  
  if (!is.na(mese)) {
    url <- paste0(
      url,
      "&_", noipa_portlet_id, "_mese=", sprintf("%02d", mese)
    )
  }
  
  url <- paste0(
    url,
    "&_", noipa_portlet_id, "_id=", dataset_id,
    "&_", noipa_portlet_id, "_id=", dataset_id,
    "&_", noipa_portlet_id, "_jspPage=%2Fdettaglio%2FdettaglioDataSet.jsp",
    "&p_p_lifecycle=1",
    "&_", noipa_portlet_id, "_javax.portlet.action=getDettaglio"
  )
  
  url
}

# 5) FUNZIONE DOWNLOAD + LETTURA --------------------------------------------

read_noipa_dataset <- function(dataset_id, anno, mese,
                               delim = ",",
                               cache_dir = DIR_NOIPA_RAW,
                               load_data_from_local = TRUE) {
  
  csv_url <- build_noipa_csv_url(dataset_id, anno, mese)
  
  file_stub <- paste0(dataset_id, "_", anno, sprintf("%02d", mese))
  csv_path <- file.path(cache_dir, paste0(file_stub, ".csv"))
  
  if (!(load_data_from_local && file.exists(csv_path))) {
    
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    
    download_ok <- tryCatch({
      utils::download.file(
        url = csv_url,
        destfile = tmp_zip,
        mode = "wb",
        method = "libcurl",
        quiet = TRUE
      )
      TRUE
    }, error = function(e) FALSE)
    
    if (!download_ok) {
      stop("Download fallito: ", dataset_id, " ", anno, "-", sprintf("%02d", mese))
    }
    
    utils::unzip(tmp_zip, exdir = tmp_dir)
    
    csv_files <- list.files(
      tmp_dir,
      pattern = "\\.csv$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    
    if (length(csv_files) == 0) {
      zip_content <- tryCatch(
        paste(utils::unzip(tmp_zip, list = TRUE)$Name, collapse = " | "),
        error = function(e) NA_character_
      )
      
      stop(
        "CSV non trovato nello ZIP: ",
        dataset_id, " ", anno, "-", sprintf("%02d", mese),
        ". Contenuto ZIP: ", zip_content
      )
    }
    
    file.copy(csv_files[1], csv_path, overwrite = TRUE)
  }
  
  df <- readr::read_delim(
    csv_path,
    delim = delim,
    show_col_types = FALSE
  )
  
  df <- tibble::as_tibble(df) %>%
    mutate(
      dataset_id = dataset_id,
      anno = anno,
      mese = mese,
      periodo = paste0(anno, sprintf("%02d", mese)),
      fonte = "NoiPA",
      .before = 1
    )
  
  list(
    data = df,
    csv_file = csv_path,
    csv_url = csv_url
  )
}

# 6) ESTRAZIONE --------------------------------------------------------------

estrazioni_noipa <- purrr::pmap(
  catalogo_noipa,
  function(dataset_name, dataset_id, periodicita, classe_uso, mese, anno, periodo, ...) {
    
    message("Scarico/leggo: ", dataset_id, " - ", periodo)
    
    out <- tryCatch(
      read_noipa_dataset(
        dataset_id = dataset_id,
        anno = anno,
        mese = mese,
        delim = ",",
        cache_dir = DIR_NOIPA_RAW,
        load_data_from_local = load_data_from_local
      ),
      error = function(e) {
        message("ERRORE: ", dataset_id, " - ", periodo, " | ", e$message)
        NULL
      }
    )
    
    if (is.null(out)) {
      return(list(
        metadata = tibble(
          dataset_name = dataset_name,
          dataset_id = dataset_id,
          periodicita = periodicita,
          classe_uso = classe_uso,
          anno = anno,
          mese = mese,
          periodo = periodo,
          n_osservazioni = NA_integer_,
          n_variabili = NA_integer_,
          variabili = NA_character_,
          esito = "errore"
        ),
        variables = NULL,
        data = NULL
      ))
    }
    
    df <- out$data
    
    metadata_row <- tibble(
      dataset_name = dataset_name,
      dataset_id = dataset_id,
      periodicita = periodicita,
      classe_uso = classe_uso,
      anno = anno,
      mese = mese,
      periodo = periodo,
      n_osservazioni = nrow(df),
      n_variabili = ncol(df),
      variabili = paste(names(df), collapse = " | "),
      csv_file = basename(out$csv_file),
      csv_url = out$csv_url,
      esito = "ok"
    )
    
    variables_table <- tibble(
      dataset_name = dataset_name,
      dataset_id = dataset_id,
      periodicita = periodicita,
      classe_uso = classe_uso,
      periodo = periodo,
      variabile = names(df),
      posizione_variabile = seq_along(names(df))
    )
    
    list(
      metadata = metadata_row,
      variables = variables_table,
      data = df
    )
  }
)

# 7) COSTRUZIONE OUTPUT ------------------------------------------------------

mappatura_noipa <- map_dfr(estrazioni_noipa, "metadata")
variabili_noipa <- map_dfr(estrazioni_noipa, "variables")

# Dataset uniti per ciascun dataset_id
dati_noipa_list <- estrazioni_noipa %>%
  map("data") %>%
  compact() %>%
  split(map_chr(., ~ unique(.x$dataset_id)[1]))

# Salva un CSV processed per ciascun dataset
iwalk(dati_noipa_list, function(df_list, nome_dataset) {
  
  df_unito <- bind_rows(df_list)
  
  write_csv(
    df_unito,
    file.path(DIR_NOIPA_PROCESSED, paste0(nome_dataset, "_2023.csv"))
  )
})

# 8) EXPORT METADATA ---------------------------------------------------------

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fontColour = "white",
  fgFill = "#2E75B6",
  halign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata", mappatura_noipa, withFilter = TRUE)
openxlsx::addStyle(
  wb, "metadata", header_style,
  rows = 1,
  cols = 1:ncol(mappatura_noipa),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "metadata", firstRow = TRUE)
openxlsx::setColWidths(wb, "metadata", cols = 1:ncol(mappatura_noipa), widths = "auto")

openxlsx::addWorksheet(wb, "variabili")
openxlsx::writeData(wb, "variabili", variabili_noipa, withFilter = TRUE)
openxlsx::addStyle(
  wb, "variabili", header_style,
  rows = 1,
  cols = 1:ncol(variabili_noipa),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "variabili", firstRow = TRUE)
openxlsx::setColWidths(wb, "variabili", cols = 1:ncol(variabili_noipa), widths = "auto")

openxlsx::saveWorkbook(
  wb,
  file = file.path(DIR_SOURCE_MET, "mappatura_stipendi_noipa_2023.xlsx"),
  overwrite = TRUE
)

# 9) LOG ---------------------------------------------------------------------

write_csv(
  mappatura_noipa,
  file.path(DIR_LOGS, "log_import_noipa_2023.csv")
)

