# ............................................................................
# Script: prepare_sim_data.R
#
# Eseguire SOLO IN LOCALE (richiede autenticazione Google Drive interattiva
# via browser), una tantum o ogni volta che cambia la lista di perimetro PA
# o si vuole aggiornare la cache dei confini NUTS2.
#
# Genera i file statici che la dashboard madre (06_dashboard_SIM_integrata.Rmd)
# legge da disco quando e' deployata su shinyapps.io, dove non e' possibile
# autenticarsi su Google Drive a runtime.
#
# Output attesi in SIM/data/:
#   - master_pa.<estensione originale>   (lista di perimetro PA)
#   - nuts_cache.rds                     (confini NUTS2 Italia + raccordo regioni)
#
# Dopo aver eseguito questo script, ricordarsi di aggiornare il parametro
# `file_master_pa` nello YAML dell'Rmd se il nome/estensione del file non
# corrisponde a "data/master_pa.rds" (vedi commento sotto).
# ............................................................................

# Adatta questi source() ai path reali del tuo progetto.
source("Preprocessing/00_config.R")
source("Preprocessing/00_drive_helpers.R")
source("Preprocessing/00_spatial_helpers.R")

suppressPackageStartupMessages({
  library(googledrive)
  library(dplyr)
  library(stringr)
})

googledrive::drive_deauth()
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive.readonly",
  cache = FALSE
)

dir.create("SIM/data", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1) Lista di perimetro PA
#    DRIVE_FILE_LISTA_RACCORDO_SIM e' gia' definita in 00_config.R
#    (e' la stessa usata da DRIVE_MASTER_PA_FILE nello script di lancio).
# ---------------------------------------------------------------------------
estensione_master <- tools::file_ext(DRIVE_FILE_LISTA_RACCORDO_SIM)
nome_locale_master <- paste0("master_pa.", estensione_master)
path_locale_master <- file.path("SIM/data", nome_locale_master)

drive_download_from_path(DRIVE_FILE_LISTA_RACCORDO_SIM, path_locale_master)

message(
  "Lista di perimetro salvata in: ", path_locale_master, "\n",
  "IMPORTANTE: se questo nome non e' 'master_pa.rds', aggiorna di conseguenza ",
  "il parametro 'file_master_pa' nello YAML di 06_dashboard_SIM_integrata.Rmd."
)

# ---------------------------------------------------------------------------
# 2) Confini NUTS2 Italia + raccordo regioni
#    Stesse funzioni gia' usate nella dashboard, chiamate una tantum qui e
#    salvate in cache per evitare il download a runtime su shinyapps.io.
# ---------------------------------------------------------------------------
nuts2_it <- scarica_nuts2_italia(year = 2024, resolution = "10")

raccordo_regioni <- get_raccordo_regioni_nuts() |>
  transmute(
    codice_regione = str_pad(as.character(codice_regione), 2, pad = "0"),
    NUTS_ID
  )

saveRDS(
  list(nuts2_it = nuts2_it, raccordo_regioni = raccordo_regioni),
  "SIM/data/nuts_cache.rds"
)

message("Cache NUTS2 salvata in: SIM/data/nuts_cache.rds")
message("Preparazione dati SIM completata.")
