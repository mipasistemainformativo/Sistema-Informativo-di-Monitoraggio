# ============================================================
# 00_spatial_helpers.R
# Funzioni spaziali riusabili per fonti raccordate alla lista
# ============================================================

normalizza_codice_regione <- function(x) {
  stringr::str_pad(as.character(x), 2, pad = "0")
}

normalizza_codice_provincia <- function(x) {
  stringr::str_pad(as.character(x), 3, pad = "0")
}

prepara_lista_geo <- function(lista) {
  
  lista %>%
    janitor::clean_names() %>%
    mutate(
      codice_regione = normalizza_codice_regione(codice_regione),
      codice_provincia = normalizza_codice_provincia(codice_provincia),
      codice_comune = as.character(codice_comune),
      codice_istat_comune = as.character(codice_istat_comune),
      regione_ente = dizione_regione,
      provincia_ente = dizione_provincia,
      comune_ente = dizione_comune
    ) %>%
    select(
      any_of(c(
        "codice_fiscale",
        "ragione_sociale",
        "denominazione",
        "codice_unita_s13",
        "codice_unita_mpa",
        "codice_regione",
        "regione_ente",
        "codice_provincia",
        "provincia_ente",
        "codice_comune",
        "codice_istat_comune",
        "comune_ente",
        "indirizzo",
        "cap",
        "codice_catastale"
      ))
    ) %>%
    distinct()
}

get_raccordo_regioni_nuts <- function() {
  tibble::tribble(
    ~codice_regione, ~NUTS_ID,
    "01", "ITC1",
    "02", "ITC2",
    "03", "ITC4",
    "04", "ITH1",
    "05", "ITH3",
    "06", "ITH4",
    "07", "ITC3",
    "08", "ITH5",
    "09", "ITI1",
    "10", "ITI2",
    "11", "ITI3",
    "12", "ITI4",
    "13", "ITF1",
    "14", "ITF2",
    "15", "ITF3",
    "16", "ITF4",
    "17", "ITF5",
    "18", "ITF6",
    "19", "ITG1",
    "20", "ITG2"
  )
}

scarica_nuts2_italia <- function(year = 2024, resolution = "10") {
  
  if (!requireNamespace("giscoR", quietly = TRUE)) {
    stop("Pacchetto 'giscoR' non installato. Installa con install.packages('giscoR').")
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Pacchetto 'sf' non installato. Installa con install.packages('sf').")
  }
  
  giscoR::gisco_get_nuts(
    year = year,
    nuts_level = 2,
    country = "IT",
    resolution = resolution,
    epsg = 4326
  )
}

aggiungi_nuts_regionale <- function(dati, col_codice_regione = "codice_regione") {
  
  raccordo_regioni_nuts <- get_raccordo_regioni_nuts()
  
  dati %>%
    mutate(
      codice_regione_join = normalizza_codice_regione(.data[[col_codice_regione]])
    ) %>%
    left_join(
      raccordo_regioni_nuts,
      by = c("codice_regione_join" = "codice_regione")
    ) %>%
    select(-codice_regione_join)
}

prepara_mappa_regionale <- function(dati_regione, year = 2024, resolution = "10") {
  
  nuts2_it <- scarica_nuts2_italia(year = year, resolution = resolution)
  
  dati_regione_nuts <- dati_regione %>%
    aggiungi_nuts_regionale(col_codice_regione = "codice_regione")
  
  nuts2_it %>%
    left_join(dati_regione_nuts, by = "NUTS_ID")
}

crea_leaflet_regionale_flussi <- function(mappa_sf) {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Pacchetto 'leaflet' non installato. Installa con install.packages('leaflet').")
  }
  
  leaflet::leaflet(mappa_sf) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillOpacity = 0.7,
      weight = 1,
      popup = ~paste0(
        "<b>", NUTS_NAME, "</b>",
        "<br>Assunzioni: ", format(n_assunzioni, big.mark = "."),
        "<br>Cessazioni: ", format(n_cessazioni, big.mark = "."),
        "<br>Saldo: ", format(saldo_assunzioni_cessazioni, big.mark = ".")
      )
    )
}