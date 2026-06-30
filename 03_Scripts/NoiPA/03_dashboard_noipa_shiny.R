# ============================================================ #
# Script: 03_dashboard_noipa_shiny.R
# Fonte: NoiPA - Dati stipendi
#
# Obiettivo:
#   Dashboard Shiny esplorativa sui dati NoiPA raccordati
#   alla master list S13+/MPA/BDAP.
#
# Input attesi:
#   - dashboard_flussi_long_noipa_2023.rds
#   - indicatori_flussi_personale_noipa_2023.rds
#   - noipa_raccordato_lista_2023.rds
#   - log_match_noipa_lista_2023.csv
#   - dashboard_assunzioni_motivo_noipa_2023.csv
#   - dashboard_cessazioni_motivo_noipa_2023.csv
#   - dashboard_accredito_noipa_2023.csv
#   - dashboard_inquadramenti_noipa_2023.csv
#
# NOTE:
# - La dashboard è esplorativa.
# - Il raccordo NoiPA-lista è basato sulla denominazione normalizzata
#   ed eventualmente sul raccordo manuale.
# ============================================================ #

rm(list = ls())

# 1) PACCHETTI ---------------------------------------------------------------

{library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(plotly)
library(DT)
library(htmltools)
library(leaflet)
}

# Pacchetti opzionali per mappa
has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
has_sf <- requireNamespace("sf", quietly = TRUE)

# 2) PARAMETRI ---------------------------------------------------------------

source("03_Scripts/00_config.R")
source(file.path(DIR_SCRIPTS, "00_spatial_helpers.R"))

anno_riferimento <- 2023

DIR_NOIPA_OUTPUT <- file.path(DIR_OUTPUT, "NoiPA")

file_flussi_long <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("dashboard_flussi_long_noipa_", anno_riferimento, ".rds")
)

file_flussi_personale <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("indicatori_flussi_personale_noipa_", anno_riferimento, ".rds")
)

file_noipa_raccordato <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("noipa_raccordato_lista_", anno_riferimento, ".rds")
)

file_log_match <- file.path(
  DIR_LOGS,
  paste0("log_match_noipa_lista_", anno_riferimento, ".csv")
)

file_assunzioni_motivo <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("dashboard_assunzioni_motivo_noipa_", anno_riferimento, ".csv")
)

file_cessazioni_motivo <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("dashboard_cessazioni_motivo_noipa_", anno_riferimento, ".csv")
)

file_accredito <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("dashboard_accredito_noipa_", anno_riferimento, ".csv")
)

file_inquadramenti <- file.path(
  DIR_NOIPA_OUTPUT,
  paste0("dashboard_inquadramenti_noipa_", anno_riferimento, ".csv")
)

nuts2_it <- scarica_nuts2_italia(
  year = 2024,
  resolution = "10"
)
# 3) IMPORT DATI -------------------------------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("File non trovato: ", path)
  }
}

purrr::walk(
  c(
    file_flussi_long,
    file_flussi_personale,
    file_noipa_raccordato,
    file_log_match,
    file_assunzioni_motivo,
    file_cessazioni_motivo,
    file_accredito,
    file_inquadramenti
  ),
  stop_if_missing
)

dashboard_flussi_long <- readRDS(file_flussi_long)
indicatori_flussi_personale <- readRDS(file_flussi_personale)
noipa_raccordato <- readRDS(file_noipa_raccordato)

log_match_noipa <- readr::read_csv(file_log_match, show_col_types = FALSE)
dashboard_assunzioni_motivo <- readr::read_csv(file_assunzioni_motivo, show_col_types = FALSE)
dashboard_cessazioni_motivo <- readr::read_csv(file_cessazioni_motivo, show_col_types = FALSE)
dashboard_accredito <- readr::read_csv(file_accredito, show_col_types = FALSE)
dashboard_inquadramenti <- readr::read_csv(file_inquadramenti, show_col_types = FALSE)

# 4) FUNZIONI DASHBOARD ------------------------------------------------------

safe_choices <- function(x) {
  x <- sort(unique(as.character(x)))
  x <- x[!is.na(x) & x != ""]
  x
}

format_num <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

filter_multi <- function(data, var, values) {
  if (is.null(values) || length(values) == 0) {
    data %>% filter(FALSE)
  } else {
    data %>% filter(.data[[var]] %in% values)
  }
}

plot_empty <- function(msg = "Nessun dato disponibile con i filtri selezionati.") {
  plot_ly() %>%
    layout(
      title = msg,
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# 5) UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
      }
      .title-panel {
        margin-bottom: 10px;
      }
      .small-note {
        color: #555;
        font-size: 12px;
        line-height: 1.35;
      }
      .metric-box {
        border: 1px solid #ddd;
        border-radius: 6px;
        padding: 12px;
        margin-bottom: 10px;
        background-color: #fafafa;
      }
      .metric-number {
        font-size: 24px;
        font-weight: bold;
      }
      .metric-label {
        color: #555;
        font-size: 12px;
      }
    "))
  ),
  
  titlePanel(
    div(
      class = "title-panel",
      paste0("NoiPA - Dashboard esplorativa ", anno_riferimento)
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      tags$p(
        class = "small-note",
        "Dashboard costruita a partire dai dataset NoiPA raccordati alla master list S13+/MPA/BDAP. ",
        "Il raccordo è basato sulla denominazione normalizzata e, se disponibile, su una tabella manuale di raccordo. ",
        "Gli indicatori descrivono il perimetro gestito da NoiPA e non l’intera Pubblica Amministrazione."
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Filtri"),
      
      checkboxGroupInput(
        inputId = "macro_gruppo",
        label = "Macro-gruppo PA",
        choices = safe_choices(dashboard_flussi_long$macro_gruppo_pa),
        selected = safe_choices(dashboard_flussi_long$macro_gruppo_pa)
      ),
      
      checkboxGroupInput(
        inputId = "tipo_flusso",
        label = "Tipo flusso",
        choices = safe_choices(dashboard_flussi_long$tipo_flusso),
        selected = safe_choices(dashboard_flussi_long$tipo_flusso)
      ),
      
      checkboxGroupInput(
        inputId = "classe_eta",
        label = "Classe di età",
        choices = safe_choices(dashboard_flussi_long$classe_eta),
        selected = safe_choices(dashboard_flussi_long$classe_eta)
      ),
      
      checkboxGroupInput(
        inputId = "genere",
        label = "Genere",
        choices = safe_choices(dashboard_flussi_long$genere),
        selected = safe_choices(dashboard_flussi_long$genere)
      ),
      
      selectInput(
        inputId = "periodo",
        label = "Periodo",
        choices = safe_choices(dashboard_flussi_long$periodo),
        selected = safe_choices(dashboard_flussi_long$periodo),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      selectInput(
        inputId = "regione_ente",
        label = "Regione ente",
        choices = safe_choices(dashboard_flussi_long$regione_ente),
        selected = safe_choices(dashboard_flussi_long$regione_ente),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      selectInput(
        inputId = "indicatore_mappa",
        label = "Indicatore mappa",
        choices = c(
          "Assunzioni" = "n_assunzioni",
          "Cessazioni" = "n_cessazioni",
          "Saldo" = "saldo_assunzioni_cessazioni"
        ),
        selected = "n_assunzioni"
      ),
      
      selectInput(
        inputId = "amministrazione",
        label = "Amministrazione",
        choices = safe_choices(dashboard_flussi_long$amministrazione_key),
        selected = character(0),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      tags$p(
        class = "small-note",
        "Nota: se non selezioni amministrazioni specifiche, la dashboard mostra tutte le amministrazioni incluse nei filtri."
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        tabPanel(
          "Sintesi flussi",
          br(),
          fluidRow(
            column(width = 4, uiOutput("box_assunzioni")),
            column(width = 4, uiOutput("box_cessazioni")),
            column(width = 4, uiOutput("box_saldo"))
          ),
          plotlyOutput("plot_flussi", height = "520px"),
          br(),
          DTOutput("tabella_flussi")
        ),
        
        tabPanel(
          "Mappa",
          br(),
          h4("Distribuzione regionale dei flussi"),
          leaflet::leafletOutput("mappa_flussi_regione", height = "650px"),
          br(),
          DTOutput("tabella_mappa_regione")
        ),
        
        tabPanel(
          "Motivi",
          br(),
          h4("Motivi di assunzione"),
          plotlyOutput("plot_motivi_assunzione", height = "420px"),
          br(),
          h4("Motivi di cessazione"),
          plotlyOutput("plot_motivi_cessazione", height = "420px")
        ),
        
        tabPanel(
          "Inquadramenti",
          br(),
          plotlyOutput("plot_inquadramenti", height = "520px"),
          br(),
          DTOutput("tabella_inquadramenti")
        ),
        
        tabPanel(
          "Accrediti",
          br(),
          plotlyOutput("plot_accredito", height = "480px"),
          br(),
          DTOutput("tabella_accredito")
        ),
        
        tabPanel(
          "Qualità raccordo",
          br(),
          plotlyOutput("plot_match", height = "520px"),
          br(),
          DTOutput("tabella_match")
        ),
        
        tabPanel(
          "Dati",
          br(),
          DTOutput("tabella_dati")
        )
      )
    )
  )
)

# 6) SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ------------------------------------------------------------------------- #
  # Dataset filtrato: flussi long
  # ------------------------------------------------------------------------- #
  
  flussi_filtrati <- reactive({
    
    data <- dashboard_flussi_long
    
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo)
    data <- filter_multi(data, "tipo_flusso", input$tipo_flusso)
    data <- filter_multi(data, "classe_eta", input$classe_eta)
    data <- filter_multi(data, "genere", input$genere)
    data <- filter_multi(data, "periodo", input$periodo)
    data <- filter_multi(data, "regione_ente", input$regione_ente)
    
    if (!is.null(input$amministrazione) && length(input$amministrazione) > 0) {
      data <- data %>% filter(amministrazione_key %in% input$amministrazione)
    }
    
    data
  })
  
  flussi_filtrati_wide <- reactive({
    
    flussi_filtrati() %>%
      group_by(
        anno,
        mese,
        periodo,
        amministrazione_key,
        macro_gruppo_pa,
        regione_ente,
        provincia_ente,
        codice_regione,
        codice_provincia,
        match_lista,
        tipo_match,
        classe_eta,
        genere,
        tipo_flusso
      ) %>%
      summarise(
        numero = sum(numero, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = tipo_flusso,
        values_from = numero,
        values_fill = 0
      ) %>%
      rename(
        n_assunzioni = any_of("Assunzioni"),
        n_cessazioni = any_of("Cessazioni")
      ) %>%
      mutate(
        n_assunzioni = if ("n_assunzioni" %in% names(.)) n_assunzioni else 0,
        n_cessazioni = if ("n_cessazioni" %in% names(.)) n_cessazioni else 0,
        saldo = n_assunzioni - n_cessazioni
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Mappa regionale
  # ------------------------------------------------------------------------- #
  
  flussi_regione_filtrati <- reactive({
    
    flussi_filtrati_wide() %>%
      filter(
        !is.na(codice_regione),
        codice_regione != "",
        !is.na(regione_ente),
        regione_ente != ""
      ) %>%
      mutate(
        codice_regione = stringr::str_pad(as.character(codice_regione), 2, pad = "0")
      ) %>%
      group_by(codice_regione, regione_ente) %>%
      summarise(
        n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
        n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
        saldo_assunzioni_cessazioni = sum(saldo, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  mappa_regione_filtrata <- reactive({
    
    raccordo_regioni_nuts <- get_raccordo_regioni_nuts()
    
    dati_regione_nuts <- flussi_regione_filtrati() %>%
      left_join(
        raccordo_regioni_nuts,
        by = "codice_regione"
      )
    
    nuts2_it %>%
      left_join(
        dati_regione_nuts,
        by = "NUTS_ID"
      )
  })
  
  output$mappa_flussi_regione <- leaflet::renderLeaflet({
    
    dati_mappa <- mappa_regione_filtrata()
    
    # Se il selectInput non esiste o è vuoto, usa assunzioni come default
    indicatore <- input$indicatore_mappa
    if (is.null(indicatore) || length(indicatore) == 0 || !(indicatore %in% names(dati_mappa))) {
      indicatore <- "n_assunzioni"
    }
    
    label_indicatore <- dplyr::case_when(
      indicatore == "n_assunzioni" ~ "Assunzioni",
      indicatore == "n_cessazioni" ~ "Cessazioni",
      indicatore == "saldo_assunzioni_cessazioni" ~ "Saldo",
      TRUE ~ indicatore
    )
    
    dati_mappa <- dati_mappa %>%
      mutate(
        valore_mappa = as.numeric(.data[[indicatore]]),
        valore_mappa_label = if_else(
          is.na(valore_mappa),
          "Nessun dato",
          format_num(valore_mappa)
        ),
        n_assunzioni_label = if_else(
          is.na(n_assunzioni),
          "Nessun dato",
          format_num(n_assunzioni)
        ),
        n_cessazioni_label = if_else(
          is.na(n_cessazioni),
          "Nessun dato",
          format_num(n_cessazioni)
        ),
        saldo_label = if_else(
          is.na(saldo_assunzioni_cessazioni),
          "Nessun dato",
          format_num(saldo_assunzioni_cessazioni)
        )
      )
    
    valori_validi <- dati_mappa$valore_mappa[!is.na(dati_mappa$valore_mappa)]
    
    if (length(valori_validi) == 0) {
      
      leaflet::leaflet(dati_mappa) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          fillColor = "#eeeeee",
          fillOpacity = 0.75,
          weight = 1,
          color = "#444444",
          popup = ~paste0(
            "<b>", NUTS_NAME, "</b>",
            "<br>Nessun dato disponibile con i filtri selezionati"
          ),
          label = ~NUTS_NAME
        )
      
    } else {
      
      pal <- leaflet::colorNumeric(
        palette = "YlOrRd",
        domain = valori_validi,
        na.color = "#eeeeee"
      )
      
      leaflet::leaflet(dati_mappa) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          fillColor = ~pal(valore_mappa),
          fillOpacity = 0.75,
          weight = 1,
          color = "#444444",
          popup = ~paste0(
            "<b>", NUTS_NAME, "</b>",
            "<br>", label_indicatore, ": ", valore_mappa_label,
            "<br>Assunzioni: ", n_assunzioni_label,
            "<br>Cessazioni: ", n_cessazioni_label,
            "<br>Saldo: ", saldo_label
          ),
          label = ~paste0(
            NUTS_NAME,
            " | ", label_indicatore, ": ", valore_mappa_label
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = valori_validi,
          opacity = 0.75,
          title = label_indicatore,
          position = "bottomright"
        )
    }
  })
  
  output$tabella_mappa_regione <- renderDT({
    
    flussi_regione_filtrati() %>%
      arrange(regione_ente) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Metric boxes
  # ------------------------------------------------------------------------- #
  
  output$box_assunzioni <- renderUI({
    
    totale <- flussi_filtrati() %>%
      filter(tipo_flusso == "Assunzioni") %>%
      summarise(totale = sum(numero, na.rm = TRUE)) %>%
      pull(totale)
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_num(totale)),
      div(class = "metric-label", "Assunzioni")
    )
  })
  
  output$box_cessazioni <- renderUI({
    
    totale <- flussi_filtrati() %>%
      filter(tipo_flusso == "Cessazioni") %>%
      summarise(totale = sum(numero, na.rm = TRUE)) %>%
      pull(totale)
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_num(totale)),
      div(class = "metric-label", "Cessazioni")
    )
  })
  
  output$box_saldo <- renderUI({
    
    ass <- flussi_filtrati() %>%
      filter(tipo_flusso == "Assunzioni") %>%
      summarise(totale = sum(numero, na.rm = TRUE)) %>%
      pull(totale)
    
    ces <- flussi_filtrati() %>%
      filter(tipo_flusso == "Cessazioni") %>%
      summarise(totale = sum(numero, na.rm = TRUE)) %>%
      pull(totale)
    
    saldo <- ass - ces
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_num(saldo)),
      div(class = "metric-label", "Saldo assunzioni - cessazioni")
    )
  })
  
  # ------------------------------------------------------------------------- #
  # Plot flussi
  # ------------------------------------------------------------------------- #
  
  output$plot_flussi <- renderPlotly({
    
    data <- flussi_filtrati() %>%
      filter(tipo_flusso %in% input$tipo_flusso) %>%
      group_by(periodo, macro_gruppo_pa, tipo_flusso) %>%
      summarise(
        numero = sum(numero, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        serie = paste(macro_gruppo_pa, tipo_flusso, sep = " - ")
      )
    
    if (nrow(data) == 0) {
      return(plot_empty())
    }
    
    plot_ly(
      data,
      x = ~periodo,
      y = ~numero,
      color = ~serie,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Periodo: ", periodo,
        "<br>Macro-gruppo: ", macro_gruppo_pa,
        "<br>Tipo flusso: ", tipo_flusso,
        "<br>Numero: ", format_num(numero)
      )
    ) %>%
      layout(
        title = paste0("Assunzioni e cessazioni per macro-gruppo PA - ", anno_riferimento),
        xaxis = list(title = "Periodo"),
        yaxis = list(title = "Numero"),
        legend = list(orientation = "h")
      )
  })
  
  output$tabella_flussi <- renderDT({
    
    flussi_filtrati_wide() %>%
      group_by(periodo, macro_gruppo_pa, regione_ente, classe_eta, genere) %>%
      summarise(
        n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
        n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
        saldo = sum(saldo, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(periodo, macro_gruppo_pa) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Motivi assunzione / cessazione
  # ------------------------------------------------------------------------- #
  
  motivi_assunzione_filtrati <- reactive({
    
    data <- dashboard_assunzioni_motivo
    
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo)
    data <- filter_multi(data, "classe_eta", input$classe_eta)
    data <- filter_multi(data, "genere", input$genere)
    data <- filter_multi(data, "periodo", input$periodo)
    
    data
  })
  
  motivi_cessazione_filtrati <- reactive({
    
    data <- dashboard_cessazioni_motivo
    
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo)
    data <- filter_multi(data, "classe_eta", input$classe_eta)
    data <- filter_multi(data, "genere", input$genere)
    data <- filter_multi(data, "periodo", input$periodo)
    
    data
  })
  
  output$plot_motivi_assunzione <- renderPlotly({
    
    data <- motivi_assunzione_filtrati() %>%
      group_by(motivo_assunzione_key) %>%
      summarise(
        n_assunzioni = sum(n_assunzioni, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(motivo_assunzione_key), motivo_assunzione_key != "") %>%
      arrange(desc(n_assunzioni)) %>%
      slice_head(n = 20)
    
    if (nrow(data) == 0) {
      return(plot_empty())
    }
    
    plot_ly(
      data,
      x = ~n_assunzioni,
      y = ~reorder(motivo_assunzione_key, n_assunzioni),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Motivo: ", motivo_assunzione_key,
        "<br>Assunzioni: ", format_num(n_assunzioni)
      )
    ) %>%
      layout(
        title = "Assunzioni per motivo",
        xaxis = list(title = "Numero assunzioni"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  output$plot_motivi_cessazione <- renderPlotly({
    
    data <- motivi_cessazione_filtrati() %>%
      group_by(motivo_cessazione_key) %>%
      summarise(
        n_cessazioni = sum(n_cessazioni, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(motivo_cessazione_key), motivo_cessazione_key != "") %>%
      arrange(desc(n_cessazioni)) %>%
      slice_head(n = 20)
    
    if (nrow(data) == 0) {
      return(plot_empty())
    }
    
    plot_ly(
      data,
      x = ~n_cessazioni,
      y = ~reorder(motivo_cessazione_key, n_cessazioni),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Motivo: ", motivo_cessazione_key,
        "<br>Cessazioni: ", format_num(n_cessazioni)
      )
    ) %>%
      layout(
        title = "Cessazioni per motivo",
        xaxis = list(title = "Numero cessazioni"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Inquadramenti
  # ------------------------------------------------------------------------- #
  
  inquadramenti_filtrati <- reactive({
    
    data <- dashboard_inquadramenti
    
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo)
    data <- filter_multi(data, "classe_eta", input$classe_eta)
    data <- filter_multi(data, "genere", input$genere)
    data <- filter_multi(data, "periodo", input$periodo)
    
    data
  })
  
  output$plot_inquadramenti <- renderPlotly({
    
    data <- inquadramenti_filtrati() %>%
      group_by(comparto_key, inquadramento_key) %>%
      summarise(
        n_inquadramenti = sum(n_inquadramenti, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(inquadramento_key), inquadramento_key != "") %>%
      arrange(desc(n_inquadramenti)) %>%
      slice_head(n = 25) %>%
      mutate(label = paste(comparto_key, inquadramento_key, sep = " - "))
    
    if (nrow(data) == 0) {
      return(plot_empty())
    }
    
    plot_ly(
      data,
      x = ~n_inquadramenti,
      y = ~reorder(label, n_inquadramenti),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Comparto: ", comparto_key,
        "<br>Inquadramento: ", inquadramento_key,
        "<br>Numero: ", format_num(n_inquadramenti)
      )
    ) %>%
      layout(
        title = "Inquadramenti contrattuali",
        xaxis = list(title = "Numero"),
        yaxis = list(title = ""),
        margin = list(l = 260)
      )
  })
  
  output$tabella_inquadramenti <- renderDT({
    
    inquadramenti_filtrati() %>%
      group_by(comparto_key, inquadramento_key) %>%
      summarise(
        n_inquadramenti = sum(n_inquadramenti, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(n_inquadramenti)) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Accrediti
  # ------------------------------------------------------------------------- #
  
  accredito_filtrato <- reactive({
    
    data <- dashboard_accredito
    
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo)
    data <- filter_multi(data, "classe_eta", input$classe_eta)
    data <- filter_multi(data, "genere", input$genere)
    data <- filter_multi(data, "periodo", input$periodo)
    
    data
  })
  
  output$plot_accredito <- renderPlotly({
    
    data <- accredito_filtrato() %>%
      group_by(modalita_pagamento_key) %>%
      summarise(
        n_accrediti = sum(n_accrediti, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(modalita_pagamento_key), modalita_pagamento_key != "") %>%
      arrange(desc(n_accrediti))
    
    if (nrow(data) == 0) {
      return(plot_empty())
    }
    
    plot_ly(
      data,
      x = ~n_accrediti,
      y = ~reorder(modalita_pagamento_key, n_accrediti),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Modalità pagamento: ", modalita_pagamento_key,
        "<br>Numero accrediti: ", format_num(n_accrediti)
      )
    ) %>%
      layout(
        title = "Modalità di accredito degli stipendi",
        xaxis = list(title = "Numero accrediti"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  output$tabella_accredito <- renderDT({
    
    accredito_filtrato() %>%
      group_by(modalita_pagamento_key) %>%
      summarise(
        n_accrediti = sum(n_accrediti, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(n_accrediti)) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Qualità raccordo
  # ------------------------------------------------------------------------- #
  
  output$plot_match <- renderPlotly({
    
    data <- log_match_noipa %>%
      arrange(quota_match_su_righe_con_amministrazione_pct)
    
    plot_ly(
      data,
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
        title = "Copertura del raccordo NoiPA-master list per dataset",
        xaxis = list(title = "% righe con amministrazione agganciate alla master list"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  output$tabella_match <- renderDT({
    
    log_match_noipa %>%
      arrange(desc(quota_match_su_righe_con_amministrazione_pct)) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  # ------------------------------------------------------------------------- #
  # Dati
  # ------------------------------------------------------------------------- #
  
  output$tabella_dati <- renderDT({
    
    flussi_filtrati_wide() %>%
      arrange(periodo, macro_gruppo_pa, amministrazione_key) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
}

# 7) RUN APP -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
