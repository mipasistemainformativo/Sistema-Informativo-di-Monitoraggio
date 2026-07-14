# 1. Configurazione deploy ----

suppressPackageStartupMessages({
  library(rsconnect)
})

# Cartella che contiene questo script
app_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

# Controllo struttura minima
required_files <- c(
  file.path(app_dir, "Conto_annuale_app.Rmd"),
  file.path(app_dir, "data")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "File o cartelle mancanti:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

# 2. Repository pacchetti ----

options(
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
  )
)

# 3. Deploy su shinyapps.io ----

rsconnect::deployApp(
  appDir = app_dir,
  appPrimaryDoc = "Conto_annuale_app.Rmd",
  appName = "Conto_annuale",
  forceUpdate = TRUE,
  launch.browser = TRUE
)