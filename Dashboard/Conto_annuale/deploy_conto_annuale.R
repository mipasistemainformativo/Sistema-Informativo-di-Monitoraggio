# 1. Configurazione deploy ----

suppressPackageStartupMessages({
  library(rsconnect)
})

# Individua automaticamente la cartella in cui si trova questo script.
script_file <- tryCatch(
  sys.frame(1)$ofile,
  error = function(e) NULL
)

if (!is.null(script_file) && nzchar(script_file)) {
  app_dir <- dirname(
    normalizePath(
      script_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
} else {
  # Fallback da usare se lo script viene eseguito con Source in RStudio.
  app_dir <- normalizePath(
    file.path(
      getwd(),
      "Dashboard",
      "Conto_annuale"
    ),
    winslash = "/",
    mustWork = TRUE
  )
}

message("Cartella dell'app: ", app_dir)

# 2. Controllo struttura minima ----

required_files <- c(
  file.path(app_dir, "Conto_annuale_app.Rmd"),
  file.path(app_dir, "data")
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "File o cartelle mancanti:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

# 3. Repository pacchetti ----

options(
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
  )
)

# 4. Deploy su shinyapps.io ----

rsconnect::deployApp(
  appDir = app_dir,
  appPrimaryDoc = "Conto_annuale_app.Rmd",
  appName = "ContoAnnuale",
  forceUpdate = TRUE,
  launch.browser = TRUE
)