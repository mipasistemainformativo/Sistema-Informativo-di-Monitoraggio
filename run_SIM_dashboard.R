# ============================================================
# SIM - Launcher R
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" Sistema Informativo di Monitoraggio (SIM)\n")
cat(" Piattaforma di consultazione\n")
cat("============================================================\n\n")

# Crea automaticamente la cartella temporanea se non esiste
if (!dir.exists("07_Temp")) {
  dir.create("07_Temp", recursive = TRUE)
  cat("✓ Creata cartella temporanea 07_Temp\n\n")
} else {
  cat("✓ Cartella temporanea 07_Temp trovata\n\n")
}

flush.console()

# Individua la root del progetto
args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)])

if (length(script_path) > 0 && nzchar(script_path)) {
  project_root <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  project_root <- getwd()
}

setwd(project_root)

cat("1/5 Controllo cartella progetto...\n")
cat("Cartella progetto:\n", project_root, "\n\n", sep = "")
flush.console()

main_script <- "03_Scripts/06_render_dashboard_SIM_integrata.R"

if (!file.exists(main_script)) {
  stop(
    "Non trovo:\n", main_script,
    "\nControlla che run_SIM_dashboard.R sia nella root del progetto.",
    call. = FALSE
  )
}

cat("✓ Script principale trovato\n\n")
flush.console()

# ============================================================
# 2/5 Pacchetti R
# ============================================================

cat("2/5 Controllo pacchetti R...\n")
flush.console()

required_packages <- c(
  "callr", "googledrive", "rmarkdown", "shiny",
  "dplyr", "stringr", "readr", "jsonlite",
  "DT", "plotly", "ggplot2", "sf",
  "leaflet", "htmltools", "bslib", "tidyr", "purrr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  cat("Pacchetti mancanti, installazione in corso...\n")
  cat("(Questa operazione puo' richiedere alcuni minuti)\n\n")
  flush.console()
  
  # Libreria utente (non richiede privilegi di amministratore)
  user_lib <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "R",
    "win-library",
    paste0(R.version$major, ".", sub("\\..*$", "", R.version$minor))
  )
  
  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  }
  
  .libPaths(c(user_lib, .libPaths()))
  
  install.packages(
    missing_packages,
    lib = user_lib,
    repos = "https://cloud.r-project.org/"
  )
  
  cat("✓ Pacchetti installati\n\n")
} else {
  cat("✓ Tutti i pacchetti sono gia' installati\n\n")
}

flush.console()

# ============================================================
# 2b/5 Pandoc / Quarto
# ============================================================

cat("2b/5 Controllo Quarto / Pandoc...\n")
flush.console()

is_win <- .Platform$OS.type == "windows"
pandoc_bin <- if (is_win) "pandoc.exe" else "pandoc"

find_pandoc_dir <- function() {

  # A) Gia' impostato dal launcher (bash/bat)
  existing <- Sys.getenv("RSTUDIO_PANDOC")
  if (nzchar(existing) && file.exists(file.path(existing, pandoc_bin))) {
    return(existing)
  }

  # B) Posizioni standard per piattaforma
  if (is_win) {
    candidates <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "tools"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin"),
      "C:/Program Files/Quarto/bin/tools",
      "C:/Program Files/Quarto/bin",
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio", "resources", "app", "quarto", "bin", "tools"),
      "C:/Program Files/RStudio/resources/app/quarto/bin/tools",
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio", "bin", "pandoc"),
      "C:/Program Files/RStudio/bin/pandoc"
    )
  } else {
    candidates <- c(
      "/Applications/quarto/bin/tools",
      "/usr/local/bin",
      "/opt/homebrew/bin",
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/arm64",
      "/Applications/RStudio.app/Contents/MacOS/pandoc",
      file.path(path.expand("~"), "Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")
    )
  }

  for (p in candidates) {
    if (file.exists(file.path(p, pandoc_bin))) return(p)
  }

  # C) rmarkdown::find_pandoc come ultima risorsa
  tryCatch({
    found <- rmarkdown::find_pandoc(cache = FALSE)
    if (!is.null(found$dir) && nzchar(found$dir)) return(found$dir)
  }, error = function(e) NULL)

  # D) Installa Pandoc nella home utente tramite rmarkdown
  # (non richiede permessi amministratore)
  cat("Pandoc non trovato nelle posizioni standard.\n")
  cat("Installazione automatica di Pandoc in corso...\n")
  cat("(Operazione una tantum, richiede connessione Internet)\n\n")
  flush.console()

  tryCatch({
    rmarkdown::install_pandoc()
    found <- rmarkdown::find_pandoc(cache = FALSE)
    if (!is.null(found$dir) && nzchar(found$dir)) {
      cat("✓ Pandoc installato correttamente\n\n")
      return(found$dir)
    }
  }, error = function(e) {
    cat("Installazione automatica non riuscita:", conditionMessage(e), "\n")
  })

  return(NULL)
}

pandoc_dir <- find_pandoc_dir()

if (is.null(pandoc_dir)) {
  cat("\n")
  cat("============================================================\n")
  cat("ERRORE: Pandoc / Quarto non trovato e non installabile.\n")
  cat("============================================================\n\n")
  cat("Per risolvere il problema:\n\n")
  cat("  1. Aprire il browser e andare su: https://quarto.org/docs/download/\n")
  if (is_win) {
    cat("  2. Scaricare il file .exe per Windows\n")
    cat("  3. Aprire il file scaricato e seguire l'installazione\n")
  } else {
    cat("  2. Scaricare il file .pkg per macOS\n")
    cat("  3. Aprire il file scaricato e seguire l'installazione\n")
  }
  cat("  4. Al termine, riaprire il SIM\n\n")
  stop("Pandoc non disponibile. Installare Quarto da https://quarto.org/docs/download/",
       call. = FALSE)
}

Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
cat("✓ Quarto / Pandoc trovato\n\n")
flush.console()

# ============================================================
# 3-5/5 Avvio
# ============================================================

cat("3/5 Avvio accesso a Google Drive...\n")
cat("Se richiesto, completare il login nel browser.\n\n")
flush.console()

cat("4/5 Download dati e preparazione input...\n")
cat("Questa fase puo' richiedere tempo. Attendere senza chiudere la finestra.\n\n")
flush.console()

cat("5/5 Avvio piattaforma SIM...\n")
cat("Il browser si aprira' automaticamente quando la Home sara' pronta.\n\n")
flush.console()

options(
  gargle_oauth_email = TRUE,
  gargle_oob_default = FALSE
)

source(
  main_script,
  local = new.env(parent = globalenv())
)
