# ============================================================
# Crea release pulita del SIM
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" Creazione release SIM\n")
cat("============================================================\n\n")

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_name <- basename(project_root)

release_root <- file.path(dirname(project_root), "SIM_release")
release_dir  <- file.path(release_root, project_name)

if (dir.exists(release_root)) {
  unlink(release_root, recursive = TRUE, force = TRUE)
}

dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

include_items <- c(
  "01_Dataset",
  "02_Metadata",
  "03_Scripts",
  "04_Output",
  "05_Logs",
  "06_Docs",
  "README.md",
  "README_LAUNCHER_SIM.txt",
  "run_SIM_dashboard.R",
  "🚀 Apri_SIM_MAC.command",
  "🚀 Apri_SIM_Windows.bat",
  "Monitoraggio-PNRR.Rproj"
)

exclude_patterns <- c(
  "^07_Temp$",
  "^\\.git$",
  "^\\.RData$",
  "^\\.Rhistory$",
  "^\\.Rproj\\.user$",
  "^\\.DS_Store$",
  "^renv$"
)

cat("Cartella progetto:\n", project_root, "\n\n", sep = "")
cat("Cartella release:\n", release_root, "\n\n", sep = "")

for (item in include_items) {
  src <- file.path(project_root, item)
  dst <- file.path(release_dir, item)
  
  if (!file.exists(src)) {
    cat("Elemento non trovato, salto: ", item, "\n", sep = "")
    next
  }
  
  cat("Copio: ", item, "\n", sep = "")
  
  if (dir.exists(src)) {
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    file.copy(
      from = list.files(src, all.files = TRUE, no.. = TRUE, full.names = TRUE),
      to = dst,
      recursive = TRUE,
      copy.date = TRUE
    )
  } else {
    file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
  }
}

# Pulizia file/cartelle inutili dentro la release
all_paths <- list.files(release_dir, all.files = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)
base_names <- basename(all_paths)

to_remove <- grepl("\\.RData$", base_names) |
  grepl("\\.Rhistory$", base_names) |
  grepl("\\.DS_Store$", base_names) |
  grepl("\\.html$", base_names) |
  grepl("_cache$", base_names) |
  grepl("^old$", base_names)

if (any(to_remove)) {
  cat("\nPulizia file/cartelle non necessari...\n")
  unlink(all_paths[to_remove], recursive = TRUE, force = TRUE)
}

# Permessi esecuzione Mac
mac_launcher <- file.path(release_dir, "🚀 Apri SIM.command")
if (file.exists(mac_launcher)) {
  Sys.chmod(mac_launcher, mode = "0755")
}

# Crea ZIP
zip_file <- file.path(release_root, paste0(project_name, "_SIM_release.zip"))

old_wd <- getwd()
setwd(release_root)

if (file.exists(zip_file)) {
  unlink(zip_file)
}

utils::zip(
  zipfile = basename(zip_file),
  files = project_name
)

setwd(old_wd)

cat("\n============================================================\n")
cat("Release creata correttamente.\n")
cat("ZIP pronto:\n")
cat(zip_file, "\n")
cat("============================================================\n\n")