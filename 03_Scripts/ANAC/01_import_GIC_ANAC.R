rm(list = ls())
################################################################################
#                                 LIBRERIE
################################################################################
library(jsonlite)
library(googledrive)
library(tidydr)
library(httr)
library(stringr)
library(writexl)
library(dplyr)

################################################################################
#                          CONFIGURATION LOG 
################################################################################
#Recupero il nome dello script attuale
nome_script <- basename(rstudioapi::getActiveDocumentContext()$path) %>% 
  str_remove("\\.[rR]$") 

#Creo il nome del file log: log_NOMESCRIPT_YYYYMMDD.txt
data_oggi <- format(Sys.time(), "%Y%m%d")
log_filename <- paste0("log_", nome_script, "_", data_oggi, ".txt")

#Definisco il percorso locale 
if (!dir.exists("07_Temp/ANAC")) dir.create("07_Temp/ANAC", recursive = TRUE)
log_path <- file.path("07_Temp/ANAC", log_filename)
#attivazione log
con <- file(log_path, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

message("--- INIZIO ELABORAZIONE: ", Sys.time(), " ---")
message("Script in esecuzione: ", nome_script)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# ID del dataset
dataset_id <- "64a3fcfd-bf5f-484d-96cd-a19804ae5bf0"
# URL API 
url_api <- paste0("https://www.dati.gov.it/opendata/api/3/action/package_show?id=", dataset_id)

#drive

DRIVE_ROOT_ID  <- "14jMYmLq78M-0LxuaIBAGao16ZhF59xDc"
DRIVE_DIR_SOURCE    <- "01_Dataset/Source"
DRIVE_DIR_SOURCE_ANAC <-  file.path(DRIVE_DIR_SOURCE, "ANAC")
DRIVE_DIR_SOURCE_ANAC_GIC2023 <-  file.path(DRIVE_DIR_SOURCE_ANAC, "GIC 2023")

temp <- "07_Temp"

################################################################################
#                   SCARICO DATI AUTOMATICO CIG CODES
################################################################################

#Chiamata API
res <- GET(url_api, add_headers(`User-Agent` = "Mozilla/5.0"))
if (status_code(res) != 200) stop("Errore API")

data <- fromJSON(content(res, "text", encoding = "UTF-8"))
files <- data$result$resources

# Selezione file JSON 2023
links <- files %>%
  filter(grepl("2023", name), tolower(format) == "json")

# Download ed estrazione
for (i in 1:nrow(links)) {
  f_url <- links$url[i]
  f_name <- basename(f_url)
  dest <- file.path(temp, f_name)
  GET(f_url, write_disk(dest, overwrite = TRUE), timeout(900))
  if (grepl("\\.zip$", f_name, ignore.case = TRUE)) {
    unzip(dest, exdir = temp)
    file.remove(dest) 
  }
}

################################################################################
#                        CONVERSIONE JSON-EXCEL
################################################################################


converti_json_excel <- function(cartella_input) {
  file_json <- list.files(cartella_input, pattern = "\\.json$", full.names = TRUE)
  for (f in file_json) {
    message("Lettura: ", basename(f))
    df <- tryCatch({
      stream_in(file(f), flatten = TRUE, verbose = FALSE)
    }, error = function(e) {
      lines <- readLines(f, warn = FALSE)
      fromJSON(paste0("[", paste(lines, collapse = ","), "]"), flatten = TRUE)
    })
    
    # Forza formato testo per preservare CIG e codici alfanumerici
    df <- df %>% mutate(across(everything(), as.character))
    if (nrow(df) > 1048570) {
      df <- df[1:1048570, ]
    }
    nome_output <- gsub("\\.json$", ".xlsx", f)
    write_xlsx(df, nome_output)
    rm(df)
    gc() 
  }
}
converti_json_excel("07_Temp")
file_da_eliminare <- list.files("07_Temp", pattern = "\\.json$", full.names = TRUE)
file.remove(file_da_eliminare)
            
################################################################################
#                        CARICAMENTO SU DRIVE
################################################################################

file_excel <- list.files(temp, pattern = "\\.xlsx$", full.names = TRUE)

DRIVE_DIR_SOURCE_ANAC_GIC2023 <- "1BMRQxs02gvtIAFvAJcKFVP7orDjSmTZp"
target_folder <- as_id(DRIVE_DIR_SOURCE_ANAC_GIC2023)
file_excel <- list.files(temp, pattern = "\\.xlsx$", full.names = TRUE)

for (f in file_excel) {
  drive_put(
    media = f,
    path = target_folder,
    name = basename(f)
  )
}

file_da_eliminare <- list.files("07_Temp", pattern = "\\.xlsx$", full.names = TRUE)
file.remove(file_da_eliminare)

message("--- FINE ELABORAZIONE: ", Sys.time(), " ---")

# Chiudiamo registrazione del log
sink(type = "message")
sink(type = "output")
close(con)

# Carica il LOG anche su Drive
id_cartella_log_drive <- as_id("1sZo_8mL2qSMk50_qOoOb1nfk9Bu6KOn0") 
drive_upload(
  media = log_path,
  path = id_cartella_log_drive,
  name = log_filename
)

rm(list = ls())

