rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(jsonlite)
library(dplyr)
library(httr)
library(googledrive)
library(stringr)
library(purrr)
library(readr)   
library(writexl)

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
if (!dir.exists("07_Temp/PagoPA")) dir.create("07_Temp/PagoPA", recursive = TRUE)
log_path <- file.path("07_Temp/PagoPA", log_filename)
#attivazione log
con <- file(log_path, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

message("--- INIZIO ELABORAZIONE: ", Sys.time(), " ---")
message("Script in esecuzione: ", nome_script)

################################################################################
#                              CONFIGURATIONS 
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")
message("Drive collegato correttamente")

dataset_ids <- c(
  "22548c08-95fa-4572-bad2-a0fefcfea03c",  # IO - messaggi inviati da servizi
  "c26c9d73-dd33-4040-9576-ccd13cc17f97",  # IO - Distribuzione geografica enti e servizi
  #  "422b9168-8379-41a1-a69e-80beab547773",  # SEND - Distribuzione delle principali tipologie di atti notificati da SEND
  #  "48a1d0e6-ea85-41bc-93ee-b116732f1a11",  # SEND - Andamento del numero di avvisi inviati
  #  "7378b7b6-8f01-4c53-b95b-e29a99e07427",  # SEND - Enti aderenti a SEND per anno
  "819c118e-a472-4817-8f95-323af08c2f92",  # SEND - Distribuzione dei principali ambiti di notifica
  "e787c0df-5abe-47e4-8489-0e21afce9aab",  # SEND - Distribuzione geografica dei Comuni su SEND
  "89170ce7-b680-47eb-9cf2-c93989941a21",  # SEND - Distribuzione del numero di notifiche
  "c8d9d7b1-aeb4-4ce9-ab9d-78dc71392e3a",  # pagoPA - Distribuzione del numero di transazioni per fascia di importo e categoria di ente creditore
  "d005ffe7-a959-4487-8805-297895de544a"   # pagoPA - Distribuzione mensile del numero di transazioni per categoria di ente creditore
  #  "65d59012-bebe-4d58-bcd6-b072def27a55",  # pagoPA - Distribuzione del numero di transazioni, valore economico ed enti creditori
)

temp_dir <- "07_Temp"

################################################################################
#                              SCARICO DATI 
################################################################################

scarica_e_converti <- function(id) {
  url_api <- paste0("https://www.dati.gov.it/opendata/api/3/action/package_show?id=", id)
  res <- GET(url_api, add_headers(`User-Agent` = "Mozilla/5.0"))
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    resources <- data$result$resources
    # prendiamo formato .xlsx o .csv
    file_da_scaricare <- resources %>% 
      filter(tolower(format) %in% c("xlsx", "csv")) 
    for (i in 1:nrow(file_da_scaricare)) {
      f_url <- file_da_scaricare$url[i]
      f_format <- tolower(file_da_scaricare$format[i])
      
      nome_pulito <- file_da_scaricare$name[i] %>% 
        str_remove_all("(?i)[\\s_-]+CSV") %>% 
        str_remove_all("(?i)\\.xlsx$|\\.csv$") %>% 
        str_trim() %>% 
        paste0(".xlsx")
      
      dest_temporaneo <- file.path(temp_dir, paste0("temp_file.", f_format))
      dest_finale <- file.path(temp_dir, nome_pulito)
      
      message("Scaricamento: ", nome_pulito)
      GET(f_url, write_disk(dest_temporaneo, overwrite = TRUE))
      if (f_format == "csv") {
        df <- tryCatch({
          read_csv(dest_temporaneo, show_col_types = FALSE)
        }, error = function(e) {
          read_csv2(dest_temporaneo, show_col_types = FALSE)
        })
        write_xlsx(df, dest_finale)
        unlink(dest_temporaneo) 
      } else {
        file.rename(dest_temporaneo, dest_finale)
      }
    }
  }
}

# Eseguiamo il download e conversione
walk(dataset_ids, scarica_e_converti)

################################################################################
#                          CARICAMENTO SU DRIVE
################################################################################
output_folder <- as_id("1qDUxN8X-dIhI6xzDpKzadWckfQOuerwK") 
file_scaricati <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)

for (f in file_scaricati) {
  drive_put(
    media = f,
    path = output_folder,
    name = basename(f)
  )
  message("Caricato su Drive: ", basename(f))
}

# Pulizia finale
file.remove(file_scaricati)
message("Tutto completato!")



message("--- FINE ELABORAZIONE: ", Sys.time(), " ---")

# Chiudiamo registrazione del log
sink(type = "message")
sink(type = "output")
close(con)

# Carica il LOG anche su Drive
id_cartella_log_drive <- as_id("1rOe3bFduWUaamWNS5y_c_YN_QvF4GH2N") 
drive_upload(
  media = log_path,
  path = id_cartella_log_drive,
  name = log_filename
)

rm(list = ls())