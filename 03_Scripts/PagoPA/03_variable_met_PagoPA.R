rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(googledrive) 
library(readxl)      
library(writexl)     
library(tidyverse)   
library(rstudioapi)

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
#                             CONFIGURATIONS
################################################################################

drive_auth(scopes = "https://www.googleapis.com/auth/drive")
message("Drive collegato correttamente")

################################################################################
#                            IMPORT Dataset
################################################################################
file_pagoPA <- drive_ls(as_id("1qDUxN8X-dIhI6xzDpKzadWckfQOuerwK"))
walk2(file_pagoPA$id, file_pagoPA$name, ~ {
  nome_variabile <- .y %>% 
    str_remove("\\.xlsx$") %>% 
    str_replace_all("[\\s-]+", "_")
  percorso_file <- file.path("07_Temp", .y)
  drive_download(as_id(.x), path = percorso_file, overwrite = TRUE, verbose = FALSE)
  dataset <- read_excel(percorso_file)
  assign(nome_variabile, dataset, envir = .GlobalEnv)
  message("Caricato dataset PagoPA: ", nome_variabile)
})

rm(file_pagoPA)
message("Tutti i file PagoPA sono stati caricati correttamente nell'ambiente R")

################################################################################
#                            FILE VARIABLE_MET
################################################################################
#cercare tutti i nomi
lista_nomi_dataset <- ls(pattern = "^IO_|^pagoPA_|^SEND_")

estrai_metadati <- function(nome_ds) {
  df <- get(nome_ds)
  # Creiamo la struttura base per ogni colonna
  tibble(
    fonte = "PagoPA S.p.a", 
    dataset_id = nome_ds,
    nome_variabile_originale = names(df),
    nome_variabile_standardizzato = names(df),
    tipo_dato_originale = map_chr(df, ~class(.x)[1]),
    tipo_dato_dopo_import = tipo_dato_originale,
    unita_di_misura = "n.d.", 
    n_missing = map_dbl(df, ~sum(is.na(.x))),
    pct_missing = map_dbl(df, ~round(mean(is.na(.x)) * 100, 2)),
    n_valori_distinti = map_dbl(df, n_distinct),
    esempi_valori = map_chr(df, function(x) {
      # Prendiamo i primi 3 valori unici non NA come esempio
      esempi <- head(unique(na.omit(x)), 3)
      paste(esempi, collapse = ", ")
    }),
    note = ""
  )
}

report_metadati <- map_df(lista_nomi_dataset, estrai_metadati)

################################################################################
#                               EXPORT
################################################################################
variables_met_id <- "13iUSiu6zWAHqYWcWR6w64LqEeZ62YM68"

#export locale in .xlsx e .csv
file_excel <- file.path("07_Temp/PagoPA", "PagoPA_variables.xlsx")
file_csv   <- file.path("07_Temp/PagoPA", "PagoPA_variables.csv")
write_xlsx(report_metadati, file_excel)
write_excel_csv2(report_metadati, file_csv)

message("Report metadati generati localmente in 02_Metadata/PagoPA")

# Salva Excel
write_xlsx(report_metadati, file_excel)
# Salva CSV 
write_excel_csv2(report_metadati, file_csv)
message("Report metadati generato con successo in 02_Metadata/PagoPA")

#export su Drive in .xlsx e .csv
drive_put(media = file_excel, path = as_id(variables_met_id), name = "PagoPA_variables.xlsx")
drive_put(media = file_csv,   path = as_id(variables_met_id), name = "PagoPA_variables.csv")
message("Upload completato.")

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

message("Pulizia cartella temporanea...")
file.remove(list.files("07_Temp", pattern = "\\.xlsx$|\\.csv$", full.names = TRUE))

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