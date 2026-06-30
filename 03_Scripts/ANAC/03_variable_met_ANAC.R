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
message("Drive collegato correttamente")

################################################################################
#                            IMPORT Dataset
################################################################################
options(googledrive_quiet = TRUE)
file_ANAC <- drive_ls(as_id("1uCyXCfMh-2da9AKRF73QbP_Okm8yQzhi"))

file_ANAC_filtrati <- file_ANAC %>% 
  filter(grepl("\\.rds$", name))
walk2(file_ANAC_filtrati$id, file_ANAC_filtrati$name, ~ {
  nome_variabile <- .y %>% 
    str_remove("\\.rds$") %>% 
    str_replace_all("[\\s-]+", "_")
  percorso_file <- file.path("07_Temp", .y)
  drive_download(as_id(.x), path = percorso_file, overwrite = TRUE)
  dataset <- read_rds(percorso_file)
  assign(nome_variabile, dataset, envir = .GlobalEnv)
  message("Caricato dataset RDS: ", nome_variabile)
})

################################################################################
#                            FILE VARIABLE_MET
################################################################################
#identificazione dataset caricato 
lista_nomi_dataset <- ls(pattern = "^CIG_")

#Estrazione metadati
estrai_metadati <- function(nome_ds) {
  df <- get(nome_ds)
  tibble(
    fonte = "ANAC",
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
      esempi <- head(unique(na.omit(x)), 3)
      if(length(esempi) == 0) return("Tutti NA")
      paste(as.character(esempi), collapse = ", ")
    }),
    
    note = ""
  )
}
report_metadati <- map_df(lista_nomi_dataset, estrai_metadati)
################################################################################
#                               EXPORT
################################################################################
variables_met_id <- "19EdIQwoAyJB5yFtXK6Fkjfvkz63qB-Ml"

#export locale in .xlsx e .csv
file_excel <- file.path("07_Temp/ANAC", "ANAC_variables.xlsx")
file_csv   <- file.path("07_Temp/ANAC", "ANAC_variables.csv")
write_xlsx(report_metadati, file_excel)
write_excel_csv2(report_metadati, file_csv)

message("Report metadati generati localmente in 02_Metadata/ANAC")

# Salva Excel
write_xlsx(report_metadati, file_excel)
# Salva CSV 
write_excel_csv2(report_metadati, file_csv)
message("Report metadati generato con successo in 02_Metadata/ANAC")

#export su Drive in .xlsx e .csv
drive_put(media = file_excel, path = as_id(variables_met_id), name = "ANAC_variables.xlsx")
drive_put(media = file_csv,   path = as_id(variables_met_id), name = "ANAC_variables.csv")
message("Upload completato.")

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

message("Pulizia cartella temporanea...")
file.remove(list.files("07_Temp", pattern = "\\.xlsx$|\\.csv$|\\.rds$", full.names = TRUE))

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