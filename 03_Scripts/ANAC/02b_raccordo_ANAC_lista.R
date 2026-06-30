rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(readr)
library(readxl)
library(dplyr)
library(stringr) 

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

temp_dir <- "07_Temp"
################################################################################
#                           IMPORT DATASET
################################################################################
file_CIG <- drive_ls(as_id("1uCyXCfMh-2da9AKRF73QbP_Okm8yQzhi")) %>% 
  filter(name == "CIG_2023.rds") 
file_raccordo <- drive_ls(as_id("15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs")) %>% 
  filter(name == "Lista_raccordo_SIM.xlsx")
file_CPV <- drive_ls(as_id("1QJXViD9ilV0VJ2n7r7RHX93z5c2IV9Oy")) %>% 
  filter(name == "fil_cpv.rds") 

#import Lista
path_lista <- file.path(temp_dir, "Lista_raccordo_SIM.xlsx")
drive_download(
  file = as_id(file_raccordo$id), 
  path = path_lista,
  overwrite = TRUE
)
lista <- read_excel(path_lista)
message("File Lista_raccordo_SIM.xlsx caricato correttamente")

#import Lista
path_CPV <- file.path(temp_dir, "fil_cpv.rds")
drive_download(
  file = as_id(file_CPV$id), 
  path = path_CPV,
  overwrite = TRUE
)
CPV <- read_rds(path_CPV)
message("File CPV.xlsx caricato correttamente")

#import CIG
path_cig <- file.path(temp_dir, "CIG_2023.rds")
drive_download(
  file = as_id(file_CIG$id), 
  path = path_cig,
  overwrite = TRUE
)
cig_2023 <- read_rds(path_cig)
message("File CIG_2023.rds caricato correttamente")

unlink(c(path_lista, path_cig))
# Verifica dimensioni
message("Righe caricatere in Lista: ", nrow(lista))
message("Righe caricate in CIG 2023: ", nrow(cig_2023))
################################################################################
#                               PULIZIA
################################################################################
cig_2023_macro <- cig_2023 %>%
  mutate(
    cpv_division = str_sub(cod_cpv, 1, 2) 
  )

table(cig_2023_macro$cpv_division)
rm(file_CIG, file_raccordo, file_CPV)

################################################################################
#                              LEFT JOIN 
################################################################################
# Eseguiamo il join con macrocategorie di cpv 
cig_2023_finale <- cig_2023_macro %>%
  left_join(CPV %>% select(cpv_division, cpv_cat), 
            by = "cpv_division")

# Eseguiamo il join con lista 
dataset_unito <- lista %>%
  left_join(cig_2023_finale, by = c("codice_fiscale" = "cf_amministrazione_appaltante"))

message("Matchate: ", nrow(dataset_unito) , " gare")

################################################################################
#                              EXPORT RDS
################################################################################
id_destinazione <- as_id("1-P9OBSKJZr4EFhyXCIQJE39ERBoZmYGh")
nome_file_output <- "Master.rds"
path_temp <- file.path("07_Temp", nome_file_output)

#Salvataggio locale in formato RDS
tryCatch({
  write_rds(dataset_unito, file = path_temp, compress = "gz")
  message("File RDS creato localmente in 07_Temp")
}, error = function(e) {
  stop("Errore durante la creazione del file RDS: ", e$message)
})

#Caricamento su Google Drive
if (file.exists(path_temp)) {
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = nome_file_output,
    overwrite = TRUE
  )
  
#Pulizia file temporaneo
  unlink(path_temp) 
  message("File Master.rds caricato correttamente su Drive")
} else {
  stop("Errore critico: Il file ", path_temp, " non esiste sul disco!")
}

message("--- FINE ELABORAZIONE: ", Sys.time(), " ---")

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


