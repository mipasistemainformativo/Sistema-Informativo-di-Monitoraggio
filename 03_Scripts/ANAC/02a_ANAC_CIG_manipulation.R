rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(purrr)
library(stringr)
library(dplyr)
library(readxl)
library(readr)

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
folder_id <- "1BMRQxs02gvtIAFvAJcKFVP7orDjSmTZp"

################################################################################
#                              IMPORT CIG
###############################################################################
file_su_drive <- drive_ls(as_id(folder_id))
mesi <- str_pad(1:12, 2, pad = "0")

walk(mesi, function(m) {
  nome_file_cercato <- str_glue("cig_json_2023_{m}.xlsx")
  file_info <- file_su_drive %>% 
    filter(name == nome_file_cercato)
  
  if (nrow(file_info) > 0) {
    path_locale <- file.path("07_Temp", nome_file_cercato)
    nome_oggetto <- str_glue("cig_{m}") 
    message("Scaricamento: ", nome_file_cercato)
    
    drive_download(
      file = as_id(file_info$id),
      path = path_locale,
      overwrite = TRUE,
      verbose = FALSE
    )
    
    # Lettura e conversione specifica
    df_mese <- read_excel(path_locale) %>% 
      # 1. Trasformiamo tutto in character per evitare conflitti tra file
      mutate(across(everything(), as.character)) %>% 
      # 2. Convertiamo in numerico solo le colonne che ci servono
      mutate(
        importo_lotto = as.numeric(importo_lotto),
        importo_complessivo_gara = as.numeric(importo_complessivo_gara),
        n_lotti_componenti = as.numeric(n_lotti_componenti)
      ) %>% 
      mutate(mese_rif = m)
    
    assign(nome_oggetto, df_mese, envir = .GlobalEnv)
    
    unlink(path_locale)
    message("Completato: ", nome_oggetto)
  } else {
    message("Attenzione: Il file ", nome_file_cercato, " non è stato trovato su Drive.")
  }
})

# Pulizia
rm(file_su_drive, folder_id)
################################################################################
#                        MANIPULATION CIG (APPEND)
###############################################################################

nomi_dataset <- str_glue("cig_{mesi}")
dataset_puliti <- mget(nomi_dataset) %>% 
  map(~ mutate(.x, DURATA_PREVISTA = as.character(DURATA_PREVISTA)))
cig_2023 <- bind_rows(dataset_puliti)


rm(list = c(nomi_dataset, "mesi", "nomi_dataset", "dataset_puliti"))

#controlliamo se ci sono duplicati di gara per errore
duplicati_summary <- cig_2023 %>%
  count(cig) %>%
  filter(n > 1) %>%
  arrange(desc(n))

print(duplicati_summary)

if (nrow(duplicati_summary) == 0) {
  message("Nessun duplicato, perfetto!")
} else {
  message("Attenzione: trovati ", nrow(duplicati_summary), " CIG duplicati.")
}
################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE (RDS)
################################################################################
id_destinazione <- as_id("1uCyXCfMh-2da9AKRF73QbP_Okm8yQzhi")
path_output_rds <- "07_Temp/cig_2023.rds"

#Salvataggio locale in formato RDS
# Usiamo write_rds (dal pacchetto readr) o saveRDS (base R)
write_rds(cig_2023, file = path_output_rds, compress = "gz") 

#carico su Drive
drive_upload(
  media = path_output_rds,
  path = id_destinazione,
  name = "CIG_2023.rds",         
  overwrite = TRUE                
)

#Pulizia file temporaneo locale
unlink(path_output_rds)
message("Export completato con successo su Drive")

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

