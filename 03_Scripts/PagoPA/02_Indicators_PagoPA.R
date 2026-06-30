rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(jsonlite)
library(purrr)  
library(stringr)
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
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
#                            IMPORT PagoPA e fil_
################################################################################

file_pagoPA <- drive_ls(as_id("1qDUxN8X-dIhI6xzDpKzadWckfQOuerwK"))
file_regioni <- drive_ls(as_id("1QJXViD9ilV0VJ2n7r7RHX93z5c2IV9Oy")) %>% 
  filter(name == "fil_reg.rds")
file_totali <- bind_rows(file_pagoPA, file_regioni)

walk2(file_totali$id, file_totali$name, ~ {
  nome_variabile <- .y %>% 
    str_remove("\\.xlsx$|\\.rds$") %>% 
    str_replace_all("[\\s-]+", "_")
  percorso_file <- file.path("07_Temp", .y)
  drive_download(as_id(.x), path = percorso_file, overwrite = TRUE, verbose = FALSE)
  if (str_ends(.y, "\\.rds")) {
    dataset <- readRDS(percorso_file)
  } else {
    dataset <- read_excel(percorso_file)
  }
  assign(nome_variabile, dataset, envir = .GlobalEnv)
})
rm(file_pagoPA, file_regioni, file_totali)

message("File caricati in R-Studio")
################################################################################
#                          INDICATORI PagoPA
################################################################################
db1 <- IO_Distribuzione_geografica_enti_e_servizi[IO_Distribuzione_geografica_enti_e_servizi$categoria=="Comuni",]
db2 <- IO_Distribuzione_geografica_enti_e_servizi[IO_Distribuzione_geografica_enti_e_servizi$categoria=="Istruzione",]

db1 <- db1 %>% 
  mutate(
    ind1 = numero_enti,
    ind2 = numero_servizi ) %>% 
  select(-numero_enti, -numero_servizi, -categoria)

db2 <- db2 %>% 
  mutate(
    ind3 = numero_enti,
    ind4 = numero_servizi ) %>% 
  select(-numero_enti, -numero_servizi, -categoria)

db3 <- left_join(db1, db2, by = "regione")

################################################################################
db4 <- SEND_Distribuzione_geografica_dei_Comuni_su_SEND

db4 <- db4 %>% 
  mutate(
    ind5 = numero_comuni,
    ind6 = percentuale_comuni ) %>% 
  select(-numero_comuni, -percentuale_comuni)
  
db5 <- left_join(db3, db4, by = "regione")

################################################################################
# Match con fil_reg + Regex
################################################################################
#pulizia fil_reg
fil_reg_match <- fil_reg %>%
  mutate(reg_key = reg %>% 
           str_to_lower() %>% 
           str_replace_all("[^a-z]", "")) %>% 
  select(codice_reg, reg_key)

# pulizia db5+join
db5 <- db5 %>%
  mutate(reg_key = regione %>% 
           str_to_lower() %>% 
           str_replace_all("[^a-z]", "")) %>%
  left_join(fil_reg_match, by = "reg_key") %>%
  select(-reg_key) 

db5 <- db5 %>% relocate(codice_reg) %>%  
  select(-regione)
indicatori_per_regione <- db5

indicatori_per_regione <- indicatori_per_regione %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(fil = "fil_reg") %>%
  rename(fil_val = codice_reg) %>%
  relocate(fil, fil_val, ind, ind_val)

message("Creati indicatori per fil_reg")

rm(SEND_Distribuzione_geografica_dei_Comuni_su_SEND, IO_Distribuzione_geografica_enti_e_servizi, db1, db2, db3, db4, db5)

################################################################################

db6 <- IO_Messaggi_inviati_da_servizi
db6 <- db6 %>% 
  mutate(
    ind7 = numero_messaggi ) %>% 
  select(-numero_messaggi)

db7 <- pagoPA_Distribuzione_mensile_del_numero_di_transazioni_per_categoria_di_ente_creditore
db8 <- db7 %>% 
  filter(categoria != "Tutte") %>% 
  pivot_wider(
    names_from = categoria,            
    values_from = numero_transazioni,  
    values_fill = 0                    
  )

db8 <- db8 %>% 
  mutate(
    ind8 = ACI,
    ind9 = Comuni,
    ind10 = `Consorzi universitari`,
    ind11 = `Enti comunali`,
    ind12 = `Enti provinciali`,
    ind13 = `Enti regionali`,
    ind14 = `Ordini, collegi e consigli professionali`,
    ind15 = Province,
    ind16 = `Pubbliche amministrazioni centrali`,
    ind17 = Regioni,
    ind18 = Ricerca,
    ind19 = `Salute centrale`,
    ind20 = `Salute locale`,
    ind21 = `Salute regionale`,
    ind22 = `Salute servizi`,
    ind23 = Scuola,
    ind24 = Università,
    ind25 = `Altri enti territoriali`,
    ind26 = Utility
  ) %>% 
  select(anno_mese, ind8:ind26)

db9 <- left_join(db6, db8, by = "anno_mese")

db10 <- SEND_Distribuzione_del_numero_di_notifiche %>% 
  mutate(
    ind27 = notifiche_analogiche,
    ind28 = notifiche_digitali,
    ind29 = notifiche_totali
  ) %>% 
  select(anno_mese, ind27:ind29)

db11 <- left_join(db9, db10, by = "anno_mese")
db11a <- db11 %>%
  separate(anno_mese, into = c("anno", "mese"), sep = "-")

indicatori_per_tempo <- db11a

indicatori_per_tempo <- indicatori_per_tempo %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_mese"
  ) %>%
  rename(
    fil_val = anno,
    sub_fil_val = mese
  ) %>%
  relocate(fil, fil_val, sub_fil, sub_fil_val, ind, ind_val)

message("Creati indicatori per fil_anno e fil_mese")

rm(db6, db7, db8, db9, db10, db11, IO_Messaggi_inviati_da_servizi, pagoPA_Distribuzione_mensile_del_numero_di_transazioni_per_categoria_di_ente_creditore, SEND_Distribuzione_del_numero_di_notifiche)
################################################################################

db12 <- SEND_Distribuzione_dei_principali_ambiti_di_notifica %>% 
  mutate(
    ind30 = numero_notifiche,
  ) %>% 
  select(ambito, ind30)
indicatori_per_ambito <- db12

indicatori_per_ambito <- indicatori_per_ambito %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(fil = "fil_ambito") %>%
  rename(fil_val = ambito) %>%
  relocate(fil, fil_val, ind, fil_val)

message("Creati indicatori per fil_ambito")

rm(db12, SEND_Distribuzione_dei_principali_ambiti_di_notifica)
################################################################################

db13 <- pagoPA_Distribuzione_del_numero_di_transazioni_per_fascia_di_importo_e_categoria_di_ente_creditore %>% 
  filter(categoria != "Tutte") %>% 
  pivot_wider(
    names_from = categoria,            
    values_from = numero_transazioni,  
    values_fill = 0                    
  )
db14 <- db13 %>% 
  mutate(
    ind31 = ACI,
    ind32 = Comuni,
    ind33 = `Consorzi universitari`,
    ind34 = `Enti comunali`,
    ind35 = `Enti provinciali`,
    ind36 = `Enti regionali`,
    ind37 = `Ordini, collegi e consigli professionali`,
    ind38 = Province,
    ind39 = `Pubbliche amministrazioni centrali`,
    ind40 = Regioni,
    ind41 = Ricerca,
    ind42 = `Salute centrale`,
    ind43 = `Salute locale`,
    ind44 = `Salute regionale`,
    ind45 = `Salute servizi`,
    ind46 = Scuola,
    ind47 = Università,
    ind48 = `Altri enti territoriali`,
    ind49 = Utility, 
    ind50 = `Enti donazioni`
  ) %>% 
  select(anno, fascia_importo, ind31:ind50)
indicatori_per_fascia_importo <- db14

indicatori_per_fascia_importo <- indicatori_per_fascia_importo %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_fascia"
  ) %>%
  rename(
    fil_val = anno,
    sub_fil_val = fascia_importo
  ) %>%
  relocate(fil, fil_val, sub_fil, sub_fil_val, ind, ind_val)

message("Creati indicatori per fil_fascia")

rm(db13, db11a, db14, pagoPA_Distribuzione_del_numero_di_transazioni_per_fascia_di_importo_e_categoria_di_ente_creditore)

################################################################################
#                         APPEND DEI DATASET
################################################################################
indicatori_per_regione <- indicatori_per_regione %>%
  mutate(sub_fil = NA_character_, sub_fil_val = NA_character_)
indicatori_per_ambito <- indicatori_per_ambito %>%
  mutate(sub_fil = NA_character_, sub_fil_val = NA_character_)

#Append
db <- bind_rows(
  indicatori_per_regione,
  indicatori_per_tempo,
  indicatori_per_ambito,
  indicatori_per_fascia_importo
)

message("Dataset unico creato")
################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("13TnQhe08KN5J4mZdG66WA1wQddFr8h-4")
nome_file_output <- "INDICATORS_PAGOPA.json"
path_temp <- file.path("07_Temp", nome_file_output)

tryCatch({
  write_json(db, path = path_temp, pretty = TRUE, dataframe = "rows")
}, error = function(e) {
  stop("Errore durante la creazione del file JSON: ", e$message)
})

#Carico su drive
if (file.exists(path_temp)) {
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = nome_file_output,
    overwrite = TRUE
  )
  unlink(path_temp) 
} else {
  stop("Errore critico: Il file ", path_temp, " non esiste sul disco!")
}

message("Files caricati correttamente su Drive")

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

file_xlsx <- list.files(
  path = "07_Temp",
  pattern = "\\.xlsx$",
  full.names = TRUE
)
file_rds <- list.files(
  path = "07_Temp",
  pattern = "\\.rds$",
  full.names = TRUE
)
file.remove(file_xlsx)
file.remove(file_rds)

message("Rimossi i files in 07_Temp")

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
