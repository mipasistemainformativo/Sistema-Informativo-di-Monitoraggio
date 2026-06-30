rm(list = ls())
################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(readr)
library(readxl)
library(tidyverse)
library(jsonlite)

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
file_master <- drive_ls(as_id("1-P9OBSKJZr4EFhyXCIQJE39ERBoZmYGh")) %>% 
  filter(name == "Master.rds")

#import master
path_master <- file.path(temp_dir, "Master.rds")
drive_download(
  file = as_id(file_master$id),
  path = path_master,
  overwrite = TRUE
)
master <- read_rds(path_master)
message("File master.rds caricato correttamente")

################################################################################
#                       DATASET CLEANING
################################################################################

master_clean <- master %>%
  mutate(
    DURATA_PREVISTA = as.numeric(DURATA_PREVISTA),
    data_scadenza_offerta = as.Date(data_scadenza_offerta),
    DATA_COMUNICAZIONE_ESITO = as.Date(DATA_COMUNICAZIONE_ESITO),
    periodo = paste0(anno_pubblicazione, "-", sprintf("%02d", as.numeric(mese_pubblicazione)))
  )

master_matched <- master_clean %>% filter(!is.na(cig))
names(master_matched)
################################################################################
#                        INDICATORS_GLOBALE
################################################################################

pa_totali_n <- n_distinct(master_clean$codice_fiscale) # Totale MPA (es. 10179)
pa_matchate_n <- n_distinct(master_matched$codice_fiscale)

indicators_globale <- tibble(
  totale_pa_lista_mpa = pa_totali_n,
  ind1 = pa_matchate_n,                                 #Numero di PA matchate
  ind2 = round((pa_matchate_n / pa_totali_n) * 100, 2), #% di PA matchate
  ind3 = nrow(master_matched),                          # Totale gare
  ind4 = sum(master_matched$ESITO == "AGGIUDICATA", na.rm = TRUE), # Totale gare aggiudicate
  
  ind5 = round(mean(master_matched$importo_lotto, na.rm = TRUE), 2),     # Media importo lotto
  ind6 = round(mean(master_matched$DURATA_PREVISTA, na.rm = TRUE), 1),   # Media durata prevista
  ind7 = round(mean(as.numeric(difftime(master_matched$DATA_COMUNICAZIONE_ESITO,    # Tempo medio tra Scadenza e Esito
                                        master_matched$data_scadenza_offerta, 
                                        units = "days")), na.rm = TRUE), 1),
  ind8 = round(mean(as.numeric(difftime(master_matched$data_scadenza_offerta,      # Tempo medio tra Pubblicazione e Scadenza
                                        master_matched$data_pubblicazione, 
                                        units = "days")), na.rm = TRUE), 1),
  ind9 = round((sum(master_matched$ESITO == "AGGIUDICATA", na.rm = TRUE) / nrow(master_matched)) * 100, 2)  # % gare aggiudicate
)
################################################################################
#                INDICATORS_ANNUALE e INDICATORS_MENSILE
################################################################################
indicators_by_pa_anno <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione) %>%
  summarise(
    ind10 = n(),                                                              # Totale gare per PA
    ind11 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                        # Totale gare aggiudicate per PA
    ind12 = round(n() / n_distinct(codice_fiscale), 2),                       # Media gare (totali) per PA (sul totale delle PA nel dataset)
    ind13 = round(sum(ESITO == "AGGIUDICATA", na.rm = TRUE) / n_distinct(codice_fiscale), 2), # Numero medio di gare AGGIUDICATE per PA
    ind14 = round((ind11 / ind10) * 100, 2),                                  # % gare aggiudicate per PA
    
    ind15 = sum(importo_lotto, na.rm = TRUE),                                 # Importo lotto totale per PA
    ind16 = round(mean(importo_lotto, na.rm = TRUE), 2),                      # Media importo lotto per PA
    ind17 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind18 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                    # Media durata prevista per PA
    ind19 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,          # Tempo medio tra Scadenza e Esito
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind20 = round(mean(as.numeric(difftime(data_scadenza_offerta,             # Tempo medio tra Pubblicazione e Scadenza
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1)
  ) %>%
  arrange(codice_fiscale)  


indicators_by_pa_mese <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione, mese_rif) %>%
  summarise(
    ind21 = n(),                                                              # Totale gare per PA per mese
    ind22 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                        # Totale gare aggiudicate per PA per mese
    ind23 = round(n() / n_distinct(codice_fiscale), 2),                       # Media gare (totali) per PA (sul totale delle PA nel dataset)
    ind24 = round(sum(ESITO == "AGGIUDICATA", na.rm = TRUE) / n_distinct(codice_fiscale), 2), # Numero medio di gare AGGIUDICATE per PA
    ind25 = round((ind22 / ind21) * 100, 2),                                  # % gare aggiudicate per PA per mese
    
    ind26 = sum(importo_lotto, na.rm = TRUE),                                 # Importo lotto totale per PA per mese
    ind27 = round(mean(importo_lotto, na.rm = TRUE), 2),                      # Media importo lotto per PA per mese
    ind28 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind29 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                    # Media durata prevista per PA per mese
    ind30 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,          # Tempo medio tra Scadenza e Esito
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind31 = round(mean(as.numeric(difftime(data_scadenza_offerta,             # Tempo medio tra Pubblicazione e Scadenza
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1)
  ) %>%
  arrange(codice_fiscale)  # Ordina per PA, anno e mese

################################################################################
#                       INDICATORS_FG 
################################################################################

indicators_by_pa_anno_fg <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione, desc_fg) %>%
  summarise(
    ind32 = n(),                                                              # Totale gare per PA
    ind33 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                        # Totale gare aggiudicate per PA
    ind34 = round(n() / n_distinct(codice_fiscale), 2),                       # Media gare (totali) per PA (sul totale delle PA nel dataset)
    ind35 = round(sum(ESITO == "AGGIUDICATA", na.rm = TRUE) / n_distinct(codice_fiscale), 2), # Numero medio di gare AGGIUDICATE per PA
    ind36 = round((ind33 / ind32) * 100, 2),                                  # % gare aggiudicate per PA
    
    ind37 = sum(importo_lotto, na.rm = TRUE),                                 # Importo lotto totale per PA
    ind38 = round(mean(importo_lotto, na.rm = TRUE), 2),                      # Media importo lotto per PA
    ind39 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind40 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                    # Media durata prevista per PA
    ind41 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,          # Tempo medio tra Scadenza e Esito
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind42 = round(mean(as.numeric(difftime(data_scadenza_offerta,             # Tempo medio tra Pubblicazione e Scadenza
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1)
  ) %>%
  arrange(codice_fiscale)  # Ordina per media gare per PA (decrescente)


indicators_by_pa_mese_fg <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione, mese_rif, desc_fg) %>%
  summarise(
    ind43 = n(),                                                              # Totale gare per PA
    ind44= sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                         # Totale gare aggiudicate per PA
    ind45 = round(n() / n_distinct(codice_fiscale), 2),                       # Media gare (totali) per PA (sul totale delle PA nel dataset)
    ind46 = round(sum(ESITO == "AGGIUDICATA", na.rm = TRUE) / n_distinct(codice_fiscale), 2), # Numero medio di gare AGGIUDICATE per PA
    ind47 = round((ind44 / ind43) * 100, 2),                                  # % gare aggiudicate per PA
    
    ind48 = sum(importo_lotto, na.rm = TRUE),                                 # Importo lotto totale per PA
    ind49 = round(mean(importo_lotto, na.rm = TRUE), 2),                      # Media importo lotto per PA
    ind50 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind51 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                    # Media durata prevista per PA
    ind52 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,          # Tempo medio tra Scadenza e Esito
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind53 = round(mean(as.numeric(difftime(data_scadenza_offerta,             # Tempo medio tra Pubblicazione e Scadenza
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1)
  ) %>%
  arrange(codice_fiscale)  # Ordina per media gare per PA (decrescente)

################################################################################
#                               Manipolazione
################################################################################

pa_uniche <- master_matched %>% 
  select(pa = codice_fiscale) %>% 
  distinct()

indicators_globale_final <- pa_uniche %>%
  cross_join(indicators_globale) 
indicators_globale_final <- subset(indicators_globale_final, 
                                   select = -totale_pa_lista_mpa)
ind01 <- indicators_globale_final %>%
  mutate(fil_anno = "2023") %>% 
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = NA_character_,     
    sub_fil_val = NA_character_,
    subsub_fil = NA_character_,     
    subsub_fil_val = NA_character_  
  ) %>%
  rename(fil_val = fil_anno) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)

################################################################################

indicators_by_pa_anno <- indicators_by_pa_anno %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione
  )
ind02 <- indicators_by_pa_anno %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = NA_character_,    
    sub_fil_val = NA_character_,
    subsub_fil = NA_character_,     
    subsub_fil_val = NA_character_  
  ) %>%
  rename(fil_val = fil_anno) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)
################################################################################

indicators_by_pa_mese <- indicators_by_pa_mese %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione,
    fil_mese = mese_rif
  )
ind03 <- indicators_by_pa_mese %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_mese",
    subsub_fil = NA_character_,     
    subsub_fil_val = NA_character_  
  ) %>%
  rename(
    fil_val = fil_anno,
    sub_fil_val = fil_mese
  ) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)
################################################################################


indicators_by_pa_anno_fg <- indicators_by_pa_anno_fg %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione,
    fil_fg = desc_fg
  )
ind04 <- indicators_by_pa_anno_fg %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_fg",
    subsub_fil = NA_character_,     
    subsub_fil_val = NA_character_  
  ) %>%
  rename(
    fil_val = fil_anno,
    sub_fil_val = fil_fg
  ) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)

################################################################################


indicators_by_pa_mese_fg <- indicators_by_pa_mese_fg %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione,
    fil_mese = mese_rif,
    fil_fg = desc_fg
  )
ind05 <- indicators_by_pa_mese_fg %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_mese",
    subsub_fil = "fil_fg",     
  ) %>%
  rename(
    fil_val = fil_anno,
    sub_fil_val = fil_mese,
    subsub_fil_val = fil_fg
  ) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)
#Append
db <- bind_rows(
  ind01, ind02, ind03, ind04, ind05
)

message("Dataset unico creato")

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("1orS5j-XxGi5_v_Cb0MfFyoXavAlPsqFH")
nome_file_output <- "INDICATORS_ANAC.json"
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
id_cartella_log_drive <- as_id("1sZo_8mL2qSMk50_qOoOb1nfk9Bu6KOn0") 
drive_upload(
  media = log_path,
  path = id_cartella_log_drive,
  name = log_filename
)

rm(list = ls())
