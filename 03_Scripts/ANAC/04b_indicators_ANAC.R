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
#                 INDICATORS_CPV_ANNO & INDICATORS_CPV_MESE
################################################################################
pa_totali_n <- 10179
 
indicators_by_cpv_anno <- master_matched %>%
  group_by(cpv_cat, anno_pubblicazione) %>%
  summarise(
    ind1 = n(),                                                                 # Totale gare nel settore
    ind2 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                           # Totale gare aggiudicate nel settore
    ind3 = round((ind2 / ind1) * 100, 2),                                       # % gare aggiudicate sul totale settore (Richiesto)
    ind4 = n_distinct(codice_fiscale),                                          # Numero di PA diverse che comprano in questo settore
    ind5 = round((ind4 / pa_totali_n) * 100, 2),                                # % di PA che acquistano questo CPV sul totale delle PA
    ind6 = round(n() / n_distinct(codice_fiscale), 2),                          # Media gare per PA in questo settore
    
    ind7 = sum(importo_lotto, na.rm = TRUE),                                    # Importo lotto per settore
    ind8 = round(mean(importo_lotto, na.rm = TRUE), 2),                         # Valore medio di un appalto nel settore
    ind9 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind10 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                       # Durata media contratti nel settore
    
    ind11 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,             # Tempo medio tra Scadenza e Esito (Efficienza PA)
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind12 = round(mean(as.numeric(difftime(data_scadenza_offerta,                # Tempo medio tra Pubblicazione e Scadenza (Tempo per offerte)
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1),
    .groups = "drop_last" 
  ) %>%
  
  ungroup() %>%
  arrange(cpv_cat, anno_pubblicazione)

indicators_by_cpv_mese <- master_matched %>%
  group_by(cpv_cat, anno_pubblicazione, mese_rif) %>%
  summarise(
    ind13 = n(),                                                                 # Totale gare nel settore
    ind14 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                           # Totale gare aggiudicate nel settore
    ind15 = round((ind14 / ind13) * 100, 2),                                       # % gare aggiudicate sul totale settore (Richiesto)
    
    ind16 = n_distinct(codice_fiscale),                                          # Numero di PA diverse che comprano in questo settore
    ind17 = round((ind16 / pa_totali_n) * 100, 2),                                # % di PA che acquistano questo CPV sul totale delle PA
    ind18 = round(n() / n_distinct(codice_fiscale), 2),                          # Media gare per PA in questo settore
    
    ind19 = sum(importo_lotto, na.rm = TRUE),                                    # Importo lotto per settore
    ind20 = round(mean(importo_lotto, na.rm = TRUE), 2),                         # Valore medio di un appalto nel settore
    ind21 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind22 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                       # Durata media contratti nel settore
    
    ind23 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,             # Tempo medio tra Scadenza e Esito (Efficienza PA)
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind24 = round(mean(as.numeric(difftime(data_scadenza_offerta,                # Tempo medio tra Pubblicazione e Scadenza (Tempo per offerte)
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1),
    .groups = "drop_last" 
  ) %>%
  ungroup() %>%
  arrange(cpv_cat, anno_pubblicazione)


################################################################################
#         INDICATORS_CPV_ANNO & INDICATORS_CPV_MESE (forma giuridica)
################################################################################

indicators_by_cpv_anno_fg <- master_matched %>%
  group_by(cpv_cat, anno_pubblicazione, desc_fg) %>%
  summarise(
    ind25 = n(),                                                                 # Totale gare nel settore
    ind26 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                           # Totale gare aggiudicate nel settore
    ind27 = round((ind26 / ind25) * 100, 2),                                       # % gare aggiudicate sul totale settore (Richiesto)
    ind28 = n_distinct(codice_fiscale),                                          # Numero di PA diverse che comprano in questo settore
    ind29 = round((ind28 / pa_totali_n) * 100, 2),                                # % di PA che acquistano questo CPV sul totale delle PA
    ind30 = round(n() / n_distinct(codice_fiscale), 2),                          # Media gare per PA in questo settore
    
    ind31 = sum(importo_lotto, na.rm = TRUE),                                    # Importo lotto per settore
    ind32 = round(mean(importo_lotto, na.rm = TRUE), 2),                         # Valore medio di un appalto nel settore
    ind33 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind34 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                       # Durata media contratti nel settore
    
    ind35 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,             # Tempo medio tra Scadenza e Esito (Efficienza PA)
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind36 = round(mean(as.numeric(difftime(data_scadenza_offerta,                # Tempo medio tra Pubblicazione e Scadenza (Tempo per offerte)
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1),
    .groups = "drop_last" 
  ) %>%
  
  ungroup() %>%
  arrange(cpv_cat, anno_pubblicazione)

indicators_by_cpv_mese_fg <- master_matched %>%
  group_by(cpv_cat, anno_pubblicazione, mese_rif, desc_fg) %>%
  summarise(
    ind37 = n(),                                                                 # Totale gare nel settore
    ind38 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),                           # Totale gare aggiudicate nel settore
    ind39 = round((ind38 / ind37) * 100, 2),                                       # % gare aggiudicate sul totale settore (Richiesto)
    
    ind40 = n_distinct(codice_fiscale),                                          # Numero di PA diverse che comprano in questo settore
    ind41 = round((ind40 / pa_totali_n) * 100, 2),                                # % di PA che acquistano questo CPV sul totale delle PA
    ind42 = round(n() / n_distinct(codice_fiscale), 2),                          # Media gare per PA in questo settore
    
    ind43 = sum(importo_lotto, na.rm = TRUE),                                    # Importo lotto per settore
    ind44 = round(mean(importo_lotto, na.rm = TRUE), 2),                         # Valore medio di un appalto nel settore
    ind45 = round(weighted.mean(importo_lotto, w = DURATA_PREVISTA, na.rm = TRUE), 2), # Media ponderata per durata
    ind46 = round(mean(DURATA_PREVISTA, na.rm = TRUE), 1),                       # Durata media contratti nel settore
    
    ind47 = round(mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,             # Tempo medio tra Scadenza e Esito (Efficienza PA)
                                           data_scadenza_offerta, 
                                           units = "days")), na.rm = TRUE), 1),
    ind48 = round(mean(as.numeric(difftime(data_scadenza_offerta,                # Tempo medio tra Pubblicazione e Scadenza (Tempo per offerte)
                                           data_pubblicazione,  
                                           units = "days")), na.rm = TRUE), 1),
    .groups = "drop_last" 
  ) %>%
  ungroup() %>%
  arrange(cpv_cat, anno_pubblicazione)

################################################################################
#                               Manipolazione
################################################################################

cpv_uniche <- master_matched %>% 
  select(cpv = cpv_cat) %>% 
  distinct()

indicators_by_cpv_anno <- indicators_by_cpv_anno %>%
  rename(
    cpv = cpv_cat,
    fil_anno = anno_pubblicazione
  )
ind01 <- indicators_by_cpv_anno %>%
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
  select(cpv, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)
################################################################################

indicators_by_cpv_mese <- indicators_by_cpv_mese %>%
  rename(
    cpv = cpv_cat,
    fil_mese = mese_rif,
    fil_anno = anno_pubblicazione
  )
ind02 <- indicators_by_cpv_mese %>%
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
  select(cpv, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)


################################################################################


indicators_by_cpv_anno_fg <- indicators_by_cpv_anno_fg %>%
  rename(
    cpv = cpv_cat,
    fil_anno = anno_pubblicazione,
    fil_fg = desc_fg
  )
ind03 <- indicators_by_cpv_anno_fg %>%
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
  select(cpv, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)

################################################################################

indicators_by_cpv_mese_fg <- indicators_by_cpv_mese_fg %>%
  rename(
    cpv = cpv_cat,
    fil_mese = mese_rif,
    fil_anno = anno_pubblicazione,
    fil_fg = desc_fg
  )
ind04 <- indicators_by_cpv_mese_fg %>%
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
  select(cpv, fil, fil_val, sub_fil, sub_fil_val, subsub_fil, subsub_fil_val, ind, ind_val)


#Append
db <- bind_rows(
  ind01, ind02, ind03, ind04
)

message("Dataset unico creato")

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("1orS5j-XxGi5_v_Cb0MfFyoXavAlPsqFH")
nome_file_output <- "INDICATORS_CPV_ANAC.json"
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

