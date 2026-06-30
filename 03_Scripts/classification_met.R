################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(purrr)
library(tidyverse)
library(readxl)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

################################################################################
#                       IMPORT Lista di Raccordo
################################################################################
file_raccordo <- drive_ls(as_id("15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs")) %>% 
  filter(name == "Lista_raccordo_SIM.xlsx")

nome_variabile <- file_raccordo$name %>% 
  str_remove("\\.xlsx$") %>% 
  str_replace_all("[\\s-]+", "_")

percorso_file <- file.path("07_Temp", file_raccordo$name)
drive_download(as_id(file_raccordo$id), path = percorso_file, overwrite = TRUE, verbose = FALSE)
dataset <- read_excel(percorso_file)
assign(nome_variabile, dataset, envir = .GlobalEnv)
rm(file_raccordo, nome_variabile, percorso_file, dataset)

################################################################################
#                         Classification_met
################################################################################
regioni01 <- Lista_raccordo_SIM %>% 
  select(codice_reg, reg=regione_bdap) %>% 
  drop_na() %>% 
  distinct()

differenze <- Lista_raccordo_SIM %>% 
  filter(codice_reg != codice_regione_bdap)

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("1QJXViD9ilV0VJ2n7r7RHX93z5c2IV9Oy")

dataset_da_caricare <- c(
  "regioni01"
)

walk(dataset_da_caricare, function(nome_dataset) {
  dati <- get(nome_dataset)
  path_temp <- file.path("07_Temp", "fil_reg.rds")
  saveRDS(dati, file = path_temp)
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = "fil_reg.rds", 
    overwrite = TRUE
  )
  unlink(path_temp)
})

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

file_xlsx <- list.files(
  path = "07_Temp",
  pattern = "\\.xlsx$",
  full.names = TRUE
)
file.remove(file_xlsx)