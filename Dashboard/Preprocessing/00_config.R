# ============================================================
# 00_config.R
# Configurazioni comuni del progetto SIM
# ============================================================

# ---------------------------------------------------------------------------
# Google Drive
# ---------------------------------------------------------------------------

DRIVE_ROOT_ID <- "14jMYmLq78M-0LxuaIBAGao16ZhF59xDc"
SIM_DRIVE_EMAIL <- "mipa.sistemainformativo@gmail.com"

# ---------------------------------------------------------------------------
# Struttura stabile delle cartelle Drive
#
# In questo file vanno definiti soltanto i percorsi stabili e condivisi.
# I percorsi dipendenti da RUN_ID restano nei singoli script.
# ---------------------------------------------------------------------------

DRIVE_DIR_DATASET <- "01_Dataset"
DRIVE_DIR_METADATA <- "02_Metadata"
DRIVE_DIR_OUTPUT <- "04_Output"
DRIVE_DIR_LOGS <- "05_Logs"
DRIVE_DIR_DOCS <- "06_Docs"

# Dataset
DRIVE_DIR_SOURCE <- file.path(DRIVE_DIR_DATASET, "Source")
DRIVE_DIR_PROCESSED <- file.path(DRIVE_DIR_DATASET, "Processed")
DRIVE_DIR_INDICATORS <- file.path(DRIVE_DIR_DATASET, "Indicators")
DRIVE_DIR_LISTS <- file.path(DRIVE_DIR_DATASET, "Lists")

# Metadati
DRIVE_DIR_SOURCE_MET <- file.path(DRIVE_DIR_METADATA, "Source_met")
DRIVE_DIR_INDICATORS_MET <- file.path(DRIVE_DIR_METADATA, "Indicators_met")
DRIVE_DIR_CLASSIFICATION_MET <- file.path(DRIVE_DIR_METADATA, "Classification_met")

# Alias temporanei per compatibilità con script meno recenti.
# Rimuoverli quando tutti gli script useranno i nomi canonici sopra.
DRIVE_DIR_INDICATORI <- DRIVE_DIR_INDICATORS
DIR_SOURCE_MET <- DRIVE_DIR_SOURCE_MET

# ---------------------------------------------------------------------------
# Cartelle stabili per fonte
# ---------------------------------------------------------------------------

# PA Digitale 2026
DRIVE_DIR_SOURCE_PAD26 <- file.path(DRIVE_DIR_SOURCE, "PADigitale2026")
DRIVE_DIR_PROCESSED_PAD26 <- file.path(DRIVE_DIR_PROCESSED, "PADigitale2026")
DRIVE_DIR_INDICATORS_PAD26 <- file.path(DRIVE_DIR_INDICATORS, "PADigitale2026")
DRIVE_DIR_SOURCE_MET_PAD26 <- file.path(DRIVE_DIR_SOURCE_MET, "PADigitale2026")
DRIVE_DIR_INDICATORS_MET_PAD26 <- file.path(DRIVE_DIR_INDICATORS_MET, "PADigitale2026")
DRIVE_DIR_OUTPUT_PAD26 <- file.path(DRIVE_DIR_OUTPUT, "PADigitale2026")
DRIVE_DIR_LOGS_PAD26 <- file.path(DRIVE_DIR_LOGS, "PADigitale2026")

# Conto annuale
DRIVE_DIR_PROCESSED_CONTO_ANNUALE <- file.path(
  DRIVE_DIR_PROCESSED,
  "Conto_annuale"
)

# ANAC
DRIVE_DIR_SOURCE_ANAC <- file.path(DRIVE_DIR_SOURCE, "ANAC")
DRIVE_DIR_SOURCE_ANAC_GIC2023 <- file.path(
  DRIVE_DIR_SOURCE_ANAC,
  "GIC 2023"
)
DRIVE_DIR_PROCESSED_ANAC <- file.path(DRIVE_DIR_PROCESSED, "ANAC")
DRIVE_DIR_PROCESSED_ANAC_CIG <- file.path(
  DRIVE_DIR_PROCESSED_ANAC,
  "GIC"
)
DRIVE_DIR_INDICATORS_ANAC <- file.path(DRIVE_DIR_INDICATORS, "ANAC")
DRIVE_DIR_SOURCE_MET_ANAC <- file.path(DRIVE_DIR_SOURCE_MET, "ANAC")
DRIVE_DIR_INDICATORS_MET_ANAC <- file.path(DRIVE_DIR_INDICATORS_MET, "ANAC")
DRIVE_DIR_OUTPUT_ANAC <- file.path(DRIVE_DIR_OUTPUT, "ANAC")
DRIVE_DIR_LOGS_ANAC <- file.path(DRIVE_DIR_LOGS, "ANAC")

#PagoPA
DRIVE_DIR_INDICATORS_PAGOPA <- file.path(DRIVE_DIR_INDICATORS, "PagoPA")
DRIVE_DIR_CLASSIFICATION_MET_PAGOPA <- file.path(DRIVE_DIR_INDICATORS_MET, "PagoPA")



# Master list SIM
DRIVE_FILE_LISTA_RACCORDO_SIM_XLSX <- file.path(
  DRIVE_DIR_LISTS,
  "Lista_raccordo_SIM.xlsx"
)

DRIVE_FILE_LISTA_RACCORDO_SIM_RDS <- file.path(
  DRIVE_DIR_LISTS,
  "Lista_raccordo_SIM.rds"
)

DRIVE_FILE_LISTA_RACCORDO_SIM_JSON <- file.path(
  DRIVE_DIR_LISTS,
  "Lista_raccordo_SIM.json"
)

# Formato operativo predefinito.
DRIVE_FILE_LISTA_RACCORDO_SIM <-
  DRIVE_FILE_LISTA_RACCORDO_SIM_RDS

# ---------------------------------------------------------------------------
# Cache locale
# ---------------------------------------------------------------------------

DIR_TEMP <- "07_Temp"
dir.create(DIR_TEMP, recursive = TRUE, showWarnings = FALSE)
