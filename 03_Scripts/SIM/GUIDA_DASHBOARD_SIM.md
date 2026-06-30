# Dashboard SIM — Guida operativa

## 1. Avvio corretto

Il punto di ingresso è:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

I file `.Rmd` non vanno eseguiti direttamente durante il normale utilizzo.

## 2. Architettura

```text
Google Drive pubblico in lettura
        ↓
07_Temp/SIM/Dashboard/<RUN_ID>/input/
        ├── Conto annuale      http://127.0.0.1:8011
        ├── PA Digitale 2026   http://127.0.0.1:8012
        ↓
Shell SIM                     http://127.0.0.1:8010
```

Il runner carica configurazione e helper, accede a Drive, scarica gli input, crea la cache locale, avvia le dashboard figlie e infine la shell principale.

## 3. Accesso a Google Drive

La cartella è pubblica in sola visualizzazione:

https://drive.google.com/drive/folders/14jMYmLq78M-0LxuaIBAGao16ZhF59xDc?usp=sharing

Non è necessaria una condivisione nominativa.

### Autorizzazione OAuth

Anche se i file sono pubblici, `googledrive` può richiedere un’autorizzazione per utilizzare le API.

Quando si apre il browser:

1. scegliere un proprio account Google;
2. approvare la richiesta;
3. tornare a RStudio.

L’autorizzazione non modifica i permessi della cartella.

Per avviare manualmente l’accesso in sola lettura:

```r
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive.readonly"
)
```

Per cambiare account:

```r
googledrive::drive_deauth()
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive.readonly"
)
```

Gli script che caricano output su Drive richiedono permessi diversi e non rientrano nelle istruzioni per il semplice uso della dashboard.

## 4. File principali

```text
03_Scripts/06_render_dashboard_SIM_integrata.R
03_Scripts/SIM/06_dashboard_SIM_integrata.Rmd
03_Scripts/Conto_annuale/05_dashboard_SIM_ContoAnnuale.Rmd
03_Scripts/PAdigitale2026/05_dashboard_SIM_PADigitale2026.Rmd
```

Helper:

```text
03_Scripts/00_config.R
03_Scripts/00_drive_helpers.R
03_Scripts/00_spatial_helpers.R
03_Scripts/helper_console_log.R
```

## 5. Dove modificare

- **Home e Perimetro PA:** `03_Scripts/SIM/06_dashboard_SIM_integrata.Rmd`
- **Conto annuale:** `03_Scripts/Conto_annuale/05_dashboard_SIM_ContoAnnuale.Rmd`
- **PA Digitale 2026:** `03_Scripts/PAdigitale2026/05_dashboard_SIM_PADigitale2026.Rmd`
- **Input, parametri, porte e orchestrazione:** `03_Scripts/06_render_dashboard_SIM_integrata.R`

Il runner non deve contenere logica analitica o grafica.

## 6. Avvio operativo

Questa procedura presuppone che la prima installazione sia già stata completata.

1. Aprire `Monitoraggio-PNRR.Rproj`.
2. Fare **Git → Pull**.
3. Aprire la **Console**.
4. Eseguire:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

5. Completare l’eventuale OAuth.
6. Attendere l’apertura del browser.

## 7. Input Drive

```text
01_Dataset/Lists/Lista_raccordo_SIM.rds
01_Dataset/Processed/Conto_annuale/<RUN_ID>/master_CA_multianno.rds
01_Dataset/Indicators/PADigitale2026/<RUN_ID>/
02_Metadata/Indicators_met/PADigitale2026/<RUN_ID>/
```

## 8. Cache e log

```text
07_Temp/SIM/Dashboard/<RUN_ID>/
├── input/
└── logs/
```

Log principali:

```text
06_render_dashboard_SIM_integrata.<RUN_ID>.log
conto_annuale_stdout.log
conto_annuale_stderr.log
padigitale_stdout.log
padigitale_stderr.log
```

## 9. Pacchetti R

```r
install.packages(c(
  "callr", "googledrive", "rmarkdown", "flexdashboard", "shiny",
  "dplyr", "tidyr", "stringr", "readr", "readxl", "jsonlite",
  "tibble", "plotly", "DT", "leaflet", "sf", "htmltools",
  "janitor", "openxlsx", "purrr", "lubridate"
))
```

## 10. Errori frequenti

### Accesso Drive non riuscito

- verificare che il link pubblico si apra;
- eseguire `drive_deauth()`;
- ripetere `drive_auth()` in sola lettura;
- rilanciare il runner.

### File non trovato

Controllare path, nome, cartella `RUN_ID` e disponibilità dell’output su Drive.

### `render params not declared in YAML`

Il runner passa un parametro non dichiarato nello YAML del file `.Rmd`.

### Porta occupata

Chiudere le precedenti sessioni R o RStudio e rilanciare.

## 11. Regole per la repository pubblica

- Non inserire token, password o credenziali.
- Non versionare `.Renviron`, `.Rhistory`, `.RData` o cache OAuth.
- Non pubblicare log con informazioni personali.
- Pubblicare solo open data autorizzati.
- Documentare fonti e licenze in `DATA_SOURCES.md`.
- Verificare sempre `git diff` prima del commit.
