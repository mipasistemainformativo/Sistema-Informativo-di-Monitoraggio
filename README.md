# Sistema Informativo di Monitoraggio

Repository ufficiale del progetto **Sistema Informativo di Monitoraggio PA (SIM)**. 

L'ecosistema fornisce una visione integrata dei dati provenienti da diverse fonti nazionali (ANAC, PagoPA, Conto Annuale, PA Digitale 2026) per monitorare l'evoluzione digitale e amministrativa degli enti nel perimetro MPA.

## 🚀 Accesso Rapido 

Per una consultazione immediata senza installare software, le dashboard sono accessibili al seguente link: [Sistema Informativo di Monitoraggio]("https://mipasistemainformativo.shinyapps.io/sistemainformativo/")

La guida copre l'installazione di R, RStudio, Git e la configurazione delle librerie necessarie 

## 🛠️ Installazione e Sviluppo Locale

### Prima installazione

Se è la prima volta che utilizzi il progetto sul tuo computer, consulta il [Manuale utende Dashboard SIM (PDF)](/Manuale%20Utente%20Dashboard%20SIM.pdf)

La guida spiega come installare R, RStudio e Git, clonare la repository pubblica, installare i pacchetti R e avviare la dashboard per la prima volta.

### Avvio rapido — dopo la prima installazione

Questi passaggi valgono quando la repository è già presente e configurata sul computer.

1. Aprire `Monitoraggio.Rproj` con RStudio.
2. Aggiornare la copia locale con **Git → Pull**.
3. Nella **Console R** eseguire:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

4. Completare l’eventuale autorizzazione Google.
5. Attendere l’apertura di:

```text
http://127.0.0.1:8010
```

## Dati

Gli input elaborati della dashboard sono disponibili in sola visualizzazione:

https://drive.google.com/drive/folders/14jMYmLq78M-0LxuaIBAGao16ZhF59xDc?usp=sharing

Le fonti originali, le licenze e le trasformazioni sono documentate in [`DATA_SOURCES.md`](DATA_SOURCES.md).

## Documentazione tecnica

La guida tecnica si trova in:

```text
03_Scripts/SIM/GUIDA_DASHBOARD_SIM.md
```

## Struttura essenziale

```text
Sistema-Informativo-di-Monitoraggio/
├── Monitoraggio.Rproj
├── README.md
├── DATA_SOURCES.md
├── docs/
│   └── PRIMA_INSTALLAZIONE_DASHBOARD_SIM.md
└── 03_Scripts/
    ├── 00_config.R
    ├── 00_drive_helpers.R
    ├── 00_spatial_helpers.R
    ├── helper_console_log.R
    ├── 06_render_dashboard_SIM_integrata.R
    ├── SIM/
    │   ├── 06_dashboard_SIM_integrata.Rmd
    │   └── GUIDA_DASHBOARD_SIM.md
    ├── Conto_annuale/
    │   └── 05_dashboard_SIM_ContoAnnuale.Rmd
    └── PAdigitale2026/
        └── 05_dashboard_SIM_PADigitale2026.Rmd
```

## Requisiti

- R;
- RStudio Desktop;
- Git;
- connessione Internet;
- browser;
- pacchetti R indicati nella guida di prima installazione.

Un account GitHub non è necessario per scaricare una repository pubblica. Serve solo per collaborare su GitHub.

## Regole essenziali

- Non versionare dataset, log, cache o output generati.
- Non inserire password, token o credenziali.
- Non usare **Run Document** sui file `.Rmd` per il normale avvio.
- Non duplicare i path definiti in `03_Scripts/00_config.R`.
- Evitare copie `_old`, `_copy` o `_fullscreen`.
- Controllare sempre che commit e log non contengano dati personali o informazioni riservate.

## Assistenza

In caso di errore, consultare:

```text
07_Temp/SIM/Dashboard/<RUN_ID>/logs/
```

e la guida tecnica in `03_Scripts/SIM/GUIDA_DASHBOARD_SIM.md`.
