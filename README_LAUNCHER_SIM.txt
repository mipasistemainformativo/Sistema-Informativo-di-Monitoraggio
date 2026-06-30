README - Launcher SIM
=====================

DOVE METTERE QUESTI FILE
------------------------
Copia questi tre file nella ROOT del progetto, cioè nella stessa cartella in cui si trova:

06_render_dashboard_SIM_integrata.R

Esempio:

Monitoraggio-PNRR/
├── Apri_SIM.command        [Mac]
├── Apri_SIM.bat            [Windows]
├── run_SIM_dashboard.R
├── 06_render_dashboard_SIM_integrata.R
├── 01_Dataset/
├── 02_Metadata/
├── 03_Scripts/
├── 04_Output/
├── 05_Logs/
└── 06_Docs/

COME SI USA
-----------
Mac:
1. Doppio click su Apri_SIM.command.
2. Se macOS blocca il file, aprire Terminale nella root e lanciare:
   chmod +x Apri_SIM.command
3. Poi riprovare con doppio click.

Windows:
1. Doppio click su Apri_SIM.bat.

REQUISITI
---------
- R installato.
- Pacchetti R necessari.
- Connessione Internet.
- Accesso Google Drive ai file usati dal progetto.
- I file Rmd e gli script devono essere presenti nella cartella 03_Scripts.

COSA SUCCEDE
------------
Il launcher esegue:

Rscript run_SIM_dashboard.R

Questo file avvia lo script principale:

06_render_dashboard_SIM_integrata.R

Lo script scarica i dati da Drive, avvia le dashboard reattive e apre la Home del SIM nel browser.
