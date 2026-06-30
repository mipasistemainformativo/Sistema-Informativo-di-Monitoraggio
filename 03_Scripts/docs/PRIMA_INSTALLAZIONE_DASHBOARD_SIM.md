# Prima installazione della dashboard SIM

Questa guida è pensata per chi non ha mai usato Git, R o RStudio.

La procedura completa si esegue una sola volta. Dopo la configurazione iniziale, per usare la dashboard sarà sufficiente aprire il progetto, aggiornare i file e lanciare un comando.

## Che cosa installeremo

- **R**: esegue il codice.
- **RStudio**: è il programma grafico con cui aprire il progetto.
- **Git**: scarica e aggiorna la repository.
- **Repository**: è la cartella del progetto salvata sul computer.
- **Pacchetti R**: sono componenti aggiuntivi usati dalla dashboard.

## Prima di iniziare

Servono:

- Windows o macOS;
- connessione Internet;
- browser;
- spazio disponibile sul disco;
- facoltativamente, un account Google per l’autorizzazione OAuth.

La repository è pubblica: **non serve un account GitHub per scaricarla**.

La cartella pubblica dei dati è:

https://drive.google.com/drive/folders/14jMYmLq78M-0LxuaIBAGao16ZhF59xDc?usp=sharing

Aprire il link e verificare che i file siano visibili.

# Parte 1 — Installare i programmi

## 1. Installare R

Aprire:

https://cran.r-project.org/

### Windows

1. Fare clic su **Download R for Windows**.
2. Fare clic su **base**.
3. Scaricare la versione proposta.
4. Aprire il file.
5. Accettare le impostazioni predefinite.
6. Terminare l’installazione.

### macOS

1. Fare clic su **Download R for macOS**.
2. Scaricare il file `.pkg` adatto al proprio Mac.
3. Aprire il file.
4. Seguire la procedura guidata.
5. Terminare l’installazione.

## 2. Installare RStudio Desktop

Aprire:

https://posit.co/download/rstudio-desktop/

1. Scaricare la versione gratuita per il proprio sistema.
2. Aprire il file.
3. Completare l’installazione.
4. Avviare RStudio.

RStudio dovrebbe riconoscere automaticamente R.

## 3. Installare Git

Aprire:

https://git-scm.com/install/

### Windows

1. Scaricare Git for Windows.
2. Aprire il programma di installazione.
3. Mantenere le opzioni predefinite.
4. Completare.
5. Chiudere e riaprire RStudio.

### macOS

1. Aprire:

```text
https://git-scm.com/install/mac
```

2. Seguire una delle modalità di installazione indicate sul sito.
3. Al termine, aprire **Terminale** e verificare:

```bash
git --version
```

Se appare un numero di versione, Git è installato correttamente.

Se GitHub o RStudio chiedono di effettuare l’accesso, scegliere preferibilmente **Sign in via browser**.

Se nel browser compare la richiesta **Authorize git-ecosystem**, è possibile autorizzarla per completare l’accesso GitHub. L’opzione tramite token è destinata a utenti più esperti e non è necessaria per il normale utilizzo della dashboard.

## 4. Verificare Git in RStudio

In RStudio:

```text
Tools → Global Options → Git/SVN
```

Controllare che **Git executable** contenga un percorso.

Se è vuoto, chiudere RStudio, verificare l’installazione di Git e riaprire.

# Parte 2 — Scaricare la repository

## 5. Creare una cartella di lavoro

Percorsi consigliati:

```text
macOS:    ~/Projects
Windows:  C:\Projects
```

Evitare, quando possibile, Desktop, Documenti, OneDrive o iCloud.

## 6. Clonare da RStudio

In RStudio:

```text
File → New Project → Version Control → Git
```

Nel campo **Repository URL** inserire:

```text
https://github.com/gaiascarponi/Monitoraggio-PNRR.git
```

Nel campo **Project directory name** deve comparire:

```text
Monitoraggio-PNRR
```

Nel campo **Create project as subdirectory of** scegliere la cartella `Projects`.

Premere **Create Project**.

RStudio scaricherà la repository e aprirà il progetto.

### Se “Version Control” non compare

Git non è stato riconosciuto. Chiudere RStudio, controllare Git e ripetere.

## 7. Verificare il progetto

In alto a destra dovrebbe comparire:

```text
Monitoraggio-PNRR
```

Nella scheda **Files** dovrebbero esserci:

```text
03_Scripts
README.md
Monitoraggio-PNRR.Rproj
```

Nella Console R eseguire:

```r
getwd()
```

Il percorso deve terminare con `Monitoraggio-PNRR`.

# Parte 3 — Installare i pacchetti R

## 8. Usare la Console corretta

La **Console R** si trova normalmente in basso a sinistra.

Il simbolo:

```text
>
```

indica dove incollare i comandi.

Non incollare i comandi R nel Terminale.

## 9. Installare i pacchetti

Copiare tutto il blocco, incollarlo nella Console e premere Invio:

```r
install.packages(c(
  "callr",
  "googledrive",
  "rmarkdown",
  "flexdashboard",
  "shiny",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "readxl",
  "jsonlite",
  "tibble",
  "plotly",
  "DT",
  "leaflet",
  "sf",
  "htmltools",
  "janitor",
  "openxlsx",
  "purrr",
  "lubridate"
))
```

L’installazione può richiedere alcuni minuti. Non chiudere RStudio e attendere che ricompaia `>`.

Questa operazione si esegue una sola volta.

Se l’installazione di `sf` fallisce, salvare il messaggio di errore e chiedere supporto.

## 10. Verificare i pacchetti

Eseguire:

```r
library(googledrive)
library(rmarkdown)
library(shiny)
library(flexdashboard)
```

Se non appare un errore, i pacchetti principali sono disponibili.

# Parte 4 — Preparare Google Drive

## 11. Verificare la cartella pubblica

Aprire:

https://drive.google.com/drive/folders/14jMYmLq78M-0LxuaIBAGao16ZhF59xDc?usp=sharing

La cartella deve essere leggibile senza una condivisione nominativa.

## 12. Autorizzazione OAuth

La cartella è pubblica, ma il pacchetto `googledrive` può chiedere un’autorizzazione per utilizzare le API.

L’autorizzazione:

- permette a R di leggere i file;
- non cambia i permessi della cartella;
- non concede diritti di modifica.

Nella Console eseguire:

```r
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive.readonly"
)
```

Nel browser:

1. scegliere un proprio account Google;
2. leggere la richiesta;
3. confermare;
4. tornare a RStudio;
5. attendere che ricompaia `>`.


### Salvare l’autorizzazione tra le sessioni

Durante la prima autenticazione, R può chiedere:

```text
Is it OK to cache OAuth access credentials in the folder
... between R sessions?
```

Su un computer personale, digitare:

```text
1
```

e premere Invio. In questo modo l’autorizzazione viene conservata localmente e non deve essere ripetuta a ogni avvio.

Su un computer pubblico o condiviso, scegliere invece `2`.

La cartella della cache OAuth è locale e non deve essere aggiunta a Git o pubblicata nella repository.


Per cambiare account:

```r
googledrive::drive_deauth()
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive.readonly"
)
```

# Parte 5 — Primo avvio

## 13. Controllare il progetto

Verificare che in alto a destra compaia `Monitoraggio-PNRR`.

Eseguire:

```r
getwd()
```

## 14. Avviare la dashboard

Nella Console R:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

Non usare **Run Document** sui file `.Rmd`.

Durante l’avvio il runner:

1. controlla la configurazione;
2. accede a Drive;
3. scarica gli input;
4. avvia le dashboard figlie;
5. avvia la shell principale;
6. apre il browser.

## 15. Aprire la dashboard

La dashboard dovrebbe aprirsi automaticamente.

In alternativa:

```text
http://127.0.0.1:8010
```

Porte delle dashboard figlie:

```text
Conto annuale       http://127.0.0.1:8011
PA Digitale 2026    http://127.0.0.1:8012
```

## 16. Fermare la dashboard

Tornare a RStudio e premere il pulsante rosso **Stop** nella Console.

# Parte 6 — Utilizzi successivi

Dopo la prima installazione:

1. aprire `Monitoraggio-PNRR.Rproj`;
2. aprire la scheda **Git**;
3. premere **Pull**;
4. attendere;
5. aprire la Console;
6. eseguire:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

# Problemi comuni

## La scheda Git non compare

Git non è installato o RStudio non lo riconosce.

## Git Pull segnala modifiche locali

Non forzare e non cancellare file senza averli controllati. Inviare uno screenshot al referente.

## `source(...)` dice che il file non esiste

Il progetto corretto non è aperto. Aprire `Monitoraggio-PNRR.Rproj`.

## Il browser non si apre

Aprire manualmente `http://127.0.0.1:8010`.

## Google mostra l’account sbagliato

Eseguire `googledrive::drive_deauth()` e ripetere l’autorizzazione.

## File non trovato su Drive

Controllare che la cartella pubblica si apra. Poi inviare il log più recente:

```text
07_Temp/SIM/Dashboard/<RUN_ID>/logs/
```

## Porta già occupata

Chiudere le precedenti sessioni RStudio e rilanciare.

# Checklist finale

- [ ] R installato
- [ ] RStudio installato
- [ ] Git installato
- [ ] repository clonata
- [ ] progetto aperto
- [ ] pacchetti installati
- [ ] cartella Drive visibile
- [ ] OAuth completato, se richiesto
- [ ] runner avviato dalla Console
- [ ] dashboard aperta su porta 8010
