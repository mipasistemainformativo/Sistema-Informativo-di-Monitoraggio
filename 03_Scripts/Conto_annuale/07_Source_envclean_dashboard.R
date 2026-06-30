source("03_Scripts/Conto_annuale/01_render_claude.R",
       echo = TRUE,
       max.deparse.length = Inf
)


# 1. Pulisci cache knitr locale (cartella del progetto)
unlink("03_Scripts/Conto_annuale/05_dashboard_SIM_CA_claude3_cache", recursive = TRUE)
unlink("03_Scripts/Conto_annuale/05_dashboard_SIM_CA_claude2_cache", recursive = TRUE)
unlink("03_Scripts/Conto_annuale/05_dashboard_SIM_CA_CLAUDE1_cache", recursive = TRUE)

# 2. Pulisci tutti i temp di rmarkdown/knitr
tmp <- tempdir()
cat("Temp dir:", tmp, "\n")
old <- list.files(tmp, pattern = "claude3|dashboard_SIM", 
                  recursive = TRUE, full.names = TRUE)
cat("File da rimuovere:\n"); cat(old, sep = "\n")
unlink(old)

# 3. Pulisci cartella temp dashboard nel progetto
unlink("07_Temp/Conto_annuale/Dashboard", recursive = TRUE)

# 4. Riavvia R completamente
.rs.restartR()  # oppure Session → Restart R in RStudio


rm(list = ls(all.names = TRUE))
gc()

try(shiny::stopApp(), silent = TRUE)

unlink(
  "07_Temp/Conto_annuale/Dashboard",
  recursive = TRUE,
  force = TRUE
)

unlink(
  list.files(
    "03_Scripts/Conto_annuale",
    pattern = "_cache$",
    full.names = TRUE
  ),
  recursive = TRUE,
  force = TRUE
)

unlink(
  list.files(
    tempdir(),
    pattern = "dashboard|Dashboard|CLAUDE|Conto",
    recursive = TRUE,
    full.names = TRUE
  ),
  recursive = TRUE,
  force = TRUE
)

.rs.restartR()
