# Eseguire questo script solo quando cambiano i dati sorgente ANAC.
# Genera un file compatto per la dashboard, evitando di caricare il JSON completo online.

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

df_anac_raw <- fromJSON("data/INDICATORS_ANAC.json") %>%
  mutate(ind_val = as.numeric(ind_val))

mappatura_regioni <- readRDS("data/Lista_raccordo_SIM.rds") %>%
  select(codice_fiscale, codice_reg, regione_bdap) %>%
  distinct()

df_anac_nat <- df_anac_raw %>%
  filter(ind %in% paste0("ind", 1:9), fil == "fil_anno") %>%
  group_by(fil_val, ind) %>%
  summarise(val_nazionale = mean(ind_val, na.rm = TRUE), .groups = "drop")

nomi_regioni_univoci <- mappatura_regioni %>%
  filter(!is.na(regione_bdap)) %>%
  select(codice_reg, regione_bdap) %>%
  distinct(codice_reg, .keep_all = TRUE)

lista_fg <- df_anac_raw %>%
  filter(!is.na(subsub_fil_val), subsub_fil == "fil_fg") %>%
  pull(subsub_fil_val) %>%
  unique() %>%
  sort()

df_pa_annuale <- df_anac_raw %>%
  filter(ind %in% c("ind10", "ind11", "ind15", "ind18", "ind19", "ind20"), fil == "fil_anno") %>%
  left_join(mappatura_regioni %>% select(codice_fiscale, codice_reg), by = c("pa" = "codice_fiscale")) %>%
  filter(!is.na(codice_reg)) %>%
  mutate(codice_reg = sprintf("%02d", as.numeric(codice_reg))) %>%
  select(codice_reg, fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_reg_annuale <- df_pa_annuale %>%
  group_by(codice_reg, fil_val) %>%
  summarise(
    ind10 = sum(ind10, na.rm = TRUE), ind11 = sum(ind11, na.rm = TRUE), ind15 = sum(ind15, na.rm = TRUE),
    ind18 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind18 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    ind19 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind19 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    ind20 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind20 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(ind14 = if_else(ind10 > 0, ind11 / ind10 * 100, NA_real_), ind16 = if_else(ind10 > 0, ind15 / ind10, NA_real_)) %>%
  pivot_longer(cols = starts_with("ind"), names_to = "ind", values_to = "ind_val") %>%
  mutate(sub_fil_val = NA_character_)

df_pa_mensile <- df_anac_raw %>%
  filter(ind %in% c("ind21", "ind22", "ind26", "ind29", "ind30", "ind31"), fil == "fil_anno", sub_fil == "fil_mese") %>%
  left_join(mappatura_regioni %>% select(codice_fiscale, codice_reg), by = c("pa" = "codice_fiscale")) %>%
  filter(!is.na(codice_reg)) %>%
  mutate(codice_reg = sprintf("%02d", as.numeric(codice_reg))) %>%
  select(codice_reg, fil_val, sub_fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_reg_mensile <- df_pa_mensile %>%
  group_by(codice_reg, fil_val, sub_fil_val) %>%
  summarise(
    ind21 = sum(ind21, na.rm = TRUE), ind22 = sum(ind22, na.rm = TRUE), ind26 = sum(ind26, na.rm = TRUE),
    ind29 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind29 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    ind30 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind30 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    ind31 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind31 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(ind25 = if_else(ind21 > 0, ind22 / ind21 * 100, NA_real_), ind27 = if_else(ind21 > 0, ind26 / ind21, NA_real_)) %>%
  pivot_longer(cols = starts_with("ind"), names_to = "ind", values_to = "ind_val")

df_anac_reg <- bind_rows(df_reg_annuale, df_reg_mensile) %>%
  left_join(nomi_regioni_univoci, by = "codice_reg")

df_pa_fg <- df_anac_raw %>%
  filter(ind %in% c("ind32", "ind33", "ind37", "ind40", "ind41", "ind42"), fil == "fil_anno", sub_fil == "fil_fg") %>%
  select(fil_val, sub_fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_fg_confronto <- df_pa_fg %>%
  group_by(sub_fil_val, fil_val) %>%
  summarise(
    ind32 = sum(ind32, na.rm = TRUE), ind33 = sum(ind33, na.rm = TRUE), ind37 = sum(ind37, na.rm = TRUE),
    ind40 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind40 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    ind41 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind41 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    ind42 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind42 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(ind36 = if_else(ind32 > 0, ind33 / ind32 * 100, NA_real_), ind38 = if_else(ind32 > 0, ind37 / ind32, NA_real_))

ind_dettaglio <- c(21, 22, 26:31)
dettaglio <- bind_rows(
  df_anac_raw %>%
    filter(ind %in% paste0("ind", ind_dettaglio)) %>%
    group_by(fil_val, sub_fil_val, fg = "TUTTE", ind) %>%
    summarise(ind_val = sum(ind_val, na.rm = TRUE), .groups = "drop"),
  df_anac_raw %>%
    filter(ind %in% paste0("ind", ind_dettaglio + 22), !is.na(subsub_fil_val)) %>%
    mutate(ind = paste0("ind", as.integer(sub("ind", "", ind)) - 22)) %>%
    group_by(fil_val, sub_fil_val, fg = subsub_fil_val, ind) %>%
    summarise(ind_val = sum(ind_val, na.rm = TRUE), .groups = "drop")
)

saveRDS(
  list(
    df_anac_nat = df_anac_nat,
    df_anac_reg = df_anac_reg,
    df_fg_confronto = df_fg_confronto,
    lista_fg = lista_fg,
    dettaglio = dettaglio
  ),
  "ANAC/data/dashboard_data.rds",
  compress = "xz"
)

