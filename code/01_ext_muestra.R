# ==================================================================================================
# ----[INFORMACIÓN]---------------------------------------------------------------------------------
# Autor: David Felipe Calvo / Giménez-Nadal
# Extrae dos muestras del MTUS usando fread para eficiencia:
#   (A) Estados Unidos, 2007-2023
#   (B) Todos los países disponibles, survey >= 2007
# Salidas: IndUSA0723 / DiariesUSA0723 / IndMulti07 / DiariesMulti07
# ==================================================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(data.table)

HAF_PATH  <- "C:/data/Childcare_ML/data/MTUS_haf.csv"
HEF_PATH  <- "C:/data/Childcare_ML/data/MTUS_hef2.csv"
OUT_DIR   <- "C:/data/Childcare_ML/data/"

# Columnas necesarias del archivo de individuos (haf) ---------------------------------
COLS_HAF <- c("country","survey","svyregion","hldid","persid","sex","age","day",
              "badcase","ocombwt","propwt","urban","civstat","cohab","singpar",
              "educa","emp","unemp","student","retired","empsp","nchild","agekid2",
              "hhldsize","income","incorig","citizen",
              "main7","main8","main9","main10","main11","main12","main13","main14",
              "main28","main29","main30","main31","main63","main64","main66","main67",
              "month","year","famstat")

# Columnas necesarias del archivo de episodios (hef2) ---------------------------------
COLS_HEF <- c("country","survey","hldid","persid","epnum","start","end","main",
              "badcase","ocombwt")

# ==================================================================================================
# (A) MUESTRA US 2007-2023
# ==================================================================================================

cat("\n[1/4] Leyendo individuos (haf) - filtrando US 2007+...\n")
# haf.csv guarda persid/hldid como float ("1.0", "20070100000001.0"); hef2.csv como integer ("1").
# Se leen como character y se elimina el sufijo ".0" para que los IDs sean comparables entre archivos.
dtInd <- fread(HAF_PATH, select = COLS_HAF,
               colClasses = list(character = c("persid", "hldid")),
               showProgress = FALSE)
dtInd[, persid := sub("\\.0$", "", persid)]
dtInd[, hldid  := sub("\\.0$", "", hldid)]
IndUSA0723 <- dtInd[country == "United States" & survey >= 2007]
cat("  Individuos US 2007-2023:", nrow(IndUSA0723), "\n")
save(IndUSA0723, file = paste0(OUT_DIR, "IndUSA0723.RData"))
rm(dtInd); gc()

cat("\n[2/4] Leyendo episodios (hef2) - filtrando US 2007+...\n")
dtDiaries <- fread(HEF_PATH, select = COLS_HEF,
                   colClasses = list(character = c("persid", "hldid")),
                   showProgress = FALSE)
# hef2 ya guarda enteros sin decimales; el sub es inofensivo pero mantiene coherencia
dtDiaries[, persid := sub("\\.0$", "", persid)]
dtDiaries[, hldid  := sub("\\.0$", "", hldid)]
DiariesUSA0723 <- dtDiaries[country == "United States" & survey >= 2007]
cat("  Episodios US 2007-2023:", nrow(DiariesUSA0723), "\n")
save(DiariesUSA0723, file = paste0(OUT_DIR, "DiariesUSA0723.RData"))

# ==================================================================================================
# (B) MUESTRA MULTI-PAÍS 2007+
# ==================================================================================================

cat("\n[3/4] Filtrando individuos multi-país (survey >= 2007)...\n")
dtInd2 <- fread(HAF_PATH, select = COLS_HAF,
                colClasses = list(character = c("persid", "hldid")),
                showProgress = FALSE)
dtInd2[, persid := sub("\\.0$", "", persid)]
dtInd2[, hldid  := sub("\\.0$", "", hldid)]
IndMulti07 <- dtInd2[survey >= 2007]
cat("  Individuos multi-país:\n")
print(IndMulti07[, .N, by = .(country, survey)][order(country, survey)])
save(IndMulti07, file = paste0(OUT_DIR, "IndMulti07.RData"))
rm(dtInd2); gc()

cat("\n[4/4] Filtrando episodios multi-país (survey >= 2007)...\n")
DiariesMulti07 <- dtDiaries[survey >= 2007]
cat("  Episodios multi-país:", nrow(DiariesMulti07), "\n")
save(DiariesMulti07, file = paste0(OUT_DIR, "DiariesMulti07.RData"))
rm(dtDiaries); gc()

cat("\n=== EXTRACCIÓN COMPLETADA ===\n")
cat("Archivos guardados en:", OUT_DIR, "\n")
