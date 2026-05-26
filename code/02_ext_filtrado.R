# ==================================================================================================
# ----[INFORMACIÓN]---------------------------------------------------------------------------------
# Autor: David Felipe Calvo / Giménez-Nadal
# Limpia y filtra las muestras extraídas en 01_ext_muestra.R.
# Cambios clave respecto a 02filtrado 10-19.R:
#   - Conserva hldid y ocombwt (necesarios para clustering y pesos)
#   - Procesa muestra US 2007-2023 y multi-país 2007+
#   - Para multi-país: usa !is.na(income) en lugar de !is.na(incorig)
#   - Crea hldid_univ (ID de hogar único entre países/encuestas)
# ==================================================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(tidyverse, dplyr)

OUT_DIR <- "C:/data/Childcare_ML/data/"

# ==================================================================================================
# FUNCIÓN COMPARTIDA: construye variables individuales a partir de dfInd
# ==================================================================================================

construirVariables <- function(dfInd) {

  # Id — persid ya viene como character desde 01_ext_muestra.R
  dfInd$id <- dfInd$persid

  # Sex (male = 1) --------------------------------------------------------------------------------
  dfInd$sex <- as.numeric(dfInd$sex == "Man") |> as.factor()

  # Day (midWeek = 1) -----------------------------------------------------------------------------
  dfInd$day <- as.numeric(dfInd$day %in%
      c("Monday","Tuesday","Wednesday","Thursday","Friday")) |> as.factor()

  # Month (estaciones) ----------------------------------------------------------------------------
  dfInd$month <- ifelse(dfInd$month %in% c("December","January","February"), "Invierno",
                 ifelse(dfInd$month %in% c("March","April","May"),            "Primavera",
                 ifelse(dfInd$month %in% c("June","July","August"),           "Verano",
                                                                              "Otono"))) |>
    as.factor()

  # Urban (urban = 1) -----------------------------------------------------------------------------
  dfInd$urban <- as.numeric(dfInd$urban == "urban/suburban") |> as.factor()

  # Civstat (pareja = 1) --------------------------------------------------------------------------
  dfInd$civstat <- as.numeric(
      dfInd$civstat == "in couple (married/cohabit/civil partnership)") |> as.factor()

  # Citizen ---------------------------------------------------------------------------------------
  dfInd$citizen <- as.numeric(dfInd$citizen == "yes") |> as.factor()

  # Singpar (monoparental = 1) --------------------------------------------------------------------
  dfInd$singpar <- as.numeric(dfInd$singpar == "Yes") |> as.factor()

  # Cohab (conviviendo = 1) -----------------------------------------------------------------------
  dfInd$cohab <- as.numeric(
      dfInd$cohab %in% c("cohabiting","married/civil partnership")) |> as.factor()

  # Income ----------------------------------------------------------------------------------------
  dfInd$incorig <- as.numeric(dfInd$incorig)

  # Tiempo de trabajo de mercado (filtro: >= 60 min) -----------------------------------------------
  work_cols <- c("main7","main8","main12","main9","main67","main10",
                 "main11","main13","main14","main63","main64")
  work_cols <- intersect(work_cols, names(dfInd))
  dfInd$marketWork <- rowSums(dfInd[work_cols], na.rm = TRUE)

  # Tiempo de childcare (descriptivo) -------------------------------------------------------------
  cc_cols <- c("main28","main29","main30","main66","main31")
  cc_cols  <- intersect(cc_cols, names(dfInd))
  dfInd$childCare <- rowSums(dfInd[cc_cols], na.rm = TRUE)

  # familyStatus ----------------------------------------------------------------------------------
  dfInd <- dfInd |>
    mutate(
      familyStatus = as.factor(case_when(
        singpar == 1                               ~ "Single Parent",
        singpar == 0 & empsp == "not in paid work" ~ "In Couple (Spouse not in paid work)",
        singpar == 0 & empsp == "employed full-time" ~ "In Couple (Spouse employed full-time)",
        singpar == 0 & empsp == "employed part-time" ~ "In Couple (Spouse employed part-time)",
        singpar == 0 & empsp == "not applicable"   ~ "In Couple (Spouse employment N/A)",
        TRUE                                       ~ "Other"
      )),
      familyStatus = relevel(familyStatus, ref = "In Couple (Spouse not in paid work)")
    )

  # emp (en trabajo remunerado = 1) ---------------------------------------------------------------
  dfInd$emp <- as.numeric(dfInd$emp == "in paid work")

  # Tipos numéricos -------------------------------------------------------------------------------
  dfInd$hhldsize <- as.numeric(dfInd$hhldsize)
  dfInd$age      <- as.numeric(dfInd$age)

  # Renombrar -------------------------------------------------------------------------------------
  dfInd <- dplyr::rename(dfInd,
    male     = sex,
    midWeek  = day,
    married  = civstat,
    ageChild = agekid2,
    hhldSize = hhldsize,
    nChild   = nchild
  )

  return(dfInd)
}

# ==================================================================================================
# A) MUESTRA US 2007-2023
# ==================================================================================================

cat("\n=== PROCESANDO US 2007-2023 ===\n")

load(paste0(OUT_DIR, "IndUSA0723.RData"))
dfInd <- as.data.frame(IndUSA0723); rm(IndUSA0723)

dfInd <- construirVariables(dfInd)

dfInd <- dfInd |>
  filter(
    student    == "not student",
    unemp      == "no",
    retired    == "not retired",
    nChild     >= 1,
    badcase    == "good quality diary",
    !is.na(incorig),
    marketWork >= 60
  )

dfInd <- dfInd |>
  select(id, hldid, ocombwt, survey,
         male, age, midWeek, hhldSize, nChild, ageChild,
         urban, income, cohab, educa, citizen, familyStatus)

dfInd$ageChild <- as.numeric(dfInd$ageChild)
dfInd$educa    <- as.factor(dfInd$educa)
dfInd$nChild   <- as.numeric(dfInd$nChild)
dfInd$income   <- as.factor(dfInd$income)

cat("Ind_0723: n =", nrow(dfInd), "\n")
Ind_0723 <- dfInd
save(Ind_0723, file = paste0(OUT_DIR, "Ind_0723.RData"))

load(paste0(OUT_DIR, "DiariesUSA0723.RData"))
dfDiaries <- as.data.frame(DiariesUSA0723); rm(DiariesUSA0723)
dfDiaries$id <- dfDiaries$persid
dfDiaries <- subset(dfDiaries,
  select = -c(persid, country, survey, hldid, badcase, ocombwt))

n_match <- length(intersect(unique(dfDiaries$id), unique(dfInd$id)))
cat("IDs en común (diarios <-> individuos):", n_match, "\n")

dfTus_0723 <- merge(x = dfDiaries, y = dfInd, by = "id") |>
  arrange(id, start, epnum)

cat("TUS_0723: n =", nrow(dfTus_0723), "episodios\n")
save(dfTus_0723, file = paste0(OUT_DIR, "TUS_0723.RData"))
rm(dfDiaries, dfTus_0723, dfInd); gc()


# ==================================================================================================
# B) MUESTRA MULTI-PAÍS 2007+
# ==================================================================================================

cat("\n=== PROCESANDO MULTI-PAÍS 2007+ ===\n")

load(paste0(OUT_DIR, "IndMulti07.RData"))
dfInd <- as.data.frame(IndMulti07); rm(IndMulti07)

dfInd <- construirVariables(dfInd)

dfInd <- dfInd |>
  filter(
    student    == "not student",
    unemp      == "no",
    retired    == "not retired",
    nChild     >= 1,
    badcase    == "good quality diary",
    !is.na(income),           # income (cuartil) disponible en todos los países
    marketWork >= 60
  )

dfInd <- dfInd |>
  select(id, hldid, ocombwt, country, survey,
         male, age, midWeek, hhldSize, nChild, ageChild,
         urban, income, cohab, educa, citizen, familyStatus)

dfInd$ageChild <- as.numeric(dfInd$ageChild)
dfInd$educa    <- as.factor(dfInd$educa)
dfInd$nChild   <- as.numeric(dfInd$nChild)
dfInd$income   <- as.factor(dfInd$income)
dfInd$country  <- as.factor(dfInd$country)

cat("Ind_Multi07 por país:\n")
print(table(dfInd$country))
cat("Total:", nrow(dfInd), "\n")

# Clave compuesta 4-partes: persid es único solo dentro del hogar en MTUS
# (p.ej. persid="1" = "primera persona del hogar" en todos los hogares de S. Korea 2009)
dfInd$id <- paste(dfInd$country, dfInd$survey, dfInd$hldid, dfInd$id, sep = "_")
dfInd$hldid_univ <- paste(dfInd$country, dfInd$survey, dfInd$hldid, sep = "_")

# Deduplicar: en MTUS algunos encuestados completan 2 diarios (2 filas en haf).
# Nos quedamos con la primera fila por id para tener una observación por persona.
dfInd <- dfInd[!duplicated(dfInd$id), ]

Ind_Multi07 <- dfInd
save(Ind_Multi07, file = paste0(OUT_DIR, "Ind_Multi07.RData"))

load(paste0(OUT_DIR, "DiariesMulti07.RData"))
dfDiaries <- as.data.frame(DiariesMulti07); rm(DiariesMulti07)
# Clave compuesta idéntica: country + survey + hldid + persid
dfDiaries$id <- paste(dfDiaries$country, dfDiaries$survey, dfDiaries$hldid, dfDiaries$persid, sep = "_")
dfDiaries <- subset(dfDiaries,
  select = -c(persid, country, survey, hldid, badcase, ocombwt))

n_match_m <- length(intersect(unique(dfDiaries$id), unique(dfInd$id)))
cat("IDs en común multi-país (diarios <-> individuos):", n_match_m, "\n")

gc()
dfTus_Multi07 <- dplyr::inner_join(dfDiaries, dfInd, by = "id",
                                   relationship = "many-to-one")
rm(dfDiaries); gc()
dfTus_Multi07 <- dplyr::arrange(dfTus_Multi07, id, start, epnum)

cat("TUS_Multi07: n =", nrow(dfTus_Multi07), "episodios\n")
save(dfTus_Multi07, file = paste0(OUT_DIR, "TUS_Multi07.RData"))
rm(dfTus_Multi07, dfInd); gc()

cat("\n=== FILTRADO COMPLETADO ===\n")
