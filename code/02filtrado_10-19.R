#----[AUTOR]----------------------------------------------------------------------------------------
'''
Autor: David Felipe Calvo
Fecha: 28/03/2025

'''

rm(list = ls())

#===================================================================================================
#----[LIBRERÍAS]------------------------------------------------------------------------------------
library(tidyverse)   # Para tratramiento de datos
library(dplyr)

#===================================================================================================
#----[LIMPIEZA DE DATOS]----------------------------------------------------------------------------
#--------[DATOS INDIVIDUALES]-----------------------------------------------------------------------
load(file = 'C:/data/Childcare_ML/IndUSA1019.RData')

glimpse(dfInd) # Visualización de los datos
summary(dfInd) # Resumen estadísticos
names(dfInd)   # Nombre de todas las variables

# ==< Id (solo observaciones con una id) >==========================================================
levels(factor(dfInd$id)) # Ninguna observación con más de una id
dfInd$id <- dfInd$persid 

# ==< Sex (male = 1) >==============================================================================
dfInd$sex <- as.numeric(dfInd$sex == 'Man') |>
  as.factor()

# ==< Day (MidWeek = 1) >===========================================================================
dfInd$day <- as.numeric(dfInd$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) |>
  as.factor()

# ==< Month >=======================================================================================
dfInd$month <- ifelse(dfInd$month %in% c('December', 'January', 'February'),
                  'Invierno', 
               ifelse(dfInd$month %in% c('March', 'April', 'May'), 
                  'Primavera',
               ifelse(dfInd$month %in% c('June', 'July', 'August'),
                  'Verano',
               'Otoño'))) |>
  as.factor()

# ==< Urban (Urban = 1) >===========================================================================
dfInd$urban <- as.numeric(dfInd$urban == 'urban/suburban') |>
  as.factor()

# ==< Civstat (Marital status) >====================================================================
dfInd$civstat <- as.numeric(dfInd$civstat == 'in couple (married/cohabit/civil partnership)') |>
  as.factor()

# ==< Citizen (citizen = 1) >=======================================================================
dfInd$citizen <- as.numeric(dfInd$citizen == 'yes') |>
  as.factor()

# ==< Singpar (Yes = 1) >===========================================================================
dfInd$singpar <- as.numeric(dfInd$singpar == 'Yes') |>
  as.factor()

# ==< Cohab (alone = 0) >===========================================================================
dfInd$cohab <- as.numeric(dfInd$cohab %in% c('cohabiting', 'married/civil partnership')) |>
  as.factor()

# ==< Income >======================================================================================
dfInd$incorig <- as.numeric(dfInd$incorig)

# ==< Min working >=================================================================================
# Sumamos todo el tiempo de trabajo para para determinar que las personas trabajen al menos 60 min
# el día que realizan la encuenta

dfInd$marketWork <- rowSums(dfInd[c('main7', 'main8', 'main12', 'main9', 'main67', 'main10', 'main11', 'main13', 
                                    'main14', 'main63', 'main64')])


dfInd$childCare <- rowSums(dfInd[c('main28', 'main29', 'main30', 'main66', 'main31')]); mean(dfInd[, "childCare"])

# ==< Employment status variables >=================================================================
# ---- Student -------------------------------------------------------------------------------------

dfInd <- dfInd |>
  filter(
    student == 'not student',          # Que no sea estudiante
    unemp == 'no',                     # Que no esté desempleado
    retired == 'not retired',          # Que no esté retirado
    nchild >= 1,                       # Familias con al menos 1 hijo (menores de 18 años)
    badcase == 'good quality diary',
    !is.na(incorig),
    marketWork >= 60
  )

dfInd <- dfInd |>
  mutate(
    familyStatus = as.factor(case_when(
      # Condición 1: Identifica a los padres/madres solteros de forma inequívoca
      singpar == 1 ~ "Single Parent",
      
      # Condición 2: Identifica a las personas en pareja con datos de empleo del cónyuge
      singpar == 0 & empsp == "not in paid work" ~ "In Couple (Spouse not in paid work)",
      singpar == 0 & empsp == "employed full-time" ~ "In Couple (Spouse employed full-time)",
      singpar == 0 & empsp == "employed part-time" ~ "In Couple (Spouse employed part-time)",
      
      # Condición 3: Aísla al grupo problemático en su propia categoría
      singpar == 0 & empsp == "not applicable" ~ "In Couple (Spouse employment N/A)",
      
      # Condición de respaldo por si queda algún caso extraño
      TRUE ~ "Other" 
    )),
    
    # Establecemos el nivel de referencia que nos interesa para la comparación
    familyStatus = relevel(familyStatus, ref = "In Couple (Spouse not in paid work)")
  )


# ---- Employed (Paid Work = 1) --------------------------------------------------------------------
dfInd$emp <- as.numeric(dfInd$emp == 'in paid work')

# ==< Hhldsize >====================================================================================
dfInd$hhldsize <- as.numeric(dfInd$hhldsize)

# ==< Age >=========================================================================================
dfInd$age <- as.numeric(dfInd$age)

# ==< Rename features >=============================================================================
dfInd <- dplyr::rename(dfInd, 
                male            = sex,
                midWeek         = day,
                married         = civstat,
                ageChild        = agekid2,
                hhldSize        = hhldsize,
                nChild          = nchild)

# ==< Remove other features >=======================================================================
dfInd <- dfInd |>
  select(
    id,
    male,
    age,
    midWeek,
    hhldSize,
    nChild,
    ageChild,
    urban,
    income,
    cohab,
    educa,
    citizen,
    familyStatus
  )

# ==< Type of variable >============================================================================
str(dfInd)

dfInd$ageChild <- dfInd$ageChild |>
  as.numeric()

dfInd$educa <- dfInd$educa |>
  as.factor()

dfInd$nChild <- dfInd$nChild |>
  as.numeric()

dfInd$income <- dfInd$income |>
  as.factor()

save(dfInd, file = 'C:/data/Childcare_ML/Ind_1019.RData')

#===================================================================================================
#--------[DATOS DIARIOS]----------------------------------------------------------------------------
load(file = 'C:/data/Childcare_ML/DiariesUSA1019.RData')

# Id (solo observaciones con una id) ---------------------------------------------------------------
dfDiaries$id <- dfDiaries$persid 

# Remove other features ----------------------------------------------------------------------------
dfDiaries <- subset(dfDiaries, 
                    select = -c(persid, country, survey, svyregion, hldid, sex, age, day, ocombwt,
                                propwt, sec, core, av, sppart, ict, badcase))

# Unimos ambas bases de datos ----------------------------------------------------------------------
dfTus <- merge(x = dfDiaries, y = dfInd, by = 'id') |>
  arrange(id, start, epnum)

save(dfTus, file = 'C:/data/Childcare_ML/TUS_1019.RData')


