# ==================================================================================================
# ----[INFORMACIÓN DEL DOCUMENTO]-------------------------------------------------------------------
# Autor: Felipe Calvo, David
# Fecha de creación: 11/07/2025
# Documento: Script final para el análisis de supervivencia de interrupciones laborales.
#
# Objetivo: Modelar el tiempo hasta la primera "interrupción pura" del trabajo por
#           motivos de cuidado de hijos (Child Care) en una muestra de padres y madres,
#           utilizando análisis de supervivencia estándar.
#
#
#
#
# ==================================================================================================
# ----[ 1. CONFIGURACIÓN DEL ENTORNO ]--------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  tidyverse,      # Para manipulación de datos
  TraMineR,       # Para análisis de secuencias de estados
  survival,       # Funciones base para análisis de supervivencia
  survminer,      # Para visualizaciones de supervivencia de alta calidad
  randomForestSRC,
  ggplot2,
  stargazer,
  moments,
  gridExtra,
  grid,
  dplyr
)

# Definir constantes y parámetros del análisis
amplitudIntervalo <- 10; totalMinutosDia <- 1440
intervalos <- totalMinutosDia / amplitudIntervalo



# ==================================================================================================
# ----[Tema para los gráficos: diseño de paper ]----------------------------------------------------

themePaper <- function(base_size = 12, base_family = "serif") {
  theme_test(base_size = base_size, base_family = base_family) +
    theme(
      #----[Panel y fondo]--------------------------------------------------------
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      
      #----[Ejes]-----------------------------------------------------------------
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.text = element_text(color = "black", size = rel(1.2)),
      axis.title = element_text(color = "black", size = rel(1.4)),
      
      #----[Leyenda]--------------------------------------------------------------
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = rel(1.2)),
      legend.title = element_text(size = rel(1.2), face = "bold"),
      legend.position = "bottom",
      
      #----[Títulos]--------------------------------------------------------------
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = rel(1), hjust = 0.5),
      
      #----[Márgenes]-------------------------------------------------------------
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}
tablaEstadisticosDescriptivos <- function(dataFrame, variable) {
  estadisticosDescriptivos <- function(variable) {
    c(Media = mean(variable),
      Mediana = median(variable),
      Desv_Est = sd(variable),
      Min = min(variable),
      Max = max(variable),
      Q1 = quantile(variable, 0.25),
      Q3 = quantile(variable, 0.75),
      Asimetria = skewness(variable),
      Curtosis = kurtosis(variable) - 3)
  }
  
  #----[Análisis estadístico]---------------------------------------------------
  if (length(variable) == 1) {
    
    #----[Análisis para una sola variable]--------------------------------------
    estadisticos <- estadisticosDescriptivos(dataFrame[[variable]])
    tablaEstadisticos <- matrix(estadisticos,
                                nrow = 1, byrow = TRUE,
                                dimnames = list(variable, names(estadisticos)))
  } else {
    
    #----[Análisis para más de una variable]------------------------------------
    tablaEstadisticos <- sapply(dataFrame[, variable], estadisticosDescriptivos)
    tablaEstadisticos <- t(tablaEstadisticos)
  }
  
  #----[Tabla de estadísticos]--------------------------------------------------
  stargazer(tablaEstadisticos,
            type = 'text',
            title = 'Estadísticos descriptivo: variable cuantitativas',
            digits = 2,
            align = TRUE)
}
analisisCualitativa <- function(dataFrame, variableCualitativa) {
  for (variable in variableCualitativa) {
    
    #----[Tabla de frecuencias]-------------------------------------------------
    tablaFrecuencia <- table(dataFrame[[variable]])
    tablaProporcion <- round(prop.table(tablaFrecuencia) * 100, 2)
    
    dfTablaFrecuencia <- data.frame(
      Categoria = names(tablaFrecuencia),
      Frecuencia = as.numeric(tablaFrecuencia),
      Porcentaje = as.numeric(tablaProporcion)
    )
    
    #----[Gráfico de barras]----------------------------------------------------
    barPlot <- ggplot(dfTablaFrecuencia, aes(x = Categoria, y = Frecuencia, fill = Categoria)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = paste0(round(Porcentaje, 1), '%')), vjust = -0.5) +
      labs(title = paste('Distribución de', variable),
           x = variable, y = 'Frecuencia') +
      themePaper() +
      theme(legend.position = 'none')
    
    
    #----[Output tabla de frecuencias]------------------------------------------
    matrixTablaFrecuencias <- data.matrix(dfTablaFrecuencia[, c(2,3)]) 
    dimnames(matrixTablaFrecuencias)[[1]] <- names(tablaFrecuencia)
    stargazer(matrixTablaFrecuencias,
              type = 'text', 
              title = paste('Tabla de frecuencias:', variable),
              digits = 2,
              align = TRUE)
  }
}




# ==================================================================================================
# ----[ 2. CARGA DE DATOS ]-------------------------------------------------------------------------

load('C:/data/Childcare_ML/TUS_1019.RData')
load('C:/data/Childcare_ML/Ind_1019.RData')




#----[ 3. PREPARACIÓN DE DATOS A NIVEL DE EPISODIO ]------------------------------------------------

dfEpisodios <- dfTus; rm(dfTus)

dfEpisodios$activityCategory <- ifelse(dfEpisodios$main %in% c('imputed personal or household care', 'sleep and naps','imputed sleep', 'wash, dress, care for self', 'meals at work or school', 'meals or snacks at home and in other places', 'consume personal care services'), 
                                       'Personal Care', 
                                ifelse(dfEpisodios$main %in% c('paid work-main job at the workplace', 'paid work at home ', 'work breaks', 'second or other job at the workplace', 'shop, person/hhld care travel', 'unpaid work to generate household income', 'travel as a part of work', 'other time at workplace', 'look for work ', 'travel to/from work', 'education travel'),
                                       'Market Work',  
                                ifelse(dfEpisodios$main %in% c('regular schooling, education', 'homework', 'food preparation, cooking', 'set table, wash/put away dishes', 'cleaning', 'pet care (not walk dog)', 'maintain home/vehicle, including collect fuel', 'household management', 'laundry, ironing, clothing repair', 'shopping', 'consume other services', 'adult care'), 
                                       'Non-Market Work', 
                                ifelse(dfEpisodios$main %in% c('physical, medical child care', 'teach, help with homework', 'read to, talk or play with child', 'child/adult care travel', 'supervise, accompany, other child care'),
                                       'Child Care', 
                                ifelse(dfEpisodios$main %in% c('leisure & other education or training', 'worship and religion', 'read', 'voluntary, civic, organisational act', 'party, social event, gambling', 'restaurant, caf?, bar, pub', 'walking', 'imputed time away from home', 'attend sporting event', 'knit, crafts or hobbies', 'other travel', 'general out-of-home leisure', 'listen to radio', 'cycling', 'walk dogs', 'no activity, imputed or recorded transport', 'other public event, venue', 'voluntary/civic/religious travel', 'cinema, theatre, opera, concert', 'general sport or exercise', 'other outside recreation', 'art or music', 'gardening/pick mushrooms', 'listen to music or other audio content', 'receive or visit friends', 'e-mail, surf internet, computing', 'games (social & solitary)/other in-home social', 'correspondence (not e-mail)', 'relax, think, do nothing', 'conversation (in person, phone)', 'general indoor leisure', 'watch TV, video, DVD, streamed film', 'computer games'), 
                                       'Leisure', 
                                       'Not recorded')))))




# ==================================================================================================
# ----[ 4. INGENIERÍA DE SECUENCIAS (TraMineR) ]----------------------------------------------------
# ---- Función para crear la secuencia de un individuo ---------------------------------------------

secuenciador <- function(df, intervalos, amplitudIntervalo) {
  secuencia <- rep(NA, intervalos)
  for (i in 1:nrow(df)) {
    episodio        <- df[i, ]
    intervaloInicio <- floor(episodio$start / amplitudIntervalo) + 1
    intervaloFinal  <- floor((episodio$end - 1) / amplitudIntervalo) + 1
    
    intervaloInicio <- max(1, intervaloInicio)
    intervaloFinal  <- min(intervalos, intervaloFinal)
    
    if (intervaloInicio <= intervaloFinal) {
      secuencia[intervaloInicio:intervaloFinal] <- episodio$activityCategory
    }
  }
  return(secuencia)
}

# ---- Crear la matriz de secuencias ---------------------------------------------------------------

listaSecuencias <- dfEpisodios %>%
  group_split(id) %>%
  map(~ secuenciador(.x, intervalos, amplitudIntervalo))
matrizSecuencias <- do.call(rbind, listaSecuencias)

# ---- Crear el objeto de secuencias de TraMineR ---------------------------------------------------

idsSecuencias <- dfEpisodios %>% distinct(id) %>% pull(id)
rownames(matrizSecuencias) <- idsSecuencias
diccionario <- c("Personal Care", "Market Work", "Non-Market Work", "Leisure", "Child Care", "Not recorded")
secuenciaEstados <- seqdef(matrizSecuencias, alphabet = diccionario, states = diccionario, xtstep = amplitudIntervalo)

colnames(secuenciaEstados) <- c("04:00", "04:10", "04:20", "04:30", "04:40", "04:50", "05:00", "05:10", "05:20", "05:30", "05:40", "05:50", "06:00", "06:10", "06:20", "06:30", "06:40", "06:50", "07:00", "07:10", "07:20", "07:30", "07:40", "07:50", "08:00", "08:10", "08:20", "08:30", "08:40", "08:50", "09:00", "09:10", "09:20", "09:30", "09:40", "09:50", "10:00", "10:10", "10:20", "10:30", "10:40", "10:50", "11:00", "11:10", "11:20", "11:30", "11:40", "11:50", "12:00", "12:10", "12:20", "12:30", "12:40", "12:50", "13:00", "13:10", "13:20", "13:30", "13:40", "13:50", "14:00", "14:10", "14:20", "14:30", "14:40", "14:50", "15:00", "15:10", "15:20", "15:30", "15:40", "15:50", "16:00", "16:10", "16:20", "16:30", "16:40", "16:50", "17:00", "17:10", "17:20", "17:30", "17:40", "17:50", "18:00", "18:10", "18:20", "18:30", "18:40", "18:50", "19:00", "19:10", "19:20", "19:30", "19:40", "19:50", "20:00", "20:10", "20:20", "20:30", "20:40", "20:50", "21:00", "21:10", "21:20", "21:30", "21:40", "21:50", "22:00", "22:10", "22:20", "22:30", "22:40", "22:50", "23:00", "23:10", "23:20", "23:30", "23:40", "23:50", "00:00", "00:10", "00:20", "00:30", "00:40", "00:50", "01:00", "01:10", "01:20", "01:30", "01:40", "01:50", "02:00", "02:10", "02:20", "02:30", "02:40", "02:50", "03:00", "03:10", "03:20", "03:30", "03:40", "03:50")

# ==================================================================================================
# ----[ INGENIERÍA DE DATOS PARA INTERRUPCIONES PURAS ]---------------------------------------------

detectarInterrupcionPura <- function(secuencia, amplitudIntervalo, interrupción = 0) {
  
  # 1. Encontrar todos los intervalos de trabajo y de cuidado de hijos.
  indicesTrabajo <- which(secuencia == "Market Work")
  indicesChildCare <- which(secuencia == "Child Care")
  
  # Si no hay trabajo o no hay cuidado de hijos, no puede haber un evento.
  if (length(indicesTrabajo) == 0 || length(indicesChildCare) == 0) {
    tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
    return(data.frame(tiempo = tiempoTotalTrabajo, estatus = "Censored", interrupción = interrupción))
  }
  
  inicioTrabajo <- indicesTrabajo[1]
  
  for (idx_cc in indicesChildCare) { # Iterar cada secuencia de childcare
    if (idx_cc > inicioTrabajo) { # 1º Debe ocurrir una vez empezado el tiempo de trabajo
      
      # 2º Debe haber un retorno al trabajo DESPUÉS de la interrupción
      retornoAlTrabajo <- which(indicesTrabajo > idx_cc)[1]
      idx_retorno <- indicesTrabajo[retornoAlTrabajo]
      
      # 3º La actividad previa a la interrupción debe ser el trabajo.
      actividadPrevia <- which(secuencia[idx_cc - 1] == "Market Work")
      
      # Si no hay un retorno al trabajo, esta no es una interrupción válida.
      if (!is.na(idx_retorno) && length(actividadPrevia) == TRUE) {
        
        # 3º Analizar si la secuencia antes de volver al trabajo ha sido totalmente de childcare o 
        # intervienen otras actividades. Desde el inicio de la interrupción hasta el retorno
        secuenciaIntermedia <- secuencia[(idx_cc):(idx_retorno - 1)]
        
        # Comprobar si TODAS las actividades en este intervalo son "Child Care".
        esInterrupcionPura <- all(secuenciaIntermedia == "Child Care")
        
        if (esInterrupcionPura) {
          # El tiempo es la duración desde el inicio del trabajo hasta esta interrupción.
          
          tiempo <- length(indicesTrabajo[indicesTrabajo < idx_cc]) * amplitudIntervalo
          interrupción <- length(secuenciaIntermedia) * amplitudIntervalo
          estatus <- "Pure Interruption"

          # Devolvemos el resultado y terminamos la función.
          return(data.frame(tiempo = tiempo, estatus = estatus, interrupción = interrupción))
        }
      }
    }
  }
  
  # 3. Si el bucle termina sin encontrar una interrupción pura, la observación es censurada.
  tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
  estatus <- "Censored"
  
  return(data.frame(tiempo = tiempoTotalTrabajo, estatus = estatus, interrupción = interrupción))
}




detectarInterrupcionMixta <- function(secuencia, amplitudIntervalo, interrupción = 0) {
  
  indicesTrabajo <- which(secuencia == "Market Work")
  indicesChildCare <- which(secuencia == "Child Care")
  
  if (length(indicesTrabajo) == 0 || length(indicesChildCare) == 0) {
    tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
    return(data.frame(tiempo = tiempoTotalTrabajo, estatus = "Censored", interrupción = interrupción))
  }
  
  inicioTrabajo <- indicesTrabajo[1]
  
  for (idx_cc in indicesChildCare) { 
    if (idx_cc > inicioTrabajo) {
      
      retornoAlTrabajo <- which(indicesTrabajo > idx_cc)[1]
      idx_retorno <- indicesTrabajo[retornoAlTrabajo]
      
      actividadPrevia <- which(secuencia[idx_cc - 1] == "Market Work")
      
      if (!is.na(idx_retorno) && length(actividadPrevia) == TRUE) {
        
        secuenciaIntermedia <- secuencia[(idx_cc):(idx_retorno - 1)]
        
        mixtoChildCare       <- which(secuenciaIntermedia == "Child Care")
        mixtoPersonalCare    <- which(secuenciaIntermedia == "Personal Care")
        mixtoLeisure         <- which(secuenciaIntermedia == "Leisure")
        mixtoNonMarketWork   <- which(secuenciaIntermedia == "Non-Market Work")
        
        if (!length(mixtoChildCare) == 0 && c(!length(mixtoPersonalCare) == 0 || !length(mixtoLeisure) == 0) && length(mixtoNonMarketWork) == 0) {
          tiempo <- length(indicesTrabajo[indicesTrabajo < idx_cc]) * amplitudIntervalo
          interrupción <- length(secuenciaIntermedia) * amplitudIntervalo
          estatus <- "Mixed Interruption"
          
          return(data.frame(tiempo = tiempo, estatus = estatus, interrupción = interrupción))
        }
      }
    }
  }
  
  # 3. Si el bucle termina sin encontrar una interrupción pura, la observación es censurada.
  tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
  estatus <- "Censored"
  
  return(data.frame(tiempo = tiempoTotalTrabajo, estatus = estatus, interrupción = interrupción))
}




detectarInterrupcion <- function(secuencia, amplitudIntervalo, interrupción = 0) {
  
  indicesTrabajo <- which(secuencia == "Market Work")
  indicesChildCare <- which(secuencia == "Child Care")
  
  if (length(indicesTrabajo) == 0 || length(indicesChildCare) == 0) {
    tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
    return(data.frame(tiempo = tiempoTotalTrabajo, estatus = "Censored", interrupción = interrupción))
  }
  
  inicioTrabajo <- indicesTrabajo[1]
  
  for (idx_cc in indicesChildCare) { 
    if (idx_cc > inicioTrabajo) {
      
      retornoAlTrabajo <- which(indicesTrabajo > idx_cc)[1]
      idx_retorno <- indicesTrabajo[retornoAlTrabajo]
      
      actividadPrevia <- which(secuencia[idx_cc - 1] == "Market Work")
      
      if (!is.na(idx_retorno) && length(actividadPrevia) == TRUE) {
        
        secuenciaIntermedia <- secuencia[(idx_cc):(idx_retorno - 1)]
        
        esInterrupcionPura <- all(secuenciaIntermedia == "Child Care")
        mixtoChildCare       <- which(secuenciaIntermedia == "Child Care")
        mixtoPersonalCare    <- which(secuenciaIntermedia == "Personal Care")
        mixtoLeisure         <- which(secuenciaIntermedia == "Leisure")
        mixtoNonMarketWork   <- which(secuenciaIntermedia == "Non-Market Work")
        
        
        if (esInterrupcionPura) {
          
          tiempo <- length(indicesTrabajo[indicesTrabajo < idx_cc]) * amplitudIntervalo
          estatus <- "Interruption"
          interrupción <- length(secuenciaIntermedia) * amplitudIntervalo
          
          return(data.frame(tiempo = tiempo, estatus = estatus, interrupción = interrupción))
        }
        
        if (!length(mixtoChildCare) == 0 && c(!length(mixtoPersonalCare) == 0 || !length(mixtoLeisure) == 0) && length(mixtoNonMarketWork) == 0) {
          tiempo <- length(indicesTrabajo[indicesTrabajo < idx_cc]) * amplitudIntervalo
          interrupción <- length(secuenciaIntermedia) * amplitudIntervalo
          estatus <- "Interruption"
          
          return(data.frame(tiempo = tiempo, estatus = estatus, interrupción = interrupción))
        }
      }
    }
  }
  
  # 3. Si el bucle termina sin encontrar una interrupción pura, la observación es censurada.
  tiempoTotalTrabajo <- length(indicesTrabajo) * amplitudIntervalo
  estatus <- "Censored"
  
  return(data.frame(tiempo = tiempoTotalTrabajo, estatus = estatus, interrupción = 0))
}

# ---- Aplicar la función a todas las secuencias ---------------------------------------------------

dfSupervivenciaPura <- map_dfr(1:nrow(matrizSecuencias), ~detectarInterrupcionPura(matrizSecuencias[.x,], amplitudIntervalo), .id = "row_index")
dfSupervivenciaPura$id <- idsSecuencias[as.numeric(dfSupervivenciaPura$row_index)]
dfSupervivenciaPura <- dfSupervivenciaPura %>% select(id, tiempo, estatus, interrupción)

round(prop.table(table(dfSupervivenciaPura$estatus))*100, 2)

dfSupervivenciaPura <- dfSupervivenciaPura |>
  filter(
    estatus == 'Censored' | (estatus == 'Pure Interruption' & interrupción <= 120)
  ); round(prop.table(table(dfSupervivenciaPura$estatus))*100, 2)

dfSupervivenciaMixta <- map_dfr(1:nrow(matrizSecuencias), ~detectarInterrupcionMixta(matrizSecuencias[.x,], amplitudIntervalo), .id = "row_index")
dfSupervivenciaMixta$id <- idsSecuencias[as.numeric(dfSupervivenciaMixta$row_index)]
dfSupervivenciaMixta <- dfSupervivenciaMixta %>% select(id, tiempo, estatus, interrupción)

round(prop.table(table(dfSupervivenciaMixta$estatus))*100, 2)

dfSupervivenciaMixta <- dfSupervivenciaMixta |>
  filter(
    estatus == 'Censored' | (estatus == 'Mixed Interruption' & interrupción <= 120)
  ); round(prop.table(table(dfSupervivenciaMixta$estatus))*100, 2)

dfSupervivenciaGlobal <- map_dfr(1:nrow(matrizSecuencias), ~detectarInterrupcion(matrizSecuencias[.x,], amplitudIntervalo), .id = "row_index")
dfSupervivenciaGlobal$id <- idsSecuencias[as.numeric(dfSupervivenciaGlobal$row_index)]
dfSupervivenciaGlobal <- dfSupervivenciaGlobal %>% select(id, tiempo, estatus, interrupción)

round(prop.table(table(dfSupervivenciaGlobal$estatus))*100, 2)

dfSupervivenciaGlobal <- dfSupervivenciaGlobal |>
  filter(
    estatus == 'Censored' | (estatus == 'Interruption' & interrupción <= 120)
  ); round(prop.table(table(dfSupervivenciaGlobal$estatus))*100, 2)

# ==================================================================================================
# ----[ 6. ENSAMBLAJE FINAL Y DIAGNÓSTICO ]---------------------------------------------------------
# ---- Preparar el dataframe de individuos ---------------------------------------------------------

dfIndividuos <- dfInd %>%
  distinct(id, .keep_all = TRUE)

# ---- Unir datos de supervivencia con datos de individuos ------------------------------------------

secuenciaEstados$id <- idsSecuencias
secuenciaEstados <- secuenciaEstados[secuenciaEstados$id  %in% dfSupervivenciaGlobal$id, ]

dfAnalisis <- left_join(dfSupervivenciaGlobal, dfIndividuos, by = "id")

seqdplot(secuenciaEstados,
         main = "State Distribution Over Time (Var. Male)",
         ylab = "Proportion", 
         border = NA,
         group = dfAnalisis$male,
         with.legend = "right")

dfAnalisisLimpio <- dfAnalisis %>%
  select(tiempo, estatus, male, age, midWeek, hhldSize, nChild, ageChild, 
         urban, income, cohab, educa, citizen, familyStatus) %>% 
  filter(tiempo > 0) %>%
  na.omit()

dfAnalisisLimpio <- dfAnalisisLimpio %>%
  mutate(hhldSizeGroup = case_when(
    hhldSize == 2    ~ "2 members",
    hhldSize == 3    ~ "3 members",
    hhldSize == 4    ~ "4 members",
    hhldSize >= 5    ~ "5+ members"
  )) %>%
  mutate(ageChildGroup = case_when(
    ageChild <= 4                  ~ "0-4 years",
    ageChild >= 5 & ageChild <= 12   ~ "5-12 years",
    ageChild >= 13                 ~ "13-17 years",
  ))

# ---- Educación agrupada 
dfAnalisisLimpio$educaGroup <- 
  ifelse(dfAnalisisLimpio$educa %in% c('1.0', '2.0'),
         'Secondary education or lower',
         ifelse(dfAnalisisLimpio$educa %in% c('3.0'), 
                "Completed secondary education",
                "Higher education")) |>
  as.factor()

dfAnalisisLimpio$educaGroup <- factor(dfAnalisisLimpio$educaGroup, levels = c('Secondary education or lower', 'Completed secondary education', 'Higher education'))
dfAnalisisLimpio$hhldSizeGroup <- factor(dfAnalisisLimpio$hhldSizeGroup, levels = c("2 members", "3 members", "4 members", "5+ members"))
dfAnalisisLimpio$ageChildGroup <- factor(dfAnalisisLimpio$ageChildGroup, levels = c("0-4 years", "5-12 years", "13-17 years"))
dfAnalisisLimpio$income <- factor(dfAnalisisLimpio$income, levels = c('lowest 25%', 'middle 50%', 'highest 25%'))
dfAnalisisLimpio$income <- relevel(dfAnalisisLimpio$income, ref = 'highest 25%')


# ==================================================================================================
# ----[ 7. ANÁLISIS EXPLORATORIO DE SUPERVIVENCIA ]-------------------------------------------------

cat("\n\n--- INICIANDO ANÁLISIS EXPLORATORIO ---\n")

# ---- 7.3. Curvas de Supervivencia de Kaplan-Meier ------------------------------------------------

# Creamos el objeto de supervivencia
surv_obj <- Surv(dfAnalisisLimpio$tiempo, dfAnalisisLimpio$estatus == "Interruption")

# Comparación por Género
ggsurvplot(survfit(surv_obj ~ male, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio, 
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           legend.title = "Gender",
           legend.labs = c("Female", "Male"),
           ylim = c(0.7, 1), 
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption",
           ggtheme = themePaper())

survdiff(surv_obj ~ male, data = dfAnalisisLimpio)

# Comparación por Nivel Educativo

ggsurvplot(survfit(surv_obj ~ educaGroup, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio,
           pval = TRUE,
           conf.int = TRUE, 
           risk.table = TRUE,
           legend.title = "Education",
           legend.labs = c('Secondary education or lower', 'Completed secondary education', 'Higher education'),
           ylim = c(0.7, 1),
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption", 
           ggtheme = themePaper())

survdiff(surv_obj ~ educaGroup, data = dfAnalisisLimpio)

ggsurvplot(survfit(surv_obj ~ hhldSizeGroup, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE, 
           legend.title = "Household size",
           legend.labs = c("2 members", "3 members", "4 members", "5+ members"),
           ylim = c(0.7, 1),
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption", 
           ggtheme = themePaper())

survdiff(surv_obj ~ hhldSizeGroup, data = dfAnalisisLimpio)

ggsurvplot(survfit(surv_obj ~ ageChildGroup, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           legend.title = "Age child",
           legend.labs = c("0-4 years", "5-12 years", "13-17 years"),
           ylim = c(0.7, 1),
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption", 
           ggtheme = themePaper())

survdiff(surv_obj ~ ageChildGroup, data = dfAnalisisLimpio)

ggsurvplot(survfit(surv_obj ~ income, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           legend.title = "Income",
           legend.labs = c('lowest 25%', 'middle 50%', 'highest 25%'),
           ylim = c(0.7, 1),
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption", 
           ggtheme = themePaper())

survdiff(surv_obj ~ income, data = dfAnalisisLimpio)

ggsurvplot(survfit(surv_obj ~ familyStatus, data = dfAnalisisLimpio), 
           data = dfAnalisisLimpio,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE, legend = 'none',
           ylim = c(0.7, 1),
           xlab = "Time (Minutes)",
           ylab = "Probability of no interruption", 
           ggtheme = themePaper())

survdiff(surv_obj ~ familyStatus, data = dfAnalisisLimpio)

# ==================================================================================================
# ----[ 8. MODELIZACIÓN DE COX ]--------------------------------------------------------------------

dfAnalisisLimpio <- dfAnalisisLimpio %>%
  mutate(estatus = ifelse(estatus == "Interruption", 1, 0))

tablaEstadisticosDescriptivos(dfAnalisisLimpio, c('age', 'nChild', 'hhldSize', 'ageChild'))
analisisCualitativa(dfAnalisisLimpio, c('male','income', 'midWeek', 'urban', 'cohab', 'educaGroup', 'citizen', 'familyStatus'))

# Ajustamos el modelo de Cox
modeloCox <- coxph(
  Surv(tiempo, estatus) ~ male + age + midWeek + hhldSize + nChild + ageChild +
    urban + income + familyStatus + educaGroup,
  data = dfAnalisisLimpio
)

("\n--- Resultados del Modelo de Regresión de Cox ---\n")
summary(modeloCox)

# ==================================================================================================
# ----[ 8.1. DIAGNÓSTICO DEL MODELO DE COX: PRUEBA DE RIESGOS PROPORCIONALES ]-----------------------

cat("\n\n--- COMPROBANDO LA SUPOSICIÓN DE RIESGOS PROPORCIONALES (PH) ---\n")

# 1. Realizar la prueba formal con cox.zph()

test_ph <- cox.zph(modeloCox)

cat("\n--- Resultados de la Prueba de Supuestos de PH ---\n")
print(test_ph)

# 2. Visualizar los resultados de la prueba
# Esta es la forma más intuitiva de ver si la suposición se viola.
# Si la línea de tendencia en el gráfico es aproximadamente horizontal (plana),
# la suposición se cumple para esa variable.
# Si la línea tiene una pendiente clara (hacia arriba o hacia abajo), la suposición se viola.

cat("\n--- Generando Gráficos de Diagnóstico de PH ---\n")
ggcoxzph(test_ph)





# ==================================================================================================
# ----[ 9. MODELIZACIÓN CON MACHINE LEARNING (RANDOM FOREST) ]--------------------------------------

cat("\n--- AJUSTANDO MODELO RANDOM SURVIVAL FOREST ---\n")

set.seed(11082025)

model_rsf <- rfsrc(
  Surv(tiempo, estatus) ~ ., 
  data = dfAnalisisLimpio %>% select(-educa, -hhldSizeGroup, -ageChildGroup),
  ntree = 500,
  importance = TRUE
)

cat("\n--- Resultados del Modelo Random Survival Forest ---\n")
print(model_rsf)

cat("\n--- Gráfico de Importancia de Variables (VIMP) ---\n")
plot(model_rsf)

# ==================================================================================================
# ----[ 10. OPTIMIZACIÓN DEL MODELO DE MACHINE LEARNING (TUNING) ]----------------------------------

cat("\n--- INICIANDO TUNING DE HIPERPARÁMETROS PARA RSF ---\n")



# Usamos tune() para encontrar los mejores mtry y nodesize
tuned_results <- tune(
  Surv(tiempo, estatus) ~ ., 
  data = dfAnalisisLimpio %>% select(-educa, -hhldSizeGroup, -ageChildGroup),
  ntree = 100 # Menos árboles para un tuning más rápido
)

cat("\n--- Resultados del Tuning ---\n")
print(tuned_results)

model_rsf_optimized <- rfsrc(
    Surv(tiempo, estatus) ~ ., 
    data = dfAnalisisLimpio %>% select(-educa, -hhldSizeGroup, -ageChildGroup),
    ntree = 1000,
    mtry = 2,
    nodesize = 7,
    importance = TRUE
  )
  
 
print(model_rsf_optimized)
  
cat("\n--- Gráfico de Importancia de Variables (VIMP) del Modelo Optimizado ---\n")
plot(model_rsf_optimized)



# ----[ 11. EVALUACIÓN DEL PODER PREDICTIVO DEL MODELO OPTIMIZADO ]--------------------------------

cat("\n\n--- EVALUANDO EL RENDIMIENTO DEL MODELO RSF OPTIMIZADO ---\n")

# El objeto del modelo ya contiene el error OOB (Out-of-Bag) calculado.
# El error reportado por defecto para modelos de supervivencia es 1 - C-index.
error_oob <- model_rsf_optimized$err.rate[model_rsf_optimized$ntree]
c_index <- 1 - error_oob

cat(paste("Error de Predicción OOB (1 - C-index):", round(error_oob, 4), "\n"))
cat(paste("Índice de Concordancia (C-index):", round(c_index, 4), "\n"))

# Interpretación:
# Un C-index de 0.684 (como el de tu modelo de Cox) significa que si tomas dos personas,
# una que sufre la interrupción y otra que no, el modelo tiene un 68.4% de probabilidad
# de asignar correctamente un mayor riesgo a la persona que sufre la interrupción.


# ----[ 12. INTERPRETACIÓN DEL MODELO: GRÁFICOS DE DEPENDENCIA PARCIAL (PDP) ]---------------------

cat("\n\n--- GENERANDO GRÁFICOS DE DEPENDENCIA PARCIAL ---\n")

# Vamos a visualizar el efecto de las variables más importantes que encontramos.

g <- model_rsf_optimized
g$xvar$familyStatus <- factor(model_rsf_optimized$xvar$familyStatus, labels = c(1, 2, 3, 4, 5))
g$xvar$educaGroup <- factor(model_rsf_optimized$xvar$educaGroup, labels = c(1, 2, 3))
g$xvar$income <- factor(model_rsf_optimized$xvar$income, labels = c(3, 1, 2))

plot.variable(
  g,
  xvar.names = "ageChild",
  partial = TRUE)

plot.variable(
  g,
  xvar.names = "male",
  partial = TRUE)

plot.variable(
  g,
  xvar.names = "nChild",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "educaGroup",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "hhldSize",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "familyStatus",
  partial = TRUE
)

plot.variable(
  g,
  xvar.names = "age",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "income",
  partial = TRUE
)

plot.variable(
  g,
  xvar.names = "midWeek",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "urban",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "citizen",
  partial = TRUE,
)

plot.variable(
  g,
  xvar.names = "cohab",
  partial = TRUE,
)

# El eje Y en estos gráficos es la predicción de mortalidad del conjunto (ensemble mortality).
# Una línea ascendente significa que, a medida que la variable aumenta, el riesgo de
# interrupción también aumenta. Una línea descendente significa que el riesgo disminuye.











