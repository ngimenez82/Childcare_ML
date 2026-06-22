# ==================================================================================================
# ----[INFORMACIÓN]---------------------------------------------------------------------------------
# Autor: David Felipe Calvo / Giménez-Nadal
# Análisis completo v2 — incluye todas las mejoras metodológicas:
#
#  Mejoras sobre v1:
#   [1] Pesos muestrales (ocombwt) en KM, Cox y RSF
#   [2] Errores estándar robustos clusterizados por hogar (cluster(hldid))
#   [3] Censura correcta a 120 min (se recalifica, no se elimina)
#   [4] Cox estratificado por midWeek (corrección violación supuesto PH)
#   [5] SHAP values con fastshap (más interpretables que VIMP)
#   [6] ALE plots con iml (en lugar de PDPs estándar)
#   [7] Gradient Boosting survival (gbm, familia CoxPH)
#   [8] Comparación de modelos: C-index + Brier score integrado (pec)
#   [9] Análisis multi-país: pooled Cox con efectos fijos de país
#       + forest plot de HR por género según país
#
#  Datos de entrada (generados por 01_ext_muestra.R + 02_ext_filtrado.R):
#   - Ind_0723.RData / TUS_0723.RData     (US 2003-2023)
#   - Ind_Multi07.RData / TUS_Multi07.RData (multi-país 2007+)
#
#  Salida:
#   - output/figures/fig_*.pdf   (figuras individuales para Overleaf)
#   - output/output_log_v2.txt   (log completo del análisis)
# ==================================================================================================

rm(list = ls())

# -- Directorios -------------------------------------------------------------------------
BASE_DIR <- "C:/data/Childcare_ML/"
DATA_DIR <- file.path(BASE_DIR, "data")
FIGS_DIR <- file.path(BASE_DIR, "output", "figures")
LOG_FILE <- file.path(BASE_DIR, "output", "output_log_v2.txt")

dir.create(file.path(BASE_DIR, "output"), showWarnings = FALSE)
dir.create(FIGS_DIR,                     showWarnings = FALSE)

sink(LOG_FILE, split = TRUE)

# Outer tryCatch only for cleanup (finally always runs sink)
tryCatch({

# ==================================================================================================
# ----[ 0. CONFIGURACIÓN ]--------------------------------------------------------------------------
# ==================================================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(
  tidyverse, dplyr,
  TraMineR,
  survival, survminer,
  randomForestSRC,
  gbm,
  fastshap,
  iml,
  pec,
  ggplot2, gridExtra, grid,
  stargazer, moments
)

amplitudIntervalo  <- 10
totalMinutosDia    <- 1440
intervalos         <- totalMinutosDia / amplitudIntervalo

# Umbral (min) por encima del cual una interrupcion se recalifica como censurada.
# Parametrizado para el analisis de sensibilidad (seccion 1.9b). El valor base es 120.
CENSOR_THRESHOLD   <- 120

# -- Helpers para guardar figuras ------------------------------------------------------------------

# ggplot / gtable → ggsave
save_gg <- function(p, fname, w = 14, h = 9) {
  path <- file.path(FIGS_DIR, fname)
  ggsave(path, plot = p, width = w, height = h, device = cairo_pdf)
  cat("  [fig]", fname, "\n")
  invisible(path)
}

# Base-R plot expresión → pdf/dev.off
save_base <- function(expr, fname, w = 14, h = 9) {
  path <- file.path(FIGS_DIR, fname)
  pdf(path, width = w, height = h)
  tryCatch(force(expr), error = function(e) cat("  [fig-warn]", fname, ":", e$message, "\n"))
  dev.off()
  cat("  [fig]", fname, "\n")
  invisible(path)
}

# -- Tema publicación ------------------------------------------------------------------------------
themePaper <- function(base_size = 11, base_family = "serif") {
  theme_test(base_size = base_size, base_family = base_family) +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line         = element_line(color = "black", linewidth = 0.3),
      axis.ticks        = element_line(color = "black", linewidth = 0.3),
      axis.text         = element_text(color = "black", size = rel(1.1)),
      axis.title        = element_text(color = "black", size = rel(1.3)),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.text       = element_text(size = rel(1.1)),
      legend.title      = element_text(size = rel(1.1), face = "bold"),
      legend.position   = "bottom",
      plot.title        = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
      plot.margin       = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

tablaDesc <- function(df, vars) {
  stats_fn <- function(x) c(Media=mean(x), Mediana=median(x), SD=sd(x),
                             Min=min(x), Max=max(x), Q1=quantile(x,.25), Q3=quantile(x,.75),
                             Asimetria=skewness(x), Curtosis=kurtosis(x)-3)
  mat <- if (length(vars)==1) {
    m <- matrix(stats_fn(df[[vars]]), nrow=1, dimnames=list(vars, names(stats_fn(df[[vars]]))))
    m
  } else t(sapply(df[,vars], stats_fn))
  stargazer(mat, type='text', digits=2, align=TRUE)
}

# ==================================================================================================
# ----[ FUNCIONES DE SECUENCIACIÓN Y DETECCIÓN DE INTERRUPCIONES ]----------------------------------
# ==================================================================================================

categorizarActividad <- function(main_vec) {
  personal  <- c('imputed personal or household care','sleep and naps','imputed sleep',
                 'wash, dress, care for self','meals at work or school',
                 'meals or snacks at home and in other places','consume personal care services')
  work      <- c('paid work-main job at the workplace','paid work at home ','work breaks',
                 'second or other job at the workplace','shop, person/hhld care travel',
                 'unpaid work to generate household income','travel as a part of work',
                 'other time at workplace','look for work ','travel to/from work','education travel')
  nonmarket <- c('regular schooling, education','homework','food preparation, cooking',
                 'set table, wash/put away dishes','cleaning','pet care (not walk dog)',
                 'maintain home/vehicle, including collect fuel','household management',
                 'laundry, ironing, clothing repair','shopping','consume other services','adult care')
  childcare <- c('physical, medical child care','teach, help with homework',
                 'read to, talk or play with child','child/adult care travel',
                 'supervise, accompany, other child care')
  leisure   <- c('leisure & other education or training','worship and religion','read',
                 'voluntary, civic, organisational act','party, social event, gambling',
                 'restaurant, caf?, bar, pub','walking','imputed time away from home',
                 'attend sporting event','knit, crafts or hobbies','other travel',
                 'general out-of-home leisure','listen to radio','cycling','walk dogs',
                 'no activity, imputed or recorded transport','other public event, venue',
                 'voluntary/civic/religious travel','cinema, theatre, opera, concert',
                 'general sport or exercise','other outside recreation','art or music',
                 'gardening/pick mushrooms','listen to music or other audio content',
                 'receive or visit friends','e-mail, surf internet, computing',
                 'games (social & solitary)/other in-home social','correspondence (not e-mail)',
                 'relax, think, do nothing','conversation (in person, phone)',
                 'general indoor leisure','watch TV, video, DVD, streamed film','computer games')
  ifelse(main_vec %in% personal,  "Personal Care",
  ifelse(main_vec %in% work,      "Market Work",
  ifelse(main_vec %in% nonmarket, "Non-Market Work",
  ifelse(main_vec %in% childcare, "Child Care",
  ifelse(main_vec %in% leisure,   "Leisure", "Not recorded")))))
}

secuenciador <- function(df, intervalos, amp) {
  seq_out <- rep(NA_character_, intervalos)
  for (i in seq_len(nrow(df))) {
    ep  <- df[i, ]
    ini <- max(1,          floor(ep$start / amp) + 1)
    fin <- min(intervalos, floor((ep$end - 1) / amp) + 1)
    if (ini <= fin) seq_out[ini:fin] <- ep$activityCategory
  }
  seq_out
}

# Detección de interrupción (pura O mixta) — devuelve data.frame(tiempo, tipo, duracion)
detectarInterrupcion <- function(secuencia, amp) {
  idxWork <- which(secuencia == "Market Work")
  idxCC   <- which(secuencia == "Child Care")
  if (!length(idxWork) || !length(idxCC))
    return(data.frame(tiempo=length(idxWork)*amp, tipo="Censored", duracion=0))

  iniWork <- idxWork[1]
  for (icc in idxCC) {
    if (icc <= iniWork) next
    if (secuencia[icc - 1] != "Market Work") next
    ret_pos <- which(idxWork > icc)[1]
    if (is.na(ret_pos)) next
    iret    <- idxWork[ret_pos]
    medio   <- secuencia[icc:(iret - 1)]

    esPura  <- all(medio == "Child Care")
    esMixta <- any(medio == "Child Care") &&
               any(medio %in% c("Personal Care","Leisure")) &&
               !any(medio == "Non-Market Work")

    if (esPura || esMixta) {
      tipo <- if (esPura) "Pure Interruption" else "Mixed Interruption"
      return(data.frame(
        tiempo   = length(idxWork[idxWork < icc]) * amp,
        tipo     = tipo,
        duracion = length(medio) * amp
      ))
    }
  }
  return(data.frame(tiempo=length(idxWork)*amp, tipo="Censored", duracion=0))
}

preparar_dfAnalisis <- function(dfTus, dfInd, etiqueta) {

  cat("\n--- Categorizando actividades [", etiqueta, "] ---\n")
  dfTus$activityCategory <- categorizarActividad(dfTus$main)

  cat("--- Construyendo matriz de secuencias ---\n")
  dfTus_slim <- dfTus[, c("id", "start", "end", "activityCategory")]
  ids   <- unique(dfTus_slim$id)
  lista <- lapply(split(dfTus_slim, dfTus_slim$id),
                  function(df) secuenciador(df, intervalos, amplitudIntervalo))
  lista <- lista[as.character(ids)]
  mat   <- matrix(unlist(lista), nrow = length(ids), byrow = TRUE)
  rownames(mat) <- ids
  rm(dfTus_slim, lista); gc()

  dic <- c("Personal Care","Market Work","Non-Market Work","Leisure","Child Care","Not recorded")
  seqObj <- seqdef(mat, alphabet = dic, states = dic, xtstep = amplitudIntervalo)

  cat("--- Detectando interrupciones ---\n")
  dfSurv <- map_dfr(seq_len(nrow(mat)),
    ~ detectarInterrupcion(mat[.x, ], amplitudIntervalo), .id = "row_index")
  dfSurv$id <- ids[as.numeric(dfSurv$row_index)]
  dfSurv <- dfSurv %>% select(id, tiempo, tipo, duracion)

  cat("Distribución ANTES de recalificar interrupciones >", CENSOR_THRESHOLD, "min:\n")
  print(round(prop.table(table(dfSurv$tipo)) * 100, 2))

  dfSurv <- dfSurv %>%
    mutate(
      tipo_raw = tipo,                                  # tipo original (pre-recalificacion)
      tipo = case_when(
        tipo %in% c("Pure Interruption","Mixed Interruption") & duracion > CENSOR_THRESHOLD ~ "Censored",
        TRUE ~ tipo
      ),
      estatus       = as.integer(tipo %in% c("Pure Interruption","Mixed Interruption")),
      estatus_pure  = as.integer(tipo == "Pure Interruption"),   # para riesgos competitivos
      estatus_mixed = as.integer(tipo == "Mixed Interruption")   # (mixto como evento; puro censurado y viceversa)
    )

  cat("Distribución DESPUÉS de recalificar:\n")
  print(round(prop.table(table(dfSurv$tipo)) * 100, 2))

  dfInd_u <- dfInd %>% distinct(id, .keep_all = TRUE)
  dfA     <- left_join(dfSurv, dfInd_u, by = "id")

  dfA <- dfA %>%
    mutate(
      hhldSizeGroup = case_when(
        hhldSize == 2 ~ "2 members",
        hhldSize == 3 ~ "3 members",
        hhldSize == 4 ~ "4 members",
        hhldSize >= 5 ~ "5+ members"),
      ageChildGroup = case_when(
        ageChild <= 4                  ~ "0-4 years",
        ageChild >= 5 & ageChild <= 12 ~ "5-12 years",
        ageChild >= 13                 ~ "13-17 years"),
      educaGroup = as.factor(
        ifelse(educa %in% c("1.0","2.0"), "Secondary education or lower",
        ifelse(educa == "3.0",            "Completed secondary education",
                                          "Higher education")))
    ) %>%
    mutate(
      educaGroup    = factor(educaGroup, levels=c("Secondary education or lower",
                               "Completed secondary education","Higher education")),
      hhldSizeGroup = factor(hhldSizeGroup, levels=c("2 members","3 members","4 members","5+ members")),
      ageChildGroup = factor(ageChildGroup, levels=c("0-4 years","5-12 years","13-17 years")),
      income        = factor(income, levels=c("lowest 25%","middle 50%","highest 25%")),
      income        = relevel(income, ref="highest 25%")
    ) %>%
    filter(tiempo > 0) %>%
    na.omit()

  cat("Dataset final [", etiqueta, "]: n =", nrow(dfA), "| eventos =", sum(dfA$estatus), "\n")

  # --- Composición de eventos para la Tabla 2 (sobre la MUESTRA FINAL dfA; suma a 100%) ----------
  comp_n     <- nrow(dfA)
  comp_pure  <- mean(dfA$tipo == "Pure Interruption")  * 100
  comp_mixed <- mean(dfA$tipo == "Mixed Interruption") * 100
  comp_any   <- mean(dfA$estatus == 1)                 * 100
  comp_cens  <- mean(dfA$tipo == "Censored")           * 100
  cat("\n>>> COMPOSICIÓN DE EVENTOS PARA TABLA 2 [", etiqueta, "] (muestra final) <<<\n")
  cat(sprintf("  Pure interruptions  : %5.2f%%\n", comp_pure))
  cat(sprintf("  Mixed interruptions : %5.2f%%\n", comp_mixed))
  cat(sprintf("  Any interruption    : %5.2f%%  (= pure + mixed)\n", comp_any))
  cat(sprintf("  Censored            : %5.2f%%\n", comp_cens))
  cat(sprintf("  CHECK suma          : %5.2f%%  | n = %d\n", comp_pure + comp_mixed + comp_cens, comp_n))

  list(df = dfA, seqObj = seqObj, ids = ids)
}


# ==================================================================================================
# ----[ 1. ANÁLISIS US 2003-2023 ]------------------------------------------------------------------
# ==================================================================================================

cat("\n\n========== ANÁLISIS ESTADOS UNIDOS 2003-2023 ==========\n")

load(file.path(DATA_DIR, "TUS_0723.RData"))
load(file.path(DATA_DIR, "Ind_0723.RData"))

res_us <- preparar_dfAnalisis(dfTus_0723, Ind_0723, "US 2003-2023")
dfA    <- res_us$df
rm(dfTus_0723, Ind_0723); gc()

surv_obj <- Surv(dfA$tiempo, dfA$estatus)


# ==================================================================================================
# ----[ 1.0b ACTIVITY STATE DISTRIBUTION PLOT ]-----------------------------------------------------

save_base({
  seqdplot(res_us$seqObj,
           border = NA,
           main   = "Activity state distribution — US 2003-2023",
           xlab   = "Time of day (10-min intervals)",
           ylab   = "Proportion",
           legend.prop = 0.2)
}, "fig_activity_states.pdf", w = 14, h = 7)


# ==================================================================================================
# ----[ 1.1 DESCRIPTIVOS ]--------------------------------------------------------------------------

cat("\n--- ESTADÍSTICOS DESCRIPTIVOS ---\n")
tablaDesc(dfA, c("age","nChild","hhldSize","ageChild"))
for (v in c("male","income","midWeek","urban","cohab","educaGroup","citizen","familyStatus")) {
  tbl <- table(dfA[[v]])
  pct <- round(prop.table(tbl)*100, 2)
  m   <- cbind(Frecuencia=as.numeric(tbl), Porcentaje=as.numeric(pct))
  rownames(m) <- names(tbl)
  stargazer(m, type="text", title=paste("Tabla de frecuencias:", v), digits=2, align=TRUE)
}


# ==================================================================================================
# ----[ 1.2 KAPLAN-MEIER (con pesos) ]--------------------------------------------------------------

cat("\n--- CURVAS KAPLAN-MEIER ---\n")

km_vars <- list(
  list(var="male",         title="Gender",         labs=c("Female","Male")),
  list(var="educaGroup",   title="Education",      labs=c("Sec. or lower","Completed sec.","Higher")),
  list(var="hhldSizeGroup",title="Household size", labs=c("2","3","4","5+")),
  list(var="ageChildGroup",title="Age of child",   labs=c("0-4","5-12","13-17")),
  list(var="income",       title="Income",         labs=c("Lowest 25%","Middle 50%","Highest 25%")),
  list(var="familyStatus", title="Family status",  labs=NULL)
)

for (km in km_vars) {
  cat("KM:", km$title, "\n")
  tryCatch({
    fmla <- as.formula(paste("Surv(tiempo, estatus) ~", km$var))
    # bquote incrusta el data.frame directamente en el call para que ggsurvplot lo encuentre
    fit  <- eval(bquote(survfit(.(fmla), data = .(dfA))))
    labs_arg <- if (!is.null(km$labs)) km$labs else names(table(dfA[[km$var]]))
    p    <- ggsurvplot(fit, data = dfA, pval = TRUE, conf.int = TRUE,
                       risk.table = FALSE, ylim = c(0.7, 1),
                       xlab = "Time (Minutes)",
                       ylab = "Probability of no interruption",
                       legend.title = km$title,
                       legend.labs  = labs_arg,
                       ggtheme = themePaper())
    fname <- paste0("fig_km_us_", tolower(gsub("[ /]","_", km$var)), ".pdf")
    save_gg(p$plot, fname, w = 10, h = 7)
    cat("Log-rank test:\n")
    print(survdiff(fmla, data = dfA))
  }, error = function(e) cat("KM error [", km$title, "]:", e$message, "\n"))
}


# ==================================================================================================
# ----[ 1.3 MODELO DE COX: pesos + clustering ]-----------------------------------------------------

cat("\n\n--- MODELO COX (pesos + clustering por hogar) ---\n")

modeloCox <- coxph(
  Surv(tiempo, estatus) ~ male + age + midWeek + hhldSize + nChild + ageChild +
    urban + income + familyStatus + educaGroup + cluster(hldid),
  weights = ocombwt,
  data    = dfA,
  x       = TRUE
)
cat("\n--- Resultados Cox con pesos y errores clusterizados ---\n")
print(summary(modeloCox))


# ==================================================================================================
# ----[ 1.4 COX ESTRATIFICADO: corrección violación PH en midWeek ]---------------------------------

cat("\n\n--- COX ESTRATIFICADO por midWeek (corrección PH) ---\n")

modeloCoxStrat <- coxph(
  Surv(tiempo, estatus) ~ male + age + hhldSize + nChild + ageChild +
    urban + income + familyStatus + educaGroup + strata(midWeek) + cluster(hldid),
  weights = ocombwt,
  data    = dfA,
  x       = TRUE
)
print(summary(modeloCoxStrat))

# Test PH en el modelo estratificado
test_ph_strat <- cox.zph(modeloCoxStrat)
cat("\n--- Test PH (modelo estratificado) ---\n")
print(test_ph_strat)

# ── FIX: ggcoxzph devuelve una lista de ggplots; combinarlos explícitamente ──
tryCatch({
  zph_plots <- ggcoxzph(test_ph_strat)
  # Extraer los ggplots limpios (cada elemento ya es un ggplot)
  gg_list <- lapply(zph_plots, function(x) {
    if (inherits(x, "ggplot")) x
    else if (is.list(x) && inherits(x[[1]], "ggplot")) x[[1]]
    else NULL
  })
  gg_list <- Filter(Negate(is.null), gg_list)
  n_plots <- length(gg_list)
  ncols   <- min(3, n_plots)
  nrows   <- ceiling(n_plots / ncols)
  p_zph   <- do.call(gridExtra::arrangeGrob,
                     c(gg_list, list(ncol = ncols, nrow = nrows,
                                     top  = "Schoenfeld residuals — PH test")))
  save_base(grid::grid.draw(p_zph), "fig_schoenfeld.pdf", w = 14, h = 4 * nrows)
}, error = function(e) {
  # Fallback: base-R plot
  cat("ggcoxzph fallback a base-R:", e$message, "\n")
  save_base(plot(test_ph_strat), "fig_schoenfeld.pdf", w = 14, h = 9)
})


# ==================================================================================================
# ----[ 1.4b RIESGOS COMPETITIVOS: interrupciones PURAS vs MIXTAS ]---------------------------------
# Modelos de hazard causa-especifico (Cox): la tipologia pure/mixed deja de ser solo
# descriptiva y se explota econometricamente. Para el hazard de cada causa, el evento
# competidor se trata como censurado (convencion estandar de cause-specific hazards).
# ==================================================================================================

cat("\n\n--- RIESGOS COMPETITIVOS: Cox causa-especifico (pure vs mixed) ---\n")
cat(sprintf("Eventos: pure = %d | mixed = %d\n",
            sum(dfA$estatus_pure), sum(dfA$estatus_mixed)))

rhs_cr <- "male + age + midWeek + hhldSize + nChild + ageChild + urban + income + familyStatus + educaGroup + cluster(hldid)"

# (i) Hazard causa-especifico de interrupcion PURA (mixtas -> censuradas) -------------------------
cox_pure <- tryCatch(
  coxph(as.formula(paste("Surv(tiempo, estatus_pure) ~", rhs_cr)),
        weights = ocombwt, data = dfA, x = TRUE),
  error = function(e) { cat("  Cox pure error:", e$message, "\n"); NULL })
if (!is.null(cox_pure)) { cat("\n--- Cox causa-especifico: PURA ---\n"); print(summary(cox_pure)) }

# (ii) Hazard causa-especifico de interrupcion MIXTA (puras -> censuradas) ------------------------
# Nota: pocos eventos mixtos (~0.7%); estimaciones potencialmente imprecisas.
cox_mixed <- tryCatch(
  coxph(as.formula(paste("Surv(tiempo, estatus_mixed) ~", rhs_cr)),
        weights = ocombwt, data = dfA, x = TRUE),
  error = function(e) { cat("  Cox mixed error:", e$message, "\n"); NULL })
if (!is.null(cox_mixed)) { cat("\n--- Cox causa-especifico: MIXTA ---\n"); print(summary(cox_mixed)) }

# (iii) Tabla comparativa del HR de genero por causa (vs. modelo agrupado) ------------------------
extract_hr <- function(fit, label) {
  if (is.null(fit) || !("male1" %in% names(coef(fit)))) return(NULL)
  cf <- coef(fit)["male1"]; se <- sqrt(vcov(fit)["male1","male1"])
  data.frame(modelo = label, HR = exp(cf),
             lower = exp(cf - 1.96*se), upper = exp(cf + 1.96*se),
             eventos = fit$nevent, row.names = NULL)
}
hr_cr <- do.call(rbind, list(
  extract_hr(modeloCox, "Agrupado (pure+mixed)"),
  extract_hr(cox_pure,  "Causa-especifico: pure"),
  extract_hr(cox_mixed, "Causa-especifico: mixed")
))
if (!is.null(hr_cr)) {
  cat("\n--- HR de genero (male) por causa ---\n")
  print(hr_cr, digits = 3)
}

# (iv) Robustez con subdistribucion de Fine-Gray (si survival::finegray esta disponible) ----------
tryCatch({
  cov_cr <- c("male","age","midWeek","hhldSize","nChild","ageChild",
              "urban","income","familyStatus","educaGroup")
  dfFG <- dfA[, c("tiempo","estatus_pure","estatus_mixed","ocombwt", cov_cr)]
  dfFG$event_cr <- factor(
    ifelse(dfFG$estatus_pure == 1, "pure",
    ifelse(dfFG$estatus_mixed == 1, "mixed", "censor")),
    levels = c("censor","pure","mixed"))
  fg_data <- finegray(Surv(tiempo, event_cr) ~ ., data = dfFG[, c("tiempo","event_cr","ocombwt", cov_cr)],
                      etype = "pure", weights = ocombwt)
  fg_fit  <- coxph(
    as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~", paste(cov_cr, collapse = " + "))),
    weights = fgwt, data = fg_data)
  cat("\n--- Fine-Gray (subdistribucion) para interrupcion PURA ---\n")
  print(summary(fg_fit))
}, error = function(e) cat("  Fine-Gray omitido:", e$message, "\n"))


# ==================================================================================================
# ----[ 1.5 RSF: pesos + tuning ]-------------------------------------------------------------------

cat("\n\n--- RANDOM SURVIVAL FOREST (con pesos) ---\n")

vars_rsf <- c("tiempo","estatus","male","age","midWeek","hhldSize","nChild","ageChild",
              "urban","income","cohab","citizen","educaGroup","familyStatus")
dfRSF <- dfA[, vars_rsf]

set.seed(11082025)
model_rsf <- rfsrc(
  Surv(tiempo, estatus) ~ .,
  data      = dfRSF,
  case.wt   = dfA$ocombwt,
  ntree     = 500,
  nodesize  = 15,     # explicito: coincide con lo reportado en el paper
  mtry      = 4,      # explicito: coincide con lo reportado en el paper
  importance= TRUE
)
cat("\n--- RSF baseline ---\n")
print(model_rsf)

# RSF convergence + VIMP (2-panel base-R plot)
save_base({
  par(mfrow = c(1, 2))
  plot(model_rsf)
}, "fig_rsf_vimp.pdf", w = 14, h = 7)

# tune() crashes R on Windows — use baseline model
cat("\n--- RSF optimizado: se usa baseline (nodesize=15, mtry=4, ntree=500) ---\n")
model_rsf_opt <- model_rsf

c_index_rsf <- 1 - model_rsf_opt$err.rate[model_rsf_opt$ntree]
cat(paste("C-index RSF optimizado:", round(c_index_rsf, 4), "\n"))


# ==================================================================================================
# ----[ 1.6 SHAP VALUES (fastshap) ]----------------------------------------------------------------

cat("\n\n--- SHAP VALUES (muestra n=2000, nsim=50) ---\n")

set.seed(42)
idx_shap <- sample(nrow(dfRSF), min(2000, nrow(dfRSF)))
X_shap   <- dfRSF[idx_shap, setdiff(names(dfRSF), c("tiempo","estatus"))]

pfun_rsf <- function(object, newdata) {
  as.numeric(predict(object, newdata = newdata)$predicted)
}

shap_vals <- tryCatch({
  explain(
    object       = model_rsf_opt,
    X            = X_shap,
    pred_wrapper = pfun_rsf,
    nsim         = 50
  )
}, error = function(e) { cat("SHAP error:", e$message, "\n"); NULL })

if (!is.null(shap_vals)) {
  shap_imp <- colMeans(abs(shap_vals))
  shap_df  <- data.frame(
    Variable   = names(shap_imp),
    SHAP_mean  = as.numeric(shap_imp)
  ) %>% arrange(desc(SHAP_mean))

  cat("\n--- Importancia SHAP (|valor medio|) ---\n")
  print(shap_df)

  p_shap <- ggplot(shap_df, aes(x=reorder(Variable, SHAP_mean), y=SHAP_mean)) +
    geom_col(fill="steelblue") +
    coord_flip() +
    labs(title="SHAP Feature Importance (RSF)", x=NULL, y="|Mean SHAP value|") +
    themePaper()
  save_gg(p_shap, "fig_shap_importance.pdf", w = 10, h = 7)

  # Beeswarm / scatter SHAP por variable (top 6) — cada una por separado
  top6 <- head(shap_df$Variable, 7)
  for (v in top6) {
    df_shap_v <- data.frame(
      feature_val = as.numeric(factor(X_shap[[v]])),
      shap_val    = shap_vals[, v]
    )
    p_bee <- ggplot(df_shap_v, aes(x=feature_val, y=shap_val)) +
      geom_point(alpha=0.3, color="steelblue", size=0.8) +
      geom_smooth(method="loess", se=FALSE, color="red", linewidth=1) +
      labs(title=paste("SHAP:", v), x=v, y="SHAP value") +
      themePaper()
    save_gg(p_bee, paste0("fig_shap_", v, ".pdf"), w = 8, h = 6)
  }
}


# ==================================================================================================
# ----[ 1.7 ALE PLOTS (iml) ]-----------------------------------------------------------------------

cat("\n\n--- ALE PLOTS (Accumulated Local Effects, muestra n=2000) ---\n")

tryCatch({
  predictor_iml <- Predictor$new(
    model            = model_rsf_opt,
    data             = X_shap,
    predict.function = function(model, newdata) pfun_rsf(model, newdata)
  )

  top_ale <- if (!is.null(shap_vals)) head(shap_df$Variable, 9) else
    c("ageChild","male","nChild","educaGroup","hhldSize","familyStatus","age","income","midWeek")

  for (v in top_ale) {
    ale <- FeatureEffect$new(predictor_iml, feature=v, method="ale")
    p_ale <- ale$plot() +
      ggtitle(paste("ALE plot:", v)) +
      ylab("Effect on ensemble mortality") +
      themePaper()
    save_gg(p_ale, paste0("fig_ale_us_", v, ".pdf"), w = 8, h = 6)
    cat("ALE:", v, "completado\n")
  }
}, error = function(e) cat("ALE error:", e$message, "\n"))


# ==================================================================================================
# ----[ 1.8 GRADIENT BOOSTING SURVIVAL (gbm) ]------------------------------------------------------

cat("\n\n--- GRADIENT BOOSTING SURVIVAL (gbm, familia CoxPH) ---\n")

dfGBM <- dfRSF %>%
  mutate(across(where(is.factor), as.integer))

tryCatch({
  set.seed(42)
  model_gbm <- gbm(
    Surv(tiempo, estatus) ~ .,
    distribution      = "coxph",
    data              = dfGBM,
    n.trees           = 500,
    interaction.depth = 3,
    shrinkage         = 0.05,
    cv.folds          = 5,
    n.cores           = 1,
    verbose           = FALSE
  )

  best_iter <- gbm.perf(model_gbm, method="cv", plot.it=FALSE)
  cat("Iteración óptima (CV):", best_iter, "\n")

  # Convergencia GBM como ggplot
  train_loss <- model_gbm$train.error
  cv_loss    <- model_gbm$cv.error
  df_gbm_cv  <- data.frame(
    iter  = seq_along(train_loss),
    train = train_loss,
    cv    = cv_loss
  ) %>% pivot_longer(c(train, cv), names_to="type", values_to="loss")
  p_gbm_cv <- ggplot(df_gbm_cv, aes(x=iter, y=loss, color=type)) +
    geom_line(linewidth=0.7) +
    geom_vline(xintercept=best_iter, linetype="dashed", color="grey40") +
    scale_color_manual(values=c(train="steelblue", cv="tomato"),
                       labels=c(train="Training", cv="CV"),
                       name="Loss") +
    labs(title="GBM: training vs. CV loss", x="Iteration", y="Loss") +
    themePaper()
  save_gg(p_gbm_cv, "fig_gbm_convergence.pdf", w = 10, h = 6)

  # Importancia GBM como ggplot
  gbm_imp <- summary(model_gbm, n.trees=best_iter, plotit=FALSE)
  cat("\n--- Importancia GBM ---\n")
  print(gbm_imp)
  p_gbm_imp <- ggplot(gbm_imp, aes(x=reorder(var, rel.inf), y=rel.inf)) +
    geom_col(fill="steelblue") +
    coord_flip() +
    labs(title="GBM Variable Importance", x=NULL, y="Relative influence (%)") +
    themePaper()
  save_gg(p_gbm_imp, "fig_gbm_importance.pdf", w = 10, h = 7)

  # C-index GBM
  # OJO orientacion: gbm(distribution='coxph', type='link') devuelve un log-hazard
  # (score de RIESGO: mayor => mayor hazard => menor supervivencia). En cambio,
  # survival::concordance(Surv ~ x) interpreta el predictor en escala de SUPERVIVENCIA
  # (mayor => vive mas). Pasar el score de riesgo sin invertir devuelve 1 - C
  # (de ahi el 0.282 = 1 - 0.718). Negamos el predictor para alinear convenciones.
  lp_gbm <- predict(model_gbm, newdata=dfGBM, n.trees=best_iter, type="link")
  c_index_gbm <- concordance(Surv(dfGBM$tiempo, dfGBM$estatus) ~ I(-lp_gbm))$concordance
  if (c_index_gbm < 0.5) {            # red de seguridad ante cambios de version
    warning("C-index GBM < 0.5 tras invertir; revisar orientacion del predictor")
    c_index_gbm <- 1 - c_index_gbm
  }
  cat("C-index GBM:", round(c_index_gbm, 4), "\n")

}, error = function(e) cat("GBM error:", e$message, "\n"))


# ==================================================================================================
# ----[ 1.9 COMPARACIÓN DE MODELOS (C-index + Brier Score) ]---------------------------------------

cat("\n\n--- COMPARACIÓN DE MODELOS ---\n")

c_cox     <- concordance(modeloCox)$concordance
c_cox_str <- concordance(modeloCoxStrat)$concordance
cat(sprintf("C-index Cox (base):         %.4f\n", c_cox))
cat(sprintf("C-index Cox (estratificado): %.4f\n", c_cox_str))
cat(sprintf("C-index RSF (optimizado):   %.4f\n",  c_index_rsf))
if (exists("c_index_gbm")) cat(sprintf("C-index GBM:                %.4f\n", c_index_gbm))

cat("\n--- Brier Score integrado (pec) ---\n")
tryCatch({
  # pec no soporta Cox con pesos; se ajustan versiones sin pesos solo para este bloque
  cox_noWt <- coxph(
    Surv(tiempo, estatus) ~ male + age + midWeek + hhldSize + nChild + ageChild +
      urban + income + familyStatus + educaGroup,
    data = dfRSF, x = TRUE
  )
  cox_strat_noWt <- coxph(
    Surv(tiempo, estatus) ~ male + age + hhldSize + nChild + ageChild +
      urban + income + familyStatus + educaGroup + strata(midWeek),
    data = dfRSF, x = TRUE
  )

  predictSurvProb.rfsrc <- function(object, newdata, times, ...) {
    ptemp <- predict(object, newdata=newdata)$survival
    pos   <- prodlim::sindex(jump.times=object$time.interest, eval.times=times)
    p     <- cbind(1, ptemp)[, pos + 1, drop=FALSE]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
      stop("predictSurvProb.rfsrc: dimensiones incorrectas")
    p
  }
  assignInNamespace("predictSurvProb.rfsrc", predictSurvProb.rfsrc, ns="pec")

  times_eval <- quantile(dfRSF$tiempo[dfRSF$estatus==1], probs=seq(0.1,0.9,by=0.1))

  pec_res <- pec(
    list(Cox=cox_noWt, Cox_Strat=cox_strat_noWt, RSF=model_rsf_opt),
    formula      = Surv(tiempo, estatus) ~ 1,
    data         = dfRSF,
    times        = times_eval,
    splitMethod  = "none"
  )
  print(pec_res)

  ibs <- crps(pec_res, times=times_eval)
  cat("\n--- Integrated Brier Score ---\n")
  print(ibs)

}, error = function(e) cat("pec/Brier error:", e$message, "\n"))


# ==================================================================================================
# ----[ 1.9b SENSIBILIDAD AL UMBRAL DE CENSURA ]----------------------------------------------------
# Reestima el HR de genero para varios umbrales (incl. sin tope) usando el tipo ORIGINAL
# (tipo_raw) y la duracion almacenada, sin re-detectar interrupciones.
# ==================================================================================================

cat("\n\n--- SENSIBILIDAD AL UMBRAL DE CENSURA (HR de genero) ---\n")

umbral_sens <- function(thr) {
  d <- dfA
  d$est_thr <- as.integer(
    d$tipo_raw %in% c("Pure Interruption","Mixed Interruption") & d$duracion <= thr)
  fit <- tryCatch(
    coxph(Surv(tiempo, est_thr) ~ male + age + midWeek + hhldSize + nChild + ageChild +
            urban + income + familyStatus + educaGroup + cluster(hldid),
          weights = ocombwt, data = d, x = FALSE),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  cf <- coef(fit)["male1"]; se <- sqrt(vcov(fit)["male1","male1"])
  data.frame(umbral = ifelse(is.finite(thr), as.character(thr), "sin tope"),
             eventos = fit$nevent, HR = exp(cf),
             lower = exp(cf - 1.96*se), upper = exp(cf + 1.96*se), row.names = NULL)
}

sens_df <- do.call(rbind, lapply(c(90, 120, 150, Inf), umbral_sens))
if (!is.null(sens_df)) {
  cat("HR de genero (male) segun umbral de recalificacion a censura:\n")
  print(sens_df, digits = 3)
}


# ==================================================================================================
# ----[ 2. ANÁLISIS MULTI-PAÍS ]--------------------------------------------------------------------
# ==================================================================================================

cat("\n\n========== ANÁLISIS MULTI-PAÍS (2007+) ==========\n")

load(file.path(DATA_DIR, "TUS_Multi07.RData"))
load(file.path(DATA_DIR, "Ind_Multi07.RData"))

res_multi <- preparar_dfAnalisis(dfTus_Multi07, Ind_Multi07, "Multi-país 2007+")
dfM       <- res_multi$df
rm(dfTus_Multi07, Ind_Multi07); gc()


# ==================================================================================================
# ----[ 2.1 DESCRIPTIVOS POR PAÍS ]-----------------------------------------------------------------

cat("\n--- Distribución por país ---\n")
pais_desc <- dfM %>%
  group_by(country) %>%
  summarise(
    n         = n(),
    eventos   = sum(estatus),
    pct_evt   = round(mean(estatus)*100, 2),
    pct_female= round(mean(as.numeric(as.character(male))==0)*100, 1)
  )
print(pais_desc)

p_paises <- ggplot(pais_desc, aes(x=reorder(country, pct_evt), y=pct_evt)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Work interruption rate by country",
       x=NULL, y="% interrupted") +
  themePaper()
save_gg(p_paises, "fig_event_rates.pdf", w = 10, h = 7)


# ==================================================================================================
# ----[ 2.1b ACTIVITY STATE DISTRIBUTION PLOT — MULTI-PAÍS ]---------------------------------------

save_base({
  seqdplot(res_multi$seqObj,
           border = NA,
           main   = "Activity state distribution — Multi-country 2007+",
           xlab   = "Time of day (10-min intervals)",
           ylab   = "Proportion",
           legend.prop = 0.2)
}, "fig_activity_states_multi.pdf", w = 14, h = 7)


# ==================================================================================================
# ----[ 2.2 KM POR GÉNERO PARA CADA PAÍS ]----------------------------------------------------------

cat("\n--- KM por género (comparación entre países) ---\n")

paises_ok <- pais_desc %>% filter(eventos >= 30) %>% pull(country) %>% as.character()

km_plots <- lapply(paises_ok, function(ctry) {
  df_c <- dfM[dfM$country == ctry, ]
  if (sum(df_c$male == 0) < 10 | sum(df_c$male == 1) < 10) return(NULL)
  tryCatch({
    fit_c <- survfit(Surv(tiempo, estatus) ~ male, data=df_c)
    p <- ggsurvplot(fit_c, data=df_c, pval=TRUE, conf.int=FALSE,
                    ylim=c(0.7,1), legend.title="Gender",
                    legend.labs=c("Female","Male"),
                    title=ctry, xlab="Time (min)",
                    ylab="P(no interruption)", ggtheme=themePaper(),
                    font.title=9, font.x=8, font.y=8)
    p$plot
  }, error=function(e) NULL)
})
km_plots <- Filter(Negate(is.null), km_plots)

if (length(km_plots) >= 2) {
  n_cols <- min(3, length(km_plots))
  p_km_multi <- do.call(gridExtra::arrangeGrob,
                         c(km_plots, list(ncol=n_cols,
                                          top="Kaplan-Meier by gender and country")))
  save_base(grid::grid.draw(p_km_multi), "fig_km_multicountry.pdf", w = 14, h = 9)
}


# ==================================================================================================
# ----[ 2.2a PREPARACIÓN dfM (pesos válidos, país como factor) ]------------------------------------

dfM <- dfM[!is.na(dfM$ocombwt) & dfM$ocombwt > 0, ]
dfM$country <- relevel(factor(dfM$country), ref = "United States")

# ==================================================================================================
# ----[ 2.2b POOLED COX CON EFECTOS FIJOS DE PAÍS (US = referencia) ]-------------------------------
# Modelo agrupado sobre los 8 paises con dummies de pais, pesos y SE clusterizados por hogar.
# Reproduce la seccion 5.8.2 del paper (HR de genero pooled + FE de pais vs. EE.UU.).
# ==================================================================================================

cat("\n\n--- POOLED COX CON FE DE PAÍS (US = referencia) ---\n")

pooled_cox <- tryCatch(
  coxph(Surv(tiempo, estatus) ~ male + age + midWeek + hhldSize + nChild + ageChild +
          urban + income + educaGroup + country + cluster(hldid_univ),
        weights = ocombwt, data = dfM, x = TRUE),
  error = function(e) { cat("  Pooled Cox error:", e$message, "\n"); NULL })

if (!is.null(pooled_cox)) {
  print(summary(pooled_cox))
  cat(sprintf("\nC-index pooled: %.4f\n", concordance(pooled_cox)$concordance))

  # HR de genero pooled
  cf_m <- coef(pooled_cox)["male1"]; se_m <- sqrt(vcov(pooled_cox)["male1","male1"])
  cat(sprintf("HR genero (male) pooled: %.3f (%.3f-%.3f)\n",
              exp(cf_m), exp(cf_m - 1.96*se_m), exp(cf_m + 1.96*se_m)))

  # Efectos fijos de pais como HR (baseline relativo a EE.UU.)
  cf_all  <- coef(pooled_cox)
  ctry_nm <- grep("^country", names(cf_all), value = TRUE)
  if (length(ctry_nm) > 0) {
    V <- vcov(pooled_cox)
    fe_df <- data.frame(
      country = sub("^country", "", ctry_nm),
      HR      = exp(cf_all[ctry_nm]),
      lower   = exp(cf_all[ctry_nm] - 1.96 * sqrt(diag(V)[ctry_nm])),
      upper   = exp(cf_all[ctry_nm] + 1.96 * sqrt(diag(V)[ctry_nm])),
      row.names = NULL)
    cat("\n--- Efectos fijos de país (HR baseline vs. EE.UU.) ---\n")
    print(fe_df, digits = 3)
  }
}


# ==================================================================================================
# ----[ 2.3 COX POR PAÍS (modelos separados) ]------------------------------------------------------

cat("\n\n--- COX POR PAÍS (modelos separados, pesos + clustering) ---\n")

# Core covariates available in all country datasets
covars_base <- c("male", "age", "midWeek", "hhldSize", "nChild", "ageChild",
                 "urban", "income", "educaGroup")

country_cox   <- list()   # fitted Cox objects, keyed by country
country_hr_df <- NULL     # HR summary for forest plot

for (ctry in paises_ok) {
  cat("\n===", ctry, "===\n")
  df_c <- dfM[dfM$country == ctry, ]

  if (sum(df_c$estatus) < 20) {
    cat("  Skipped:", sum(df_c$estatus), "events — insufficient\n")
    next
  }

  # Eliminar niveles de factor vacios en este pais (causa del fallo de Canada:
  # un nivel de income/educaGroup/familyStatus sin observaciones reventaba summary()).
  df_c <- droplevels(df_c)

  # Candidatas: base + familyStatus si esta presente
  covars_c <- if ("familyStatus" %in% names(df_c)) c(covars_base, "familyStatus") else covars_base

  # Conservar solo covariables utilizables: factores con >= 2 niveles; numericas con varianza.
  # 'male' debe sobrevivir (es la variable de interes); si no varia, se omite el pais.
  keep_var <- function(v) {
    x <- df_c[[v]]
    if (is.factor(x))      nlevels(droplevels(x[!is.na(x)])) > 1
    else                   length(unique(na.omit(x))) > 1
  }
  covars_c <- covars_c[vapply(covars_c, keep_var, logical(1))]
  if (!"male" %in% covars_c) {
    cat("  Skipped [", ctry, "]: 'male' sin variacion\n"); next
  }

  fmla_c <- as.formula(
    paste("Surv(tiempo, estatus) ~",
          paste(covars_c, collapse = " + "),
          "+ cluster(hldid_univ)")
  )

  # 1) Ajuste del modelo (si falla aqui, el pais no es estimable)
  fit_c <- tryCatch(
    coxph(fmla_c, weights = ocombwt, data = df_c, x = TRUE),
    error = function(e) { cat("  Cox fit error [", ctry, "]:", e$message, "\n"); NULL })
  if (is.null(fit_c)) next
  country_cox[[ctry]] <- fit_c

  # 2) Extraer HR de genero PRIMERO (no depende de summary/cox.zph) ------------------
  cf_all  <- coef(fit_c)
  male_nm <- grep("^male", names(cf_all), value = TRUE)[1]
  if (!is.na(male_nm) && !is.na(cf_all[male_nm])) {
    se <- sqrt(vcov(fit_c)[male_nm, male_nm])
    cf <- cf_all[male_nm]
    country_hr_df <- rbind(country_hr_df, data.frame(
      country = ctry,
      HR      = exp(cf),
      lower   = exp(cf - 1.96 * se),
      upper   = exp(cf + 1.96 * se),
      events  = sum(df_c$estatus),
      n       = nrow(df_c),
      stringsAsFactors = FALSE
    ))
  } else {
    cat("  Aviso [", ctry, "]: coeficiente de 'male' no disponible\n")
  }

  # 3) Resumen completo (en bloque propio: si falla, el HR ya esta guardado) ---------
  tryCatch({
    cat("\n--- Cox results:", ctry, "---\n")
    print(summary(fit_c))
  }, error = function(e) cat("  summary error [", ctry, "]:", e$message, "\n"))

  # 4) Test PH (en bloque propio) ----------------------------------------------------
  tryCatch({
    ph_c <- cox.zph(fit_c)
    cat("PH test:", ctry, "\n"); print(ph_c)
  }, error = function(e) cat("  PH test error [", ctry, "]:", e$message, "\n"))
}

if (!is.null(country_hr_df) && nrow(country_hr_df) > 0) {
  cat("\n--- Gender HR by country (sorted) ---\n")
  print(country_hr_df[order(country_hr_df$HR), ])
}


# ==================================================================================================
# ----[ 2.4 FOREST PLOT: HR por género según país ]-------------------------------------------------

cat("\n\n--- FOREST PLOT: hazard ratio del género por país ---\n")

if (!is.null(country_hr_df) && nrow(country_hr_df) > 0) {
  p_forest <- ggplot(country_hr_df,
                     aes(x = reorder(country, HR), y = HR, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.7, color = "steelblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() +
    scale_y_log10() +
    labs(x    = NULL,
         y    = "HR (log scale) — values < 1: men less likely to interrupt") +
    themePaper() +
    theme(panel.grid.major.x = element_line(color = "grey85"))
  save_gg(p_forest, "fig_forest_plot.pdf", w = 10, h = 7)
}


# ==================================================================================================
# ----[ 2.5 RSF POR PAÍS (mínimo 50 eventos) ]------------------------------------------------------

cat("\n\n--- RSF POR PAÍS (500 árboles, con pesos) ---\n")

vars_rsf_c <- c("tiempo", "estatus", "male", "age", "midWeek", "hhldSize", "nChild",
                "ageChild", "urban", "income", "cohab", "educaGroup")

for (ctry in paises_ok) {
  df_c <- dfM[dfM$country == ctry, ]

  if (sum(df_c$estatus) < 50) {
    cat("  RSF skipped [", ctry, "]: only", sum(df_c$estatus), "events\n")
    next
  }

  vars_c   <- intersect(vars_rsf_c, names(df_c))
  df_rsf_c <- df_c[, vars_c]

  tryCatch({
    cat("\nRSF:", ctry, "\n")
    set.seed(11082025)
    rsf_c <- rfsrc(
      Surv(tiempo, estatus) ~ .,
      data      = df_rsf_c,
      case.wt   = df_c$ocombwt,
      ntree     = 500,
      nodesize  = 15,
      mtry      = 4,
      importance = TRUE
    )
    cat("C-index RSF [", ctry, "]:",
        round(1 - rsf_c$err.rate[rsf_c$ntree], 4), "\n")

    fname_rsf <- paste0("fig_rsf_", gsub("[ /]", "_", tolower(ctry)), "_vimp.pdf")
    save_base({
      par(mfrow = c(1, 2))
      plot(rsf_c)
    }, fname_rsf, w = 14, h = 7)

  }, error = function(e) cat("  RSF error [", ctry, "]:", e$message, "\n"))
}


# ==================================================================================================
cat("\n\n=== ANÁLISIS COMPLETO V2 FINALIZADO ===\n")
cat("Figuras: ", FIGS_DIR, "\n")
cat("Log:     ", LOG_FILE,  "\n")

}, error = function(e) {
  cat("\n!!! ERROR EN EL ANÁLISIS !!!\n")
  cat("Mensaje:", conditionMessage(e), "\n")
}, finally = {
  sink()
})
