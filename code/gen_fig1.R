# Generate Figure 1: Distribution of activity states by gender across the day
# Output: output/figures/fig_activity_states.pdf

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

BASE_DIR <- "C:/data/Childcare_ML/"
DATA_DIR <- file.path(BASE_DIR, "data")
FIGS_DIR <- file.path(BASE_DIR, "output", "figures")
dir.create(FIGS_DIR, showWarnings = FALSE, recursive = TRUE)

amplitudIntervalo <- 10
totalMinutosDia   <- 1440
intervalos        <- totalMinutosDia / amplitudIntervalo

# ── Activity categorisation ────────────────────────────────────────────────────
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

# ── Load data ──────────────────────────────────────────────────────────────────
cat("Loading data...\n")
load(file.path(DATA_DIR, "TUS_0723.RData"))
load(file.path(DATA_DIR, "Ind_0723.RData"))

cat("Categorising activities...\n")
dfTus_0723$activityCategory <- categorizarActividad(dfTus_0723$main)

cat("Building sequence matrix...\n")
dfTus_slim <- dfTus_0723[, c("id", "start", "end", "activityCategory")]
ids   <- unique(dfTus_slim$id)
lista <- lapply(split(dfTus_slim, dfTus_slim$id),
                function(df) secuenciador(df, intervalos, amplitudIntervalo))
lista <- lista[as.character(ids)]
mat   <- matrix(unlist(lista), nrow = length(ids), byrow = TRUE)
rownames(mat) <- ids
rm(dfTus_slim, lista); gc()

# ── Merge gender ───────────────────────────────────────────────────────────────
Ind_u     <- Ind_0723 %>% distinct(id, .keep_all = TRUE) %>% select(id, male)
id_gender <- data.frame(id = ids, stringsAsFactors = FALSE) %>%
  left_join(Ind_u, by = "id")

# ── Compute proportions per interval per gender ────────────────────────────────
cat("Computing proportions...\n")
act_levels <- c("Personal Care", "Market Work", "Non-Market Work",
                "Leisure", "Child Care", "Not recorded")

compute_props <- function(gender_label, gender_val) {
  idx     <- which(id_gender$male == gender_val)
  sub_mat <- mat[idx, , drop = FALSE]
  do.call(rbind, lapply(seq_len(ncol(sub_mat)), function(j) {
    col_vals <- sub_mat[, j]
    col_vals[is.na(col_vals)] <- "Not recorded"
    tbl   <- table(factor(col_vals, levels = act_levels))
    props <- prop.table(tbl) * 100
    data.frame(interval = j, activity = names(props),
               pct = as.numeric(props), gender = gender_label,
               stringsAsFactors = FALSE)
  }))
}

df_plot <- rbind(compute_props("Women", 0), compute_props("Men", 1))
df_plot$hour     <- (df_plot$interval - 1) * amplitudIntervalo / 60
df_plot          <- df_plot[df_plot$activity != "Not recorded", ]
df_plot$activity <- factor(df_plot$activity,
  levels = c("Personal Care", "Market Work", "Non-Market Work", "Child Care", "Leisure"))
df_plot$gender   <- factor(df_plot$gender, levels = c("Women", "Men"))

act_colours <- c(
  "Personal Care"   = "#4E79A7",
  "Market Work"     = "#F28E2B",
  "Non-Market Work" = "#59A14F",
  "Child Care"      = "#E15759",
  "Leisure"         = "#B07AA1"
)

themePaper <- function(base_size = 11, base_family = "serif") {
  theme_test(base_size = base_size, base_family = base_family) +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.text         = element_text(color = "black", size = rel(1.1)),
      axis.title        = element_text(color = "black", size = rel(1.3)),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.text       = element_text(size = rel(1.1)),
      legend.title      = element_text(size = rel(1.1), face = "bold"),
      legend.position   = "bottom",
      plot.title        = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
      plot.margin       = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      strip.background  = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
      strip.text        = element_text(size = rel(1.2), face = "bold")
    )
}

p <- ggplot(df_plot, aes(x = hour, y = pct, fill = activity)) +
  geom_area(colour = NA, alpha = 0.9, position = "stack") +
  scale_fill_manual(values = act_colours, name = "Activity") +
  scale_x_continuous(breaks = seq(0, 24, by = 4),
                     labels = function(x) sprintf("%02d:00", x %% 24),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 25),
                     labels = function(y) paste0(y, "%"),
                     expand = c(0, 0), limits = c(0, 100)) +
  facet_wrap(~ gender, ncol = 2) +
  labs(x = "Time of day", y = "Share of individuals (%)",
       title = "Distribution of daily activities by gender") +
  themePaper() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

out_path <- file.path(FIGS_DIR, "fig_activity_states.pdf")
ggsave(out_path, plot = p, width = 12, height = 5, device = cairo_pdf)
cat("Saved:", out_path, "\n")
