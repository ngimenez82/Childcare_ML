# Work Interruptions and the Child Penalty: The Role of Parental Gender

**David Felipe-Calvo · José Ignacio Giménez-Nadal · José Alberto Molina**
University of Zaragoza · IEDIS

Replication package (code, results and figures) for the paper.

📄 **Paper (PDF):** [`paper/FGM_Work_Interruptions.pdf`](paper/FGM_Work_Interruptions.pdf)

---

## Abstract

This paper examines within-day work interruptions due to childcare as a candidate micro-level mechanism behind the child penalty. Using time-use diaries from the American Time Use Survey (ATUS, 2003–2023), we apply survival analysis combined with Random Survival Forests, Gradient Boosting, SHAP values, and Accumulated Local Effects plots to model the time until a working parent's first interruption. Parental gender is the dominant predictor across all methods: fathers face an instantaneous interruption hazard about 61 per cent below mothers' (HR = 0.389; 95% CI: 0.346–0.438), a gap robust to competing-risks decomposition, alternative censoring thresholds, and a stratified specification. Higher education and income are associated with greater, not lower, interruption risk. A cross-national extension to eight countries confirms the universality of the gender gap, with male hazard ratios below one everywhere (0.388 in the United States to 0.657 in Finland). These findings document a high-frequency margin of gender inequality and provide cross-sectional evidence of a daily mechanism that may contribute to explaining the child penalty.

**Samples:** United States, 36,339 working parents (1,544 interruption events); pooled eight-country sample, 42,344 parents (1,915 events).

---

## Repository Structure

```
Childcare_ML/
├── code/
│   ├── 01_ext_muestra.R        Extracts the US and multi-country samples from MTUS
│   ├── 02_ext_filtrado.R       Filters and builds the analysis datasets
│   └── run_analisis_v2.R       Main analysis: Kaplan-Meier, Cox, RSF, SHAP, ALE, GBM, multi-country
├── output/
│   ├── figures/                All figures of the paper and appendix (PDF)
│   └── output_log_v2.txt       Full log of the last run of the analysis
├── paper/
│   └── FGM_Work_Interruptions.pdf
└── data/                       Not distributed (see data/README.md)
```

## Data

The analysis uses the harmonised release of the [Multinational Time Use Study (MTUS)](https://www.timeuse.org/mtus), which includes the ATUS for the United States (2003–2023) and the multi-country diaries (2007 onwards). The data are **not included** in this repository because of redistribution restrictions; researchers can request access directly from the MTUS project. See `data/README.md` for the files required.

## Reproducing the Results

1. Obtain the MTUS harmonised files (`MTUS_haf.csv`, `MTUS_hef1.csv`, `MTUS_hef2.csv`) and place them in `data/`.
2. Set the paths at the top of each script to the folder where you cloned this repository. They are currently set to `C:/data/Childcare_ML/`:
   - `01_ext_muestra.R`: `HAF_PATH`, `HEF_PATH`, `OUT_DIR`
   - `02_ext_filtrado.R`: `OUT_DIR`
   - `run_analisis_v2.R`: `BASE_DIR`
3. Run the scripts in order:
   1. `code/01_ext_muestra.R`: extracts the US and multi-country samples.
   2. `code/02_ext_filtrado.R`: builds the cleaned analysis datasets.
   3. `code/run_analisis_v2.R`: estimates all models and produces the figures and the log.

Figures are saved to `output/figures/` and the full log to `output/output_log_v2.txt`.

## Methods

`run_analisis_v2.R` implements:

- Survey weights (`ocombwt`) in Kaplan-Meier, Cox and Random Survival Forest models
- Robust standard errors clustered by household
- Interruptions longer than 120 minutes recoded as censored (`CENSOR_THRESHOLD`)
- Cox model stratified by mid-week day (to address the proportional-hazards violation)
- Competing-risks robustness (Fine-Gray subdistribution, pure vs. mixed interruptions)
- SHAP values (`fastshap`) and Accumulated Local Effects plots (`iml`)
- Gradient Boosting survival model (`gbm`, Cox family)
- Model comparison with the C-index and the integrated Brier score (`pec`)
- Multi-country pooled Cox model with country fixed effects, and a forest plot of the male hazard ratio by country

## Output Files

| Files | Content |
|---|---|
| `fig_activity_states.pdf`, `fig_activity_states_multi.pdf` | Activity-state sequences across the workday (US and multi-country) |
| `fig_km_us_*.pdf` | Kaplan-Meier curves for the US by gender, education, income, family status, household size and age of youngest child |
| `fig_schoenfeld.pdf` | Schoenfeld residuals (proportional-hazards test) |
| `fig_rsf_vimp.pdf` | Random Survival Forest variable importance, US |
| `fig_shap_*.pdf` | SHAP importance and dependence plots |
| `fig_ale_us_*.pdf` | Accumulated Local Effects plots, US |
| `fig_gbm_convergence.pdf`, `fig_gbm_importance.pdf` | Gradient Boosting convergence and variable importance |
| `fig_event_rates.pdf` | Interruption rates by country |
| `fig_km_multicountry.pdf` | Kaplan-Meier curves by country |
| `fig_forest_plot.pdf` | Male hazard ratios by country |
| `fig_rsf_<country>_vimp.pdf` | Random Survival Forest variable importance by country |

All files are in `output/figures/`.

## Software

- **R** ≥ 4.6.0
- Packages are loaded with `pacman::p_load()`. Key packages: `data.table`, `tidyverse`, `survival`, `survminer`, `randomForestSRC`, `fastshap`, `iml`, `gbm`, `pec`, `TraMineR`, `ggplot2`

## Citation

> Felipe-Calvo, D., Giménez-Nadal, J. I., & Molina, J. A. *Work Interruptions and the Child Penalty: The Role of Parental Gender*. Working paper, University of Zaragoza.

## Contact

José Ignacio Giménez-Nadal — [ngimenez@unizar.es](mailto:ngimenez@unizar.es)
Department of Economic Analysis, University of Zaragoza
Gran Vía 2, 50005 Zaragoza, Spain
