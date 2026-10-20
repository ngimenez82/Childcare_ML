# Data

The data are **not distributed** in this repository because of redistribution restrictions.

The analysis uses the harmonised release of the [Multinational Time Use Study (MTUS)](https://www.timeuse.org/mtus), which includes the American Time Use Survey (ATUS) for the United States. Researchers can request access directly from the MTUS project.

Place the following raw files in this folder:

| File | Content |
|---|---|
| `MTUS_haf.csv` | Harmonised adult file (individual-level variables) |
| `MTUS_hef1.csv`, `MTUS_hef2.csv` | Harmonised episode files (time-use diaries) |

Running the scripts in `code/` then creates:

| Script | Files created |
|---|---|
| `01_ext_muestra.R` | `IndUSA0723.RData`, `DiariesUSA0723.RData`, `IndMulti07.RData`, `DiariesMulti07.RData` |
| `02_ext_filtrado.R` | `Ind_0723.RData`, `TUS_0723.RData`, `Ind_Multi07.RData`, `TUS_Multi07.RData` |
