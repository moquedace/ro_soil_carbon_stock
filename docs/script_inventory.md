# Script Inventory

The original project had several script snapshots. The current repository keeps
the publication-oriented scripts from `scripts2.zip` and only two contextual
legacy scripts from `scripts_b.zip`. Selection was guided by the article, not by
the chronological history of the local folders.

## Included final scripts

| Folder | Purpose |
| --- | --- |
| `scripts/01_data_preparation` | Bulk-density pedotransfer modeling, covariate preparation, raster harmonization, and extraction of response/predictor tables. |
| `scripts/02_modeling` | Model comparison, RFE variable selection, and final Random Forest runs. |
| `scripts/03_prediction` | Tiled spatial prediction, summary rasters, quantiles, CV, and stock summaries. |
| `scripts/04_figures_maps` | Publication figures, map layouts, model performance, partial dependence, and external-map comparisons. |
| `scripts/05_legacy_context` | Older scripts retained because they document steps not fully represented in the final snapshot. |

## Legacy material reviewed but not imported wholesale

`scripts_b.zip` contained 99 files, including preliminary covariate processing,
HPC/PBS drafts, older Random Forest prototypes, future-scenario scripts,
download checks, experimental downscaling/upscaling utilities, and exploratory
soil/land-use summaries. Most of those files use old project roots such as
`C:/R/mestrado/soil_carbon`, `D:/OneDrive/...`, or early 200 m workflows and
were not part of the final 30 m publication workflow.

`scripts_pc_clara.zip` mostly duplicated `scripts2.zip`. When both existed,
`scripts2.zip` was treated as the more complete source because it included
additional scripts for map layout, total-stock uncertainty, and protected-area
layouts.

## Known reproducibility caveats

- Several scripts still contain absolute paths from the original machines.
- Some helper scripts referenced from outside the repository are not present,
  especially custom utilities such as `gbm_custom.R` and `s_tiles.R`.
- The repository does not include the large raster stack, intermediate model
  objects, or input spreadsheets.
- The final maps are hosted on Zenodo rather than committed to Git.
