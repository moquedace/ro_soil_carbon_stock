# Scripts

The scripts are grouped by the publication workflow rather than by the original
working-folder snapshots.

## Execution note

These files were recovered from historical working directories. They preserve
the original analysis logic, but they are not yet fully portable because many
scripts still contain absolute paths such as `C:/R/ro_soil_carbon`,
`D:/Usuario/cassio/R/ro_soil_carbon`, and `E:/bkp_cassio/R/ro_soil_carbon`.

Before rerunning the workflow, define your local project root, recreate the data
folders described in `../data/README.md`, and update path references.
`_project_setup.R` records the intended shared project variables for a future
path-cleanup pass.

## Folders

| Folder | Description |
| --- | --- |
| `01_data_preparation` | Bulk-density estimation, covariate preparation, raster harmonization, and extraction of point-level response/predictor data. |
| `02_modeling` | Model comparison, RFE variable selection, and final Random Forest runs. |
| `03_prediction` | Tiled spatial prediction, mean/CV rasters, quantile rasters, and stock summaries. |
| `04_figures_maps` | Figure production, map layouts, partial dependence, sampling plots, and external product comparisons. |
| `05_legacy_context` | Older scripts kept only to document important methodological steps not fully represented in the final snapshot. |

## Missing external helpers

Some scripts call local helper files that were not present in the recovered ZIP
archives:

- `gbm_custom.R`
- `s_tiles.R`
- `s_bioclim.R`
- `s_fmorpho.R`
- `grass_error.R`

Those helpers should be added in a future pass if exact rerun reproducibility is
required.
