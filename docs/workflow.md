# Reproducibility Workflow

This repository is organized around the final workflow described in the
published article. The article is the canonical source; this repository is only
the supporting code and reproducibility layer.

Moquedace, C. M., Baldi, C. G. O., Siqueira, R. G., Cardoso, I. M., de Souza,
E. F. M., Fontes, R. L. F., Francelino, M. R., Gomes, L. C., &
Fernandes-Filho, E. I. (2024). High-resolution mapping of soil carbon stocks in
the western Amazon. Geoderma Regional, 36, e00773.
https://doi.org/10.1016/j.geodrs.2024.e00773

## 1. Soil profile harmonization

The study used 2,914 soil profiles from Rondonia. Soil organic carbon stock was
calculated from SOC content, bulk density, and layer thickness. Missing bulk
density values were estimated with a pedotransfer model.

Relevant scripts:

- `scripts/01_data_preparation/01_pedotransfer_bulk_density.R`
- `scripts/05_legacy_context/legacy_spline_profile_harmonization.R`

## 2. Environmental predictors

The study began with 77 SCORPAN environmental predictors, including climate,
terrain, soil class, geology, land use/land cover, and vegetation indices.
Predictors were harmonized to 30 m resolution in South America Lambert
Conformal Conic (`ESRI:102015`).

Relevant scripts:

- `scripts/01_data_preparation/02_prepare_covariates.R`
- `scripts/01_data_preparation/03_prepare_categorical_covariates.R`
- `scripts/01_data_preparation/04_align_crop_covariates.R`
- `scripts/01_data_preparation/05_google_earth_engine_indices.R`
- `scripts/01_data_preparation/06_bioclim_covariates.R`
- `scripts/01_data_preparation/07_mosaic_morphometric_covariates.R`

## 3. Variable selection and model fitting

Predictors were filtered by Spearman correlation with a cutoff of `|0.95|`.
Recursive feature elimination (RFE) was then used to select parsimonious
predictor subsets. Six machine-learning algorithms were compared, and Random
Forest was used for the final products.

Relevant scripts:

- `scripts/01_data_preparation/09_extract_response_predictors.R`
- `scripts/02_modeling/01_fit_compare_ml_models.R`
- `scripts/02_modeling/02_fit_random_forest_runs.R`

## 4. Spatial prediction and uncertainty

The final Random Forest models were run 100 times for each depth interval.
Outputs were summarized as mean, Q05, Q95, and coefficient of variation.

Relevant scripts:

- `scripts/03_prediction/01_predict_soc_stock_tiles.R`
- `scripts/03_prediction/03_summarize_mean_cv_rasters.R`
- `scripts/03_prediction/04_summarize_quantile_rasters.R`
- `scripts/03_prediction/05_calculate_total_stock_uncertainty.R`
- `scripts/03_prediction/06_summarize_stock_by_strata.R`

## 5. Figures and comparisons

The article compared the regional model with national and global SOC products
and summarized stocks by protected areas and soil classes.

Relevant scripts:

- `scripts/04_figures_maps/01_compare_external_soc_maps.R`
- `scripts/04_figures_maps/02_model_performance_figures.R`
- `scripts/04_figures_maps/03_partial_dependence.R`
- `scripts/04_figures_maps/06_soc_stock_maps.R`
- `scripts/05_legacy_context/legacy_stock_protected_areas_soil_classes.R`
