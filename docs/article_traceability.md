# Article Traceability

The published article is the canonical source for the scientific narrative,
methods, results, and interpretation. This repository and the Zenodo record are
supporting materials: they should help readers inspect the code and access data
products without changing the article's story.

## Canonical Article

Moquedace, C. M., Baldi, C. G. O., Siqueira, R. G., Cardoso, I. M., de Souza,
E. F. M., Fontes, R. L. F., Francelino, M. R., Gomes, L. C., & Fernandes-Filho,
E. I. (2024). High-resolution mapping of soil carbon stocks in the western
Amazon. Geoderma Regional, 36, e00773.
https://doi.org/10.1016/j.geodrs.2024.e00773

## Method-to-Script Map

| Article component | Canonical content | Supporting scripts |
| --- | --- | --- |
| Section 2.1, Soil data | ZEERO/SoilData profiles, SOC stock calculation, bulk-density pedotransfer model, spline harmonization to GlobalSoilMap depths. | `scripts/01_data_preparation/01_pedotransfer_bulk_density.R`; `scripts/05_legacy_context/legacy_spline_profile_harmonization.R` |
| Section 2.2, Environmental predictors | 77 SCORPAN predictors; NASADEM terrain derivatives; WorldClim bioclimatic variables; geology, soil class, LULC, and vegetation indices; 30 m harmonization in `ESRI:102015`. | `scripts/01_data_preparation/02_prepare_covariates.R`; `03_prepare_categorical_covariates.R`; `04_align_crop_covariates.R`; `05_google_earth_engine_indices.R`; `06_bioclim_covariates.R`; `07_mosaic_morphometric_covariates.R` |
| Section 2.3, Variable selection | Spearman correlation cutoff `|0.95|`; RFE; 2% maximum performance-loss criterion for parsimony. | `scripts/02_modeling/01_fit_compare_ml_models.R`; `scripts/02_modeling/02_fit_random_forest_runs.R` |
| Section 2.4, Machine learning | Cubist, GBM, GLMNET, KNN, Random Forest, and SVM radial model comparison; 100 runs per depth. | `scripts/02_modeling/01_fit_compare_ml_models.R` |
| Final prediction and uncertainty | Random Forest final maps summarized as mean, Q05, Q95, and coefficient of variation. | `scripts/03_prediction/01_predict_soc_stock_tiles.R`; `03_summarize_mean_cv_rasters.R`; `04_summarize_quantile_rasters.R`; `05_calculate_total_stock_uncertainty.R` |

## Result-to-Script Map

| Article result | Scientific role | Supporting scripts/products |
| --- | --- | --- |
| Fig. 1 | Study area, soil classes, and soil profile distribution. | `scripts/04_figures_maps/09_location_map.R`; `scripts/04_figures_maps/05_sampling_figures.R` |
| Fig. 2 | Modeling workflow diagram. | `docs/workflow.md`; modeling scripts in `scripts/02_modeling/` |
| Fig. 3 | SOC stock frequency distributions by depth. | `scripts/04_figures_maps/04_stock_density_plot.R` |
| Fig. 4 | Performance of tested algorithms by depth. | `scripts/04_figures_maps/02_model_performance_figures.R` |
| Fig. 5 | Predictor importance and selected-predictor frequency in Random Forest models. | `scripts/04_figures_maps/02_model_performance_figures.R`; `scripts/04_figures_maps/03_partial_dependence.R` |
| Fig. 6 | Mean SOC stock maps. | `scripts/04_figures_maps/06_soc_stock_maps.R`; Zenodo mean rasters |
| Fig. 7 | Lower uncertainty range, Q05 maps. | `scripts/03_prediction/04_summarize_quantile_rasters.R`; Zenodo Q05 rasters |
| Fig. 8 | Upper uncertainty range, Q95 maps. | `scripts/03_prediction/04_summarize_quantile_rasters.R`; Zenodo Q95 rasters |
| Fig. 9 | Coefficient-of-variation maps. | `scripts/03_prediction/03_summarize_mean_cv_rasters.R`; Zenodo CV rasters |
| Fig. 10 | SOC stock stratified by soil classes. | `scripts/03_prediction/06_summarize_stock_by_strata.R`; `scripts/05_legacy_context/legacy_stock_protected_areas_soil_classes.R` |
| Fig. 11 | SOC stock in protected and unprotected areas. | `scripts/03_prediction/06_summarize_stock_by_strata.R`; `scripts/04_figures_maps/08_protected_area_map_layout.R` |
| Fig. 12 and Fig. 13 | Comparison with regional, national, and global SOC maps. | `scripts/04_figures_maps/01_compare_external_soc_maps.R` |

## Repository Boundary

The repository should contain:

- The cleaned script set that supports the article's methods and results.
- Documentation explaining data access, workflow, and known caveats.
- Visual identity and citation metadata.

The repository should not contain:

- Large rasters, shapefiles, model objects, or intermediate local products.
- Obsolete exploratory scripts that distract from the article's final workflow.
- A second interpretation of the results that conflicts with the article.

Large final products belong in Zenodo. The GitHub repository should point to
Zenodo and make the code provenance understandable.
