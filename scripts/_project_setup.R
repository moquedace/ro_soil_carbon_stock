# Central project setup for future script portability work.
#
# The publication-era scripts still preserve the original absolute paths used
# during analysis. This file records the intended project structure and can be
# sourced after replacing script-level setwd() calls in a later technical pass.

project_root <- normalizePath(
  Sys.getenv("RO_SOIL_CARBON_ROOT", unset = getwd()),
  winslash = "/",
  mustWork = FALSE
)

paths <- list(
  data = file.path(project_root, "data"),
  covariates = file.path(project_root, "covariaveis"),
  extracted_xy = file.path(project_root, "extract_xy"),
  results = file.path(project_root, "results_ocs_100"),
  tiles = file.path(project_root, "tiles"),
  figures = file.path(project_root, "fig"),
  sheets = file.path(project_root, "sheet"),
  shapefiles = file.path(project_root, "shp"),
  comparisons = file.path(project_root, "comparacao")
)

depths <- c("0_5", "5_15", "15_30", "30_60", "60_100")
depth_labels <- c("0-5", "5-15", "15-30", "30-60", "60-100")
target_crs <- "ESRI:102015"
target_resolution_m <- 30
model_runs <- 100
