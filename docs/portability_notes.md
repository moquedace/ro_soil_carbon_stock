# Portability Notes

The current scripts are coherent as a record of the article workflow, but they
are not yet a one-command reproducible pipeline. That is intentional for this
repository pass: running the full workflow would require large geospatial data,
intermediate model objects, and substantial processing time.

## Absolute paths found

The recovered scripts reference several historical working roots:

- `C:/R/ro_soil_carbon`
- `D:/Usuario/cassio/R/ro_soil_carbon`
- `E:/bkp_cassio/R/ro_soil_carbon`
- `C:/R/mestrado/soil_carbon`
- `C:/Usuario/Ganso/mde/dados`
- `C:/R/co2`

These paths should be replaced in a future technical cleanup with a shared
project-root object. A starter file is available at:

- `scripts/_project_setup.R`

## Missing local helper scripts

The recovered ZIP files did not include several local helper scripts:

- `gbm_custom.R`
- `s_tiles.R`
- `s_bioclim.R`
- `s_fmorpho.R`
- `grass_error.R`

The scripts that depend on them are still included because they preserve the
article's workflow logic. Exact rerun reproducibility will require recovering
or rewriting those helpers.

## What is considered sufficient for this repository stage

For the GitHub release aligned with the article, the priority is:

1. Keep only scripts that support the article.
2. Make the workflow order understandable.
3. Document all non-portable assumptions.
4. Point final raster products to Zenodo.

Full execution should only be attempted after data paths, helper scripts, and
package versions are reconstructed.
