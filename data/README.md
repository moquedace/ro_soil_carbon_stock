# Data Access

Large raster, vector, model-output, and spreadsheet files are intentionally not
stored in this Git repository.

## Final map products

The final 30 m maps of soil organic carbon stock estimates and uncertainty are
available on Zenodo:

- Zenodo record: https://zenodo.org/records/10543942
- DOI: `10.5281/zenodo.10543942`
- License: Creative Commons Attribution 4.0 International

The Zenodo record includes mean, Q05, Q95, and coefficient-of-variation rasters
for the following depth intervals:

- `0-5 cm`
- `5-15 cm`
- `15-30 cm`
- `30-60 cm`
- `60-100 cm`

## Soil profile data

The article reports that the soil data came from the second approximation of the
Socioeconomic Ecological Zoning of Rondonia (ZEERO), also hosted by SoilData.

- SEDAM technical archive: https://cogeo.sedam.ro.gov.br/acervo-tecnico/
- SoilData DOI reported in the article: `10.60502/SoilData/WI9BIH`

## Expected local working structure

The original scripts use local working directories such as:

```text
covariaveis/
extract_xy/
results_ocs_100/
tiles/
fig/
sheet/
shp/
comparacao/
```

These folders are ignored by Git because they contain large, local, or derived
files. Recreate them locally when rerunning the workflow.
