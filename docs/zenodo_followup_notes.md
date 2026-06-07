# Zenodo Follow-Up Notes

These notes are for the later Zenodo cleanup pass. The GitHub repository should
be finalized first.

## Current known records

### Final map products

- DOI: `10.5281/zenodo.10543942`
- URL: https://zenodo.org/records/10543942
- Role: final raster map products for mean, Q05, Q95, and CV at 30 m.

### Code/data DOI reported in the article

The article's Data availability section reports:

- DOI: `10.5281/zenodo.10558334`
- Role reported by article: R code and datasets used in the research.

This DOI should be verified during the Zenodo pass and then reconciled with the
cleaned GitHub release. It should not be conflated with the final-map Zenodo
record.

## Recommended order after GitHub cleanup

1. Decide whether the cleaned GitHub repository will be archived on Zenodo as a
   new version or linked to the existing code/data DOI.
2. Ensure the Zenodo metadata uses the article as the canonical citation.
3. Keep final rasters in the map-products record.
4. Keep code provenance, script inventory, and GitHub release metadata aligned.
