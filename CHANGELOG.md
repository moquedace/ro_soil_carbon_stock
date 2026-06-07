# Changelog

All notable repository curation changes are documented here.

## v1.0.0-cleanup - Unreleased

Initial cleaned GitHub-ready version aligned with the published article.

### Added

- Publication-oriented README with visual identity, badges, workflow diagram,
  article-first positioning, and Zenodo map-product links.
- Organized script folders:
  - `scripts/01_data_preparation`
  - `scripts/02_modeling`
  - `scripts/03_prediction`
  - `scripts/04_figures_maps`
  - `scripts/05_legacy_context`
- Article-to-script traceability in `docs/article_traceability.md`.
- Reader guide for article visitors in `docs/reader_guide.md`.
- Data access notes in `data/README.md`.
- Portability notes documenting absolute paths and missing helper scripts.
- Script inventory documenting which recovered script snapshots were retained.
- Visual identity notes and a custom README banner.
- `CITATION.cff`, `codemeta.json`, `.zenodo.json`, `DESCRIPTION`, and MIT
  `LICENSE`.
- GitHub issue and pull-request templates.

### Curated

- Treated `scripts2.zip` as the primary publication-era script source.
- Retained selected legacy scripts from `scripts_b.zip` only when they helped
  document article-relevant methodological steps.
- Excluded large raster, vector, spreadsheet, model-output, and intermediate
  data files from Git.

### Known Limitations

- Scripts preserve publication-era logic but are not yet a turnkey pipeline.
- Several scripts still contain absolute paths from original machines.
- Some local helper scripts referenced by the workflow were not present in the
  recovered archives.
- Full execution requires large geospatial inputs and intermediate products.
