# GitHub Release Checklist

Use this checklist before publishing or tagging the cleaned repository.

## Repository Content

- [ ] README presents the article as the canonical reference.
- [ ] `docs/reader_guide.md` gives article readers a fast route through the repository.
- [ ] `docs/article_traceability.md` maps article sections and figures to scripts.
- [ ] `docs/portability_notes.md` documents absolute paths and missing helpers.
- [ ] README visual identity matches `docs/visual_identity.md`.
- [ ] `CHANGELOG.md` and `docs/release_summary.md` are reviewed.
- [ ] `docs/commit_plan.md` is reviewed before committing.
- [ ] Large data files are absent from the Git history.
- [ ] Final raster products are referenced through Zenodo.
- [ ] Legacy scripts are limited to material needed to understand the article workflow.

## Metadata

- [ ] `CITATION.cff` matches the article citation.
- [ ] `codemeta.json` and `.zenodo.json` are reviewed before release/archive.
- [ ] Repository description mentions the article and R code.
- [ ] Repository topics are set, for example: `r`, `soil-carbon`,
      `digital-soil-mapping`, `random-forest`, `rondonia`, `amazon`,
      `geospatial`, `machine-learning`.
- [ ] Code license is declared before advertising software reuse.
- [ ] Zenodo record for code, if used, points back to the GitHub release.
- [ ] The article-reported code/data DOI `10.5281/zenodo.10558334` is verified
      during the later Zenodo pass.

## Suggested GitHub Description

R scripts supporting "High-resolution mapping of soil carbon stocks in the
western Amazon" (Geoderma Regional, 2024).

## Suggested About Links

- Article: https://doi.org/10.1016/j.geodrs.2024.e00773
- Final maps: https://doi.org/10.5281/zenodo.10543942
