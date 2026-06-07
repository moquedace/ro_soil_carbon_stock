# Commit Plan

## Suggested Commit Message

```text
Curate article-support repository
```

## Suggested Commit Body

```text
Organize R scripts recovered from publication-era archives into a
paper-first repository structure.

Add README visual identity, article traceability, data access notes,
portability notes, release metadata, MIT license, and GitHub templates.

Document that this repository supports the published article and final
Zenodo map products, but is not yet a one-command reproducible pipeline.
```

## Files Expected In Commit

- Updated `README.md`
- New repository metadata: `CITATION.cff`, `codemeta.json`, `.zenodo.json`,
  `DESCRIPTION`, `LICENSE`, `.gitattributes`, `.gitignore`
- New GitHub templates under `.github/`
- New documentation under `docs/`
- New data access notes under `data/`
- New organized scripts under `scripts/`
- New README visual banner under `img/readme_banner.svg`

## Pre-Commit Checks Already Run

- `git diff --check`
- JSON validation for `.zenodo.json` and `codemeta.json`
- SVG XML validation for `img/readme_banner.svg`
- Size check: no files larger than 1 MB; repository files total about 0.67 MB
